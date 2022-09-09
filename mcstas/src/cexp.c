/*******************************************************************************
*
* McStas, neutron ray-tracing package
*         Copyright 1997-2002, All rights reserved
*         Risoe National Laboratory, Roskilde, Denmark
*         Institut Laue Langevin, Grenoble, France
*
* Kernel: cexp.c
*
* %Identification
* Written by: K.N.
* Date: Aug  7, 1997
* Origin: Risoe
* Release: McStas 1.6
* Version: $Revision$
*
* Handle expressions used as arguments to components etc.
*
*******************************************************************************/

#include <stdarg.h>
#include <stdio.h>
#include <string.h>

#include "mccode.h"

/* The internal structure implementing a C expression. */
struct cexp
  {
    char *s;		    /* String representation */
    int isvalue;	    /* True if identifier or string/number constant */
    int lineno;		    /* Starting line number, or zero */
  };

/* Create an expression from a string representing a value (either identifier,
   constant number, or constant string). */
static CExp
mkvalueexp(char *s)
{
  CExp e;
  palloc(e);
  e->s = s;
  e->isvalue = 1;
  e->lineno = 0;		/* Initially no line number set */
  return e;
}

/* Create an expression from a string not representing a value. */
static CExp
mknonvalueexp(char *s)
{
  CExp e;
  palloc(e);
  e->s = s;
  e->isvalue = 0;
  e->lineno = 0;		/* Initially no line number set */
  return e;
}

/*******************************************************************************
* Handle identifiers used as arguments to components.
* There are two types of identifiers: normal and extern. Normal identifiers
* refer to formal arguments of the instrument, and are always of type
* double. These can be varied eg. to perform a scan. Extern identifiers refer
* to user-supplied C identifiers that is typically put in the declaration and
* initialization sections of the instrument definition.
*
* The final version will distinguish (using a union) between these two types,
* and will maintain a mapping from formal parameters of the instrument to
* generated names in the generated simulation (the extern names must be
* copied unchanged). But in this first version a simpler scheme is used: all
* identifier expressions are simply strings, and all normal identifiers refer
* to instrument parameters.
*******************************************************************************/

CExp
exp_id(char *id)  /* used in e.g. cogen:cogen_comp_init_par AND splits*/
{
  return mkvalueexp(str_cat("_instrument_var._parameters.", id, NULL));
}

CExp
exp_extern_id(char *id)
{
  return mkvalueexp(str_dup(id));
}

CExp
exp_number(char *n)
{
  return mkvalueexp(str_dup(n));
}

CExp
exp_string(char *s)
{
  char *quoted, *result;
  quoted = str_quote(s);
  result =  str_cat("\"", s, "\"", NULL);
  str_free(quoted);
  return mkvalueexp(result);
}

CExp
exp_ctoken(char *s)
{
  return mknonvalueexp(str_dup(s));
}

CExp
exp_compound(int n, ...)
{
  char *result, *new;
  CExp e;
  va_list ap;
  char *separator = "";		/* Token separator, initially empty */

  va_start(ap, n);
  result = str_dup("");
  while(n-- > 0)
  {
    e = va_arg(ap, CExp);
    new = str_cat(result, separator, e->s, NULL);
    str_free(result);
    result = new;
    separator = " ";		/* Now use space separator for rest. */
  }
  return mknonvalueexp(result);
}

void
exp_free(CExp e)
{
  str_free(e->s);
  memfree(e);
}

char *
exp_tostring(CExp e)
{
  char *s = e->s;
  if(s == NULL)
  {
    s = "";
    debugn((DEBUG_HIGH, "exp_tostring(): NULL cexp received.\n"));
  }
  return str_dup(s);
}

void
exp_fprint(FILE *f, CExp e)
{
  fputs(e->s, f);
}

int
exp_isvalue(CExp e)
{
  return e->isvalue;
}

void
exp_setlineno(CExp e, int n)
{
  e->lineno = n;
}

int
exp_getlineno(CExp e)
{
  return e->lineno;
}


static int identify_id(CExp e, char ** id){
  int signal = 0; //1:=, 2:*=, 3:+=, 4:-=, 5:/=, 6:[id]++, 7:[id]--, 8:++[id], 9:--[id]
  // look for an assignment '='
  char* pos = strchr(e->s, '=');
  if (pos){
    signal = 1; // a 'standard' asignment -- grab the identifier
    // copy the string up to the =, this *should be* the identifier
    size_t len = pos - e->s;
    *id = (char *)malloc(len + 1);
    memcpy(id, e->s, len);
    *id[len] = '\0';
    // ... we may have caught the {*+-/} of *=, +=, -=, /= !
    if (*id[len-1] == '*') signal = 2;
    if (*id[len-1] == '+') signal = 3;
    if (*id[len-1] == '-') signal = 4;
    if (*id[len-1] == '/') signal = 5;
    if (signal > 1) *id[len-1] = '\0';
  } else {
    // [id]++, [id]--, ++[id], --[id]
    size_t slen = strlen(e->s);
    if (slen > 2 && (
                        (e->s[0] == '-' && e->s[1] == '-') || (e->s[0] == '+' && e->s[1] == '+')
                        || (e->s[slen-2] == '-' && e->s[slen-1] == '-')
                        || (e->s[slen-2] == '+' && e->s[slen-1] == '+')
                            )){
      int is_pre = (e->s[0] == '-' || e->s[0] == '+') ? 1 : 0;
      char* start = is_pre ? e->s+2 : e->s;
      char* stop = is_pre ? e->s + slen : e->s + slen - 2;
      *id = malloc((stop-start) + 1);
      memcpy(id, start, (stop-start));
      *id[stop-start] = '\n';
      signal = 6 + (is_pre ? 2 : 1) * (((is_pre ? e->s[0] : e->s[slen-1]) == '+') ? 0 : 1);
    }
  }
  // if [id] is a *real* identifier it can not contain any spaces or any of {*+-/}
  if (!strpbrk(*id, "*+-/ ")) signal = 0;
  return signal;
}

static char * substr_in_str(char* str, char* substr){
  //if (str ==NULL || substr == NULL) return NULL;
  char * pos = str;
  while (*pos != '\0'){
    size_t n = strspn(pos, substr);
    if (n) {
      int found = 1;
      for (size_t i = 0; i < strlen(substr); ++i) if (pos[i] != substr[i]) found=0;
      if (found) break;
    }
    pos++;
  }
  return *pos == '\0' ? NULL : pos;
}

int exp_issimpleassignment(CExp e){
  /*[id]\s*=\s*{value} | [id]\s*=\s[id]{*+-/}{value} */
  if (e->isvalue) return 0; // values can not be assignments
  char * id = NULL;
  int has_id = identify_id(e, &id);
  if (has_id && has_id < 6) {
    // look for *other* [id]s to the right of the assignment
    // *simple* assignments can not refer to any non-const values
    char* pos = strchr(e->s, '=');
    if (!pos) {
      print_error("Impossible error condition!");
      exit(-1);
    }
    while (*pos != '\0'){
      pos++; // increment first to avoid the '=' sign
      size_t n = strspn(pos, id);
      if (n && n != strlen(id)){
        has_id = 0;
        break;
      }
      if (n) {
        for (int i=0; i<strlen(id); ++i) if (pos[i] != id[i]) {
            has_id = 0;
            break;
          }
        pos += n;
      }
      if (!strchr("0123456789.*+-/() ", *pos)){
        print_error("Simple assignment does not allow reference to other identifiers\n");
        exit(-1);
      }
    }
  }
  if (id) free(id);
  return has_id ? 1 : 0;
}

static int evaluate_string(char* s){
  printf("Not actually implemented yet :(\n");
  return 0;
}

int exp_newvalue(CExp e, int old_value){
  if (e->isvalue){
    print_error("Values can not take a new value after assignment!");
  }
  char* id = 0;
  int signal = identify_id(e, &id);
  if (signal == 6 || signal == 8) return old_value+1;
  if (signal == 7 || signal == 9) return old_value-1;
  // build a new string representation with the old value inserted
  // start by copying the right-hand-side
  char * pos = strchr(e->s, '=');
  char src_buffer[1024], des_buffer[1024];
  if (strlen(e->s) - (pos - e->s) > 1023){
    print_error("Insufficient buffer space to store the right-hand-size");
  }
  strcpy(src_buffer, pos+1);
  // find any instances of [id] in the right-hand side:
  pos = substr_in_str(src_buffer, id);
  // setup pointers to the two buffers
  char * src = src_buffer, *des = des_buffer;
  while (pos && *pos != '\0') {
    // overwrite the first character of the id
    *pos = '\0';
    // so that sprintf will only copy up to the character before,
    // we can insert the old value, then append the rest of the string
    sprintf(des, "%s %d %s", src, old_value, pos + strlen(id));
    // move points around
    char * tmp = src;
    src = des;
    des=  tmp;
    // and search again for the [id] in the shortened rhs
    pos = substr_in_str(src, id);
  }
  // When pos is finally NULL, then char array starting at src is [id]-free
  int rhs = evaluate_string(src);

  switch(signal){
  case 1: return rhs;
  case 2: return old_value * rhs;
  case 3: return old_value + rhs;
  case 4: return old_value - rhs;
  case 5: return old_value / rhs;
  default:
    print_error("Unhandled expression type %d\n", signal);
    exit(-1);
  }
}