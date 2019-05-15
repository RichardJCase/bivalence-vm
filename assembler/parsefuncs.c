#include "strings.h"
#include "parsefuncs.h"

typedef enum {
  none,
  sr,
  ur,
  fr,
  addr,
  lit
} parse_type;

void poke_parse(const char *line){
  token token;
  next_token(&line, token);

  puts(token);
}

void peek_parse(const char *line){unused(line);}
void add_parse(const char *line){unused(line);}
void sub_parse(const char *line){unused(line);}
void mul_parse(const char *line){unused(line);}
void div_parse(const char *line){unused(line);}
void and_parse(const char *line){unused(line);}
void or_parse(const char *line){unused(line);}
void xor_parse(const char *line){unused(line);}
void mov_parse(const char *line){unused(line);}
void call_parse(const char *line){unused(line);}
void jmp_parse(const char *line){unused(line);}
void ret_parse(const char *line){unused(line);}
void stop_parse(const char *line){unused(line);}
