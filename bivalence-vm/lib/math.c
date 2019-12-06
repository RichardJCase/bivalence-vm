#include "math.h"

bool and(bool p, bool q){
  return p && q;
}

bool or(bool p, bool q){
  return p || q;
}

bool not(bool p){
  return !p;
}

bool implies(bool p, bool q){
  return !p || (p && q);
}


bool forall(pointer set_ptr, lemma func, pointer out_set_ptr){
  return false;
}

bool all(pointer set_ptr){
  return false;
}

bool exists(pointer set_ptr, lemma func){
  return false;
}

bool single(pointer set_ptr, lemma func){
  return false;
}

bool any(pointer set_ptr){
  return false;
}


bool union_(pointer set_ptr, pointer comp_set_ptr){
  return false;
}

bool intersection(pointer set_ptr, pointer comp_set_ptr){
  return false;
}


bool parseNat(const char *str, Nat *out_nat){
  return false;
}

bool showNat(Nat nat, char *out_str){
  return false;
}

bool add(Nat x, Nat y){
  return false;
}

bool sub(Nat x, Nat y){
  return false;
}

bool mul(Nat x, Nat y){
  return false;
}

bool div(Nat x, Nat y){
  return false;
}

bool mod(Nat x, Nat y){
  return false;
}
