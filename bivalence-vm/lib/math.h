#pragma once

typedef Nat pointer;

bool and(bool p, bool q);
bool or(bool p, bool q);
bool not(bool p);
bool implies(bool p, bool q);

bool forall(const pointer set_ptr, const lemma func, pointer out_set_ptr);
bool all(const pointer set_ptr);
bool exists(const pointer set_ptr, const lemma func);
bool single(const pointer set_ptr, const lemma func);
bool any(const pointer set_ptr, const lemma func);

bool union_(const pointer set_ptr, const pointer comp_set_ptr);
bool intersection(const pointer set_ptr, const pointer comp_set_ptr);

bool parseNat(const char *str, Nat *out_nat);
bool showNat(const Nat nat, char *out_str);
bool add(const const Nat x, Nat y, Nat *out_nat);
bool sub(const Nat x, const Nat y, Nat *out_nat);
bool mul(const Nat x, const Nat y, Nat *out_nat);
bool div(const Nat x, const Nat y, Nat *out_nat);
bool mod(const Nat x, const Nat y, Nat *out_nat);
bool equal(const Nat x, const Nat y);
