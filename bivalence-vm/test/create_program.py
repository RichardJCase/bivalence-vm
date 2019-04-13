#!/usr/bin/python

import os

ops = [
    "UNINIT",
    "POKE_SR",
    "POKE_UR",
    "POKE_FR",
    "PEEK_SR",
    "PEEK_UR",
    "PEEK_FR",
    "ADD_SR",
    "ADD_UR",
    "ADD_FR",
    "SUB_SR",
    "SUB_UR",
    "SUB_FR",
    "MUL_SR",
    "MUL_UR",
    "MUL_FR",
    "DIV_SR",
    "DIV_UR",
    "DIV_FR",
    "AND_SR",
    "AND_UR",
    "OR_SR",
    "OR_UR",
    "XOR_SR",
    "XOR_UR",
    "MOV_SR_SR",
    "MOV_SR_UR",
    "MOV_SR_FR",
    "MOV_UR_SR",
    "MOV_UR_UR",
    "MOV_UR_FR",
    "MOV_FR_SR",
    "MOV_FR_UR",
    "MOV_FR_FR",
    "MOV_SR_N",
    "MOV_UR_N",
    "MOV_FR_N",
    "CALL_ADDR",
    "CALL_UR",
    "JMP_ADDR",
    "JMP_UR",
    "RET",
    "STOP",
    "LOAD",
    "SYM",
    "CCALL"
]


def bytes(instructions):
    ret = ""
    for instrcution in instructions:
        ret += chr(ops.index(instrcution))

    return ret

open("test.b", "w").write("\0" * int(os.environ["PAGE_SIZE"]) * (int(os.environ["NUM_PAGE"]) + 1))

open("assign.b", "w").write(bytes(["MOV_SR_SR"]))

open("math.b", "w").write("")
open("mem.b", "w").write("")
open("logic.b", "w").write("")
open("library.b", "w").write("")
