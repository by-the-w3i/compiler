#! /usr/bin/env python3
import ply.lex as lex
import sys

if __name__ == "__main__":
    lexer = lex.lex()
    data = sys.stdin.read()
