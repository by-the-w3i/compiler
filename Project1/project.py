#! /usr/bin/env python3
# btw3i 2017.9.10
import ply.lex as lex
import sys

tokens = ('TYPE', 'COMMAND_PRINT', 'COMMAND_RANDOM', 'ID', 'VAL_LITERAL', \
'CHAR_LITERAL', 'STRING_LITERAL', 'ASCII_CHAR', 'ASSIGN_ADD', 'ASSIGN_SUB', \
'ASSIGN_MULT', 'ASSIGN_DIV', 'COMP_EQU', 'COMP_NEQU', 'COMP_LESS', 'COMP_LTE', \
'COMP_GTR', 'COMP_GTE', 'BOOL_AND', 'BOOL_OR', 'WHITESPACE', 'COMMENT', 'UNKNOWN')

def t_TYPE(t):
    r'(val|char|string)\b'
    return t

def t_COMMAND_PRINT(t):
    r'print\b'
    return t

def t_COMMAND_RANDOM(t):
    r'random\b'
    return t

def t_ID(t):
    r'[a-zA-Z_]\w*'
    return t

def t_VAL_LITERAL(t):
    r'([\d]*\.)?[\d]+'
    return t

def t_CHAR_LITERAL(t):
    r"'(\\n|\\\\|\\t|\\'|.)'"
    return t

def t_STRING_LITERAL(t):
    r'"[^"\n]*"'
    return t

def t_ASSIGN_ADD(t):
    r'\+='
    return t

def t_ASSIGN_SUB(t):
    r'-='
    return t

def t_ASSIGN_MULT(t):
    r'\*='
    return t

def t_ASSIGN_DIV(t):
    r'/='
    return t

def t_COMP_EQU(t):
    r'=='
    return t

def t_COMP_NEQU(t):
    r'!='
    return t

def t_COMP_LTE(t):
    r'<='
    return t

def t_COMP_LESS(t):
    r'<'
    return t

def t_COMP_GTE(t):
    r'>='
    return t

def t_COMP_GTR(t):
    r'>'
    return t

def t_ASCII_CHAR(t):
    r'[+-/()=,.{};\[\]\*]'
    return t

def t_BOOL_AND(t):
    r'&&'
    return t

def t_BOOL_OR(t):
    r'\|\|'
    return t

def t_WHITESPACE(t):
    r'[ \t\n]'
    return t

def t_COMMENT(t):
    r'\#.*'
    return t

def t_UNKNOWN(t):
    r'.'
    return t

def t_error(t):
    pass


if __name__ == "__main__":
    lexer = lex.lex()
    data = sys.stdin.read()

    lexer.input(data)
    cnt = 1
    while True:
        tok = lexer.token()
        if not tok:
            break # no more input
        if tok.type == 'UNKNOWN':
            print('Unknown token on line ' + str(cnt) + ': ' + tok.value)
            sys.exit(0)
        if tok.type == 'WHITESPACE':
            if tok.value == '\n':
                cnt += 1
        elif tok.type != 'COMMENT':
            if tok.type == "CHAR_LITERAL":
                print(tok.type + ': ' + tok.value)
    print("Line Count:", cnt)
