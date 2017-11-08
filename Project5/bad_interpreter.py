#! /usr/bin/env python3
"""
Do Not Modify This File
"""

import sys
import random
import numbers
import operator
import copy
import argparse

import ply.lex as lex
import ply.yacc as yacc


DEBUG_MODE = False

SYMBOL_TABLE = {}
INSTRUCTIONS = []
SCALAR_STACK = []
ARRAY_STACK = []
OUTPUT = ""


class Var:
    def __init__(self, memory_location):
        self.memory_location = memory_location
    def __str__(self):
        return "{}({})".format(type(self), self.memory_location)
    __repr__ = __str__

    def __eq__(self, other):
        return ((type(self) == type(other))
                and (self.memory_location == other.memory_location))
    def __hash__(self):
        return hash(self.memory_location)

class ScalarVar(Var):
    pass

class ArrayVar(Var):
    pass

class Label:
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return "Label({})".format(self.name)
    __repr__ = __str__
    def __eq__(self, other):
        return ((type(self) == type(other))
                and (self.name == other.name))
    def __hash__(self):
        return hash(self.name)


commands = [
    "val_copy",
    "add",
    "sub",
    "mult",
    "div",
    "test_less",
    "test_gtr",
    "test_equ",
    "test_nequ",
    "test_gte",
    "test_lte",
    "jump",
    "jump_if_0",
    "jump_if_n0",
    "random",
    "out_val",
    "out_char",
    "nop",
    "push",
    "pop",
    "ar_get_idx",
    "ar_set_idx",
    "ar_get_size",
    "ar_set_size",
    "ar_copy",
    "ar_push",
    "ar_pop"
]

literals = ":"

tokens = [
    'ID',
    'VAL_LITERAL',
    'CHAR_LITERAL',
    'SCALAR_VAR',
    'ARRAY_VAR',
    ] + [command.upper() for command in commands]


def t_SCALAR_VAR(t):
    r's\d+'
    t.value = ScalarVar(t.value)
    return t


def t_ARRAY_VAR(t):
    r'a\d+'
    t.value = ArrayVar(t.value)
    return t


def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)


def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    if t.value in commands:
        t.type = t.value.upper()
    return t


def t_VAL_LITERAL(t):
    r'-?((\d+)(\.\d+)?)|(\.\d+)'
    float_value = float(t.value)
    if float_value.is_integer():
        t.value = int(float_value)
    else:
        t.value = float_value
    return t


def t_CHAR_LITERAL(t):
    r"'([^\\']|\\n|\\t|\\'|\\\\)'"
    return t


def t_WHITESPACE(t):
    r'[ \t]'
    pass


def t_COMMENT(t):
    r'\#[^\n]*'
    pass


def t_error(t):
    error = "Unknown token on line {}: {}".format(t.lexer.lineno, t.value[0])
    raise SyntaxError(error)

def p_program(p):
    """
    program : statements
    """


def p_statements_empty(p):
    """
    statements :
    """


def p_statements_nonempty(p):
    """
    statements : statements statement
    """
    INSTRUCTIONS.append(p[2])

def p_val_copy(p):
    """
    statement : VAL_COPY value SCALAR_VAR
    """
    p[0] = p[1:]

def p_add(p):
    """
    statement : ADD value value SCALAR_VAR
    """
    p[0] = p[1:]


def p_sub(p):
    """
    statement : SUB value value SCALAR_VAR
    """
    p[0] = p[1:]

def p_mult(p):
    """
    statement : MULT value value SCALAR_VAR
    """
    p[0] = p[1:]

def p_div(p):
    """
    statement : DIV value value SCALAR_VAR
    """
    p[0] = p[1:]

def p_test_less(p):
    """
    statement : TEST_LESS value value SCALAR_VAR
    """
    p[0] = p[1:]

def p_test_gtr(p):
    """
    statement : TEST_GTR value value SCALAR_VAR
    """
    p[0] = p[1:]

def p_test_equ(p):
    """
    statement : TEST_EQU value value SCALAR_VAR
    """
    p[0] = p[1:]

def p_test_nequ(p):
    """
    statement : TEST_NEQU value value SCALAR_VAR
    """
    p[0] = p[1:]

def p_test_gte(p):
    """
    statement : TEST_GTE value value SCALAR_VAR
    """
    p[0] = p[1:]

def p_test_lte(p):
    """
    statement : TEST_LTE value value SCALAR_VAR
    """
    p[0] = p[1:]


def p_label_statement(p):
    """
    statement : ID ':'
    """
    p[0] = [Label(p[1])]

def p_label_value(p):
    """
    value : ID
    """
    p[0] = Label(p[1])

def p_jump(p):
    """
    statement : JUMP value
    """
    p[0] = p[1:]

def p_jump_if_0(p):
    """
    statement : JUMP_IF_0 value value
    """
    p[0] = p[1:]

def p_jump_if_n0(p):
    """
    statement : JUMP_IF_N0 value value
    """
    p[0] = p[1:]

def p_random(p):
    """
    statement : RANDOM value SCALAR_VAR
    """
    p[0] = p[1:]

def p_out_val(p):
    """
    statement : OUT_VAL value
    """
    p[0] = p[1:]

def p_out_char(p):
    """
    statement : OUT_CHAR value
    """
    p[0] = p[1:]

def p_nop(p):
    """
    statement : NOP
    """
    p[0] = p[1:]

def p_push(p):
    """
    statement : PUSH value
    """
    p[0] = p[1:]

def p_pop(p):
    """
    statement : POP SCALAR_VAR
    """
    p[0] = p[1:]

def p_ar_get_idx(p):
    """
    statement : AR_GET_IDX ARRAY_VAR value SCALAR_VAR
    """
    p[0] = p[1:]

def p_ar_set_idx(p):
    """
    statement : AR_SET_IDX ARRAY_VAR value value
    """
    p[0] = p[1:]

def p_ar_get_size(p):
    """
    statement : AR_GET_SIZE ARRAY_VAR SCALAR_VAR
    """
    p[0] = p[1:]

def p_ar_set_size(p):
    """
    statement : AR_SET_SIZE ARRAY_VAR value
    """
    p[0] = p[1:]

def p_ar_copy(p):
    """
    statement : AR_COPY ARRAY_VAR ARRAY_VAR
    """
    p[0] = p[1:]

def p_ar_push(p):
    """
    statement : AR_PUSH ARRAY_VAR ARRAY_VAR
    """
    p[0] = p[1:]

def p_ar_pop(p):
    """
    statement : AR_POP ARRAY_VAR ARRAY_VAR
    """
    p[0] = p[1:]


def p_char_literal(p):
    """
    value : CHAR_LITERAL
    """
    char_str = p[1]
    assert char_str[0] == "'"
    assert char_str[-1] == "'"
    char_str_without_quotes = char_str[1:-1]
    if len(char_str_without_quotes) == 1:
        p[0] = ord(char_str_without_quotes)
        return

    assert len(char_str_without_quotes) == 2
    assert char_str_without_quotes[0] == "\\"
    escaped_char = char_str_without_quotes[1]
    mapping = {
        'n': '\n',
        '\\': '\\',
        't': '\t',
        "'": "'"
    }
    assert escaped_char in mapping
    p[0] = ord(mapping[escaped_char])

def p_val_literal(p):
    """
    value : VAL_LITERAL
    """
    p[0] = p[1]

def p_scalar_var_value(p):
    """
    value : SCALAR_VAR
    """
    p[0] = p[1]

def p_error(p):
    line = 0 if p is None else p.lineno
    output = "ERROR(line {}): syntax error({})".format(line, p)
    raise SyntaxError(output)

def reset():
    global SYMBOL_TABLE
    SYMBOL_TABLE.clear()
    global INSTRUCTIONS
    INSTRUCTIONS = []
    global SCALAR_STACK
    SCALAR_STACK = []
    global ARRAY_STACK
    ARRAY_STACK = []
    global OUTPUT
    OUTPUT = ""

    random.seed(0)

def lookup_value(value):
    if isinstance(value, numbers.Number):
        return value
    if isinstance(value, ScalarVar):
        return SYMBOL_TABLE[value]
    if isinstance(value, ArrayVar):
        if value not in SYMBOL_TABLE:
            SYMBOL_TABLE[value] = []
        return SYMBOL_TABLE[value]
    if isinstance(value, Label):
        return SYMBOL_TABLE[value]
    raise NotImplementedError(value)


def execute_bad_instruction(instruction):
    if DEBUG_MODE:
        print("Executing instruction: {}".format(instruction))
    command = instruction[0]
    binary_arithmetic_commands = {
        "add": operator.add,
        "sub": operator.sub,
        "div": operator.truediv,
        "mult": operator.mul
    }
    binary_logic_commands = {
        "test_less": operator.lt,
        "test_gtr": operator.gt,
        "test_equ": operator.eq,
        "test_nequ": operator.ne,
        "test_gte": operator.ge,
        "test_lte": operator.le
    }
    if command == "val_copy":
        a = lookup_value(instruction[1])
        SYMBOL_TABLE[instruction[2]] = a

    if command in binary_arithmetic_commands:
        a = lookup_value(instruction[1])
        b = lookup_value(instruction[2])
        result = binary_arithmetic_commands[command](a, b)
        SYMBOL_TABLE[instruction[3]] = result
        return
    if command in binary_logic_commands:
        a = lookup_value(instruction[1])
        b = lookup_value(instruction[2])
        is_true = binary_logic_commands[command](a, b)
        if is_true:
            result = 1
        else:
            result = 0
        SYMBOL_TABLE[instruction[3]] = result
        return

    if isinstance(command, Label):
        return
    if command == "jump":
        a = lookup_value(instruction[1])
        return a
    if command == "jump_if_0":
        a = lookup_value(instruction[1])
        b = lookup_value(instruction[2])
        if a == 0:
            return b
        else:
            return None
    if command == "jump_if_n0":
        a = lookup_value(instruction[1])
        b = lookup_value(instruction[2])
        if a != 0:
            return b
        else:
            return None

    if command == "random":
        a = lookup_value(instruction[1])
        result = random.randrange(int(a))
        SYMBOL_TABLE[instruction[2]] = result

    global OUTPUT
    if command == "out_val":
        a = lookup_value(instruction[1])
        OUTPUT += str(a)
        return
    if command == "out_char":
        a = lookup_value(instruction[1])
        result = chr(a)
        OUTPUT += result
        return

    if command == "nop":
        return

    if command == "push":
        a = lookup_value(instruction[1])
        assert isinstance(a, numbers.Number)
        SCALAR_STACK.append(copy.deepcopy(a))
    if command == "pop":
        a = SCALAR_STACK.pop()
        SYMBOL_TABLE[instruction[1]] = a

    if command == "ar_get_idx":
        array = lookup_value(instruction[1])
        idx = lookup_value(instruction[2])
        result = array[int(idx)]
        SYMBOL_TABLE[instruction[3]] = result
    if command == "ar_set_idx":
        array = lookup_value(instruction[1])
        idx = lookup_value(instruction[2])
        value = lookup_value(instruction[3])
        array[int(idx)] = value
    if command == "ar_get_size":
        array = lookup_value(instruction[1])
        result = len(array)
        SYMBOL_TABLE[instruction[2]] = result
    if command == "ar_set_size":
        array = lookup_value(instruction[1])
        new_size = lookup_value(instruction[2])
        while new_size < len(array):
            array.pop()
        while new_size > len(array):
            array.append(0)
    if command == "ar_copy":
        source_array = lookup_value(instruction[1])
        SYMBOL_TABLE[instruction[2]] = copy.deepcopy(source_array)

    if command == "ar_push":
        a = lookup_value(instruction[1])
        assert isinstance(a, list)
        ARRAY_STACK.append(copy.deepcopy(a))
    if command == "ar_pop":
        a = ARRAY_STACK.pop()
        SYMBOL_TABLE[instruction[1]] = a


def add_label_locations_to_symbol_table(INSTRUCTIONS):
    for i, instruction in enumerate(INSTRUCTIONS):
        first_instruction = instruction[0]
        if isinstance(first_instruction, Label):
            assert first_instruction not in SYMBOL_TABLE
            SYMBOL_TABLE[first_instruction] = i

def execute_bad_instructions(instructions):
    add_label_locations_to_symbol_table(instructions)
    instruction_pointer = 0
    while instruction_pointer < len(instructions):
        instruction = instructions[instruction_pointer]
        new_ip = execute_bad_instruction(instruction)
        if new_ip is not None:
            instruction_pointer = new_ip
        else:
            instruction_pointer += 1


def run_bad_code_from_string(input_):
    reset()

    lexer = lex.lex()
    parser = yacc.yacc(debug=False, errorlog=yacc.NullLogger())
    parser.parse(input_, lexer=lexer)

    execute_bad_instructions(INSTRUCTIONS)
    global OUTPUT
    return OUTPUT

def main():
    parser = argparse.ArgumentParser(description="""Takes BAD code from stdin,
    runs it, and prints output to stdout.""")
    args = parser.parse_args()

    source = sys.stdin.read()
    print(run_bad_code_from_string(source))

if __name__ == "__main__":
    main()
