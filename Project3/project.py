#! /usr/bin/env python3
import sys
import ply.lex as lex
import ply.yacc as yacc

tokens = ('TYPE', 'COMMAND_PRINT', 'COMMAND_RANDOM', 'ID', 'VAL_LITERAL',\
'ASSIGN_ADD', 'ASSIGN_SUB', 'ASSIGN_MULT', 'ASSIGN_DIV', 'COMP_EQU', \
'COMP_NEQU', 'COMP_LTE', 'COMP_GTE', 'BOOL_AND', 'BOOL_OR', 'WHITESPACE', 'COMMENT')

literals = "=+-*/<>!&|();,"


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

def t_COMP_GTE(t):
    r'>='
    return t

def t_BOOL_AND(t):
    r'&&'
    return t

def t_BOOL_OR(t):
    r'\|\|'
    return t

def t_WHITESPACE(t):
    r'[ \t\n]'
    pass

def t_COMMENT(t):
    r'\#.*'
    pass


def t_error(t):
    print("Unknown token on line {}: {}".format(t.lexer.lineno, t.value[0]))
    exit(1)


################# Global Variable part #########################
SYMBOL_TABLE = {}
precedence = (('right', '='),('left', '+', '-'),('left', '*', '/'))
S_CNT = 0
##################################################################


################# Node Class Part #########################
class BaseNode:
    def __init__(self, children):
        self.children = children

    def generate_bad_code(self, output):
        pass

class BlockNode(BaseNode):
    def __init__(self, children):
        super().__init__(children)


###############################################################

def p_program(p):
    """
    program : statements
    """
    print("program End")
    p[0] = p[1]

def p_statements(p):
    """
    statements : statements statement
               |
    """
    if len(p) > 1:
        p[1].children.append(p[2])
        p[0] = p[1]
    else:
        print("program start")
        p[0] = BlockNode([])

def p_statement(p):
    """
    statement : expr ';'
              | declaration ';'
    """
    pass



def p_declaration(p):
    """
    declaration : TYPE ID
                | TYPE ID '=' expr
    """
    if p[2] in SYMBOL_TABLE:
        raise NameError("redeclaration of variable '{}'".format(p[2]))
    ### just leave 0 for now
    SYMBOL_TABLE[p[2]] = 0




def p_expr_addition(p):
    """
    expr : expr '+' expr
    """
    pass

def p_expr_subtraction(p):
    """
    expr : expr '-' expr
    """
    pass

def p_expr_multiplication(p):
    """
    expr : expr '*' expr
    """
    pass


def p_expr_division(p):
    """
    expr : expr '/' expr
    """
    pass


def p_expr_comp_addition(p):
    """
    expr : expr ASSIGN_ADD expr
    """
    pass

def p_expr_comp_subtraction(p):
    """
    expr : expr ASSIGN_SUB expr
    """
    pass

def p_expr_comp_multiplication(p):
    """
    expr : expr ASSIGN_MULT expr
    """
    pass


def p_expr_comp_division(p):
    """
    expr : expr ASSIGN_DIV expr
    """
    pass

def p_expr_prethesis(p):
    """
    expr : '(' expr ')'
    """
    pass

def p_expr_eq(p):
    """
    expr : expr COMP_EQU expr
    """
    pass


def p_expr_neq(p):
    """
    expr : expr COMP_NEQU expr
    """
    pass


def p_expr_LT(p):
    """
    expr : expr '<' expr
    """
    pass

def p_expr_LE(p):
    """
    expr : expr COMP_LTE expr
    """
    pass

def p_expr_GT(p):
    """
    expr : expr '>' expr
    """
    pass

def p_expr_GE(p):
    """
    expr : expr COMP_GTE expr
    """
    pass

def p_expr_and(p):
    """
    expr : expr BOOL_AND expr
    """
    pass

def p_expr_or(p):
    """
    expr : expr BOOL_OR expr
    """
    pass


def p_expr_print(p):
    """
    expr : COMMAND_PRINT '(' arguments ')'
    """
    pass

def p_expr_print_args(p):
    """
    arguments : expr
              | expr ',' arguments
    """
    pass

def p_expr_random(p):
    """
    expr : COMMAND_RANDOM '(' expr ')'
    """
    pass


def p_expr_simple(p):
    """
    expr : VAL_LITERAL
         | variable
         | '-' VAL_LITERAL
         | '-' variable
    """
    pass


def p_assignment(p):
    """
    expr : variable '=' expr
    """
    pass


def p_check_ID(p):
    """
    variable : ID
    """
    if p[1] not in SYMBOL_TABLE:
        raise NameError("unknown variable '{}'".format(p[1]))


def p_error(p):
    raise SyntaxError(p)


def generate_bad_code_from_string(input_):
    lexer = lex.lex()
    parser = yacc.yacc()
    program = parser.parse(input_, lexer=lexer)

    # clear symbol table
    SYMBOL_TABLE.clear()
    ##########################

    output = []
    program.generate_bad_code(output)
    return "\n".join(output) + "\n"


if __name__ == "__main__":
    source = sys.stdin.read()
    result = generate_bad_code_from_string(source)
    print(result)
