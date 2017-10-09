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
    raise SyntaxError("Unknown token on line {}: {}".format(t.lexer.lineno, t.value[0]))
    exit(1)


############################### Global variables
SCOPE_TABLE = {0:{}}
SCP = 0 # current scope
S_CNT = 0
precedence = (('right', '='),('left', 'BOOL_OR'),('left', 'BOOL_AND'),('left', '<','>', 'COMP_EQU', \
'COMP_NEQU', 'COMP_LTE', 'COMP_GTE'),('left', '+', '-'),('left', '*', '/'))

def get_entry():
    global S_CNT
    entry = "s{}".format(S_CNT)
    S_CNT += 1
    return entry
##############################################################

######################### Node class parts:
class Node:
    def __init__(self, data = None, children = []):
        self.data = data
        self.children  = children
        self.register = 'x' # indicate no register associate

    def generate_bad_code(self, output):
        raise NotImplemented("Base Node!!!")

class BlockNode(Node):
    def __init__(self, children, scope):
        super().__init__(None, children)
        self.scope = scope

    def generate_bad_code(self, output):
        # output.append("# BlockNode START (Scope {})".format(self.scope))
        for child in self.children:
            child.generate_bad_code(output)
        # output.append("# BlockNode END")

class DeclareNode(Node):
    def __init__(self, children, type):
        super().__init__(None, children)
        self.type = type

    def generate_bad_code(self, output):
        if self.register == 'x':
            self.register = get_entry()
            if self.children:
                output.append("val_copy {} {}".format(self.children[0].generate_bad_code(output), self.register))
        return self.register


class DataNode(Node):
    def __init__(self, data):
        super().__init__(data, [])

    def generate_bad_code(self, output):
        if self.register == 'x':
            self.register = get_entry()
            output.append("val_copy {} {}".format(self.data, self.register))
        return self.register


class PrintNode(Node):
    def __init__(self, children):
        super().__init__(None, children)

    def generate_bad_code(self, output):
        for child in self.children:
            output.append("out_val {}".format(child.generate_bad_code(output)))
        output.append("out_char '\\n'")




class RandomNode(Node):
    def __init__(self, children):
        super().__init__(None, children)

    def generate_bad_code(self, output):
        if self.register == 'x':
            self.register = get_entry()
            output.append("random {} {}".format\
            (self.children[0].generate_bad_code(output), self.register))
        return self.register



class OperationNode(Node):
    def __init__(self, children, op):
        super().__init__(None, children)
        self.op = op
        self.CMD_TABLE = {"+":"add", "-":"sub", "*":"mult", "/":"div",\
         "<":"test_less", ">":"test_gtr", "==":"test_equ", \
         "!=":"test_nequ", ">=":"test_gte", "<=":"test_lte"}

    def generate_bad_code(self, output):
        if self.register == 'x':
            if self.op in ["+","-","*","/", ">", "<", "<=", ">=","==","!="]:
                left = self.children[0].generate_bad_code(output)
                right = self.children[1].generate_bad_code(output)
                self.register = get_entry()
                output.append("{} {} {} {}".format\
                (self.CMD_TABLE[self.op], left, right, self.register))

            elif self.op == "=":
                frm = self.children[1].generate_bad_code(output)
                to = self.children[0].generate_bad_code(output)
                output.append("val_copy {} {}".format(frm, to))
                self.register = to
                # self.children[0].register = 'x'

            elif self.op in ["&&", "||"]:
                left = self.children[0].generate_bad_code(output)
                right = self.children[1].generate_bad_code(output)
                temp = get_entry()
                self.register = get_entry()
                if self.op == "&&":
                    output.append("mult {} {} {}".format(left, right, temp))
                    output.append("test_nequ 0 {} {}".format(temp, self.register))
                elif self.op == "||":
                    output.append("add {} {} {}".format(left, right, temp))
                    output.append("test_gtr {} 0 {}".format(temp, self.register))

        return self.register





##################################################

def p_program(p):
    """
    program : statements
    """
    # print(len(p[1]))
    p[0] = BlockNode(p[1], 0)

def p_statements(p):
    """
    statements : statements statement
               |
    """
    if len(p) > 1:
        p[1].append(p[2])
        p[0] = p[1]
    else:
        p[0] = []


def p_statement(p):
    """
    statement : expr ';'
              | declaration ';'
    """
    p[0] = p[1]


def p_declaration(p):
    """
    declaration : TYPE ID
                | TYPE ID '=' expr
    """
    if p[2] in SCOPE_TABLE[SCP]:
        raise NameError("redeclaration of variable '{}'".format(p[2]))

    if len(p) > 3:
        p[0] = DeclareNode([p[4]], p[1])
    else:
        p[0] = DeclareNode([],p[1]) # indicate declared but not initialized
    SCOPE_TABLE[SCP][p[2]] = p[0]



def p_expr_addition(p):
    """
    expr : expr '+' expr
    """
    p[0] = OperationNode([p[1],p[3]],'+')

def p_expr_subtraction(p):
    """
    expr : expr '-' expr
    """
    p[0] = OperationNode([p[1],p[3]],'-')

def p_expr_multiplication(p):
    """
    expr : expr '*' expr
    """
    p[0] = OperationNode([p[1],p[3]],'*')


def p_expr_division(p):
    """
    expr : expr '/' expr
    """
    p[0] = OperationNode([p[1],p[3]],'/')


def p_expr_comp_addition(p):
    """
    expr : expr ASSIGN_ADD expr
    """
    p[0] = OperationNode([p[1], OperationNode([p[1],p[3]],'+')],'=')

def p_expr_comp_subtraction(p):
    """
    expr : expr ASSIGN_SUB expr
    """
    p[0] = OperationNode([p[1], OperationNode([p[1],p[3]],'-')],'=')

def p_expr_comp_multiplication(p):
    """
    expr : expr ASSIGN_MULT expr
    """
    p[0] = OperationNode([p[1], OperationNode([p[1],p[3]],'*')],'=')


def p_expr_comp_division(p):
    """
    expr : expr ASSIGN_DIV expr
    """
    p[0] = OperationNode([p[1], OperationNode([p[1],p[3]],'/')],'=')

def p_expr_prethesis(p):
    """
    expr : '(' expr ')'
    """
    p[0] = p[2]

def p_expr_eq(p):
    """
    expr : expr COMP_EQU expr
    """
    p[0] = OperationNode([p[1],p[3]],'==')


def p_expr_neq(p):
    """
    expr : expr COMP_NEQU expr
    """
    p[0] = OperationNode([p[1],p[3]],'!=')


def p_expr_LT(p):
    """
    expr : expr '<' expr
    """
    p[0] = OperationNode([p[1],p[3]],'<')

def p_expr_LE(p):
    """
    expr : expr COMP_LTE expr
    """
    p[0] = OperationNode([p[1],p[3]],'<=')

def p_expr_GT(p):
    """
    expr : expr '>' expr
    """
    p[0] = OperationNode([p[1],p[3]],'>')

def p_expr_GE(p):
    """
    expr : expr COMP_GTE expr
    """
    p[0] = OperationNode([p[1],p[3]],'>=')

def p_expr_and(p):
    """
    expr : expr BOOL_AND expr
    """
    p[0] = OperationNode([p[1],p[3]],'&&')

def p_expr_or(p):
    """
    expr : expr BOOL_OR expr
    """
    p[0] = OperationNode([p[1],p[3]],'||')


def p_expr_print(p):
    """
    expr : COMMAND_PRINT '(' arguments ')'
    """
    p[0] = PrintNode(p[3])

def p_expr_print_args(p):
    """
    arguments : expr
              | arguments ',' expr
    """
    if len(p) > 2:
        p[1].append(p[3])
        p[0] = p[1]
    else:
        p[0] = [p[1]]

def p_expr_random(p):
    """
    expr : COMMAND_RANDOM '(' expr ')'
    """
    p[0] = RandomNode([p[3]])


def p_expr_simple_val(p):
    """
    expr : VAL_LITERAL
         | '-' VAL_LITERAL
    """
    if len(p) > 2:
        p[0] = DataNode('-'+p[2])
    else:
        p[0] = DataNode(p[1])

def p_expr_simple_var(p):
    """
    expr : variable
         | '-' variable
    """
    if len(p) > 2:
        p[0] = OperationNode([DataNode(0), p[2]], '-')
    else:
        p[0] = p[1]


def p_assignment(p):
    """
    expr : variable '=' expr
    """
    p[0] = OperationNode([p[1],p[3]], '=')


def p_check_ID(p):
    """
    variable : ID
    """
    if SCP not in SCOPE_TABLE:
        SCOPE_TABLE[SCP] = {}

    temp = SCP
    while temp >= 0:
        symbol_table = SCOPE_TABLE[temp]
        if p[1] in symbol_table:
            p[0] = symbol_table[p[1]]
            break
        temp -= 1
    if temp < 0:
        raise NameError("unknown variable '{}'".format(p[1]))
    # p[0] = p[1]


def p_error(p):
    raise SyntaxError(p)


def generate_bad_code_from_string(input_):
    ### clear all the global data
    global SCOPE_TABLE
    global S_CNT
    global SCP
    SCOPE_TABLE.clear()
    SCOPE_TABLE = {0:{}}
    SCP = 0 # current scope
    S_CNT = 0
    ##########################
    lexer = lex.lex()
    parser = yacc.yacc()
    program = parser.parse(input_, lexer=lexer)
    output = []
    program.generate_bad_code(output)
    return "\n".join(output) + "\n"


if __name__ == "__main__":
    source = sys.stdin.read()
    result = generate_bad_code_from_string(source)
    print(result)
