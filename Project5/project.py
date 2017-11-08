#! /usr/bin/env python3
import sys
import ply.lex as lex
import ply.yacc as yacc

tokens = ('TYPE', 'COMMAND_PRINT', 'COMMAND_RANDOM', 'ID', 'VAL_LITERAL',\
'CHAR_LITERAL', 'ASSIGN_ADD', 'ASSIGN_SUB', 'ASSIGN_MULT', 'ASSIGN_DIV', 'COMP_EQU', \
'COMP_NEQU', 'COMP_LTE', 'COMP_GTE', 'BOOL_AND', 'BOOL_OR', 'WHITESPACE', 'COMMENT', \
'OPEN_BLOCK', 'CLOSE_BLOCK', 'COMMAND_IF', 'COMMAND_ELSE', 'COMMAND_WHILE', 'COMMAND_BREAK',\
'ARRAY_TYPE', 'STRING_TYPE', 'STRING_LITERAL', 'AR_METHOD_SIZE', 'AR_METHOD_RESIZE')

literals = "=+-*/<>!&|();,[]."

############################### Global variables
SCOPE_TABLE = {0:{}}
SCP = 0 # current scope
S_CNT = 0
LINENO = 1 # tracking the line number
IF_NUM = 0
# WHILE_CNT = 0
WHILE_NUM = 0
WHILE_STACK = []


precedence = (('right', '='),('left', 'BOOL_OR'),('left', 'BOOL_AND'),('left', '<','>', 'COMP_EQU', \
'COMP_NEQU', 'COMP_LTE', 'COMP_GTE'),('left', '+', '-'),('left', '*', '/'),('right', '!'), ('right','AR_METHOD_SIZE', 'AR_METHOD_RESIZE'))

def get_entry(v='s'):
    global S_CNT
    entry = "{}{}".format(v,S_CNT)
    S_CNT += 1
    return entry

def get_if_entry():
    global IF_NUM
    label = str(IF_NUM)
    IF_NUM += 1
    return label

def get_while_entry():
    global WHILE_NUM
    label = str(WHILE_NUM)
    WHILE_NUM += 1
    return label
##############################################################


def t_TYPE(t):
    r'(val|char)\b'
    return t

def t_ARRAY_TYPE(t):
    r'array'
    return t

def t_STRING_TYPE(t):
    r'string'
    return t

def t_COMMAND_PRINT(t):
    r'print\b'
    return t

def t_COMMAND_RANDOM(t):
    r'random\b'
    return t

def t_COMMAND_IF(t):
    r'if\b'
    return t

def t_COMMAND_ELSE(t):
    r'else\b'
    return t

def t_COMMAND_WHILE(t):
    r'while\b'
    return t

def t_COMMAND_BREAK(t):
    r'break\b'
    return t

def t_AR_METHOD_SIZE(t):
    r'\.size'
    return t

def t_AR_METHOD_RESIZE(t):
    r'\.resize'
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
    r'"((\\")|[^"\n])*"'
    return t

def t_OPEN_BLOCK(t):
    r'{'
    global SCP
    global SCOPE_TABLE
    SCP += 1
    SCOPE_TABLE[SCP] = {}
    return t

def t_CLOSE_BLOCK(t):
    r'}'
    global SCP
    global SCOPE_TABLE
    del SCOPE_TABLE[SCP]
    SCP -= 1
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
    if t.value == '\n':
        global LINENO
        LINENO += 1

def t_COMMENT(t):
    r'\#.*'
    pass


def t_error(t):
    raise SyntaxError("ERROR(line {}): unknown token '{}'".format(LINENO, t.value[0]))
    exit(1)



######################### Node class parts:
class Node:
    def __init__(self, data = None, children = []):
        self.data = data
        self.children  = children
        self.register = 'x' # indicate no register associate
        self.type = None

    def generate_bad_code(self, output):
        raise NotImplemented("Base Node!!!")

class BlockNode(Node):
    def __init__(self, children):
        super().__init__(None, children)

    def generate_bad_code(self, output):
        # output.append("# BlockNode START (Scope {})".format(self.scope))
        for child in self.children:
            child.generate_bad_code(output)
        # output.append("# BlockNode END")

class DeclareNode(Node):
    def __init__(self, children, type):
        if len(children) > 0:
            if children[0].type != type:
                raise TypeError("ERROR(line {}): types do not match for assignment (lhs='{}', rhs='{}')".\
            format(LINENO, type, children[0].type))
        super().__init__(None, children)
        self.type = type

    def generate_bad_code(self, output):
        if self.register == 'x':
            if "array" in self.type:
                self.register = get_entry('a')
                # put type for []
                if self.children[0].type == "UNDEFINED":
                    self.children[0].type = self.type
                ##
                output.append("ar_copy {} {}".format(self.children[0].generate_bad_code(output), self.register))
            else:
                self.register = get_entry()
                if self.children:
                    output.append("val_copy {} {}".format(self.children[0].generate_bad_code(output), self.register))

        return self.register


class DataNode(Node):
    def __init__(self, data, type):
        super().__init__(data, [])
        self.type = type

    def generate_bad_code(self, output):
        if self.register == 'x':
            if self.type == "char":
                self.register = self.data
            else: # val type
                self.register = get_entry()
                output.append("val_copy {} {}".format(self.data, self.register))
        return self.register


class ArrayNode(Node):
    def __init__(self, data, type="UNDEFINED"): # data is list of elements
        super().__init__(data, [])
        self.type = type

    def generate_bad_code(self, output):
        if self.register == 'x':
            self.register = get_entry('a')
            output.append("ar_set_size {} {}".format(self.register, len(self.data)))
            for i in range(len(self.data)):
                output.append("ar_set_idx {} {} {}".format(self.register, i, self.data[i].generate_bad_code(output)))
        return self.register


class ArraySizeNode(Node):
    def __init__(self, data, func):
        if "array" not in data[0].type:
            raise TypeError("ERROR(line {}): array methods cannot be run on type '{}'".format(LINENO, data[0].type))
        super().__init__(data, [])
        self.func = func;
        if self.func == "get":
            self.type = "val"
        else: # set
            if self.data[1].type != "val":
                raise TypeError("ERROR(line {}): array resize() method argument must be of type val.".format(LINENO))

    def generate_bad_code(self, output):
        if self.register == 'x':
            if self.func == "get":
                self.register = get_entry()
                output.append("ar_get_size {} {}".format(self.data[0].register, self.register))
            else: # set
                sz_register = self.data[1].generate_bad_code(output)
                output.append("ar_set_size {} {}".format(self.data[0].register, sz_register))
        return self.register


class ArrayIndexNode(Node):
    # data = [arraynode, index_num]
    def __init__(self, data):
        if "array" not in data[0].type:
            raise TypeError("ERROR(line {}): cannot index into a non-array type".format(LINENO))
        if data[1].type != "val":
            raise TypeError("ERROR(line {}): array indices must be of type val".format(LINENO))
        super().__init__(data, [])
        self.type = data[0].type[6:-1]
        self.func = "get"

    def generate_bad_code(self, output):
        ind_register = self.data[1].generate_bad_code(output)
        if self.func != "get":
            output.append("ar_set_idx {} {} {}".format(self.data[0].register, ind_register, self.func))
        else:
            if self.register =='x':
                self.register = get_entry()
                output.append("ar_get_idx {} {} {}".format(self.data[0].register, ind_register, self.register))
        return self.register






class PrintNode(Node):
    def __init__(self, children):
        super().__init__(None, children)

    def generate_bad_code(self, output):
        for child in self.children:
            if "array" in child.type:
                while_no = get_while_entry()
                global WHILE_STACK
                WHILE_STACK.append(while_no)
                temp = get_entry()
                diff = get_entry()
                el = get_entry()
                sz = get_entry()
                output.append("val_copy 0 {}".format(temp))
                output.append("ar_get_size {} {}".format(child.generate_bad_code(output), sz))
                output.append("start_while_{}:".format(while_no))
                output.append("sub {} {} {}".format(sz, temp, diff))
                output.append("jump_if_0 {} end_while_{}".format(diff, while_no))
                output.append("ar_get_idx {} {} {}".format(child.generate_bad_code(output), temp, el))
                output.append("out_{} {}".format(child.type[6:-1], el))
                output.append("add 1 {} {}".format(temp, temp))
                output.append("jump start_while_{}".format(while_no))
                output.append("end_while_{}:".format(while_no))
                WHILE_STACK.pop()
            else:
                output.append("out_{} {}".format(child.type, child.generate_bad_code(output)))
        output.append("out_char '\\n'")




class RandomNode(Node):
    def __init__(self, children):
        if children[0].type != "val":
            raise TypeError("ERROR(line {}): cannot use type '{}' as an argument to random".\
        format(LINENO, children[0].type))
        super().__init__(None, children)
        self.type = "val"

    def generate_bad_code(self, output):
        if self.register == 'x':
            self.register = get_entry()
            output.append("random {} {}".format\
            (self.children[0].generate_bad_code(output), self.register))
        return self.register


class IfNode(Node):
    def __init__(self, children):
        if children[0].type != "val":
            raise TypeError("ERROR(line {}): condition for if statements must evaluate to type val".format(LINENO))
        super().__init__(None, children)

    def generate_bad_code(self, output):
        exp = self.children[0].generate_bad_code(output)
        if_num = get_if_entry()

        if len(self.children) > 2: # with else
            output.append("jump_if_0 {} else_{}".format(exp, if_num))
            self.children[1].generate_bad_code(output)
            output.append("jump end_if_{}".format(if_num))
            output.append("else_{}:".format(if_num))
            self.children[2].generate_bad_code(output)
            output.append("end_if_{}:".format(if_num))
        else: # without else
            output.append("jump_if_0 {} end_if_{}".format(exp, if_num))
            self.children[1].generate_bad_code(output)
            output.append("end_if_{}:".format(if_num))

class WhileNode(Node):
    def __init__(self,children):
        if children[0].type != "val":
            raise TypeError("ERROR(line {}): condition for while statements must evaluate to type val ".format(LINENO))
        super().__init__(None, children)

    def generate_bad_code(self, output):
        while_no = get_while_entry()
        global WHILE_STACK
        WHILE_STACK.append(while_no)
        output.append("start_while_{}:".format(while_no))
        expr = self.children[0].generate_bad_code(output)
        output.append("jump_if_0 {} end_while_{}".format(expr, while_no))
        self.children[1].generate_bad_code(output)
        output.append("jump start_while_{}".format(while_no))
        output.append("end_while_{}:".format(while_no))
        WHILE_STACK.pop()




class BreakNode(Node):
    def __init__(self):
        super().__init__()

    def generate_bad_code(self, output):
        global WHILE_STACK
        # global WHILE_NUM
        if  WHILE_STACK == []:
            raise SyntaxError("ERROR(line {}): 'break' command used outside of any loops".format(LINENO))
        output.append("jump end_while_{}".format(WHILE_STACK[-1]))
        # WHILE_STACK.pop()


class OperationNode(Node):
    def __init__(self, children, op):
        super().__init__(None, children)
        self.op = op
        self.type = "val" #default val
        self.CMD_TABLE = {"+":"add", "-":"sub", "*":"mult", "/":"div",\
         "<":"test_less", ">":"test_gtr", "==":"test_equ", \
         "!=":"test_nequ", ">=":"test_gte", "<=":"test_lte"}

    def generate_bad_code(self, output):
        if self.register == 'x':
            if len(self.children) == 2 and self.children[0].type != self.children[1].type:
                raise TypeError("ERROR(line {}): types do not match for relationship operator (lhs='{}', rhs='{}')".\
            format(LINENO, self.children[0].type, self.children[1].type))

            if self.children[0].type != "val" and self.op in ["+","-","*","/", "&&", "||", "!"]:
                raise TypeError("ERROR(line {}): cannot use type '{}' in mathematical expressions".\
            format(LINENO, self.children[0].type))

            if self.op in ["+","-","*","/", ">", "<", "<=", ">=","==","!="]:
                left = self.children[0].generate_bad_code(output)
                right = self.children[1].generate_bad_code(output)
                self.register = get_entry()
                output.append("{} {} {} {}".format\
                (self.CMD_TABLE[self.op], left, right, self.register))

            elif self.op == "=":
                # put type for []
                if "array" in self.children[0].type and self.children[1].type == "UNDEFINED":
                    self.children[1].type == self.children[0].type
                #############
                if self.children[0].type != self.children[1].type:
                    raise TypeError("ERROR(line {}): types do not match for assignment (lhs='{}', rhs='{}')".\
                format(LINENO, self.children[0].type, self.children[1].type))

                frm = self.children[1].generate_bad_code(output)
                if isinstance(self.children[0], ArrayIndexNode):
                    self.children[0].func = frm
                    self.children[0].generate_bad_code(output)
                else:
                    to = self.children[0].generate_bad_code(output)
                    if "array" in self.children[0].type:
                        output.append("ar_copy {} {}".format(frm, to))
                    else:
                        output.append("val_copy {} {}".format(frm, to))
                    self.register = to
                    self.type = self.children[0].type
                    # self.children[0].register = 'x'

            elif self.op in ["&&", "||"]:
                left = self.children[0].generate_bad_code(output)
                label = "end_if_" + get_if_entry()
                self.register = get_entry()
                temp = get_entry()
                if self.op == "&&":
                    output.append("val_copy 0 {}".format(temp))
                    output.append("jump_if_0 {} {}".format(left, label))
                    right = self.children[1].generate_bad_code(output)
                    output.append("test_nequ 0 {} {}".format(right, temp))
                elif self.op == "||":
                    output.append("val_copy 1 {}".format(temp))
                    output.append("jump_if_n0 {} {}".format(left, label))
                    right = self.children[1].generate_bad_code(output)
                    output.append("test_nequ 0 {} {}".format(right, temp))
                output.append("{}:".format(label))
                output.append("val_copy {} {}".format(temp, self.register))

            elif self.op == '!':
                exp = self.children[0].generate_bad_code(output)
                self.register = get_entry()
                output.append("test_equ 0 {} {}".format(exp, self.register))

            # set the type of the OperationNode
            #self.type = self.children[0].type
        return self.register





##################################################

def p_program(p):
    """
    program : statements
    """
    # print(len(p[1]))
    p[0] = BlockNode(p[1])

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
              | block
              | if_block
              | while_loop
              | break ';'
    """
    p[0] = p[1]

def p_empty_statement(p):
    """
    statement : ';'
    """
    p[0] = BlockNode([])

def p_block(p):
    """
    block : OPEN_BLOCK statements CLOSE_BLOCK
    """
    p[0] = BlockNode(p[2])

def p_if_block(p):
    """
    if_block : COMMAND_IF '(' expr ')' statement
             | COMMAND_IF '(' expr ')' statement COMMAND_ELSE statement
    """
    children = []
    for i in range(3, len(p), 2):
        children.append(p[i])
    p[0] = IfNode(children)

def p_break(p):
    """
    break : COMMAND_BREAK
    """
    p[0] = BreakNode()


def p_while_loop(p):
    """
    while_loop : COMMAND_WHILE '(' expr ')' statement
    """
    p[0] = WhileNode([p[3], p[5]])


def p_declaration(p):
    """
    declaration : TYPE ID
                | TYPE ID '=' expr
    """
    if p[2] in SCOPE_TABLE[SCP]:
        raise NameError("ERROR(line {}): redeclaration of variable '{}'".format(LINENO,p[2]))

    if len(p) > 3:
        p[0] = DeclareNode([p[4]], p[1])
    else:
        p[0] = DeclareNode([],p[1]) # indicate declared but not initialized
    SCOPE_TABLE[SCP][p[2]] = p[0]


def p_declaration_array(p):
    """
    declaration : ARRAY_TYPE '(' TYPE ')' ID
                | ARRAY_TYPE '(' TYPE ')' ID '=' expr
    """
    if p[5] in SCOPE_TABLE[SCP]:
        raise NameError("ERROR(line {}): redeclaration of variable '{}'".format(LINENO,p[2]))

    if len(p) > 6:
        p[0] = DeclareNode([p[7]], "array({})".format(p[3]))
    else:
        t = "array({})".format(p[3])
        p[0] = DeclareNode([ArrayNode([], t)], t) # not initialized
    SCOPE_TABLE[SCP][p[5]] = p[0]


def p_declaration_string(p):
    """
    declaration : STRING_TYPE ID
                | STRING_TYPE ID '=' expr
    """
    if p[2] in SCOPE_TABLE[SCP]:
        raise NameError("ERROR(line {}): redeclaration of variable '{}'".format(LINENO,p[2]))

    if len(p) > 3:
        p[0] = DeclareNode([p[4]], "array(char)")
    else:
        p[0] = DeclareNode([ArrayNode([], "array(char)")], "array(char)") # not initialized
    SCOPE_TABLE[SCP][p[2]] = p[0]




def p_expr_ar_literal(p):
    """
    expr : '[' elements ']'
         | '[' ']'
         | STRING_LITERAL
    """
    if len(p) == 4:
        p[0] = ArrayNode(p[2], "array({})".format(p[2][0].type))
    elif len(p) == 3:
        # initialize with empty [] but dont know the type
        # put "UNDEFINED" as the default type, dont forget to check later
        p[0] = ArrayNode([])
    else:
        # split the string to array(char)
        lst = []
        escape = False
        for c in p[1][1:-1]:
            fmt = "'{}'"
            if escape:
                if c != '"':
                    fmt = "'\\{}'"
                escape = False
            elif c == '\\':
                escape = True
                continue
            lst.append(DataNode(fmt.format(c),"char"))
        p[0] = ArrayNode(lst, "array(char)")




def p_elements(p):
    """
    elements : expr
             | elements ',' expr
    """
    if len(p) > 2:
        if p[1][0].type != p[3].type:
            raise SyntaxError("ERROR: does not support multi types in an Array!!!")
        p[1].append(p[3])
        p[0] = p[1]
    else:
        p[0] = [p[1]]



def p_ar_method_size(p):
    """
    expr : expr AR_METHOD_SIZE '(' ')'
         | expr AR_METHOD_SIZE '(' expr ')'
    """
    if len(p) > 6:
        raise TypeError("ERROR(line {}): array size() method does not take any arguments.".format(LINENO))
    p[0] = ArraySizeNode([p[1]], "get")


def p_ar_method_resize(p):
    """
    expr : expr AR_METHOD_RESIZE '(' arguments ')'
    """
    if len(p[4]) != 1:
        raise TypeError("ERROR(line {}): array resize() method takes exactly one (val) argument.".format(LINENO))
    p[0] = ArraySizeNode([p[1], p[4][0]], "set")


def p_ar_index(p):
    """
    expr : variable '[' expr ']'
         | variable '[' expr ']' '=' expr
    """
    p[0] = ArrayIndexNode([p[1], p[3]])
    if len(p) > 5:
        p[0] = OperationNode([p[0], p[6]], "=")





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

def p_expr_not(p):
    """
    expr : '!' expr
    """
    p[0] = OperationNode([p[2]],'!')


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
        p[0] = DataNode('-'+p[2], "val")
    else:
        p[0] = DataNode(p[1],"val")

def p_expr_simple_char(p):
    """
    expr : CHAR_LITERAL
    """
    p[0] = DataNode(p[1], "char")

def p_expr_simple_var(p):
    """
    expr : variable
         | '-' variable
    """
    if len(p) > 2:
        p[0] = OperationNode([DataNode("0", "val"), p[2]], '-')
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
    temp = SCP
    while temp >= 0:
        symbol_table = SCOPE_TABLE[temp]
        if p[1] in symbol_table:
            p[0] = symbol_table[p[1]]
            break
        temp -= 1
    if temp < 0:
        raise NameError("ERROR(line {}): unknown variable '{}'".format(LINENO, p[1]))
    # p[0] = p[1]


def p_error(p):
    # raise SyntaxError("ERROR(line {}): yacc parse error".format(LINENO))
    raise SyntaxError("ERROR(line {}): syntax error".format(LINENO))



########################################################################

def generate_bad_code_from_string(input_):
    ### clear all the global data
    global SCOPE_TABLE
    global S_CNT
    global SCP
    global LINENO
    SCOPE_TABLE.clear()
    SCOPE_TABLE = {0:{}}
    SCP = 0 # current scope
    S_CNT = 0
    LINENO = 1
    IF_NUM = 0
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
