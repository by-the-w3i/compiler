# Project 1: Lexical Analysis
>For this project, you will write the lexical analysis phase (i.e., the "scanner") of a simple compiler for a subset of the language "Tubular". We will start with only one variable type ("val"), basic math, and the print command to output results; basically it will be little more than a simple calculator. Over the next two projects we will turn this into a working compiler, and in the five projects following that, we will expand the functionality and efficiency of the language.

## Description
The program you turn in must accept input via stdin and process it line-by-line, removing whitespace and comments and categorizing each word or symbol as a token. A token represents an atomic unit to be parsed, and is typically realized as one or a short series of characters in a source file such as "val", "+", or "42". You will then output (to standard out) a tokenized version of the file, as described in detail below.

We will develop our compiler using the library PLY (which uses Lex and Yacc). This project will only require the use of lex, which will handle lexical analysis for us, taking as input a set of regular expressions associated with each token type. Both of these tools are included in the Python3 package "ply", which can be installed on your local machines for working on personal devices (it is already available on Mimir).

Your scanner should be able to identify each of the following tokens that it encounters:

| Token | Desciption |
| ----- | ---------- |
| TYPE	| Data types: currently "val", "char", and "string", but more types may be introduced in future projects. |
|COMMAND_PRINT|	The built-in command "print".|
|COMMAND_RANDOM|	The built-in command "random"|
|ID|	A sequence beginning with a letter or underscore ('_'), followed by a sequence of zero or more characters that may contain letters, numbers and underscores. Currently just variable names, but in the future they will also be used for function names.|
|VAL_LITERAL|	Any literal number. These consist of sequences of digits, with no other characters except a single, optional, decimal point; If a decimal point exists, it must be followed by at least one digit. Examples: 123.456, 10.0, or .0001|
|CHAR_LITERAL|	Any literal character. These are a single quote followed by any single character, followed by another single quote. Example: 'c'|
|STRING_LITERAL|	A literal string: A double quote followed by a series of internal characters and ending in a second double quote. The internal characters can be anything except a double quote or a newline. Note: in a future project we will implement escape characters. Example: "This is my string"|
|ASCII_CHAR|	Single-character operators: + - * / ( ) = , { } [ ] . ;|
ASSIGN_ADD, ASSIGN_SUB, ASSIGN_MULT, ASSIGN_DIV|	Compound math operators: += -= *= /=|
|COMP_EQU, COMP_NEQU, COMP_LESS, COMP_LTE, COMP_GTR, COMP_GTE|	Comparison operators: == != < <= > >=|
|BOOL_AND, BOOL_OR|	Boolean operators: && \|\||
|WHITESPACE|	Any number of consecutive spaces, tabs, or newlines.|
|COMMENT|	Everything on a single line following an octothorpe (pound-sign), '#'.|
|UNKNOWN|	An unknown character or a sequence that does not match any of the tokens above.|   


A pattern is a rule by which a token is identified. It will be up to you as part of this project to identify the patterns associated with each token. A lexeme is an instance of a pattern from a source file. For example, "300" is a lexeme that would be categorized as the token VAL_LITERAL. On the other hand "my_var" is a lexeme that would get identified as a token of the type ID.

When multiple patterns match the text being processed, choose the one that produces the longest lexeme that starts at the current position. If two different patterns produce lexemes of the same length, choose the one that comes first in the list above. For example, the lexeme "my_val" might be incorrectly read as the ID "my_" followed by the TYPE "val", but "my_val" is longer than "my_", so it should be chosen. Likewise, the lexeme "print" could match either the pattern for COMMAND_PRINT or the pattern for ID, but COMMAND_PRINT should be chosen since it comes first in the list above.



## Output
Your program should convert the source file into a series of tokens that it then outputs (In Project 2, these tokens will be processed by your parser instead). The output should list each token, on a separate line, followed by a colon, a space, a nd then the associated lexeme. DO NOT output the WHITESPACE or COMMENT tokens, as they will not be needed by your parser. Finally, on the last line print "Line Count: " and then output the total number of lines in the source file.

## Notes
Any deviation from the specified format will result in deductions from your score. This means no additional formatting of the output and no extra information.
Your output must go only to standard out (i.e., do not send any output to a file or to standard error).
You should halt further scanning after printing an UNKNOWN token as this indicates that the source file must contain an error.
The program you write may be used as the basis for future projects so you want to make it easily extensible.

## Example
Here is the contents of an example input file:
```
val test_num = 3 * (7.2 + 12.1);  # This is my comment.

# The next line intentionally left blank!;

print test_num;
```
Your project 1 executable should produce:
```
TYPE: val
ID: test_num
ASCII_CHAR: =
VAL_LITERAL: 3
ASCII_CHAR: *
ASCII_CHAR: (
VAL_LITERAL: 7.2
ASCII_CHAR: +
VAL_LITERAL: 12.1
ASCII_CHAR: )
ASCII_CHAR: ;
COMMAND_PRINT: print
ID: test_num
ASCII_CHAR: ;
Line Count: 4
```
When unknown tokens are found, your program should halt (with a normal, 0, exit code). In Python this can be achieved using the exit function from the sys module (see https://docs.python.org/3/library/sys.html#sys.exit). Successful lexing of source files should raise a zero exit code upon completing, which is the default.

For example, the input program:
```
It is illegal 2 include an @ here
```
The @ symbol is not an allowed token, so your program should exit after handling the @ symbol.

You can find an example of a fully-working compiled lexer executable named project1_lexer. This can be useful for local testing or edge case analysis.
