
import re
import sys

src = sys.stdin.read()

result = re.findall(r"(define\s(string|val|char|array\((char|val)\))\s\w+\s?\(.*?\))", src)

print(result)
