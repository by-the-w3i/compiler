
import re
import sys

src = sys.stdin.read()

global FUNC_TABLE
result = re.findall(r"^\s*(define\s+(string|val|char|array\((char|val)\))\s+\w+\s*\(.*\))\s*\{?", src, re.MULTILINE)
for tup in result:
    signature = re.findall(r"define\s+(string|val|char|array\((char|val)\))\s+(\w+)\s*\((.*)\)", tup[0])[0]
    return_type = signature[0]
    name = signature[2]
    paras_lst = signature[3].split(',')

    if name in FUNC_TABLE:
        raise NameError("Error: REDEFINE function {}".format(name))

    paras = []
    for p in paras_lst:
        p = para.strip()
        if p != '':
            p_lst = p.split()
            p_type = p_lst[0]
            p_name = p_lst[1]
            para = ParameterNode(p_naem, p_type)
            paras.append(para)

    FUNC_TABLE[name] = FuncDefineNode(name, paras, return_type)


def header(src):
    import re
    global FUNC_TABLE
    result = re.findall(r"^\s*(define\s+(string|val|char|array\((char|val)\))\s+\w+\s*\(.*\))\s*\{?", src, re.MULTILINE)
    for tup in result:
        signature = re.findall(r"define\s+(string|val|char|array\((char|val)\))\s+(\w+)\s*\((.*)\)", tup[0])[0]
        return_type = signature[0]
        if return_type == "string":
            return_type = "array(char)"
        name = signature[2]
        paras_lst = signature[3].split(',')
        #
        # if name in FUNC_TABLE:
        #     raise NameError("Error: REDEFINE function {}".format(name))

        paras = []
        for p in paras_lst:
            p = para.strip()
            if p != '':
                p_lst = p.split()
                p_type = p_lst[0]
                if p_type == "string":
                    p_type = "array(char)"
                p_name = p_lst[1]
                para = ParameterNode(p_naem, p_type)
                paras.append(para)

        FUNC_TABLE[name] = FuncDefineNode(name, paras, return_type)
