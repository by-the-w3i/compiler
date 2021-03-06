
# parsetab.py
# This file is automatically generated. Do not edit.
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = "LABEL VAL_LITERAL CHAR_LITERAL SCALAR_VAR ARRAY_VAR REGISTER COMMENT NEWLINE VAL_COPY ADD SUB MULT DIV TEST_LESS TEST_GTR TEST_EQU TEST_NEQU TEST_GTE TEST_LTE JUMP JUMP_IF_0 JUMP_IF_N0 RANDOM OUT_VAL OUT_CHAR NOP AR_GET_IDX AR_SET_IDX AR_GET_SIZE AR_SET_SIZE AR_COPY AR_PUSH AR_POP PUSH POP LOAD STORE MEM_COPY\n    program : statements\n    \n    statements :\n    \n    statements : statements complex_statement\n    \n    complex_statement : statement_comment NEWLINE\n    \n    statement_comment : optional_statement optional_comment\n    \n    optional_statement : statement\n    \n    optional_statement :\n    \n    optional_comment : COMMENT\n    \n    optional_comment :\n    \n    statement : VAL_COPY value store_value\n    \n    math_command : ADD\n                 | SUB\n                 | MULT\n                 | DIV\n                 | TEST_LESS\n                 | TEST_GTR\n                 | TEST_EQU\n                 | TEST_NEQU\n                 | TEST_GTE\n                 | TEST_LTE\n    \n    statement : math_command value value store_value\n    \n    statement : LABEL ':'\n    \n    value : LABEL\n    \n    statement : JUMP value\n    \n    statement : JUMP_IF_0 value value\n    \n    statement : JUMP_IF_N0 value value\n    \n    statement : RANDOM value store_value\n    \n    statement : OUT_VAL value\n    \n    statement : OUT_CHAR value\n    \n    statement : NOP\n    \n    statement : PUSH value\n    \n    statement : POP store_value\n    \n    statement : AR_GET_IDX ARRAY_VAR value SCALAR_VAR\n    \n    statement : AR_SET_IDX ARRAY_VAR value value\n    \n    statement : AR_GET_SIZE ARRAY_VAR SCALAR_VAR\n    \n    statement : AR_SET_SIZE ARRAY_VAR value\n    \n    statement : AR_COPY ARRAY_VAR ARRAY_VAR\n    \n    statement : AR_PUSH ARRAY_VAR ARRAY_VAR\n    \n    statement : AR_POP ARRAY_VAR ARRAY_VAR\n    \n    value : CHAR_LITERAL\n    \n    store_value : SCALAR_VAR\n                | REGISTER\n    \n    value : VAL_LITERAL\n    \n    value : SCALAR_VAR\n    \n    value : REGISTER\n    \n    statement : LOAD value value\n    \n    statement : STORE value value\n    \n    statement : MEM_COPY value value\n    "
    
_lr_action_items = {'VAL_COPY':([0,2,3,39,],[-2,7,-3,-4,]),'LABEL':([0,2,3,7,8,10,11,12,13,14,15,17,26,27,28,29,30,31,32,33,34,35,36,37,38,39,43,44,45,46,47,48,51,52,60,61,63,67,68,69,76,],[-2,9,-3,43,43,43,43,43,43,43,43,43,43,43,43,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-4,-23,-40,-43,-44,-45,43,43,43,43,43,43,43,43,43,43,]),'JUMP':([0,2,3,39,],[-2,10,-3,-4,]),'JUMP_IF_0':([0,2,3,39,],[-2,11,-3,-4,]),'JUMP_IF_N0':([0,2,3,39,],[-2,12,-3,-4,]),'RANDOM':([0,2,3,39,],[-2,13,-3,-4,]),'OUT_VAL':([0,2,3,39,],[-2,14,-3,-4,]),'OUT_CHAR':([0,2,3,39,],[-2,15,-3,-4,]),'NOP':([0,2,3,39,],[-2,16,-3,-4,]),'PUSH':([0,2,3,39,],[-2,17,-3,-4,]),'POP':([0,2,3,39,],[-2,18,-3,-4,]),'AR_GET_IDX':([0,2,3,39,],[-2,19,-3,-4,]),'AR_SET_IDX':([0,2,3,39,],[-2,20,-3,-4,]),'AR_GET_SIZE':([0,2,3,39,],[-2,21,-3,-4,]),'AR_SET_SIZE':([0,2,3,39,],[-2,22,-3,-4,]),'AR_COPY':([0,2,3,39,],[-2,23,-3,-4,]),'AR_PUSH':([0,2,3,39,],[-2,24,-3,-4,]),'AR_POP':([0,2,3,39,],[-2,25,-3,-4,]),'LOAD':([0,2,3,39,],[-2,26,-3,-4,]),'STORE':([0,2,3,39,],[-2,27,-3,-4,]),'MEM_COPY':([0,2,3,39,],[-2,28,-3,-4,]),'ADD':([0,2,3,39,],[-2,29,-3,-4,]),'SUB':([0,2,3,39,],[-2,30,-3,-4,]),'MULT':([0,2,3,39,],[-2,31,-3,-4,]),'DIV':([0,2,3,39,],[-2,32,-3,-4,]),'TEST_LESS':([0,2,3,39,],[-2,33,-3,-4,]),'TEST_GTR':([0,2,3,39,],[-2,34,-3,-4,]),'TEST_EQU':([0,2,3,39,],[-2,35,-3,-4,]),'TEST_NEQU':([0,2,3,39,],[-2,36,-3,-4,]),'TEST_GTE':([0,2,3,39,],[-2,37,-3,-4,]),'TEST_LTE':([0,2,3,39,],[-2,38,-3,-4,]),'NEWLINE':([0,2,3,4,5,6,16,39,40,41,43,44,45,46,47,49,50,54,55,56,57,58,59,70,72,73,74,77,78,79,80,81,82,83,84,85,86,87,],[-2,-7,-3,39,-9,-6,-30,-4,-5,-8,-23,-40,-43,-44,-45,-22,-24,-28,-29,-31,-32,-41,-42,-10,-25,-26,-27,-35,-36,-37,-38,-39,-46,-47,-48,-21,-33,-34,]),'COMMENT':([0,2,3,5,6,16,39,43,44,45,46,47,49,50,54,55,56,57,58,59,70,72,73,74,77,78,79,80,81,82,83,84,85,86,87,],[-2,-7,-3,41,-6,-30,-4,-23,-40,-43,-44,-45,-22,-24,-28,-29,-31,-32,-41,-42,-10,-25,-26,-27,-35,-36,-37,-38,-39,-46,-47,-48,-21,-33,-34,]),'$end':([0,1,2,3,39,],[-2,0,-1,-3,-4,]),'CHAR_LITERAL':([7,8,10,11,12,13,14,15,17,26,27,28,29,30,31,32,33,34,35,36,37,38,43,44,45,46,47,48,51,52,60,61,63,67,68,69,76,],[44,44,44,44,44,44,44,44,44,44,44,44,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-23,-40,-43,-44,-45,44,44,44,44,44,44,44,44,44,44,]),'VAL_LITERAL':([7,8,10,11,12,13,14,15,17,26,27,28,29,30,31,32,33,34,35,36,37,38,43,44,45,46,47,48,51,52,60,61,63,67,68,69,76,],[45,45,45,45,45,45,45,45,45,45,45,45,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,-23,-40,-43,-44,-45,45,45,45,45,45,45,45,45,45,45,]),'SCALAR_VAR':([7,8,10,11,12,13,14,15,17,18,26,27,28,29,30,31,32,33,34,35,36,37,38,42,43,44,45,46,47,48,51,52,53,60,61,62,63,67,68,69,71,75,76,],[46,46,46,46,46,46,46,46,46,58,46,46,46,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,58,-23,-40,-43,-44,-45,46,46,46,58,46,46,77,46,46,46,46,58,86,46,]),'REGISTER':([7,8,10,11,12,13,14,15,17,18,26,27,28,29,30,31,32,33,34,35,36,37,38,42,43,44,45,46,47,48,51,52,53,60,61,63,67,68,69,71,76,],[47,47,47,47,47,47,47,47,47,59,47,47,47,-11,-12,-13,-14,-15,-16,-17,-18,-19,-20,59,-23,-40,-43,-44,-45,47,47,47,59,47,47,47,47,47,47,59,47,]),':':([9,],[49,]),'ARRAY_VAR':([19,20,21,22,23,24,25,64,65,66,],[60,61,62,63,64,65,66,79,80,81,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'program':([0,],[1,]),'statements':([0,],[2,]),'complex_statement':([2,],[3,]),'statement_comment':([2,],[4,]),'optional_statement':([2,],[5,]),'statement':([2,],[6,]),'math_command':([2,],[8,]),'optional_comment':([5,],[40,]),'value':([7,8,10,11,12,13,14,15,17,26,27,28,48,51,52,60,61,63,67,68,69,76,],[42,48,50,51,52,53,54,55,56,67,68,69,71,72,73,75,76,78,82,83,84,87,]),'store_value':([18,42,53,71,],[57,70,74,85,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> program","S'",1,None,None,None),
  ('program -> statements','program',1,'p_program','bad_and_ugly_interpreter.py',295),
  ('statements -> <empty>','statements',0,'p_statements_empty','bad_and_ugly_interpreter.py',300),
  ('statements -> statements complex_statement','statements',2,'p_statements_nonempty','bad_and_ugly_interpreter.py',305),
  ('complex_statement -> statement_comment NEWLINE','complex_statement',2,'p_complex_statement','bad_and_ugly_interpreter.py',311),
  ('statement_comment -> optional_statement optional_comment','statement_comment',2,'p_statement_comment','bad_and_ugly_interpreter.py',317),
  ('optional_statement -> statement','optional_statement',1,'p_statement','bad_and_ugly_interpreter.py',323),
  ('optional_statement -> <empty>','optional_statement',0,'p_no_statement','bad_and_ugly_interpreter.py',329),
  ('optional_comment -> COMMENT','optional_comment',1,'p_comment','bad_and_ugly_interpreter.py',335),
  ('optional_comment -> <empty>','optional_comment',0,'p_no_comment','bad_and_ugly_interpreter.py',340),
  ('statement -> VAL_COPY value store_value','statement',3,'p_val_copy','bad_and_ugly_interpreter.py',345),
  ('math_command -> ADD','math_command',1,'p_math_command','bad_and_ugly_interpreter.py',351),
  ('math_command -> SUB','math_command',1,'p_math_command','bad_and_ugly_interpreter.py',352),
  ('math_command -> MULT','math_command',1,'p_math_command','bad_and_ugly_interpreter.py',353),
  ('math_command -> DIV','math_command',1,'p_math_command','bad_and_ugly_interpreter.py',354),
  ('math_command -> TEST_LESS','math_command',1,'p_math_command','bad_and_ugly_interpreter.py',355),
  ('math_command -> TEST_GTR','math_command',1,'p_math_command','bad_and_ugly_interpreter.py',356),
  ('math_command -> TEST_EQU','math_command',1,'p_math_command','bad_and_ugly_interpreter.py',357),
  ('math_command -> TEST_NEQU','math_command',1,'p_math_command','bad_and_ugly_interpreter.py',358),
  ('math_command -> TEST_GTE','math_command',1,'p_math_command','bad_and_ugly_interpreter.py',359),
  ('math_command -> TEST_LTE','math_command',1,'p_math_command','bad_and_ugly_interpreter.py',360),
  ('statement -> math_command value value store_value','statement',4,'p_math_statement','bad_and_ugly_interpreter.py',366),
  ('statement -> LABEL :','statement',2,'p_label_statement','bad_and_ugly_interpreter.py',372),
  ('value -> LABEL','value',1,'p_label_value','bad_and_ugly_interpreter.py',378),
  ('statement -> JUMP value','statement',2,'p_jump','bad_and_ugly_interpreter.py',384),
  ('statement -> JUMP_IF_0 value value','statement',3,'p_jump_if_0','bad_and_ugly_interpreter.py',390),
  ('statement -> JUMP_IF_N0 value value','statement',3,'p_jump_if_n0','bad_and_ugly_interpreter.py',396),
  ('statement -> RANDOM value store_value','statement',3,'p_random','bad_and_ugly_interpreter.py',402),
  ('statement -> OUT_VAL value','statement',2,'p_out_val','bad_and_ugly_interpreter.py',408),
  ('statement -> OUT_CHAR value','statement',2,'p_out_char','bad_and_ugly_interpreter.py',414),
  ('statement -> NOP','statement',1,'p_nop','bad_and_ugly_interpreter.py',420),
  ('statement -> PUSH value','statement',2,'p_push','bad_and_ugly_interpreter.py',426),
  ('statement -> POP store_value','statement',2,'p_pop','bad_and_ugly_interpreter.py',432),
  ('statement -> AR_GET_IDX ARRAY_VAR value SCALAR_VAR','statement',4,'p_ar_get_idx','bad_and_ugly_interpreter.py',438),
  ('statement -> AR_SET_IDX ARRAY_VAR value value','statement',4,'p_ar_set_idx','bad_and_ugly_interpreter.py',444),
  ('statement -> AR_GET_SIZE ARRAY_VAR SCALAR_VAR','statement',3,'p_ar_get_size','bad_and_ugly_interpreter.py',450),
  ('statement -> AR_SET_SIZE ARRAY_VAR value','statement',3,'p_ar_set_size','bad_and_ugly_interpreter.py',456),
  ('statement -> AR_COPY ARRAY_VAR ARRAY_VAR','statement',3,'p_ar_copy','bad_and_ugly_interpreter.py',462),
  ('statement -> AR_PUSH ARRAY_VAR ARRAY_VAR','statement',3,'p_ar_push','bad_and_ugly_interpreter.py',468),
  ('statement -> AR_POP ARRAY_VAR ARRAY_VAR','statement',3,'p_ar_pop','bad_and_ugly_interpreter.py',474),
  ('value -> CHAR_LITERAL','value',1,'p_char_literal','bad_and_ugly_interpreter.py',480),
  ('store_value -> SCALAR_VAR','store_value',1,'p_store_value','bad_and_ugly_interpreter.py',486),
  ('store_value -> REGISTER','store_value',1,'p_store_value','bad_and_ugly_interpreter.py',487),
  ('value -> VAL_LITERAL','value',1,'p_val_literal','bad_and_ugly_interpreter.py',493),
  ('value -> SCALAR_VAR','value',1,'p_scalar_var_value','bad_and_ugly_interpreter.py',499),
  ('value -> REGISTER','value',1,'p_register','bad_and_ugly_interpreter.py',505),
  ('statement -> LOAD value value','statement',3,'p_load','bad_and_ugly_interpreter.py',511),
  ('statement -> STORE value value','statement',3,'p_store','bad_and_ugly_interpreter.py',517),
  ('statement -> MEM_COPY value value','statement',3,'p_mem_copy','bad_and_ugly_interpreter.py',523),
]
