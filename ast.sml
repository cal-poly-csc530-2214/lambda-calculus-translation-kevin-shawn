datatype binaryOperator =
     BOP_PLUS
   | BOP_TIMES
;

datatype expression =
     EXP_ID of string
   | EXP_NUM of int
;

datatype statement =
     ST_EXP of {exp: expression}
   | ST_IFLEQ0 of {guard: expression, th: statement, el: statement}
   | ST_PRINTLN of {exp: statement}
   | ST_LAMBDA of {id: expression, st: statement}
   | ST_BINARY of {opr: binaryOperator, lft: statement, rht: statement}
;

datatype program =
   PROGRAM of {s: statement}
;
