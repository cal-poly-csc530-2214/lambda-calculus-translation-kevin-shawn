use "parser.sml";

fun printOut msg = TextIO.output (TextIO.stdOut, msg); 

fun bopToStr bop = 
    case bop of BOP_PLUS => "+"
    | BOP_TIMES => "*"
;

fun printExpression (EXP_NUM n) = 
	(
		printOut (Int.toString n)
	)
	| printExpression (EXP_ID id) = 
	(
		printOut id
	)
;

fun printStatement (ST_IFLEQ0 {guard, th, el}) =
    (
        printOut "if ( "; 
		printExpression guard;
		printOut " <= 0 )\n\t";
        printStatement th; 
		printOut "\n";
		printOut "else\n\t";
		printStatement el
    )
    | printStatement (ST_PRINTLN {exp}) = 
    (
        printOut "println ";
        printStatement exp
    )
    | printStatement (ST_LAMBDA {id, st}) = 
    (
	    printOut "( ";
        printExpression id;
        printOut " => ";
		printStatement st;
		printOut " )"
    )
	| printStatement (ST_BINARY {opr, lft, rht}) = 
    (
        printOut "(";
        printStatement lft;
        printOut " ";
        printOut (bopToStr opr);
        printOut " ";
        printStatement rht;
        printOut ")"
    )
    | printStatement (ST_EXP {exp}) =
    (
        printExpression exp
    )
;

fun printjs (PROGRAM {s}) =
(
    printStatement s;
	printOut "\n"
)
;