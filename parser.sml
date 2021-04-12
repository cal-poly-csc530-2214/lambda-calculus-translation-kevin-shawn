use "tokenizer.sml";
use "ast.sml";

fun tkToStr (t: token) : string = 
    case t of TK_LPAREN => "("
   | TK_RPAREN => ")"
   | (TK_NUM n) => Int.toString n
   | (TK_ID x) => x
   | TK_PLUS => "+"
   | TK_TIMES => "*"
   | TK_LAMBDA => "/"
   | TK_ASSIGN => "=>"
   | TK_PRINTLN => "println"
   | TK_IFLEQ0 => "ifleq0"
;

fun tkListToStr tks = 
    case tks of [] => ""
    | x::xs => (tkToStr x) ^ (tkListToStr xs)
;

(* helper functions *)
fun exitFail msg = 
(
      TextIO.output (TextIO.stdErr, msg^"\n"); 
      OS.Process.exit OS.Process.failure
)
;

fun readFile file =
    TextIO.inputAll (TextIO.openIn file)
;

fun matchErr expected found  =
    exitFail ("expected '" ^ expected ^ "', found '" ^ found)
;

fun noneFoundErr expected =
    exitFail ("expected '" ^ expected ^ "', no token found")
;

fun matchToken tks expect =
    case tks of [] => matchErr (tkToStr expect) "[]"
    | x::xs =>
    (
        if x = expect then xs
        else matchErr (tkToStr expect) (tkToStr x)
    )
;

fun startsExpression t =
    case t of (TK_NUM _) => true
    | (TK_ID _) => true
    | TK_PLUS => true
    | _ => false
;

fun startsStatement t = 
    case t of TK_LAMBDA => true
    | TK_PRINTLN => true
    | TK_IFLEQ0 => true
    | TK_TIMES => true
    | TK_LPAREN => true	
    | _ =>
    (
        if startsExpression t then true
        else false
    )
;

fun parseExpression tks =
	case tks of [] => matchErr "expression" "[]"
	| TK_LPAREN::xs => 
	(
		let 
			val (ex1, tks1) = parseExpression xs;
			val tks2 = matchToken tks1 TK_RPAREN;
        in
			(ex1, tks2)
        end
	)
	| _ => parsePrimaryExpression tks
and parsePrimaryExpression (TK_LPAREN::tks) =
(
    let
        val (ex, tks2) = parseExpression tks;
        val tks2 = matchToken tks2 TK_RPAREN;
    in
        (ex, tks2)
    end
)
| parsePrimaryExpression (TK_ID id::tks)     = (EXP_ID id, tks)
| parsePrimaryExpression (TK_NUM n::tks)     = (EXP_NUM n, tks)
| parsePrimaryExpression (t::_)              = exitFail (tkToStr t)
| parsePrimaryExpression ([])                = noneFoundErr "expression"
;

fun parseStatement tks = 
    case tks of [] => matchErr "statement" "[]"
    | TK_PRINTLN::xs => parsePrintStatement xs
    | TK_IFLEQ0::xs => parseIfLEq0Statement xs
	| TK_LAMBDA::xs => parseLambdaStatement xs
	| TK_PLUS::xs => parseAddExpression xs
	| TK_TIMES::xs => parseMultiExpression xs
	| TK_LPAREN::xs =>
	(
		let 
			val tks1 = matchToken tks TK_LPAREN;
            val (st1, tks2) = parseStatement tks1;
			val tks3 = matchToken tks2 TK_RPAREN;
        in
			(st1, tks3)
        end
	)
	| x::xs =>
	(
        if startsExpression x then 
        (
            let 
                val (ex1, tks1) = parseExpression tks;
            in
                (ST_EXP {exp=ex1}, tks1)
            end
        )
        else matchErr "statement" (tkToStr x)		
	)
and parseAddExpression tks =
(
	let 
		val (ex1, tks2) = parseStatement tks;
        val (ex2, tks3) = parseStatement tks2;
    in
		(ST_BINARY {opr=BOP_PLUS, lft=ex1, rht=ex2}, tks3)
    end	
)
and parseMultiExpression tks =
(
	let 
		val (s1, tks2) = parseStatement tks;
        val (s2, tks3) = parseStatement tks2;
    in
		(ST_BINARY {opr=BOP_TIMES, lft=s1, rht=s2}, tks3)
    end	
)	
and parsePrintStatement tks =
    let 
        val (printExp, tks1) = parseStatement tks;
    in
        (ST_PRINTLN {exp=printExp}, tks1)
    end       
and parseIfLEq0Statement tks =
    let
        val (ex1, tks1) = parseExpression tks;
        val (st1, tks2) = parseStatement tks1;
        val (st2, tks3) = parseStatement tks2;		
    in 
        (ST_IFLEQ0 {guard=ex1, th=st1, el=st2}, tks3)
    end    
and parseLambdaStatement tks =
    let
		val (id1, tks1) = parseExpression tks;
		val tks2 = matchToken tks1 TK_ASSIGN;
        val (st1, tks3) = parseStatement tks2;
    in 
        (ST_LAMBDA {id=id1, st=st1}, tks3)
    end 	
;

fun parseProgram tks =
    let 
        val (s1, tks2) = parseStatement tks;
    in
        PROGRAM {s=s1}
    end
;

(* Part 1, 2 *)
fun parse file = 
    let 
        val tks = tokenize (readFile file);
    in
        parseProgram tks
    end
;

