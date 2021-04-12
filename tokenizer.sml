Control.Print.printDepth := 20;
Control.Print.printLength := 100;

use "tokens.sml";

val OPTIONAL = true;

exception InvalidSymbol of string;
exception InvalidNumber of string;
exception IllegalCharacter of char;

val keywordTokens =
   [
      ("println", TK_PRINTLN),
      ("ifleq0", TK_IFLEQ0)
   ]
;

fun member s xs = List.exists (fn st => st = s) xs;

fun pairLookup s xs =
   case List.find (fn (st, _) => st = s) xs of
      NONE => NONE
   |  SOME (_, v) => SOME v
;

local
   fun findPrefix pred (prefix, []) = (rev prefix, [])
     | findPrefix pred (prefix, suffix as x::xs) =
         if pred x
         then findPrefix pred (x::prefix, xs)
         else (rev prefix, suffix)
   ;
in
   fun prefixBy (pred, vals) = findPrefix pred ([], vals)
end;

fun skipSpaces (chars, rtokens) =
   (#2 (prefixBy (Char.isSpace, chars)), rtokens)
;

fun makeIdentifierKeyword id =
   case pairLookup id keywordTokens of
      NONE => TK_ID id
   |  SOME tk => tk
;

fun makeNumber s =
   case Int.fromString s of
      SOME n => TK_NUM n
   |  NONE => raise InvalidNumber s
;

fun buildToken isChar make (chars, rtokens) =
   let
      val (token, rest) = prefixBy (isChar, chars);
   in
      (rest, (make (implode token))::rtokens)
   end
;

val buildIdentifier = buildToken Char.isAlphaNum makeIdentifierKeyword;
val buildNumber = buildToken Char.isDigit makeNumber;

fun buildSymbol (chars, rtokens) =
   let
      fun single tk = (List.drop (chars, 1), tk::rtokens);
      fun double tk = (List.drop (chars, 2), tk::rtokens);
   in
      case chars of
         #"="::(#">"::_) => double TK_ASSIGN
       | #"/"::_ => single TK_LAMBDA
       | #"("::_ => single TK_LPAREN
       | #")"::_ => single TK_RPAREN
       | #"+"::_ => single TK_PLUS
       | #"*"::_ => single TK_TIMES
       | c::_ => raise IllegalCharacter c
       | _ => raise InvalidSymbol "<empty input>"
   end
;

local
   fun selectPath c =
      if Char.isSpace c then skipSpaces
      else if Char.isAlpha c then buildIdentifier
      else if Char.isDigit c then buildNumber
      else buildSymbol
   ;

   fun gatherTokens ([], rtokens) = rev rtokens
     | gatherTokens (chars, rtokens) =
         gatherTokens ((selectPath (hd chars)) (chars, rtokens))
   ;
in
   fun tokenize input = gatherTokens (explode input, [])
end
;

fun runit filename = tokenize (TextIO.inputAll (TextIO.openIn filename));
