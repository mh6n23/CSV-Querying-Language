{
module Lexer where
}
%wrapper "basic"
$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
$white+             ; -- Ignore whitespace
"--".*              ; -- Ignore comments
$digit+             {\s -> TokenInt (read s)}
"csv"               {\s -> TokenCSV}
"X"                 {\s -> TokenCartesian}
"ON"                {\s -> TokenOn}
"SELECT"                 {\s -> TokenSelectionOperator}
"REPLACE"           {\s -> TokenReplaceOperator}
"PROJECT"                 {\s -> TokenProjectionOperator}
"("                 {\s -> TokenLeftBracket}
")"                 {\s -> TokenRightBracket}
"["                 {\s -> TokenLeftSquareBracket}
"]"                 {\s -> TokenRightSquareBracket}
","                 {\s -> TokenComma}
"."                 {\s -> TokenDot}
"AND"               {\s -> TokenCombinator "AND"}
"OR"                {\s -> TokenCombinator "OR"}
"=="                {\s -> TokenOperator "Equals"}
"<="                {\s -> TokenOperator "LessThanOrEqualTo"}
">="                {\s -> TokenOperator "GreaterThanOrEqualTo"}
"<"                 {\s -> TokenOperator "LessThan"}
">"                 {\s -> TokenOperator "GreaterThan"}
"->"                {\s -> TokenArrow}
"NaturalJoin"       {\s -> TokenJoinType "Natural"}
"OuterJoin"         {\s -> TokenJoinType "Outer"}
"SemiJoin"          {\s -> TokenJoinType "Semi"}
$alpha [$alpha $digit \_ \']*       { \s -> TokenString s } 

{
    data Token =
        TokenInt Int            |
        TokenCSV                |
        TokenCartesian          |
        TokenOn                 |
        TokenReplaceOperator    |
        TokenSelectionOperator  |
        TokenProjectionOperator |
        TokenLeftBracket        |
        TokenRightBracket       |
        TokenLeftSquareBracket  |
        TokenRightSquareBracket |
        TokenComma              |
        TokenDot                |
        TokenCombinator String  |
        TokenOperator String    |
        TokenArrow              |
        TokenString String      |
        TokenJoinType String
        deriving (Eq, Show)
}