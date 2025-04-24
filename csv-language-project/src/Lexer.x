{
    module Lexer where
}
%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]

tokens:-
$white+             ; -- Ignore whitespace
"--".*              ; -- Ignore comments
$digit+             {\s -> TokenInt (read s)}
"X"                 {\s -> TokenCartesian}
"ON"                {\s -> TokenOn}
"σ"                 {\s -> TokenSelectionOperator}
"REPLACE"           {\s -> TokenReplaceOperator}
"π"                 {\s -> TokenReplaceOperator}
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
        TokenInt Int deriving   |
        TokenCartesian          |
        TokenOn                 |
        TokenReplaceOperator    |
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
        TokenString s           |
        TokenJoinType String
        deriving (Eq, Show)


}