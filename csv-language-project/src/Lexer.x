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
"SELECT"            {\s -> TokenSelectionOperator}
"REPLACE"           {\s -> TokenReplaceOperator}
"WHERE"             {\s -> TokenWhere}
"PROJECT"           {\s -> TokenProjectionOperator}
"("                 {\s -> TokenLeftBracket}
")"                 {\s -> TokenRightBracket}
"["                 {\s -> TokenLeftSquareBracket}
"]"                 {\s -> TokenRightSquareBracket}
","                 {\s -> TokenComma}
"."                 {\s -> TokenDot}
"AND"               {\s -> TokenCombinator "AND"}
"OR"                {\s -> TokenCombinator "OR"}
"=="                {\s -> TokenOperator "Equals"}
"!="                {\s -> TokenOperator "NotEquals"}
"->"                {\s -> TokenArrow}
"NaturalJoin"       {\s -> TokenJoinType "Natural"}
"SemiJoin"          {\s -> TokenJoinType "Semi"}
"LeftJoin"          {\s -> TokenJoinType "Left"}
"SAVE"              {\s -> TokenSave}
"UNION"             {\s -> TokenUnion}
"-"                 {\s -> TokenDifference}
\"[^\"]*\"          {\s -> TokenQuotedString (init (tail s))}

{
    data Token =
        TokenInt Int            |
        TokenCSV                |
        TokenCartesian          |
        TokenOn                 |
        TokenReplaceOperator    |
        TokenWhere              |
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
        TokenQuotedString String|
        TokenString String      |
        TokenJoinType String    |
        TokenSave               |
        TokenUnion              |
        TokenDifference
        deriving (Eq, Show)
}