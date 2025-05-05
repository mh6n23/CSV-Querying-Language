{
module Parser where
import Lexer
}

%name csvParser
%tokentype { Token }
%error { parseError }
%token
    int                 { TokenInt $$ }
    quotedstring        { TokenQuotedString $$}
    string              { TokenString $$ }
    csv                 { TokenCSV }
    cartesian           { TokenCartesian }
    on                  { TokenOn }
    selection           { TokenSelectionOperator }
    replace             { TokenReplaceOperator }
    where               { TokenWhere }
    projection          { TokenProjectionOperator }
    leftbracket         { TokenLeftBracket }
    rightbracket        { TokenRightBracket }
    leftsquarebracket   { TokenLeftSquareBracket }
    rightsquarebracket  { TokenRightSquareBracket}
    comma               { TokenComma }
    dot                 { TokenDot}
    combinator          { TokenCombinator $$ }
    operator            { TokenOperator $$}
    arrow               { TokenArrow}
    join                { TokenJoinType $$}

%%
operation : string dot csv {OperationFileName ($1 ++ ".csv")}
            | selection leftsquarebracket condition rightsquarebracket leftbracket operation rightbracket { OperationSelection $3 $6 }
            | replace leftsquarebracket replaceList rightsquarebracket where leftsquarebracket condition rightsquarebracket leftbracket operation rightbracket { OperationReplace $3 $7 $10 }
            | projection leftsquarebracket columnList rightsquarebracket leftbracket operation rightbracket { OperationProject $3 $6}
            | join leftbracket operation rightbracket on leftsquarebracket condition rightsquarebracket leftbracket operation rightbracket { OperationJoin $1 $3 $7 $10 }
            | leftbracket operation rightbracket cartesian leftbracket operation rightbracket { OperationCartesian $2 $6 }

columnList : int { ColumnListSingle $1 }
                | int comma columnList {ColumnListMultiple $1 $3}

condition : conditionUnit { ConditionSingle $1 }
            | conditionUnit combinator condition {ConditionList $1 $2 $3}

conditionUnit : column operator quotedstring { ConditionUnitValue $1 $2 $3 }
                | column operator column {ConditionUnitColumn $1 $2 $3}

replaceList : replaceUnit { ReplaceListSingle $1 }
                | replaceUnit comma replaceList { ReplaceListMultiple $1 $3 }

replaceUnit : column arrow column { ReplaceUnitColumn $1 $3 }
                | column arrow quotedstring { ReplaceUnitValue $1 $3 }

column : string dot int { ColumnRef $1 $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error" 

data Operation = OperationFileName String |
                     OperationSelection Condition Operation |
                     OperationReplace ReplaceList Condition Operation |
                     OperationProject ColumnList Operation |
                     OperationJoin String Operation Condition Operation |
                     OperationCartesian Operation Operation
                     deriving (Show, Eq)

data Condition = ConditionList ConditionUnit String Condition -- not sure if combinator needed here, string instead? -- ConditionUnit Combinator Condition
                | ConditionSingle ConditionUnit
                        deriving (Show, Eq)

data ConditionUnit = ConditionUnitValue Column String String -- not sure if operator needed
                            | ConditionUnitColumn Column String Column
                        deriving (Show, Eq)


data ReplaceList = ReplaceListMultiple ReplaceUnit ReplaceList |
                            ReplaceListSingle ReplaceUnit
                            deriving (Show, Eq)

data Column = ColumnRef String Int
                    deriving (Show, Eq)

data ReplaceUnit = ReplaceUnitColumn Column Column |
                            ReplaceUnitValue Column String
                            deriving (Show, Eq)

data ColumnList = ColumnListMultiple Int ColumnList |
                  ColumnListSingle Int
                    deriving (Show, Eq)
}