{
    module Parser where
    import Lexer.x
}

%name csvParser
%tokentype { Token }
%error { parseError }
%token
    int                 { TokenInt $$ }
    string              { TokenString $$ }
    cartesian           { TokenCartesian }
    on                  { TokenOn }
    selection           { TokenSelectionOperator }
    replace             { TokenReplaceOperator }
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
    operation : string {OperationFileName $1}
            | selection leftsquarebracket condition rightsquarebracket leftbracket operation rightbracket { OperationSelection $3 $6 }
            | replace leftsquarebracket replaceList rightsquarebracket leftbracket operation rightbracket { OperationReplace $3 $6 }
            | projection leftsquarebracket columnList rightsquarebracket leftbracket operation rightbracket { Operation Project $3 $6}
            | joinType leftbracket operation rightbracket on leftsquarebracket condition rightsquarebracket leftbracket operation rightbracket { OperationJoin $1 $3 $7 $10 }
            | leftbracket operation rightbracket cartesian leftbracket operation rightbracket { OperationCartesian $2 $6 }

    columnList : int { ColumnList $1 }
                | int comma columnList {columnList $1 $3}

    condition : conditionUnit { ConditionSingle $1 }
            | conditionUnit combinator conditionUnit {ConditionList $1 $2 $3}

    conditionUnit : column operator value { ConditionUnitValue $1 $2 $3 }
                | column operator column {ConditionUnitColumn $1 $2 $3}

    replaceList : replaceUnit { ReplaceListSingle $1 }
                | replaceUnit + "," + replaceList { ReplaceListMultiple $1 $3 }

    replaceUnit : column arrow column { ReplaceUnitColumn $1 $3 }
                | column arrow string { ReplaceUnitValue $1 $3 }

    column : string dot int { ColumnRef $1 $3 }

   


    joinType :

    {
        parseError :: [Token] -> a
        parseError _ = error "Parse error" 

        data Operation = OperationFileName String |
                         OperationSelection Condition Operation |
                         OperationReplace ReplaceList Operation |
                         OperationProject ColumnList Operation |
                         OperationJoin JoinType Operation Condition Operation |
                         OperationCartesian Operation Operation
                        deriving (Show, Eq)

        data Condition = ConditionList ConditionUnit Combinator Condition -- not sure if combinator needed here, string instead?
                        | ConditionSingle ConditionUnit
                        deriving (Show, Eq)

        data ConditionUnit = ConditionUnitValue Column Operator String -- not sure if operator needed
                            | ConditionUnitColumn Column Operator Column
                        deriving (Show, Eq)



        data ReplaceList = ReplaceListMultiple ReplaceUnit ReplaceList |
                            ReplaceListSingle ReplaceUnit
                            deriving (Show, Eq)

        data Column = ColumnRef String Int
                    deriving (Show, Eq)

        data ReplaceUnit = ReplaceUnitColumn Column Column |
                            ReplaceUnitValue Column String
                            deriving (Show, Eq)

        data ColumnList = ColumnList Int ColumnList


    }