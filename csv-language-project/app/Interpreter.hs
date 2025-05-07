module Main where
import Lexer
import Parser
import System.IO
import Data.List.Split
import System.Environment ( getArgs )
import Control.Exception ( catch, ErrorCall )
import Data.List (intercalate, sort, isSuffixOf)
import Data.String.Utils (strip)

type Row = [String]
type Table = [Row]

eval :: Operation -> IO Table
eval (OperationFileName fileName) = do
                                let validFile = ".csv" `isSuffixOf` fileName
                                (if validFile then (do fileContent <- readFile fileName
                                                       let fileLines = lines fileContent
                                                       let table = map (map strip . splitOn ",") fileLines
                                                       (if null table then return [] else (do let arityList = map length table
                                                                                              let firstArity = head arityList
                                                                                              let equalArities = all (== firstArity) arityList

                                                                                              let lastLine = last fileLines
                                                                                              let trailingNewLine = "" == lastLine
                                                                                              let invalidTrailingNewLine = trailingNewLine && firstArity > 1

                                                                                              case (equalArities, invalidTrailingNewLine) of
                                                                                                  (True, False) -> return table
                                                                                                  (False, _) -> error ("Error: The CSV File " ++ fileName ++ " does not have the same arity on each row.")
                                                                                                  (_, True) -> error ("Error: The CSV File " ++ fileName ++ " has an invalid trailing new line.")))) 
                                    else error "Error: You entered a file name without the .csv extension")

eval (OperationSelection conditionList operation) = do
                                table <- eval operation
                                let appliedFilters = filter (applyConditionList conditionList) table
                                return appliedFilters
eval (OperationReplace replaceList condition operation) = do
                                table <- eval operation
                                let appliedReplace = map (applyReplaceList replaceList condition) table
                                return appliedReplace
eval (OperationProject columnList operation) = do
                                table <- eval operation
                                let appliedProjection = map (applyProjectionList columnList) table
                                return appliedProjection
eval (OperationCartesian operation1 operation2) = do
                                table1 <- eval operation1
                                table2 <- eval operation2
                                let appliedCartesian = [row1 ++ row2 | row1 <- table1, row2 <- table2]
                                return appliedCartesian
eval (OperationUnion operation1 operation2) = do
                                table1 <- eval operation1
                                table2 <- eval operation2
                                let table1Arity = if null table1 then 0 else length (head table1)
                                let table2Arity = if null table2 then 0 else length (head table2)
                                (if table1Arity == table2Arity then (do
                                        let appliedUnion = table1 ++ table2
                                        return appliedUnion) else error "Error: You are trying to perform a union on two sets of data with different arities.")
eval (OperationDifference operation1 operation2) = do
                                table1 <- eval operation1
                                table2 <- eval operation2
                                let table1Arity = if null table1 then 0 else length (head table1)
                                let table2Arity = if null table2 then 0 else length (head table2)
                                (if table1Arity == table2Arity then (do
                                        let appliedDifference = filter (`notElem` table2) table1
                                        return appliedDifference) else error "Error: You are trying to perform a difference on two sets of data with different arities.")
eval (OperationJoin "Natural" operation1 condition operation2) = do
                                                                table1 <- eval operation1
                                                                table2 <- eval operation2
                                                                let naturalJoined = applyNaturalJoin table1 table2 condition
                                                                return naturalJoined
eval (OperationJoin "Left" operation1 condition operation2) = do
                                                                table1 <- eval operation1
                                                                table2 <- eval operation2
                                                                let leftJoined = applyLeftJoin table1 table2 condition
                                                                return leftJoined
eval (OperationJoin "Semi" operation1 condition operation2) = do
                                                                table1 <- eval operation1
                                                                table2 <- eval operation2
                                                                let semiJoined = applySemiJoin table1 table2 condition
                                                                return semiJoined
eval (OperationJoin {}) = error "Error: Invalid Join type"
eval (OperationSave fileName operation) = do table <- eval operation
                                             let csvFormattedOutput = intercalate "\n" (map (intercalate ",") (sort table))
                                             writeFile fileName csvFormattedOutput
                                             return table


applyConditionList :: Condition -> Row -> Bool
applyConditionList (ConditionSingle condition) row = applyCondition condition row
applyConditionList (ConditionList condition combinator remainingConditions) row = case combinator of
                                                                                    "AND" -> applyCondition condition row && applyConditionList remainingConditions row
                                                                                    "OR" -> applyCondition condition row || applyConditionList remainingConditions row
                                                                                    _ -> error ("You are using an invalid word to combine conditions: " ++ combinator)

applyCondition :: ConditionUnit -> Row -> Bool
applyCondition (ConditionUnitValue columnNumber operator value) row = case(operator, checkColumn) of
                                                                                    (_,False) -> error ("Error: You are checking a condition for Column Number " ++ show columnNumber ++ " which doesn't exist in row " ++ show row)
                                                                                    ("Equals", _) -> (row !! columnNumber) == value
                                                                                    ("NotEquals", _) -> (row !! columnNumber) /= value
                                                                                    _ -> error "Error: Invalid operator used within a condition"
                                                                    where checkColumn = columnNumber < length row
applyCondition (ConditionUnitColumn columnNumber1 operator columnNumber2) row = case (operator, checkColumn1, checkColumn2) of
                                                                                            (_,False,_) -> error ("Error: You are checking a condition for Column Number " ++ show columnNumber1 ++ " which doesn't exist in row " ++ show row)
                                                                                            (_,_,False) -> error ("Error: You are checking a condition for Column Number " ++ show columnNumber2 ++ " which doesn't exist in row " ++ show row)
                                                                                            ("Equals",_,_) -> (row !! columnNumber1) == (row !! columnNumber2)
                                                                                            ("NotEquals",_,_) -> (row !! columnNumber1) /= (row !! columnNumber2)
                                                                                            _ -> error "Error: Invalid operator used within a condition"
                                                                    where checkColumn1 = columnNumber1 < length row
                                                                          checkColumn2 = columnNumber2 < length row


applyReplaceList :: ReplaceList -> Condition -> Row -> Row
applyReplaceList (ReplaceListSingle replacement) condition row = if conditionMet then applyReplacement replacement row else row
    where conditionMet = applyConditionList condition row
applyReplaceList (ReplaceListMultiple replacement remainingReplacements) condition row = if conditionMet then applyReplaceList remainingReplacements condition (applyReplacement replacement row) else row
    where conditionMet = applyConditionList condition row


applyReplacement :: ReplaceUnit -> Row -> Row
applyReplacement (ReplaceUnitValue columnNumber newValue) row | columnNumber < length row = take columnNumber row ++ [newValue] ++ drop (columnNumber+1) row
                                                              | otherwise = error ("Error: You are trying to replace the value in Column Number " ++ show columnNumber ++ " which doesn't exist in row " ++ show row)
applyReplacement (ReplaceUnitColumn from to) row | from < rowLength && to < rowLength = take from row ++ [replacementValue] ++ drop (from+1) row
                                                 | from >= rowLength && to >= rowLength = error ("You are trying to perform a replacement with two non existing columns " ++ show from ++ " " ++ show to ++ " in row " ++ show row)
                                                 | from >= rowLength = error ("Error: You are trying to replace the value in Column Number " ++ show from ++ " which doesn't exist in row " ++ show row)
                                                 | to >= rowLength = error ("Error: You are trying to replace Column Number " ++ show from ++ " with the value in Column Number " ++ show to ++ " which doesn't exist in row " ++ show row)
                                                 | otherwise = error "Error: Invalid replacement"
    where rowLength = length row
          replacementValue = row !! to


applyProjectionList :: ColumnList -> Row -> Row
applyProjectionList (ColumnListSingle columnNumber) row = [applyProjection columnNumber row]
applyProjectionList (ColumnListMultiple columnNumber remainingColumns) row = applyProjection columnNumber row : applyProjectionList remainingColumns row

applyProjection :: Int -> Row -> String
applyProjection columnNumber row | columnNumber < length row = row !! columnNumber
                                 | otherwise = error ("Error: You are trying to project Column Number " ++ show columnNumber ++ " which doesn't exist in row " ++ show row)

applyNaturalJoin :: Table -> Table -> ConditionUnit -> Table
applyNaturalJoin table1 table2 (ConditionUnitColumn column1 "Equals" column2) = concatMap (\table1Row ->
            if column1 >= length table1Row then
                error ("Error: You are trying to perform a Natural Join using Column Number " ++ show column1 ++ " which doesn't exist in row " ++ show table1Row)
            else
                let combinedRows = [table1Row ++ table2Row | table2Row <- table2, if column2 >= length table2Row then
                                                                        error ("Error: You are trying to perform a Natural Join using Column Number " ++ show column2 ++ " which doesn't exist in row " ++ show table2Row)
                                                                     else
                                                                        table1Row !! column1 == table2Row !! column2]
                in combinedRows
    ) table1
applyNaturalJoin _ _ _ = error "Invalid join condition. Should be in the form of {columnNumber1} == {columnNumber2}"

applyLeftJoin :: Table -> Table -> ConditionUnit -> Table
applyLeftJoin table1 table2 (ConditionUnitColumn column1 "Equals" column2) = concatMap (\table1Row ->
            if column1 >= length table1Row then
                error ("Error: You are trying to perform a Left Join using Column Number " ++ show column1 ++ " which doesn't exist in row " ++ show table1Row)
            else
                let matchingRows = [table2Row | table2Row <- table2, if column2 >= length table2Row then
                                                                        error ("Error: You are trying to perform a Left Join using Column Number " ++ show column2 ++ " which doesn't exist in row " ++ show table2Row)
                                                                     else
                                                                        table1Row !! column1 == table2Row !! column2]
                    combinedRows = if null matchingRows then
                         [table1Row ++ replicate (if null table2 then 0 else length (head table2)) " "]
                         else [table1Row ++ row2 | row2 <- matchingRows]
                in combinedRows
    ) table1
applyLeftJoin _ _ _ = error "Invalid join condition. Should be in the form of {columnNumber1} == {columnNumber2}"

applySemiJoin :: Table -> Table -> ConditionUnit -> Table
applySemiJoin table1 table2 (ConditionUnitColumn column1 "Equals" column2) = [table1Row | table1Row <- table1, if column1 >= length table1Row then
                                                                                                        error ("Error: You are trying to perform a Semi Join using Column Number " ++ show column1 ++ " which doesn't exist in row " ++ show table1Row)
                                                                                                     else
                                                                                                        any (\table2Row -> if column2 >= length table2Row then
                                                                                                        error ("Error: You are trying to perform a Semi Join using Column Number " ++ show column2 ++ " which doesn't exist in row " ++ show table2Row)
                                                                                                                        else table1Row !! column1 == table2Row !! column2) table2]
applySemiJoin _ _ _ = error "Invalid join condition. Should be in the form of {columnNumber1} == {columnNumber2}"


noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

main :: IO ()
main = catch main' noParse

main' :: IO ()
main' = do (fileName : _ ) <- getArgs
           sourceText <- readFile fileName
           let parsedProg = csvParser (alexScanTokens sourceText)
           result <- eval parsedProg
           let csvFormattedOutput = unlines (map (intercalate ",") (sort result))
           putStr csvFormattedOutput