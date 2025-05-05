module Main where
import Lexer
import Parser
import System.IO
import Data.List.Split
import System.Environment ( getArgs )
import Control.Exception ( catch, ErrorCall )
import Control.Monad (mapM_, ap)
import Data.List (intercalate, sort)
import Data.String.Utils (strip)

type Row = [String]
type Table = [Row]

eval :: Operation -> IO [[String]]
eval (OperationFileName fileName) = do
                                fileContent <- readFile fileName
                                let fileLines = lines fileContent
                                let table = map (map strip . splitOn ",") fileLines
                                putStr (unlines (map (intercalate ",") (sort table)))

                                let arityList = map length table
                                let firstArity = head arityList
                                let equalArities = all (== firstArity) arityList

                                let lastLine = last fileLines
                                let trailingNewLine = "" == lastLine
                                let invalidTrailingNewLine = trailingNewLine && firstArity > 1

                                case (equalArities, invalidTrailingNewLine) of
                                    (True, False) -> return table
                                    (False, _) -> error ("CSV File " ++ fileName ++ " does not have the same arity on each row.")
                                    (_, True) -> error ("CSV File " ++ fileName ++ " has an invalid trailing new line.")
                                
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

applyConditionList (ConditionSingle condition) row = applyCondition condition row
applyConditionList (ConditionList condition "AND" remainingConditions) row = applyCondition condition row && applyConditionList remainingConditions row
applyConditionList (ConditionList condition "OR" remainingConditions) row = applyCondition condition row || applyConditionList remainingConditions row

applyCondition (ConditionUnitValue (ColumnRef fileName columnNumber) operator value) row = case operator of
                                                                                            "Equals" -> (row !! columnNumber) == value
                                                                                            "NotEquals" -> (row !! columnNumber) /= value
applyCondition (ConditionUnitColumn (ColumnRef fileName1 columnNumber1) operator (ColumnRef fileName2 columnNumber2)) row = case operator of
                                                                                                                                "Equals" -> (row !! columnNumber1) == (row !! columnNumber2)
                                                                                                                                "NotEquals" -> (row !! columnNumber1) /= (row !! columnNumber2)


applyReplaceList (ReplaceListSingle replacement) condition row = if conditionMet then applyReplacement replacement row else row
    where conditionMet = applyConditionList condition row
applyReplaceList (ReplaceListMultiple replacement remainingReplacements) condition row = if conditionMet then applyReplaceList remainingReplacements condition (applyReplacement replacement row) else row
    where conditionMet = applyConditionList condition row


applyReplacement (ReplaceUnitValue (ColumnRef fileName columnNumber) newValue) row = take columnNumber row ++ [newValue] ++ drop (columnNumber+1) row
applyReplacement (ReplaceUnitColumn (ColumnRef fileName1 from) (ColumnRef fileName2 to)) row = take from row ++ [replacementValue] ++ drop (from+1) row
    where replacementValue = row !! to

applyProjectionList (ColumnListSingle columnNumber) row = [applyProjection columnNumber row]
applyProjectionList (ColumnListMultiple columnNumber remainingColumns) row = applyProjection columnNumber row : applyProjectionList remainingColumns row

applyProjection columnNumber row = row !! columnNumber

applyNaturalJoin table1 table2 (ConditionSingle (ConditionUnitColumn (ColumnRef fileName1 column1) "Equals" (ColumnRef fileName2 column2))) = [row1 ++ row2 | row1 <- table1, row2 <- table2, (row1 !! column1) == (row2 !! column2)]

applyLeftJoin table1 table2 (ConditionSingle (ConditionUnitColumn (ColumnRef fileName1 column1) "Equals" (ColumnRef fileName2 column2))) = concatMap (\row1 ->
      let matchingRows = [row2 | row2 <- table2, row1 !! column1 == row2 !! column2]
      in if null matchingRows
         then [row1 ++ replicate (length (head table2)) " "]
         else [row1 ++ row2 | row2 <- matchingRows]
    ) table1

applySemiJoin table1 table2 (ConditionSingle (ConditionUnitColumn (ColumnRef fileName1 column1) "Equals" (ColumnRef fileName2 column2))) = [row1 | row1 <- table1, any (\row2 -> row1 !! column1 == row2 !! column2) table2]

noParse :: ErrorCall -> IO ()
noParse e = do let err =  show e
               hPutStr stderr err
               return ()

main :: IO ()
main = catch main' noParse

main' = do (fileName : _ ) <- getArgs
           sourceText <- readFile fileName
           putStrLn ("To parse: " ++ sourceText)
           let parsedProg = csvParser (alexScanTokens sourceText)
           putStrLn ("Parsed: " ++ (show parsedProg))
           result <- eval parsedProg
           let csvFormattedOutput = unlines (map (intercalate ",") (sort result))
           putStrLn "Evaluated:"
           putStr csvFormattedOutput
           --let result = eval (parsedProg)
           --putStrLn ("Evaluated: " ++ (result) ++ "\n")


