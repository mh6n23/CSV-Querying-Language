module Main where
import Lexer
import Parser
import System.IO
import Data.List.Split
import System.Environment ( getArgs )
import Control.Exception ( catch, ErrorCall )
import Control.Monad (mapM_, ap)
import Data.List (intercalate)
import Data.String.Utils (strip)

type Row = [String]
type Table = [Row]

eval :: Operation -> IO [[String]]
eval (OperationFileName fileName) = do 
                                fileContent <- readFile fileName
                                let fileLines = lines fileContent
                                let table = map (map strip . splitOn ",") fileLines
                                return table
eval (OperationSelection conditionList operation) = do
                                table <- eval operation
                                let appliedFilters = filter (applyConditionList conditionList) table
                                return appliedFilters
eval (OperationReplace replaceList operation) = do
                                table <- eval operation
                                let appliedReplace = map (applyReplaceList replaceList) table
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

applyConditionList (ConditionSingle condition) row = applyCondition condition row
applyConditionList (ConditionList condition "AND" remainingConditions) row = applyCondition condition row && applyConditionList remainingConditions row
applyConditionList (ConditionList condition "OR" remainingConditions) row = applyCondition condition row || applyConditionList remainingConditions row

applyCondition (ConditionUnitValue (ColumnRef fileName columnNumber) "Equals" value) row = (row !! columnNumber) == value  

applyReplaceList (ReplaceListSingle replacement) row = applyReplacement replacement row
applyReplaceList (ReplaceListMultiple replacement remainingReplacements) row = applyReplaceList remainingReplacements (applyReplacement replacement row)

applyReplacement (ReplaceUnitValue (ColumnRef fileName columnNumber) newValue) row = take columnNumber row ++ [newValue] ++ drop (columnNumber+1) row 

applyProjectionList (ColumnListSingle columnNumber) row = [applyProjection columnNumber row]
applyProjectionList (ColumnListMultiple columnNumber remainingColumns) row = applyProjection columnNumber row : applyProjectionList remainingColumns row

applyProjection columnNumber row = row !! columnNumber

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
           let csvFormattedOutput = unlines (map (intercalate ",") result)
           putStrLn "Evaluated:"
           putStr csvFormattedOutput
           --let result = eval (parsedProg)
           --putStrLn ("Evaluated: " ++ (result) ++ "\n")


