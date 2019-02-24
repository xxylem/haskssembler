module RunParser 
    ( parseASMLines
    , getErrLineNumber
    , getErrLineCode
    )
    where
        
-- ====================================================================================== --
-- RunParser.hs
-- The parseASMLines function will take a Program ([ByteString] alias) and output the program parsed
-- into the intermediate data representation described in the IntructionDataModel module.
-- ====================================================================================== --

import InstructionDataModel
import qualified Parsers as P

import Data.Attoparsec.ByteString.Char8
import qualified Data.Map.Strict as Map

-- ====================================================================================== --
-- Initialisation --

type Label = String
type AddressVal = Integer
type SymbolTable = Map.Map Label AddressVal
type NextAddressVal = Integer

addressInitVal :: NextAddressVal
addressInitVal = 16
initHSLineNumber :: AddressVal
initHSLineNumber = 0

initSymbolTable :: SymbolTable
initSymbolTable =
    Map.fromList [  ("SP",      0)
                 ,  ("LCL",     1)
                 ,  ("ARG",     2)
                 ,  ("THIS",    3)
                 ,  ("THAT",    4)
                 ,  ("R0",      0)
                 ,  ("R1",      1)
                 ,  ("R2",      2)
                 ,  ("R3",      3)
                 ,  ("R4",      4)
                 ,  ("R5",      5)
                 ,  ("R6",      6)
                 ,  ("R7",      7)
                 ,  ("R8",      8)
                 ,  ("R9",      9)
                 ,  ("R10",     10)
                 ,  ("R11",     11)
                 ,  ("R12",     12)
                 ,  ("R13",     13)
                 ,  ("R14",     14)
                 ,  ("R15",     15)
                 ,  ("SCREEN",  16384)
                 ,  ("KBD",     24576)
                 ]

-- ====================================================================================== --

-- ====================================================================================== --
-- Error Formats --

type ErrorMsg = String
data ParseError = InvalidLine ErrorMsg ASMLine
                | NoSymbol    ErrorMsg ASMLine
                    deriving (Show)
getErrLineNumber :: ParseError -> ASMLineNumber
getErrLineNumber (InvalidLine _ line) = getASMLineNumber line
getErrLineCode :: ParseError -> ASMCode
getErrLineCode (InvalidLine _ line) = getASMLineCode line

-- ====================================================================================== --

runParseSymbol :: SymbolTable
               -> NextAddressVal
               -> ASMLine 
               -> Either ParseError (SymbolTable, NextAddressVal, Instruction)
runParseSymbol symTab nextVal l =
    case parseOnly P.parseAddressSymbol (getASMLineCode l) of
        (Right sym) -> case Map.lookup sym symTab of
                            Just val -> Right (symTab, 
                                               nextVal, 
                                               AddressInstruction $ Address val)
                            Nothing  -> Right (Map.insert sym nextVal symTab,
                                               nextVal + 1,
                                               AddressInstruction $ Address nextVal)
        (Left err)    -> Left $ NoSymbol err l
        -- TODO a lot of state is managed through here, does state monad help?

runParseInstructionLine :: SymbolTable
                        -> NextAddressVal
                        -> ASMLine 
                        -> Either ParseError (SymbolTable, NextAddressVal, Instruction)
runParseInstructionLine symTab nextVal l =
    case parseOnly P.parseInstruction (getASMLineCode l) of
        (Right r)   -> Right (symTab, nextVal, r)
        (Left err)  -> case runParseSymbol symTab nextVal l of
                        res@(Right _) -> res
                        Left _ ->  Left $ InvalidLine err l

runParseASMInstructionLines :: SymbolTable 
                            -> [ASMLine] 
                            -> Either ParseError Program
runParseASMInstructionLines symTab asmLines = go symTab addressInitVal asmLines 0
            where go _      _              []     _          = Right []
                  go symTab' nextAddressVal (l:ls) lineNumber = 
                    case runParseInstructionLine symTab' nextAddressVal l of
                        Right (symTab'', nextAddressVal', instr) ->
                                                (:) 
                                            <$> Right (Line 
                                                       l 
                                                       (HSLine lineNumber instr)) 
                                            <*> go symTab'' nextAddressVal' ls (lineNumber + 1) 
                        Left err  -> Left err

moveLabelsToSymbolTable :: [ASMLine] 
                       -> ([ASMLine], SymbolTable)
moveLabelsToSymbolTable asmLines = 
    go asmLines initHSLineNumber initSymbolTable
        where   tupApply (f, g) (a, b) = (f a, g b)
                go []     _          symTab = ([], symTab)
                go (l:ls) lineNumber symTab =
                    case parseOnly P.parseAddressLabel (getASMLineCode l) of
                        Right label ->  tupApply (id, Map.insert label lineNumber)
                                                 (go ls lineNumber symTab)
                        Left  _     ->  tupApply ((:) l, id)
                                                 (go ls (lineNumber + 1) symTab)
                
removeCommentsAndEmptyLines :: [ASMLine] 
                            -> [ASMLine]
removeCommentsAndEmptyLines = filter (not . runParseIsEmptyLineOrComment)
            where runParseIsEmptyLineOrComment l =
                    case parseOnly P.parseComment (getASMLineCode l) of
                        (Right _) -> True
                        (Left _)  -> False

parseASMLines :: [ASMLine] 
              -> Either ParseError Program
parseASMLines ls =
    runParseASMInstructionLines labelMap withoutCommentsOrLabels
        where (withoutCommentsOrLabels, labelMap) = 
                moveLabelsToSymbolTable $ removeCommentsAndEmptyLines ls