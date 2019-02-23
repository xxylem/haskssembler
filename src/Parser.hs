{-# LANGUAGE OverloadedStrings #-}

module Parser 
    ( parseASMLines
    , getErrLineNumber
    , getErrLineCode
    )
    where

import InstructionDataModel

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator (lookAhead)
import qualified Data.Map.Strict as Map

addressInitVal :: NextAddressVal
addressInitVal = 16
initHSLineNumber :: AddressVal
initHSLineNumber = 0

-- ====== --
-- PARSER --
-- The parseASMLines function will take a Program ([ByteString] alias) and output the program parsed
-- into the intermediate data representation described in the IntructionDataModel module.
-- ====== --

-- A Instructions
parseAddress :: Parser Address
parseAddress =
        skipSpace
    >>  char '@' 
    >>  Address <$> decimal 

parseAddressSymbol :: Parser String
parseAddressSymbol =
        skipSpace
    >>  char '@'
    >>  manyTill' anyChar   (lookAhead  (eitherP    endOfInput
                                                    (       char ' '
                                                        <|> char '/'
                                                        <|> char '\r'
                                                    )
                                        )
                            )
    <*  parseComment

parseAddressInstruction :: Parser Instruction
parseAddressInstruction = AddressInstruction <$> parseAddress

-- C Instructions
parseComputation :: Parser Computation
parseComputation =
        (char '0'       >> return ZERO)
    <|> (char '1'       >> return ONE)
    <|> (string "-1"    >> return MINUS_ONE)
    <|> (string "!D"    >> return NOT_D)
    <|> (string "!A"    >> return NOT_A)
    <|> (string "-D"    >> return NEG_D)
    <|> (string "-A"    >> return NEG_A)
    <|> (string "D+1"   >> return D_PLUS_1)
    <|> (string "A+1"   >> return A_PLUS_1)
    <|> (string "D-1"   >> return D_MINUS_1)
    <|> (string "A-1"   >> return A_MINUS_1)
    <|> (string "D+A"   >> return D_PLUS_A)
    <|> (string "D-A"   >> return D_MINUS_A)
    <|> (string "A-D"   >> return A_MINUS_D)
    <|> (string "D&A"   >> return D_AND_A)
    <|> (string "D|A"   >> return D_OR_A)
    <|> (string "!M"    >> return NOT_M)
    <|> (string "-M"    >> return NEG_M)
    <|> (string "M+1"   >> return M_PLUS_1)
    <|> (string "M-1"   >> return M_MINUS_1)
    <|> (string "D+M"   >> return D_PLUS_M)
    <|> (string "D-M"   >> return D_MINUS_M)
    <|> (string "M-D"   >> return M_MINUS_D)
    <|> (string "D&M"   >> return D_AND_M)
    <|> (string "D|M"   >> return D_OR_M)
    <|> (char 'D'       >> return D_COMP)
    <|> (char 'A'       >> return A_COMP)
    <|> (char 'M'       >> return M_COMP)


parseDestination :: Parser Destination
parseDestination =
        (   (   (string "AMD" >> return AMD)
            <|> (string "AM" >> return AM)
            <|> (string "AD" >> return AD)
            <|> (string "MD" >> return MD)
            <|> (char 'M' >> return M_DEST)
            <|> (char 'A' >> return A_DEST)
            <|> (char 'D' >> return D_DEST)
            ) 
            <*  char '=')
    <|> return NULL_DEST

parseJump :: Parser Jump
parseJump =
        (   char ';' 
        >>
            (   (string "JGT" >> return JGT)
            <|> (string "JEQ" >> return JEQ)
            <|> (string "JGE" >> return JGE)
            <|> (string "JLT" >> return JLT)
            <|> (string "JNE" >> return JNE)
            <|> (string "JLE" >> return JLE)
            <|> (string "JMP" >> return JMP)
            )
        )
    <|> return NULL_JUMP

parseComputationInstruction :: Parser Instruction
parseComputationInstruction = do
    skipSpace
    dest <- parseDestination
    skipSpace
    comp <- parseComputation
    skipSpace
    jump <- parseJump
    return $ ComputeInstruction comp dest jump

parseComment :: Parser ()
parseComment =
        skipSpace 
    >>  (   (string "//" >> return ())
        <|> endOfInput
        )

parseInstruction :: Parser Instruction
parseInstruction = 
        skipSpace 
    >>  (   parseAddressInstruction
        <|> parseComputationInstruction
        )
    <*  parseComment

parseLabel :: Parser String
parseLabel =
        skipSpace
    >>  char '('
    >>  manyTill' anyChar (char ')') 
    <*  parseComment

type ErrorMsg = String
data ParseError = InvalidLine ErrorMsg ASMLine
                | NoSymbol    ErrorMsg ASMLine
                    deriving (Show)
getErrLineNumber :: ParseError -> ASMLineNumber
getErrLineNumber (InvalidLine _ line) = getASMLineNumber line
getErrLineCode :: ParseError -> ASMCode
getErrLineCode (InvalidLine _ line) = getASMLineCode line

type Label = String
type AddressVal = Integer
type SymbolTable = Map.Map Label AddressVal
type NextAddressVal = Integer

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

runParseSymbol :: SymbolTable
               -> NextAddressVal
               -> ASMLine 
               -> Either ParseError (SymbolTable, NextAddressVal, Instruction)
runParseSymbol symTab nextVal l =
    case parseOnly parseAddressSymbol (getASMLineCode l) of
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
    case parseOnly parseInstruction (getASMLineCode l) of
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
                    case parseOnly parseLabel (getASMLineCode l) of
                        Right label ->  tupApply (id, Map.insert label lineNumber)
                                                 (go ls lineNumber symTab)
                        Left  _     ->  tupApply ((:) l, id)
                                                 (go ls (lineNumber + 1) symTab)
                
removeCommentsAndEmptyLines :: [ASMLine] 
                            -> [ASMLine]
removeCommentsAndEmptyLines = filter (not . runParseIsEmptyLineOrComment)
            where runParseIsEmptyLineOrComment l =
                    case parseOnly parseComment (getASMLineCode l) of
                        (Right _) -> True
                        (Left _)  -> False

parseASMLines :: [ASMLine] 
              -> Either ParseError Program
parseASMLines ls =
    runParseASMInstructionLines labelMap withoutCommentsOrLabels
        where (withoutCommentsOrLabels, labelMap) = 
                moveLabelsToSymbolTable $ removeCommentsAndEmptyLines ls