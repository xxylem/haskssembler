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
import qualified Data.Map.Strict as Map
-- import qualified Data.ByteString.Char8 as BS

-- ====== --
-- PARSER --
-- The parseASMLines function will take a Program ([ByteString] alias) and output the program parsed
-- into the intermediate data representation described in the IntructionDataModel module.
-- ====== --

-- A Instructions
parseAddress :: Parser Address
parseAddress =
        char '@' 
    >>  Address <$> decimal 

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

-- Both Instructions
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

-- (LOOP) skipspace -> open bracket -> many Not ')' -> ')' -> parseComment

type ErrorMsg = String
data ParseError = InvalidLine ErrorMsg ASMLine
                    deriving (Show)
getErrLineNumber :: ParseError -> ASMLineNumber
getErrLineNumber (InvalidLine _ line) = getASMLineNumber line
getErrLineCode :: ParseError -> ASMCode
getErrLineCode (InvalidLine _ line) = getASMLineCode line

runParseInstructionLine :: ASMLine -> Either ParseError Instruction
runParseInstructionLine l = 
    case parseOnly parseInstruction (getASMLineCode l) of
        (Right r)   -> Right r
        (Left err)  -> Left $ InvalidLine err l

runParseASMInstructionLines :: [ASMLine] -> Either ParseError Program
runParseASMInstructionLines asmLines = go asmLines 1
            where go []     _           = Right []
                  go (l:ls) lineNumber  = 
                    case runParseInstructionLine l of
                        Right instr           -> (:) <$> Right (Line l (HSLine lineNumber instr)) <*> 
                                                    go ls (lineNumber + 1)
                        Left err  -> Left err

moveLabelsToDictionary :: [ASMLine] -> ([ASMLine], Map.Map String Integer)
moveLabelsToDictionary asmLines = 
    go asmLines 0 (asmLines, Map.empty)
        where   go []     _          rsf                = rsf 
                go (l:ls) lineNumber (l':ls', rsfMap)   =
                    case parseOnly parseLabel (getASMLineCode l) of
                        Right label -> go ls 
                                          lineNumber
                                          (ls', Map.insert label lineNumber rsfMap)
                        Left  _     -> go ls 
                                          (lineNumber + 1) 
                                          (l':ls', rsfMap)

removeCommentsAndEmptyLines :: [ASMLine] -> [ASMLine]
removeCommentsAndEmptyLines = filter (not . runParseIsEmptyLineOrComment)
            where runParseIsEmptyLineOrComment l =
                    case parseOnly parseComment (getASMLineCode l) of
                        (Right _) -> True
                        (Left _)  -> False

parseASMLines :: [ASMLine] -> Either ParseError Program
parseASMLines ls = runParseASMInstructionLines $ removeCommentsAndEmptyLines ls
-- take the ASMLines
-- remove the comments
-- get labels -> we now have a tuple (ASMLines (the ones left), Data Map with labels)
-- get instructions from remaining data lines
-- return the program to the caller

-- (For now we can ignore the labels, we only want to successfully parse and remove them,
-- they will be used later when symbols functionality is added.)
-- might be clearer as do notation