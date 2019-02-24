{-# LANGUAGE OverloadedStrings #-}

module Parsers where

import InstructionDataModel

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator (lookAhead)

    
-- ====================================================================================== --
-- Parsers.hs Module --
-- This module provides the suite of parsers that are used by the RunParser module 
-- to parse .asm assembly code to .hack machine code.
-- ====================================================================================== --

-- ====================================================================================== --
-- A Instructions --

-- Basic address instructions with only a number, 
-- e.g. @17, @25093
parseAddress :: Parser Address
parseAddress =
        skipSpace
    >>  char '@' 
    >>  Address <$> decimal 

-- Returns the symbol string from an A instruction containing a symbol/label ref
-- e.g. @LOOP, @counter
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

-- Returns the label (as a string) from a label declaration
-- e.g. (LOOP), (END)
parseAddressLabel :: Parser String
parseAddressLabel =
        skipSpace
    >>  char '('
    >>  manyTill' anyChar (char ')') 
    <*  parseComment

-- Wraps the parsed address value in a wrapper (TODO possibly redundant)
parseAddressInstruction :: Parser Instruction
parseAddressInstruction = AddressInstruction <$> parseAddress

-- ====================================================================================== --
-- C Instructions --

-- Parses the <<computation>> section of the C instruction.
-- e.g. the "A+1" in the instruction "M=A+1;JMP"
-- ASSUMES: The optional destination section of the C instruction has already been parsed.
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

-- Parses the <<destination>> section of the C instruction
-- If the <<destination>> section is not found, returns NULL_JUMP, assuming it
-- was intentionally omited.
-- e.g. the "M=" in the instruction "M=A+1;JMP"
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

-- Parses the <<jump>> section of the C instruction
-- e.g. the ";JMP" in the instruction "M=A+1;JMP"
-- If the <<jump>> section is not found, returns NULL_JUMP, assuming it
-- was intentionally omited.
-- ASSUMES: all previous parts of the C instruction have been parsed and consumed.
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

-- Combines the above parsers into one that deals with the full C instruction
-- e.g. parses all parts of "M=A+1;JMP" into their relevant components and combines them
-- to a full Instruction.
parseComputationInstruction :: Parser Instruction
parseComputationInstruction = do
    skipSpace
    dest <- parseDestination
    skipSpace
    comp <- parseComputation
    skipSpace
    jump <- parseJump
    return $ ComputeInstruction comp dest jump

-- ====================================================================================== --

-- Checks for comments, whitespace, or end of line, e.g. "   //this is a comment"
parseComment :: Parser ()
parseComment =
        skipSpace 
    >>  (   (string "//" >> return ())
        <|> endOfInput
        )

-- Checks for A instructions (without labels) and C instructions for one line of code
-- Also checks that the rest of the line is valid (i.e. contains only empty space, a comment,
-- or is the end of the line).
parseInstruction :: Parser Instruction
parseInstruction = 
        skipSpace 
    >>  (   parseAddressInstruction
        <|> parseComputationInstruction
        )
    <*  parseComment