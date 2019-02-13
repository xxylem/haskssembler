{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import InstructionDataModel

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Prelude hiding (lines)
import ReadArgs (readArgs)
import System.FilePath (dropExtension)
import System.IO

-- =========================================== --
-- HACK ASSEMBLER
-- Given an input file.asm in assembly language, 
-- will parse file and convert to machine language.
-- Output file is written to same directory with
-- same filename and extension .hack

-- Example usage:
--    haskssembler.exe file.asm

-- Note: Ver 0. No support for Labels or Symbols
-- A Instructions are only valid with integers,
-- e.g. @3, @500
-- =========================================== --

-- ====== --
-- PARSER --
-- ====== --

-- A Instructions
parseAddress :: Parser Address
parseAddress =
    char '@' >>
    Address <$> decimal 

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
        <* char '=')
    <|> return NULL_DEST

parseJump :: Parser Jump
parseJump =
    (char ';' >>
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

-- Both Instructions
parseInstruction :: Parser Instruction
parseInstruction = 
   skipSpace >>
        parseAddressInstruction
    <|> parseComputationInstruction

parseLine :: BS.ByteString -> Maybe Instruction
parseLine l = eitherRight $ 
    parseOnly parseInstruction l
        where eitherRight (Right r) = Just r
              eitherRight _ = Nothing 

parseASMLines :: [BS.ByteString] -> Program
parseASMLines [] = []
parseASMLines (l:ls) =
    case parseLine l of
        Just instr -> instr : parseASMLines ls
        Nothing    -> parseASMLines ls
    
-- ============= --
-- OUTPUT WRITER --
-- ============= --

writeProgramToFile :: FilePath -> Program -> IO ()
writeProgramToFile fp program = 
    withFile fp WriteMode (\h -> writeProgram h program)
                where writeProgram _ []     = return ()
                      writeProgram h (l:ls) = BS.hPutStr h (showB l) >> BS.hPutStr h "\n" >> writeProgram h ls

-- ============ --
-- MAIN PROGRAM --
-- ============ --
main :: IO ()
main = do
    (path :: FilePath) <- readArgs
    file <- BS.readFile path
    let program = parseASMLines $ BS.lines file
    writeProgramToFile (changeExt path) $! program
        where changeExt fp = dropExtension fp ++ ".hack" 

-- TODO version 2: supports labels and symbols

-- TODO check for failure and report line
--      currently ignores the line and continues
--      will require parsing specifically for comments
--      something that is not : a comment
--   !!                         empty line
--                              valid instruction
--          should cause assembler to fail

-- TODO arg handling now added and will provide usage hints, but is very primitive,
--    only saying <program> String. rather than something like usage: <program> filepath