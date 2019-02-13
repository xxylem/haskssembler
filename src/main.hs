{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import Prelude hiding (lines)
import ReadArgs (readArgs)
import System.FilePath (dropExtension)
import System.IO
import Text.Printf (printf)

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

-- ================ --
-- DATA DEFINITIONS --
-- ================ --

class ShowB a where
    showB :: a -> BS.ByteString

-- C Instructions
data Computation =
    ZERO
  | ONE
  | MINUS_ONE
  | D_COMP
  | A_COMP
  | NOT_D
  | NOT_A
  | NEG_D
  | NEG_A
  | D_PLUS_1
  | A_PLUS_1
  | D_MINUS_1
  | A_MINUS_1
  | D_PLUS_A
  | D_MINUS_A
  | A_MINUS_D
  | D_AND_A
  | D_OR_A
  | M_COMP
  | NOT_M
  | NEG_M
  | M_PLUS_1
  | M_MINUS_1
  | D_PLUS_M
  | D_MINUS_M
  | M_MINUS_D
  | D_AND_M
  | D_OR_M

instance ShowB Computation where
    showB cmd =
        case cmd of
            ZERO        -> "0101010"
            ONE         -> "0111111"
            MINUS_ONE   -> "0111010"
            D_COMP      -> "0001100"
            A_COMP      -> "0110000"
            NOT_D       -> "0001101"
            NOT_A       -> "0110001"
            NEG_D       -> "0001111"
            NEG_A       -> "0110001"
            D_PLUS_1    -> "0011111"
            A_PLUS_1    -> "0110111"
            D_MINUS_1   -> "0001110"
            A_MINUS_1   -> "0110010"
            D_PLUS_A    -> "0000010"
            D_MINUS_A   -> "0010011"
            A_MINUS_D   -> "0000111"
            D_AND_A     -> "0000000"
            D_OR_A      -> "0010101"
            M_COMP      -> "1110000"
            NOT_M       -> "1110001"
            NEG_M       -> "1110011"
            M_PLUS_1    -> "1110111"
            M_MINUS_1   -> "1110010"
            D_PLUS_M    -> "1000010"
            D_MINUS_M   -> "1010011"
            M_MINUS_D   -> "1000111"
            D_AND_M     -> "1000000"
            D_OR_M      -> "1010101"

data Destination =
    NULL_DEST
  | M_DEST
  | D_DEST
  | MD
  | A_DEST
  | AM
  | AD
  | AMD

instance ShowB Destination where
    showB dest =
        case dest of
            NULL_DEST   -> "000"
            M_DEST      -> "001"
            D_DEST      -> "010"
            MD          -> "011"
            A_DEST      -> "100"
            AM          -> "101"
            AD          -> "110"
            AMD         -> "111"

data Jump =
    NULL_JUMP
  | JGT
  | JEQ
  | JGE
  | JLT
  | JNE
  | JLE
  | JMP

instance ShowB Jump where
    showB jump =
        case jump of
            NULL_JUMP   -> "000"
            JGT         -> "001"
            JEQ         -> "010"
            JGE         -> "011"
            JLT         -> "100"
            JNE         -> "101"
            JLE         -> "110"
            JMP         -> "111"

-- A Instructions
newtype Address = Address Integer

instance ShowB Address where
    showB (Address location) = BS.pack (printf "%015b" location :: String)
    --TODO a bit hacky: printf casts location to string and then gets converted to lazy BS then to strict BS
    --is there a way to convert an integer e.g. 7, to a formatted ByteString with the appropriate number of
        -- leading zeros?

data Instruction =
    AddressInstruction Address
  | ComputeInstruction Computation Destination Jump

instance ShowB Instruction where
    showB (AddressInstruction addr) =
        "0" `BS.append` showB addr

    showB (ComputeInstruction comp dest jmp) =
        "111" `BS.append` showB comp `BS.append` showB dest `BS.append` showB jmp

type Program = [Instruction]

-- ============ --
-- EXAMPLE DATA --
-- ============ --

-- A Instructions in ASM file format
aInsASM1 :: BS.ByteString
aInsASM1 = "@2"
aInsASM2 :: BS.ByteString
aInsASM2 = "@3"
aInsASM3 :: BS.ByteString
aInsASM3 = "@0"
-- A Instructions in Haskell Data Format
aInsHask1 :: Instruction
aInsHask1 = AddressInstruction $ Address 2
aInsHask2 :: Instruction
aInsHask2 = AddressInstruction $ Address 3
aInsHask3 :: Instruction
aInsHask3 = AddressInstruction $ Address 0
-- A Instructions in Binary Code
aInsBC1 :: BS.ByteString
aInsBC1 = "0000000000000010"
aInsBC2 :: BS.ByteString
aInsBC2 = "0000000000000011"
aInsBC3 :: BS.ByteString
aInsBC3 = "0000000000000000"

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