{-# LANGUAGE OverloadedStrings #-}

module Parser 
    ( parseASMLines )
    where

import InstructionDataModel

import Control.Applicative ((<|>))
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

-- ====== --
-- PARSER --
-- The parseASMLines function will take a Program ([ByteString] alias) and output the program parsed
-- into the intermediate data representation described in the IntructionDataModel module.
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

parseComment :: Parser ()
parseComment =
    skipSpace >>
    (   (string "//" >> return ())
    <|> endOfInput)

-- Both Instructions
parseInstruction :: Parser Instruction
parseInstruction = 
   skipSpace >>
    (   parseAddressInstruction
    <|> parseComputationInstruction)
    <*  parseComment
  
newtype ParseError = InvalidLine String
                    deriving (Eq, Show)

runParseInstructionLine :: BS.ByteString -> Either ParseError Instruction
runParseInstructionLine l = 
    case parseOnly parseInstruction l of
        (Right r)   -> Right r
        (Left err)  -> Left $ InvalidLine err

-- this will better be expressed as list func. that incorporates failure.
parseASMInstructionLines :: [BS.ByteString] -> Program
parseASMInstructionLines [] = []
parseASMInstructionLines (l:ls) =
    case runParseInstructionLine l of
        Right instr           -> instr : parseASMInstructionLines ls
        Left (InvalidLine _)  -> [] -- should send error up chain really with line number.

removeCommentsAndEmptyLines :: [BS.ByteString] -> [BS.ByteString]
removeCommentsAndEmptyLines = filter (not . runParseIsEmptyLineOrComment)
            where runParseIsEmptyLineOrComment l =
                    case parseOnly parseComment l of
                        (Right _) -> True
                        (Left _)  -> False

parseASMLines :: [BS.ByteString] -> Program
parseASMLines ls = parseASMInstructionLines $ removeCommentsAndEmptyLines ls