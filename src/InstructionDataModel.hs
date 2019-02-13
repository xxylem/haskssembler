{-# LANGUAGE OverloadedStrings #-}

module InstructionDataModel where

import qualified Data.ByteString.Char8 as BS
import Text.Printf (printf)

    
-- ================ --
-- DATA DEFINITIONS --
-- ================ --

-- TODO currently using self-made class to convert custom data representation to ByteString.
--      is there default functionality I can use to do this?

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
