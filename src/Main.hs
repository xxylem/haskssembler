{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import InstructionDataModel
import Parser (parseASMLines)

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