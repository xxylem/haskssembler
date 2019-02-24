{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import InstructionDataModel
import RunParser (parseASMLines, getErrLineNumber, getErrLineCode)

import qualified Data.ByteString.Char8 as BS
import Prelude hiding (lines)
import ReadArgs (readArgs)
import System.FilePath (dropExtension)
import System.IO

-- ============= --
-- OUTPUT WRITER --
-- ============= --

writeProgramToFile :: FilePath -> Program -> IO ()
writeProgramToFile fp program = 
    withFile fp WriteMode (\h -> writeProgram h program)
                where writeProgram _ []     = return ()
                      writeProgram h (l:ls) = BS.hPutStr h (showB $ getHSLineCode $ getHSLine l) >> BS.hPutStr h "\n" >> writeProgram h ls

-- ============ --
-- MAIN PROGRAM --
-- ============ --
main :: IO ()
main = do
    (path :: FilePath) <- readArgs
    file <- BS.readFile path
    case parseASMLines $ makeASMList $ BS.lines file of
        Right program -> writeProgramToFile (changeExt path) $! program
                where changeExt fp = dropExtension fp ++ ".hack" 
        Left err -> putStrLn ("Parse error on Line " 
                                <> show (getErrLineNumber err) 
                                <> ": "
                                <> show (getErrLineCode err)
                                <> ".\nDebug data: "
                                <> show err
                                )
