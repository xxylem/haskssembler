{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- import InstructionDataModel
-- import RunParser (parseASMLines, getErrLineNumber, getErrLineCode)

import Data.Hack.ASM.Model as ASM
import Data.Hack.ASM.ConversionTo.ByteString as ASMB
import Parser.ASM as P

import qualified Data.ByteString.Char8 as BS
import Prelude hiding (lines)
import ReadArgs (readArgs)
import System.FilePath (dropExtension)

-- ============ --
-- MAIN PROGRAM --
-- ============ --
main :: IO ()
main = do
    (path :: FilePath) <- readArgs
    file <- BS.readFile path
    case P.parseASMFile file of
        Right program -> writeAsHackFile (changeExt path) ( program)
                where changeExt fp = dropExtension fp ++ ".hack" 
        Left _ -> putStrLn "failed"

writeAsHackFile :: FilePath -> ASM.Program -> IO ()
writeAsHackFile fp file =
    BS.writeFile fp (ASMB.convert file)