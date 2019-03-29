{-# LANGUAGE ScopedTypeVariables #-}

import Data.Hack.ASM.ConversionTo.MachineCode as ASM2MC
import Data.Hack.MachineCode.ConversionTo.ByteString as MC2BS
import Data.Source.Model as SRC
import Data.Output.Model as OUT
import Parser.ASM as P

import qualified Data.ByteString.Char8 as BS
import System.FilePath (isExtensionOf)
import ReadArgs (readArgs)

-- ============ --
-- MAIN PROGRAM --
-- ============ --
main :: IO ()
main = do
    (filePath :: FilePath) <- readArgs
    if ".asm" `isExtensionOf` filePath then do
        file <- BS.readFile filePath
        let unparsedFile = SRC.toUnparsedFile filePath 1 file  
        case P.parseASMFile unparsedFile of
            Right asmProgram -> (OUT.writeOutputFile . MC2BS.convert . ASM2MC.convert) asmProgram
            Left err -> putStrLn $ "Failed to parse " <> filePath <> ".\n"
                              <> "Error: " <> show err
    else putStrLn "Usage: haskssembler.exe \"file.asm\""