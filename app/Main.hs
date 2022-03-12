module Main where

import qualified Codec.Dotcode as Dotcode
import Data.Char (intToDigit)
import Data.List (intercalate, unfoldr)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TIO
import GHC.IO.StdHandles (stderr)
import GHC.Num (Natural)
import Numeric (showIntAtBase)
import Options.Applicative
import System.Exit (exitFailure)
import System.IO (hPutStrLn)

data Opts = Encode Natural | Decode String

encodeParser :: Parser Natural
encodeParser = argument auto (metavar "NUM" <> help "Input integer")

decodeParser :: Parser String
decodeParser = strArgument (metavar "FILE" <> help "Input file")

commandLineParser :: Parser Opts
commandLineParser = hsubparser (encodeCommand <> decodeCommand)
  where
    encodeCommand :: Mod CommandFields Opts
    encodeCommand =
      command
        "encode"
        ( info
            (Encode <$> encodeParser)
            (progDesc "Convert from integer to dotcode")
        )
    decodeCommand :: Mod CommandFields Opts
    decodeCommand =
      command
        "decode"
        ( info
            (Decode <$> decodeParser)
            (progDesc "Convert from dotcode to integer")
        )

main :: IO ()
main = run =<< execParser opts
  where
    opts = info (commandLineParser <**> helper) fullDesc

run :: Opts -> IO ()
run (Encode i) = encode i
run (Decode f) = decode f

errExit :: String -> IO ()
errExit msg = do
  hPutStrLn stderr msg
  exitFailure

-- Copied from https://stackoverflow.com/a/12659698
insertAtN :: Int -> a -> [a] -> [a]
insertAtN n y = intercalate [y] . groups n
  where
    groups ng xsg = takeWhile (not . null) . unfoldr (Just . splitAt ng) $ xsg

showIntAtBase2 :: (Integral a, Show a) => a -> String
showIntAtBase2 i = showIntAtBase 2 intToDigit i ""

encode :: Natural -> IO ()
encode i =
  if i >= 1
    then do
      let (encoded, width) = Dotcode.encode i
      putStrLn $ insertAtN width '\n' $ showIntAtBase2 encoded
    else errExit "Invalid argument: input number needs to be at least 1"

binaryToNatural :: Text -> Natural
binaryToNatural =
  Text.foldl'
    ( \acc c ->
        acc * 2
          + ( if c == '0'
                then 0
                else 1
            )
    )
    0

decode :: String -> IO ()
decode f = do
  inputText <- TIO.readFile f
  let filteredInput = Text.filter (\c -> c == '0' || c == '1') inputText
  let inputNumber = binaryToNatural filteredInput
  case Dotcode.decode inputNumber of
    Just n -> print n
    Nothing -> errExit "Invalid dotcode"
