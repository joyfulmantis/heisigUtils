{-# LANGUAGE DeriveGeneric #-}

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.UTF8       as BU
import           Data.Char
import qualified Data.Csv                   as C
import           Data.Foldable
import           Data.List.Split            (splitOn)
import           GHC.Generics
import           Options.Applicative

type ErrorMsg = String

type File          = FilePath
type CharsOrKeyws  = String
data Options       = Options { _fname   :: File
                             , _command :: Command }

data Command = Dependencies CharsOrKeyws
             | Search CharsOrKeyws
             | Verify

data Heisig = Heisig
  { rtID           :: String
  , rtCharacter    :: String
  , rtPOS          :: String
  , rtKeyword      :: String
  , rtReading      :: String
  , rtLesson       :: String
  , rtDependencies :: String
  , rtAlias        :: String }
 deriving (Generic, Show)

instance C.FromNamedRecord Heisig
instance C.ToNamedRecord Heisig


main :: IO ()
main = do options <- execParser (parserOptions `withInfo` "A simple prgram which provides search and dependecy listing for characters in Heisig's books")
          contents <- BL.readFile (_fname options)
          case parseCSV contents of
            Left e -> putStrLn e
            Right r -> putStr $ uncurry (run ( _command options )) r

parserOptions :: Parser Options
parserOptions = Options <$> parserFile <*> parserCommand

parserFile :: Parser File
parserFile = strOption $
    short 'f' <> long "file" <> metavar "FILE" <>
    help "The CSV file to use as a databse"

parserCommand :: Parser Command
parserCommand = subparser $
    command "dependencies" (parserDependencies   `withInfo` "Lists all the Heisig dependencies for provided keyword and characters returning a csv of the results")
    <> command "search"    (parserSearch         `withInfo` "Returns a csv of the all the entries of the provided characters and keywords")
    <> command "verify"    (parserVerify         `withInfo` "Verifys that there are no missing dependencies in the csv file")

parserDependencies :: Parser Command
parserDependencies = Dependencies
    <$> argument str (metavar "<Chinese Characters and/or Heisig Keywords>")

parserSearch :: Parser Command
parserSearch = Search <$> argument str (metavar "<Chinese Characters and/or Heisig Keywords>")

parserVerify :: Parser Command
parserVerify = pure Verify

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCSV :: BLC.ByteString -> Either ErrorMsg (C.Header, [Heisig])
parseCSV contents = do
  (x, y) <- C.decodeByName contents
  return (x, toList y)

run :: Command -> C.Header -> [Heisig] -> String
run cmmnd header csv = case cmmnd of
    Dependencies charOrKeyws -> formater header $ dependencies csv (splitOn ", " charOrKeyws)
    Search charOrKeyws -> formater header $ search csv (splitOn ", " charOrKeyws)
    Verify -> verify (reverse csv)

formater :: C.Header -> Either ErrorMsg [Heisig] -> String
formater header toFormat = case toFormat of
                             Left e -> e
                             Right r -> BU.toString (BL.toStrict (C.encodeByName header r))

dependencies :: [Heisig] -> [String] -> Either String [Heisig]
dependencies database (dependant:xs) =
  case findHeisig database dependant of
   Nothing -> Left $ "Can not find " ++ dependant
   Just x -> (x :) <$> dependencies database (xs ++ parseDependencies x)
dependencies _ [] = Right []

search :: [Heisig] -> [String] -> Either ErrorMsg [Heisig]
search database (x:xs) =
  case findHeisig database x of
    Nothing -> Left ("could not find " ++ x )
    Just z -> (z :) <$> search database xs
search _ [] = Right []

verify :: [Heisig] -> String
verify (x:xs) =
  case dependencies xs (parseDependencies x) of
    Left e -> e
    Right _ -> rtID x ++ "...\n" ++ verify xs
verify [] = "Looks spiffy...\n"

findHeisig :: [Heisig] -> String -> Maybe Heisig
findHeisig database x = find matcher database
  where matcher z = rtCharacter z == x
                    || map toLower (rtKeyword z) == map toLower x

parseDependencies :: Heisig -> [String]
parseDependencies x =
  case rtDependencies x of
    [] -> []
    y -> filter (\z -> z /= rtKeyword x) $ splitOn ", " y

