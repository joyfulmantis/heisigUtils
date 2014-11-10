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
main = do options <- execParser (parserOptions `withInfo` parserInfo)
          contents <- BL.readFile (_fname options)
          case parseCSV contents of
            Left e -> putStrLn e
            Right r -> putStr $ uncurry (run ( _command options )) r
  where parserInfo = "A simple program which provides search and dependency listing for characters in Heisig's books"

parserOptions :: Parser Options
parserOptions = Options <$> parserFile <*> parserCommand

parserFile :: Parser File
parserFile = strOption $
    short 'f' <> long "file" <> metavar "FILE" <>
    help "The CSV file to use as a database"

parserCommand :: Parser Command
parserCommand = subparser $
    command "dependencies" (parserDependencies `withInfo` depInfo)
  <> command "search"      (parserSearch `withInfo` serInfo)
  <> command "verify"      (parserVerify `withInfo` verInfo)
  where depInfo = "Returns a csv of all the Heisig dependencies for the provided characters and keywords"
        serInfo = "Returns a csv of the all the entries for the provided characters and keywords"
        verInfo = "Verifys that there are no missing dependencies in the csv file"

parserDependencies :: Parser Command
parserDependencies =
  Dependencies <$> argument str (metavar "<Characters and/or Keywords>")

parserSearch :: Parser Command
parserSearch =
  Search <$> argument str (metavar "<Characters and/or Keywords>")

parserVerify :: Parser Command
parserVerify = pure Verify

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCSV :: BLC.ByteString -> Either ErrorMsg (C.Header, [Heisig])
parseCSV contents = do
  (x, y) <- C.decodeByName contents
  return (x, toList y)

run :: Command -> C.Header -> [Heisig] -> String
run (Dependencies charOrKeyws) header =
  formater header . dependencies (splitOn ", " charOrKeyws)
run (Search charOrKeyws) header =
  formater header . search (splitOn ", " charOrKeyws)
run Verify header = verify . reverse

formater :: C.Header -> Either ErrorMsg [Heisig] -> String
formater header (Left e) = e
formater header (Right r) =
  BU.toString $ BL.toStrict $ C.encodeByName header r

dependencies :: [String] -> [Heisig] -> Either String [Heisig]
dependencies (dependant:xs) database =
  case findHeisig dependant database of
   Nothing -> Left $ "Can not find " ++ dependant
   Just x -> (x :) <$> dependencies (xs ++ parseDependencies x) database
dependencies [] _ = Right []

search :: [String] -> [Heisig] -> Either ErrorMsg [Heisig]
search (x:xs) database =
  case findHeisig x database of
    Nothing -> Left $ "could not find " ++ x
    Just z -> (z :) <$> search xs database
search [] _ = Right []

verify :: [Heisig] -> String
verify (x:xs) =
  case dependencies (parseDependencies x) xs of
    Left e -> e
    Right _ -> rtID x ++ "...\n" ++ verify xs
verify [] = "Looks spiffy...\n"

findHeisig :: String -> [Heisig] -> Maybe Heisig
findHeisig x = find matcher
  where matcher z = rtCharacter z == x
                    || map toLower (rtKeyword z) == map toLower x

parseDependencies :: Heisig -> [String]
parseDependencies x =
  case rtDependencies x of
    [] -> []
    y -> filter (\z -> z /= rtKeyword x) $ splitOn ", " y
