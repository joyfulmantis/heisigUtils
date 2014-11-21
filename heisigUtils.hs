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
import           System.Exit
import           System.IO

type Zfunc = [String] -> [Heisig] -> Either String [Heisig]

data Options       = Options { _fname  :: FilePath
                            , _command :: Command }

data Command = Dependencies String
             | Search String
             | Verify
             | Interactive

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
            Right r -> case _command options of
                         Interactive -> uncurry interactiveRun r
                         _ -> putStr $ uncurry (run ( _command options )) r
  where parserInfo = "A simple program which provides search and dependency listing for characters in Heisig's books"

parserOptions :: Parser Options
parserOptions = Options <$> parserFile <*> parserCommand

parserFile :: Parser FilePath
parserFile = strOption $
    short 'f' <> long "file" <> metavar "FILE" <>
    help "The CSV file to use as a database"

parserCommand :: Parser Command
parserCommand = subparser $
    command "dependencies" (parserDependencies `withInfo` depInfo)
  <> command "search"      (parserSearch `withInfo` serInfo)
  <> command "verify"      (pure Verify `withInfo` verInfo)
  <> command "interactive" (pure Interactive `withInfo` interInfo)
  where depInfo = "Returns a csv of all the Heisig dependencies for the provided characters and keywords"
        serInfo = "Returns a csv of the all the entries for the provided characters and keywords"
        verInfo = "Verifys that there are no missing dependencies in the csv file"
        interInfo = "Run's heisigUtils interactively"

parserDependencies :: Parser Command
parserDependencies =
  Dependencies <$> argument str (metavar "<Characters and/or Keywords>")

parserSearch :: Parser Command
parserSearch =
  Search <$> argument str (metavar "<Characters and/or Keywords>")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCSV :: BLC.ByteString -> Either String (C.Header, [Heisig])
parseCSV contents = do
  (x, y) <- C.decodeByName contents
  return (x, toList y)

run :: Command -> C.Header -> [Heisig] -> String
run (Dependencies charOrKeyws) headr =
  formater headr . dependencies (splitOn ", " charOrKeyws)
run (Search charOrKeyws) headr =
  formater headr . search (splitOn ", " charOrKeyws)
run Verify _ = verify . reverse

interactiveRun :: C.Header -> [Heisig] -> IO ()
interactiveRun headr dbase =
  do putStr "> "
     hFlush stdout
     input <- getLine
     case input of
       "dependencies" -> interactiveDependencies headr dbase
       "search" -> interactiveSearch headr dbase
       "exit" -> exitWith ExitSuccess
       _ -> putStrLn helpMsg >> interactiveRun headr dbase
  where helpMsg = "please use one of 'dependencies search exit'"

interactiveZ :: String -> Zfunc -> C.Header -> [Heisig] -> IO ()
interactiveZ name func headr dbase =
  do putStr $ name  ++ "> "
     hFlush stdout
     input <- getLine
     case input of
       "exit" -> interactiveRun headr dbase
       charOrKeyws ->
         putStr (formater headr (func (splitOn ", " charOrKeyws) dbase))
         >> interactiveZ name func headr dbase

interactiveDependencies :: C.Header -> [Heisig] -> IO ()
interactiveDependencies = interactiveZ "dependencies" dependencies

interactiveSearch :: C.Header -> [Heisig] -> IO ()
interactiveSearch = interactiveZ "search" search

formater :: C.Header -> Either String [Heisig] -> String
formater _ (Left e) = e ++ "\n"
formater headr (Right r) =
  BU.toString $ BL.toStrict $ C.encodeByName headr r

dependencies :: [String] -> [Heisig] -> Either String [Heisig]
dependencies (dependant:xs) database =
  case findHeisig dependant database of
   Nothing -> Left $ "Can not find " ++ dependant
   Just x -> (x :) <$> dependencies (xs ++ parseDependencies x) database
dependencies [] _ = Right []

search :: [String] -> [Heisig] -> Either String [Heisig]
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
