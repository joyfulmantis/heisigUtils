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

data Options = Options { _fname   :: FilePath
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
                         z -> putStr $ uncurry (run z) r
  where parserInfo = "A simple program which provides search and dependency listing for characters in Heisig's books"

parserOptions :: Parser Options
parserOptions = Options <$> parserFile <*> parserCommand

parserFile :: Parser FilePath
parserFile = strOption $ short 'f' <> long "file" <> metavar "FILE"
             <> help "The CSV file to use as a database"

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
parseCSV contents =  do (x, y) <- C.decodeByName contents
                        return (x, toList y)

abstractInteractive :: String -> (String -> IO ()) -> IO ()
abstractInteractive prompt inputHandler = do putStr prompt
                                             hFlush stdout
                                             input <- getLine
                                             inputHandler input

interactiveRun :: C.Header -> [Heisig] -> IO ()
interactiveRun header database = abstractInteractive "> " handler
  where handler "dependencies" = interactiveDependencies header database
        handler "search" = interactiveSearch header database
        handler "exit" = exitWith ExitSuccess
        handler _ = putStrLn helpMsg >> interactiveRun header database
        helpMsg = "please use one of 'dependencies search exit'"

interactiveDependencies :: C.Header -> [Heisig] -> IO ()
interactiveDependencies header database =
  abstractInteractive "dependencies> " handler
  where handler "exit" = interactiveRun header database
        handler input = do putStr $ run (Dependencies input) header database
                           interactiveDependencies header database

interactiveSearch :: C.Header -> [Heisig] -> IO ()
interactiveSearch header database = abstractInteractive "search> " handler
  where handler "exit" = interactiveRun header database
        handler input = do putStr $ run (Search input) header database
                           interactiveSearch header database

run :: Command -> C.Header -> [Heisig] -> String
run (Dependencies query) header =
  formater header . dependencies (splitOn ", " query)
run (Search query) header = formater header . search (splitOn ", " query)
run Verify _ = verify . reverse

formater :: C.Header -> Either String [Heisig] -> String
formater _ (Left e) = e ++ "\n"
formater header (Right r) =
  BU.toString $ BL.toStrict $ C.encodeByName header r

dependencies :: [String] -> [Heisig] -> Either String [Heisig]
dependencies (d:ds) database =
  do first <- findHeisig d database
     theRest <- dependencies (ds ++ parseDependencies first) database
     pure $ first : theRest
dependencies [] _ = pure []

search :: [String] -> [Heisig] -> Either String [Heisig]
search (x:xs) database =
  do first <- findHeisig x database
     theRest <- search xs database
     pure $ first : theRest
search [] _ = pure []

verify :: [Heisig] -> String
verify (x:xs) =
  case dependencies (parseDependencies x) xs of
    Left e -> e
    Right _ -> rtID x ++ "...\n" ++ verify xs
verify [] = "Looks spiffy...\n"

findHeisig :: String -> [Heisig] -> Either String Heisig
findHeisig x database = case find matcher database of
                          Nothing -> Left $ "Could not find " `T.append` x
                          Just y -> pure y
  where matcher z = rtCharacter z == x
                    || map toLower (rtKeyword z) == map toLower x

parseDependencies :: Heisig -> [String]
parseDependencies Heisig {rtDependencies = []} = []
parseDependencies x @ Heisig {rtDependencies = y} =
  filter (\z -> z /= rtKeyword x) $ splitOn ", " y
