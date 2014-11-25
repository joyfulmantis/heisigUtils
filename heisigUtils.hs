{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Csv                   as C
import           Data.Foldable
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.IO               as TIO
import           GHC.Generics
import           Options.Applicative
import           System.Exit
import           System.IO

data Options = Options { _fname   :: FilePath
                       , _command :: Command }

data Command = Dependencies T.Text
             | Search T.Text
             | Verify
             | Interactive

data Heisig = Heisig
  { rtID           :: T.Text
  , rtCharacter    :: T.Text
  , rtPOS          :: T.Text
  , rtKeyword      :: T.Text
  , rtReading      :: T.Text
  , rtLesson       :: T.Text
  , rtDependencies :: T.Text
  , rtAlias        :: T.Text }
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
                         z -> TIO.putStr $ uncurry (run z) r
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
  Dependencies <$> T.pack <$> strArgument (metavar "<Characters and/or Keywords>")

parserSearch :: Parser Command
parserSearch =
  Search <$> T.pack <$> strArgument (metavar "<Characters and/or Keywords>")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCSV :: BLC.ByteString -> Either String (C.Header, [Heisig])
parseCSV contents =  do (x, y) <- C.decodeByName contents
                        return (x, toList y)

abstractInteractive :: T.Text -> (T.Text -> IO ()) -> IO ()
abstractInteractive prompt inputHandler = do TIO.putStr prompt
                                             hFlush stdout
                                             input <- TIO.getLine
                                             inputHandler input

interactiveRun :: C.Header -> [Heisig] -> IO ()
interactiveRun header database = abstractInteractive "> " handler
  where handler "dependencies" = interactiveDependencies header database
        handler "search" = interactiveSearch header database
        handler "exit" = exitWith ExitSuccess
        handler _ = TIO.putStrLn helpMsg >> interactiveRun header database
        helpMsg = "please use one of 'dependencies search exit'"

interactiveDependencies :: C.Header -> [Heisig] -> IO ()
interactiveDependencies header database =
  abstractInteractive "dependencies> " handler
  where handler "exit" = interactiveRun header database
        handler input = do TIO.putStr $ run (Dependencies input) header database
                           interactiveDependencies header database

interactiveSearch :: C.Header -> [Heisig] -> IO ()
interactiveSearch header database = abstractInteractive "search> " handler
  where handler "exit" = interactiveRun header database
        handler input = do TIO.putStr $ run (Search input) header database
                           interactiveSearch header database

run :: Command -> C.Header -> [Heisig] -> T.Text
run (Dependencies query) header =
  formater header . dependencies (T.strip <$> T.splitOn ", " query)
run (Search query) header =
  formater header . search (T.strip <$> T.splitOn ", " query)
run Verify _ = verify . reverse

formater :: C.Header -> Either T.Text [Heisig] -> T.Text
formater _ (Left e) = e `T.append` "\n"
formater header (Right r) =
 TE.decodeUtf8 $ BL.toStrict $ C.encodeByName header r

dependencies :: [T.Text] -> [Heisig] -> Either T.Text [Heisig]
dependencies (d:ds) database =
  do first <- findHeisig d database
     theRest <- dependencies (ds ++ parseDependencies first) database
     pure $ first : theRest
dependencies [] _ = pure []

search :: [T.Text] -> [Heisig] -> Either T.Text [Heisig]
search (x:xs) database =
  do first <- findHeisig x database
     theRest <- search xs database
     pure $ first : theRest
search [] _ = pure []

verify :: [Heisig] -> T.Text
verify (x:xs) =
  case dependencies (parseDependencies x) xs of
    Left e -> e
    Right _ -> rtID x `T.append` "...\n" `T.append` verify xs
verify [] = "Looks spiffy...\n"

findHeisig :: T.Text -> [Heisig] -> Either T.Text Heisig
findHeisig x database = case find matcher database of
                          Nothing -> Left $ "Could not find " `T.append` x
                          Just y -> pure y
  where matcher z = rtCharacter z == x
                    || T.toCaseFold (rtKeyword z) == T.toCaseFold x

parseDependencies :: Heisig -> [T.Text]
parseDependencies Heisig {rtDependencies = ""} = []
parseDependencies x @ Heisig {rtDependencies = y} =
  filter (\z -> z /= rtKeyword x) $ T.strip <$> T.splitOn ", " y
