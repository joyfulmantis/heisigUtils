{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad.Writer
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Csv                   as C
import           Data.Either
import qualified Data.Foldable              as F
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Text.IO               as TIO
import           GHC.Generics
import           Options.Applicative
import           Prelude
import           System.Exit
import           System.IO

data Options = Options { _fname   :: FilePath
                       , _command :: Command }

data Command = Dependencies T.Text
             | Search T.Text
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
main =
  do options <- execParser (parserOptions `withInfo` parserInfo)
     contents <- BL.readFile (_fname options)
     case parseCSV contents of
       Left e -> putStrLn e >> failExit
       Right r ->
         let (k, l) = runWriter $ verify (snd r)
         in ( TIO.putStr l >>
              if k
                then uncurry (run (_command options)) r
                else failExit)
  where parserInfo = "A simple program which provides search and dependency listing for characters in Heisig's books"
        failExit = exitWith $ ExitFailure 1

parserOptions :: Parser Options
parserOptions = Options <$> parserFile <*> parserCommand

parserFile :: Parser FilePath
parserFile = strOption $ short 'f' <> long "file" <> metavar "FILE"
             <> help "The CSV file to use as a database"

parserCommand :: Parser Command
parserCommand = subparser $
    command "dependencies" (parserQueryer Dependencies `withInfo` depInfo)
  <> command "search"      (parserQueryer Search `withInfo` serInfo)
  <> command "interactive" (pure Interactive `withInfo` interInfo)
  where depInfo = "Returns a csv of all the Heisig dependencies for the provided characters and keywords"
        serInfo = "Returns a csv of the all the entries for the provided characters and keywords"
        interInfo = "Run's heisigUtils interactively"

parserQueryer :: (T.Text -> Command) -> Parser Command
parserQueryer q = q <$> T.pack <$> strArgument (metavar "<Characters and/or Keywords>")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parseCSV :: BLC.ByteString -> Either String (C.Header, [Heisig])
parseCSV contents =  do (x, y) <- C.decodeByName contents
                        return (x, F.toList y)

verify :: [Heisig] -> Writer T.Text Bool
verify xs = do a <- checkDependencies (reverse xs) (True, T.empty)
               b <- checkDuplicates ys S.empty (True, T.empty)
               return $ a && b
  where ys = concatMap xToY xs
        xToY Heisig {rtLesson = ""} = []
        xToY z = [rtKeyword z] ++ parseCommas (rtAlias z)

checkDuplicates :: [T.Text] -> S.Set T.Text -> (Bool, T.Text) -> Writer T.Text Bool
checkDuplicates (x:xs) s a@(_, t) =
  checkDuplicates xs (S.insert x s) a'
  where a' = if S.member x s
                 then ( False
                      , "FATAL: Found duplicate "
                        `T.append` x
                        `T.append` "\n"
                        `T.append` t)
                 else a
checkDuplicates [] _ a = writer a

checkDependencies :: [Heisig] -> (Bool, T.Text) -> Writer T.Text Bool
checkDependencies (x:xs)  a@(_, t) =
  checkDependencies xs a'
  where a' = case dependencies (parseDependencies x) xs of
               Left e -> ( False
                         , "FATAL: "
                           `T.append` rtID x
                           `T.append` ": "
                           `T.append` e
                           `T.append` "\n"
                           `T.append` t)
               Right _ -> a
checkDependencies [] a = writer a

run :: Command -> C.Header -> [Heisig] -> IO ()
run (Dependencies query) header =
  TIO.putStr . formater header . dependencies (parseCommas query)
run (Search query) header =
  TIO.putStr . formater header . search (parseCommas query)
run Interactive header = interactiveRun header

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
        handler input =
          do run (Dependencies input) header database
             interactiveDependencies header database

interactiveSearch :: C.Header -> [Heisig] -> IO ()
interactiveSearch header database = abstractInteractive "search> " handler
  where handler "exit" = interactiveRun header database
        handler input = do run (Search input) header database
                           interactiveSearch header database

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

findHeisig :: T.Text -> [Heisig] -> Either T.Text Heisig
findHeisig x database =
  case F.find matcher database of
    Nothing -> Left $ "Could not find: " `T.append` x
    Just y -> pure y
  where matcher z =
          rtCharacter z == x
          || T.toCaseFold (rtKeyword z) == T.toCaseFold x
          || T.toCaseFold x `elem` (T.toCaseFold <$> parseCommas (rtAlias z))

parseDependencies :: Heisig -> [T.Text]
parseDependencies x =
  filter (\z -> isLeft (findHeisig z [x])) $ parseCommas (rtDependencies x)

parseAliass :: Heisig -> [T.Text]
parseAliass z = k <$> parseCommas (rtAlias z)
  where k = T.append "#" . T.dropAround ('#' == )

parseCommas :: T.Text -> [T.Text]
parseCommas "" = []
parseCommas y = T.strip <$> T.splitOn ", " y
