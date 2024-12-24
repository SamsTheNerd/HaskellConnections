module Connection where
import Styling
import System.Console.Haskeline (CompletionFunc, Completion (Completion), completeWord, InputT, getHistory, defaultSettings, setComplete, runInputT, putHistory)
import Data.List (isPrefixOf)
import Data.Char (toUpper, toLower)
import Control.Monad.State (StateT, gets, state, MonadState (get), MonadTrans (lift), MonadIO (liftIO))
import Control.Monad.Syntax.Two ((==<<))
import Data.Aeson
import qualified Data.Aeson.Key as Key

-- Connection has a label and a list of words in that category. It is expected to have 4 words.
data Connection = Connection String Char [String] deriving (Show,Read)

-- gets the label of this connection
lbl :: Connection -> String
lbl (Connection l _ _ ) = l

-- gets the tier of this connection
tier :: Connection -> Char
tier (Connection _ t _ ) = t

-- gets the ansi formatting code for the given connection
getFormatting :: Connection -> String
getFormatting ctn = case tier ctn of
    'P' -> fColor fPurple
    'G' -> fColor fGreen
    'Y' -> fColor fYellow
    'B' -> fColor fBlue
    _ -> ""

tiers :: String
tiers = "YGBP"

-- converts a connection into its corresponding square emoji
getEmoji :: Connection -> String
getEmoji ctn = case tier ctn of
    'P' -> "🟪"
    'G' -> "🟩"
    'Y' -> "🟨"
    'B' -> "🟦"
    _   -> "⬜"

-- converts a connection into its corresponding heart emoji <3
getHeartEmoji :: Connection -> String
getHeartEmoji ctn = case tier ctn of
    'P' -> "💜"
    'G' -> "💚"
    'Y' -> "💛"
    'B' -> "💙"
    _   -> "🤍"

-- gets the words in this connection
cnwrds :: Connection -> [String]
cnwrds (Connection _ _ w) = w

-- Game has a list of connections and a marker for if they've been guessed yet
-- and a guesses counter. It is expected to have 4 connections. It also keeps a list of past guesses
data Game = Game [(Bool, Connection)] Integer [[String]] deriving Show

-- gets the connections for this game with a boolean for if it's been successfully guessed yet
getCtns :: Game -> [(Bool, Connection)]
getCtns (Game ctns _ _) = ctns

-- gets the number of mistakes made in the game.
mistakeCount :: Game -> Integer
mistakeCount (Game _ g _) = g

-- gets the guess history
guessHistory :: Game -> [[String]]
guessHistory (Game _ _ h) = h

-- gets all words in the game
allWords :: Game -> [String]
allWords game = concatMap (cnwrds . snd) (getCtns game)

-- gets all remaining (unguessed) words in the game
rWords :: Game -> [String]
rWords game = concatMap (\ctns -> case ctns of
    (False, ctn) -> cnwrds ctn
    _ -> []) (getCtns game)

-- returns a count of how many categories have been guessed
guessedCats :: [(Bool, Connection)] -> Int
guessedCats = foldr (\t s -> if fst t then s + 1 else s) 0

-- gets the length of the longest word in the game
longestWord :: Game -> Int
longestWord game = foldr (max . length) 0 (allWords game)

-- gets the category that the word appears in
catOfWord :: Game -> String -> Connection
catOfWord game guess = (snd . head) (filter (\ctn -> guess `elem` (cnwrds . snd) ctn) (getCtns game))

type GameIO = StateT (Game, [String]) IO

gameIOT :: InputT IO a -> InputT GameIO a
gameIOT inm = do
    hist <- getHistory
    st <- lift get
    let inIO = runInputT (setComplete (makeGameCompletion' st) defaultSettings) (putHistory hist >> inm)
    liftIO inIO

makeGameCompletion :: CompletionFunc GameIO
makeGameCompletion = completeWord Nothing [' '] (gameCmplF ==<< get)

makeGameCompletion' :: Monad m => (Game, [String]) -> CompletionFunc m
makeGameCompletion' st = completeWord Nothing [' '] (gameCmplF st)

gameCmplF :: Monad m => (Game, [String]) -> String -> m [Completion]
gameCmplF (game, _) str' = do
        let str = toUpper <$> str' 
        let validWords = filter (isPrefixOf str) (rWords game) -- filter remaining words to possible matches
        let cmpls = case validWords of -- weird behavior with case sensitivity and multiple matches. it autocompletes the caps before suggesting all options
                [w] -> [Completion w w False] -- single valid, just use
                _ -> (\w -> Completion (matchCaseCmpl str' w) w False) <$> validWords -- multiple options, make them all start with input
        return cmpls

matchCaseCmpl :: [a] -> [a] -> [a]
matchCaseCmpl [] ws = ws
matchCaseCmpl (g:gs) (_:ws) = g : matchCaseCmpl gs ws
matchCaseCmpl _ [] = []


data DatedGame = DatedGame Int String [Connection] deriving (Show, Read)

-- JSON STUFF

instance FromJSON Connection where
    parseJSON = withObject "Connection" $ \obj -> do
        ws <- obj .: Key.fromString "members"
        label <- obj .: Key.fromString "group"
        lvl <- obj .: Key.fromString "level"
        return $ Connection label (tiers !! lvl) ws

instance FromJSON DatedGame where
    parseJSON = withObject "Game" $ \obj -> do
        cns <- obj .: Key.fromString "answers"
        date <- obj .: Key.fromString "date"
        ident <- obj .: Key.fromString "id"
        return $ DatedGame ident date cns
