{-# LANGUAGE LambdaCase #-}
module Game where
import Connection
import Styling
import Utils
import Data.Char (toUpper)
import System.Console.Haskeline hiding (display)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Maybe (fromMaybe)
import Control.Monad.State (StateT, MonadState (get, put), MonadTrans (lift), gets, modify, execStateT, void)
import Control.Arrow (Arrow(second))

-- turns a word into a fancy little card. boolean for if it's selected or not
makeWordCard :: Game -> Bool -> String -> [String]
makeWordCard game sel w =
    [
        [(sL . sU) b] ++ replicate lgst ((sC . sU) b) ++ [(sR . sU) b],
        [(sL . sM) b] ++ replicate lgst ((sC . sM) b) ++ [(sR . sM) b],
        [(sL . sM) b] ++ replicate buf ((sC . sM) b) ++ w ++ replicate (lgst - wlen - buf) ' ' ++ [(sR . sM) b],
        [(sL . sM) b] ++ replicate lgst ((sC . sM) b) ++ [(sR . sM) b],
        [(sL . sB) b] ++ replicate lgst ((sC . sB) b) ++ [(sR . sB) b]
    ]
    where lgst = longestWord game; wlen = length w; buf = div (lgst - wlen) 2
          b = if sel then selBorder else normBorder

-- makes a pretty little card for a correctly guessed category
makeCategoryCard :: Game -> Connection -> String
makeCategoryCard game ctn =
    getFormatting ctn ++ foldMap (++ "\n") [
        [(sL . sU) b] ++ replicate totLen ((sC . sU) b) ++ [(sR . sU) b],
        [(sL . sM) b] ++ replicate lblBuf ((sC . sM) b) ++ replace (== '_') (const ' ') (lbl ctn)
            ++ replicate (totLen - lblBuf - (length . lbl) ctn) ((sC . sM) b) ++ [(sR . sM) b],
        [(sL . sM) b] ++ replicate totLen ((sC . sM) b) ++ [(sR . sM) b],

        [(sL . sM) b] ++ replicate wExtBuf ((sC . sM) b) ++ wStr
            ++ replicate (totLen - length wStr - wExtBuf) ((sC . sM) b)
            ++ [(sR . sM) b],

        [(sL . sB) b] ++ replicate totLen ((sC . sB) b) ++ [(sR . sB) b]
    ] ++ fReset
    where totLen = longestWord game * 4 + 6;
          lblBuf = div (totLen - (length . lbl) ctn) 2; -- side buffer for the label text
          wBuf = div (totLen - (sum . map length) (cnwrds ctn)) 5; -- in between buffer for the words
          wStr = foldMap (++ replicate wBuf ' ') (cnwrds ctn); -- words string spaced nicely internally
          wExtBuf = div (totLen - length wStr) 2 -- external buffer for the words
          b = selBorder

-- display the remaining unguessed words
displayRemWords :: Game -> [String] -> String
displayRemWords game guesses = foldMap ((++ "") . foldMap (++ "\n") . foldr (zipWith (++)) (repeat "")) cardChunks
    where ws = (shuffle . rWords) game
          cards = map (\x -> makeWordCard game (x `elem` guesses) x) ws
          cardChunks = chunk 4 cards

-- display the correct guesses
displayGuessed  :: Game -> String
displayGuessed game = foldMap ((++ "") . (\ctn ->
        if fst ctn then makeCategoryCard game $ snd ctn else ""
    )) (getCtns game)

-- display the mistakes counter
displayMistakesLeft :: Game -> String
displayMistakesLeft game = replicate buf ' ' ++
    foldMap ((++ " ") . \x ->
        if x > mistakeCount game
            then "◯"
            else "\ESC[1;31m◉\ESC[0m"
    ) [1..4]
    ++ replicate (totLen - 4 - buf) ' '
    where totLen = longestWord game * 4 + 6; buf = div (totLen - 4) 2

-- display an ending message and guess history emojis
displayEndGame :: Game -> String
displayEndGame game = [fColor fPurple ++ "Perfect!", fColor fCyan ++ "You win yay!",
    fColor fGreen ++ "Good job!", fColor fYellow ++ "That was close!!", fColor fRed ++ "Better luck next time :/"]
    !! fromIntegral (mistakeCount game) ++ fReset
    ++ "\n" ++ foldMap ((++ "\n") . foldMap (getHeartEmoji . catOfWord game)) ((reverse . guessHistory) game)

-- display the game
display :: Game -> String
display game = displayWithGuesses game []

-- display the game while mid-guess with the given guessing words
displayWithGuesses :: Game -> [String] -> String
displayWithGuesses game guesses = displayGuessed game ++ displayRemWords game guesses ++ displayMistakesLeft game


-- tries to parse a string into a puzzle
readPuzzle :: String -> Maybe [Connection]
readPuzzle str = mapM (\line -> case words line of
    (lblish:wrds) -> Just $ Connection (drop 2 lblish) (head lblish) wrds
    _ -> Nothing) (lines str)

-- makes a new game with the given connections.
newGame :: [Connection] -> Game
newGame ctns = Game (zip (repeat False) ctns) 0 []

-- takes a game state and a guess string and returns an error message if one is needed
isValidGuess :: Game -> String -> Bool
isValidGuess game guess = elem guess $ rWords game

-- reveal all categories for final display
revealAll :: Game -> Game
revealAll (Game ctns mistakes gHis) = Game (map (\x -> (True, snd x)) ctns) mistakes gHis

-- takes a *valid* guess and a game state and returns a new game state.
makeGuess :: GameIO ()
makeGuess = do
    (game, guesses) <- get
    let newCtns = map (\case
                    gctn@(True, _) -> gctn -- if it's already guessed just ignore
                    (False, ctn@(Connection _ _ ws)) -> (all (`elem` guesses) ws, ctn) -- check if the guess contains everything in that category
                ) (getCtns game)
    let game' = Game newCtns
                (if guessedCats newCtns == (guessedCats . getCtns) game -- increment guess counter if 
                    then mistakeCount game + 1 else mistakeCount game) (guesses:guessHistory game)
    put (game', [])

-- prompts the user for a connections puzzle
getPuzzle :: InputT IO [Connection]
getPuzzle = do
    outputStr "Enter Puzzle Filename > "
    filename <- (fromMaybe "") <$> (getInputLine "> ")
    fc <- liftIO $ readFile filename -- TODO: proper error handling would be nice here.
    case readPuzzle fc of
        (Just ctns) -> return ctns
        Nothing -> outputStrLn "Invalid Puzzle File" >> getPuzzle

-- prints the help screen
printHelp :: InputT IO ()
printHelp = do
    outputStrLn (replicate 40 '\n')
    outputStrLn "Connections is a game about finding associatons between words."
    outputStrLn "  Your goal is to guess all 4 categories in under 4 mistakes."
    outputStrLn "  You can enter guesses one word/phrase per line."
    outputStrLn "  Once you enter 4 words/lines, your guess will be checked against the game\n"
    outputStrLn "You can also use the following commands:"
    outputStrLn "  :help     --  shows this screen"
    outputStrLn "  :del      --  removes the most recently guessed word from your guess"
    outputStrLn "  :clear    --  clears your current guess"
    outputStrLn "\n[ Press Enter to continue ]"
    _ <- (getInputLine "> ")
    return ()

-- prints the title screen all pretty
printTitle :: InputT IO ()
printTitle = outputStr $ foldMap (++ "\n") [
        "                            \ESC[1;34m,---------------.",
        "                            |\ESC[1;35m■■■■■■■\ESC[1;34m|\ESC[0m■■■\ESC[1;34m|\ESC[1;35m■■■\ESC[1;34m|",
        "                            |\ESC[1;35m■■■■■■■■■■■\ESC[1;34m|\ESC[0m■■■\ESC[1;34m|",
        "                            |\ESC[1;35m■■■\ESC[1;34m|\ESC[0m■■■\ESC[1;34m|\ESC[1;35m■■■■■■■\ESC[1;34m|",
        "                            |\ESC[0m■■■\ESC[1;34m|\ESC[1;35m■■■■■■■■■■■\ESC[1;34m|",
        "                            `---------------'",
        "\ESC[1;33m ,-----.                                      ,--.  ,--.                       ",
        "\ESC[1;33m'  .--./ ,---. ,--,--, ,--,--,  ,---.  ,---.,-'  '-.`--' ,---. ,--,--,  ,---.  ",
        "\ESC[1;32m|  |    | .-. ||      \\|      \\| .-. :| .--''-.  .-',--.| .-. ||      \\(  .-'  ",
        "\ESC[1;34m'  '--'\\' '-' '|  ||  ||  ||  |\\   --.\\ `--.  |  |  |  |' '-' '|  ||  |.-'  `)",
        "\ESC[1;35m `-----' `---' `--''--'`--''--' `----' `---'  `--'  `--' `---' `--''--'`----' \ESC[0m\n"
    ]

-- eventually returns a valid guess input. 
-- otherwise dispatches to commands and in-between printing
doInputLoop :: InputT GameIO () -> InputT GameIO ()
doInputLoop extInfo = do
    -- get >>= \(game, cGuesses)
    (game, cGuesses) <- lift get
    -- if showFirst then 
    outputStrLn (unwords $ replicate 40 "\n")
    outputStrLn (displayWithGuesses game cGuesses)
    outputStrLn ""
    extInfo
    -- else return ()
    outputStrLn "Enter a guess/command or type :help for more info"
    userInput <- fromMaybe "" <$> getInputLine "> "

    case userInput of
        ":help" -> gameIOT printHelp >> doInputLoop (return ())
        ":del" -> lift (modify (second tail)) >> doInputLoop (return ())
        ":clear" -> startInputLoop
        (':':_) -> outputStrLn "Unrecognized Command" >> gameIOT printHelp >> doInputLoop (return ())
        guess -> if isValidGuess game sGuess then
            lift (modify (second (sGuess:))) >> if length cGuesses == 3 then return () else doInputLoop (return ())
            else doInputLoop (outputStrLn ("Guess \"" ++ guess ++ "\" is not in the game or has already been guessed"))
            where sGuess = replace (== ' ') (const '_') $ map toUpper guess

-- starts the input loop, returning an IO locked final guesses string.
startInputLoop :: InputT GameIO ()
startInputLoop = doInputLoop (return ())

-- runs the game loop until the game is over
doGameLoop :: InputT GameIO ()
doGameLoop = do
    origGame <- lift $ gets fst
    -- Show game state
    outputStrLn (unwords $ replicate 40 "\n")
    outputStrLn $ display origGame

    if (guessedCats . getCtns) origGame == 4 || mistakeCount origGame == 4 then
        outputStrLn (unwords $ replicate 40 "\n") >>
        (outputStrLn . display . revealAll) origGame >> -- show the 
        (outputStrLn . displayEndGame) origGame -- print like, congrats and emoji grid
    else do
        -- Get guess
        startInputLoop
        -- Act on guess
        lift makeGuess
        -- Loop
        doGameLoop

-- runs a game of connections (mostly handling io dispatch)
runConnectionsGame :: Game -> IO ()
runConnectionsGame game = void (execStateT gIO (game, []))
    where gIO = runInputT (setComplete makeGameCompletion defaultSettings) doGameLoop
    

-- start up a game of connections from the title screen
connections :: IO ()
connections = runInputT defaultSettings $ (do
                    outputStrLn (unwords $ replicate 40 "\n"); printTitle
                    newGame <$> getPuzzle) >>= (lift . runConnectionsGame)

-- a helper function for testing that starts up the game skipping the title/puzzle select screen.
testconnections :: IO ()
testconnections = runInputT defaultSettings $ (do
    fc <- liftIO $ readFile "puzzle447.txt"
    newGame <$> case readPuzzle fc of
        (Just ctns) -> return ctns
        Nothing -> outputStrLn "Invalid Puzzle File" >> getPuzzle) >>= (lift . runConnectionsGame)
