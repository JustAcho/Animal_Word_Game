-- Всички основни функции и магия, чрез които работи Играта

{- ЗАБЕЛЕЖКА: Менютата, опциите, въпросите и т.н са на Английски, тъй като 
играта отказваше да тръгне като се опитвах да изпечатам нещо на кирилица.
В коментарите съответните функции са написани с английските си имена, с цел
по-добра ориентация. -}


module TheAnimalWordGame(main) where
import Game
import System.Exit
import System.IO
import Data.Map as M
import Prelude as P

-- Показва отговора като взимаме предвид LCA (Lowest Common Ancestor).
-- Ако е Lca, ще съдържа три възможности (стрингове).
-- Ако е First, е само първата опция .
-- Ако е Second, е само втората опция.
-- Ако е None, няма опция.
data Lca =  Lca String String String | First | Second | None

-- Началното меню, при което потребителят трябва да зареди базата данни, която е записана във файл
-- и да започне играта.
-- Когато приключи, ще има възможността да запише играта с новите опции в базата данни и да излезе.
-- Също така има проверки за невалидни команди или опит за започване на игра, без заредена база данни.
menu :: Maybe Game -> IO()
menu (Just gameActual) = do
        putStrLn "Enter an option."
        putStrLn . unlines $ P.map showChoices choices
        choice <- getLine
        gameNew <- case validate choice of
            Just 1 -> predict gameActual
            Just 2 -> saveToFile gameActual
            Just 3 -> loadGameFile
            Just 4 -> exitSuccess
            Nothing -> do
                putStrLn "Wrong option."
                return gameActual
        menu (Just gameNew)
    where showChoices (i, s) = show i ++ ". " ++ s
menu Nothing = do
        putStrLn "Enter an option."
        putStrLn . unlines $ P.map showChoices choices
        choice <- getLine
        case validate choice of
            Just 3 -> do
                o <- loadGameFile
                menu (Just o)
            Just 4 -> exitSuccess
            Nothing -> do
                putStrLn "Wrong option."
                menu Nothing
            _ -> do
                putStrLn "There is no Data Base loaded."
                menu Nothing
    where showChoices (i, s) = show i ++ ". " ++ s

validate :: String -> Maybe Int
validate s = isValid (reads s)
    where isValid [] = Nothing
          isValid ((n, _):_) 
                | (n < 1) || (n > length choices) = Nothing
                | otherwise = Just n



-- Опциите, които излизат в менюто
choices :: [(Int, String)]
choices = zip [1.. ] [
         "Play the Game (All answers ARE case sensitive)",
         "Save current Game to the Data Base",
         "Load Game`s Data Base from file",
         "Exit"
    ]


--Зареждане на базата данни чрез оказване на пътя до файла (ако е в същата папка, може и само името му).
loadGameFile :: IO Game
loadGameFile = do
        putStrLn "Enter the name (if it is in the same folder) or location of the file"
        myFile <- getLine
        s <- readFile myFile
        return $ game s
    where
        game s = read s :: Game


-- Записване на настоящата игра към базата данни
saveToFile :: Game -> IO Game
saveToFile game = do
    putStrLn "Enter the name of the file to which the Game will be saved."
    myFile <- getLine
    writeFile myFile (show game)
    return $ game


--Разграничаващи въпроси и опции за отговор (при два различни отговора, т.е. "Да" или "Не", не може да бъде дадено едно и също предположение)
lca :: String -> String -> Game -> Lca
lca s1 s2 (Prediction prediction)
    | prediction == s1  = First
    | prediction == s2  = Second
    | otherwise         = None
lca s1 s2 game = case s1 == s2 of
    True -> error("The predictions cannot be the same")
    otherwise ->
        case P.filter noNone listLcas of
            []          -> None
            [(x, op)]   -> x
            [(First,  op1), (Second, op2)]  -> Lca (question game) op1 op2
            [(Second, op2), (First,  op1)]  -> Lca (question game) op1 op2
        where
            lcaAux (a, b) = (lca s1 s2 b, a)
            listLcas = P.map lcaAux (toList (options game))
            noNone (None, _) = False
            noNone _ = True


-- Функцията, която започва играта
predict :: Game -> IO Game
predict (Prediction p)    = predictPrediction (Prediction p)
predict (Question p ops)  = predictQuestion (Question p ops)

-- Потребителят трябва да отговори на въпроса с "Yes" или "No". 
-- При коректен отговор, играта продължава напред.
predictQuestion :: Game -> IO Game
predictQuestion game = do 
        putStrLn $ question game
        putStrLn $ showOptions $ list
        user <- checkIfAnswerIsValid ("any": (listOptions (options game)))
        case user of
            "any" -> do
                predictionCorrect <- getPredictionCorrect
                option <- getReply list (question game) predictionCorrect
                return (insertNew option (createGame predictionCorrect) game)
            option -> do
                subgame <- predict (reply game option)
                return (insertNew option subgame game)
    where
        list = listOptions $ options game

-- Играта казва своето предположение на потребителя и той трябва да каже, дали това е вярно
-- Ако Играта не е познала, то тогава потребителят трябва да въведе верният отговор
predictPrediction :: Game -> IO Game
predictPrediction game = do
    putStrLn $ "Is it a " ++ (prediction game) ++ "?"
    putStrLn $ showOptions optionsYesNo
    x <- checkIfAnswerIsValid optionsYesNo
    case x of
        "Yes" -> return game
        "No" -> predictionFail game
    where optionsYesNo = ["Yes", "No"]

-- Потребителят бива попитан за верният отговор и чрез какъв въпрос може да бъде достигнат той
-- В Играта бива записана новата информация
predictionFail :: Game -> IO Game
predictionFail game = do
    predictionCorrect <- getPredictionCorrect
    question <- (getQuestion predictionCorrect)
    optionPredictionCorrect  <- (getReply [] question predictionCorrect)
    optionPredictionGame   <- (getReply [optionPredictionCorrect] question (prediction game))
    return ( newQuestion  [optionPredictionGame, optionPredictionCorrect]
                        [game, createGame predictionCorrect]
                        question )

--Добавяне на нови отговори
insertNew :: String -> Game -> Game -> Game
insertNew op prediction (Question p ops)  = Question p (insert op prediction ops)
insertNew _ _ _= error ("Error")


------ Помощни функции (за комуникация, проверка и т.н)

-- Проверяваме дали отговорът на потребителя е валиден
-- За да е валиден, трябва да е част от опциите (т.е Дали е написано точно "Yes" или "No")
checkIfAnswerIsValid :: [String] -> IO String
checkIfAnswerIsValid options = do
    reply <- getLine
    case (elem reply options) of
        True  -> return reply
        False -> do
            putStrLn "Invalid option, try again!"
            checkIfAnswerIsValid options

-- Помощна функция, която пита потребителя за намисленото животно
getPredictionCorrect :: IO String
getPredictionCorrect = do
    putStrLn "I have failed! What was the correct answer?"
    getLine

-- Функция, която пита потребителя кой е разграничителният върпос за намисленото животно
getQuestion :: String -> IO String
getQuestion predictionCorrect = do
    putStrLn $ "What question distinguishes " ++ predictionCorrect
                ++ " from my prediction?"
    getLine

-- Какъв е отговора на разграничителният въпрос, който е написал потребителя
getReply :: [String] -> String -> String -> IO String
getReply ops question prediction = do
    putStrLn $ "What is the answer to \"" ++ question ++
                "\" for " ++ prediction ++ "?"
    checkIfOptionIsValid ops

-- Проверяваме дали въведената от потребителя опция за отговор е валидна
checkIfOptionIsValid :: [String] -> IO String
checkIfOptionIsValid ops = do
    option <- getLine
    case option of
        "any" -> do
            putStrLn "\"any\" it can't be an answer. Try again."
            checkIfOptionIsValid ops
        _ -> case elem option ops of
            True -> do
                putStrLn "There is already an answer with that name."
                checkIfOptionIsValid ops
            False -> do 
                return option

-- Събираме възможните опции за отговор ("Yes" или "No")
listOptions :: Options -> [String]
listOptions ops = P.map fst (toList ops)

-- Изпечатване на възможните опции за отговор ("Yes" или "No")
showOptions :: [String] -> String
showOptions ops = P.foldl concat (head ops) (tail ops)
    where concat a b = a ++ " / " ++ b


-- Пускане на първоначалното меню
main::IO()
main = menu Nothing