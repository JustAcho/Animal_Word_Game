-- Файл с имплементация на някои основни функции (за да не е сбито абсолютно всичко в един файл)

{- ЗАБЕЛЕЖКА: Менютата, опциите, въпросите и т.н са на Английски, тъй като 
играта отказваше да тръгне като се опитвах да изпечатам нещо на кирилица.
В коментарите съответните функции са написани с английските си имена, с цел
по-добра ориентация. -}

module Game where
import Data.Map as M

-- Option работи като речник, в който има информацията от отговора на предишният въпрос
-- и как трябва да продължи играта
type Options = Map String Game

-- "Prediction" е финалният отговор на играта
-- "Question" е въпрос, който играта задава с възможните опции за отговор към него ("Yes" или "No")
data Game = Question String Options | Prediction String 
                deriving(Show,Read)

-- Създава играта (може и наново, след допълнително добавяне на животни от потребителя)
createGame :: String -> Game
createGame s = Prediction s

-- Връщаме подаденият "prediction" и проверяваме дали е правилен.
prediction :: Game -> String
prediction (Prediction s) = s
prediction _ = error "Error, not a prediction"

-- Връщаме подаденият "question" и проверяваме дали е правилен.
question :: Game -> String
question (Question s _) = s
question _ = error "Error, not a question"

-- Получаваме "question" и връщаме съответните "options" за него, както и проверяваме за валидност
options :: Game -> Options
options (Question _ options) = options
options _ = error "Error, not a question"

-- Получаваме отговорa на потребителя и проверяваме неговата валидност
reply :: Game -> String -> Game
reply (Question _ options) key =
    case (M.lookup key options) of
        Just r -> r
        Nothing -> error "Error, not a valid answer."
reply _ _ = error "Error, not a question."

-- Получаваме нов въпрос, който Играта трябва да задава при следващо преиграване
newQuestion :: [String] -> [Game] -> String -> Game
newQuestion options games question =
    Question question (fromList (zip options games))
