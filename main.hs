import System.IO
import System.Directory
import System.Environment
import Data.List
import Data.Char

-- Defines the data types we will use.
data Command = Add | Move | Complete | Show deriving (Show, Read, Eq)
data Context = None | Home | Work | Shopping deriving (Show, Read, Eq)
data Task = Task { text :: String, context :: Context } deriving (Show, Read)
type Tasks = [Task] 

-- Defines mapping of commands to their functions.
dispatch :: [(String, [String] -> Tasks -> Tasks)]
dispatch =  [ ("add", addAction)
            , ("show", showAction)
            , ("complete", completeAction)
            , ("move", moveAction)
            ]

main :: IO()
main = do
	(command:args) <- getArgs
	let (Just action) = lookup command dispatch

	let filename = "tasks.txt"
	tasksString <- readListFromFile filename
	let tasks = read tasksString 

	let newTasks = action args tasks

	putStr $ showTasks newTasks
	writeFile filename $ show newTasks

-- Reads in the contents of a file. If the file doesn't exist
-- it returns an empty list string.
readListFromFile :: FilePath -> IO(String)
readListFromFile filename = do
	fileExists <- doesFileExist filename
	case fileExists of
		True -> readFile filename
		False -> return "[]"

-- Converts the tasks into a string ready for printing to the screen.
showTasks :: Tasks -> String
showTasks tasks = unlines $ (map join2Tuple (zip intStrings (map showTask tasks)))

-- Infinite list of numbers as strings.
intStrings :: [String]
intStrings = map show [0..]

-- Joins a list with indexes.
-- Concatenates a 2-tuple together with a colon inbetween.
join2Tuple :: (String, String) -> String
join2Tuple (x, y) = x ++ ". " ++ y

-- Converts the task into a string ready for printing to the screen.
showTask :: Task -> String
showTask task = (show $ context task) ++ ": " ++ (text task)

-- Adds a new task to the tasks.
addAction :: [String] -> Tasks -> Tasks
addAction [text] tasks = tasks ++ [Task text None]

-- Identity.
showAction :: [String] -> Tasks -> Tasks
showAction [] tasks = tasks

-- Removes the first task from the tasks.
completeAction :: [String] -> Tasks -> Tasks
completeAction [] tasks = tail tasks

-- Changes the context of the first task and moves it to the end of the tasks.
moveAction :: [String] -> Tasks -> Tasks
moveAction [newContextString] tasks = tail tasks ++ [(head tasks) { context = readContext newContextString }]

-- Converts a string into a Context.
readContext :: String -> Context
readContext contextString = read $ capWord contextString

-- Capitalizes the first letter of a word.
capWord :: String -> String
capWord [] = []
capWord (firstChar:remainingString) = toUpper firstChar : map toLower remainingString
