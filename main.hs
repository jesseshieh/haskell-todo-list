import System.Environment
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

	-- TODO: What if the file is empty or does not exist?
	let filename = "tasks.txt"
	tasksString <- readFile filename
	let tasks = read tasksString 

	let newTasks = action args tasks

	putStr $ showTasks newTasks
	writeFile filename $ show newTasks

-- Converts the tasks into a string ready for printing to the screen.
showTasks :: Tasks -> String
showTasks tasks = unlines $ map showTask tasks

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
