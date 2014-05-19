-- Example usage
-- ghc --make main && ./main Add "new task"
-- ghc --make main && ./main Add "new task"
-- ghc --make main && ./main Add "new task"
-- ghc --make main && ./main Add "new task"
-- Jesse Shieh <jesse.shieh.pub@gmail.com>
import System.Environment
import Data.Char

data Command = Add | Move | Complete | Show deriving (Show, Read, Eq)
data Context = None | Home | Work | Shopping deriving (Show, Read, Eq)
data Task = Task { text :: String, context :: Context } deriving (Show, Read)
data Tasks = Tasks { tasks :: [Task] } deriving (Show, Read)

main = do
	args <- getArgs
	let (command, opt) = parseArgs args

	-- TODO: what if the file is empty or does not exist?
	let filename = "tasks.txt"
	tasksString <- readFile filename
	let tasks = read tasksString 
	let newTasks = executeCommand command opt tasks

	putStr $ showTasks newTasks

	-- This line forces the entire file to be parsed so that we aren't
	-- reading and writing to the same file simultaneously.
	-- http://stackoverflow.com/questions/2527271/in-haskell-i-want-to-read-a-file-and-then-write-to-it-do-i-need-strictness-ann
	if command /= Show then
		length tasksString `seq` (writeFile filename $ show newTasks)
			else return ()

parseArgs :: [String] -> (Command, String)
parseArgs ["add", text] = (Add, text)
parseArgs ["move", context] = (Move, context)
parseArgs ["show"] = (Show, "")
parseArgs ["complete"] = (Complete, "")

showTasks :: Tasks -> String
showTasks myTasks = do
	unlines $ map showTask (tasks myTasks)

showTask :: Task -> String
showTask task = (text task) ++ ", " ++ (show $ context task)

isInContext :: String -> Task -> Bool
isInContext contextString task = (context task) == (readContext contextString)

executeCommand :: Command -> String -> Tasks -> Tasks
-- executeCommand Show context myTasks = Tasks (filter (isInContext context) (tasks myTasks))
executeCommand Show _ myTasks = myTasks
executeCommand Add text myTasks = addTask text myTasks

-- TODO: can I just 'read' the context instead of providing my own translation function?
executeCommand Move context myTasks = moveHeadTask (readContext context) myTasks

-- TODO: write completed tasks to an archive file.
executeCommand Complete _ myTasks = completeHeadTask myTasks

readContext :: String -> Context
readContext context = stringToContext $ map toLower context

stringToContext :: String -> Context
stringToContext "none" = None
stringToContext "home" = Home
stringToContext "work" = Work
stringToContext "shopping" = Shopping

addTask :: String -> Tasks -> Tasks
addTask text myTasks = Tasks $ 
	(tasks myTasks) ++ [Task text None]

completeHeadTask :: Tasks -> Tasks
completeHeadTask myTasks = Tasks $ tail $ tasks myTasks

moveHeadTask :: Context -> Tasks -> Tasks
moveHeadTask context myTasks = Tasks $ (tail $ tasks myTasks) ++ [
	let myTask = head $ tasks myTasks 
		in Task (text myTask) context]
