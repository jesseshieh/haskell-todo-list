import System.Environment
import Data.Char

data Command = Add | Move | Complete | Show deriving (Show, Read, Eq)
data Context = None | Home | Work | Shopping deriving (Show, Read, Eq)
data Task = Task { text :: String, context :: Context } deriving (Show, Read)
data Tasks = Tasks { tasks :: [Task] } deriving (Show, Read)

dispatch :: [(String, [String] -> Tasks -> Tasks)]
dispatch =  [ ("add", addAction)
            , ("show", showAction)
            , ("complete", completeAction)
            , ("move", moveAction)
            ]

main = do
	(command:args) <- getArgs
	let (Just action) = lookup command dispatch

	-- TODO: what if the file is empty or does not exist?
	let filename = "tasks.txt"
	tasksString <- readFile filename
	let tasks = read tasksString 
	let newTasks = action args tasks

	putStr $ showTasks newTasks

	-- This line forces the entire file to be parsed so that we aren't
	-- reading and writing to the same file simultaneously.
	-- http://stackoverflow.com/questions/2527271/in-haskell-i-want-to-read-a-file-and-then-write-to-it-do-i-need-strictness-ann
	if command /= "show" then
		length tasksString `seq` (writeFile filename $ show newTasks)
			else return ()

showTasks :: Tasks -> String
showTasks myTasks = do
	unlines $ map showTask (tasks myTasks)

showTask :: Task -> String
showTask task = (text task) ++ ", " ++ (show $ context task)

readContext :: String -> Context
readContext context = stringToContext $ map toLower context

stringToContext :: String -> Context
stringToContext "none" = None
stringToContext "home" = Home
stringToContext "work" = Work
stringToContext "shopping" = Shopping

addAction :: [String] -> Tasks -> Tasks
addAction [text] myTasks = Tasks $ (tasks myTasks) ++ [Task text None]

showAction :: [String] -> Tasks -> Tasks
showAction [] myTasks = myTasks

completeAction :: [String] -> Tasks -> Tasks
completeAction [] myTasks = Tasks $ tail (tasks myTasks)

moveAction :: [String] -> Tasks -> Tasks
moveAction [newContextString] myTasks = let newContext = (readContext newContextString) in
	Tasks $ tail (tasks myTasks) ++ [(head (tasks myTasks)) { context = newContext }]
