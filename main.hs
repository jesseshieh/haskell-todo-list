import System.Environment
import Data.Char

data Command = Add | Move | Complete | Show deriving (Show, Read, Eq)
data Context = None | Home | Work | Shopping deriving (Show, Read, Eq)
data Task = Task { text :: String, context :: Context } deriving (Show, Read)
type Tasks = [Task] 

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
	length tasksString `seq` (maybeSave command filename newTasks)

maybeSave :: String -> String -> Tasks -> IO()
maybeSave "show" _ _ = return ()
maybeSave _ filename newTasks = writeFile filename $ show newTasks

showTasks :: Tasks -> String
showTasks tasks = unlines $ map showTask tasks

showTask :: Task -> String
showTask task = (show $ context task) ++ ": " ++ (text task)

addAction :: [String] -> Tasks -> Tasks
addAction [text] tasks = tasks ++ [Task text None]

showAction :: [String] -> Tasks -> Tasks
showAction [] tasks = tasks

completeAction :: [String] -> Tasks -> Tasks
completeAction [] tasks = tail tasks

moveAction :: [String] -> Tasks -> Tasks
moveAction [newContextString] tasks = tail tasks ++ [(head tasks) { context = readContext newContextString }]

readContext :: String -> Context
readContext contextString = read $ capWord contextString

capWord :: String -> String
capWord [] = []
capWord (firstChar:remainingString) = toUpper firstChar : map toLower remainingString
