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

	-- TODO: What if the file is empty or does not exist?
	let filename = "tasks.txt"
	tasksString <- readFile filename
	let tasks = read tasksString 
	let newTasks = action args tasks

	-- This line also ensures that the file is finished reading before
	-- the next line starts writing.
	putStr $ showTasks newTasks

	maybeSave command filename newTasks

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
