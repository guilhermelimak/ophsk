module OPass where
import Control.Exception
import Data.Maybe
import System.IO
import System.Process

getList :: [[Char]]
-- getList = readCreateProcess (shell "op list items") ""
getList = return "'lol\nwut'"

run command = readCreateProcess (shell (command)) ""

getPassword :: IO String
getPassword = do
  putStr "Password: "
  hFlush stdout
  pass <- hasEcho False getLine
  putChar '\n'
  return pass

-- Hide password when typing on terminal
hasEcho :: Bool -> IO a -> IO a
hasEcho echo action =
    bracket (hGetEcho stdin)
            (hSetEcho stdin)
            (const $ hSetEcho stdin echo >> action)

signIn pass = run ("op signin my '" ++ pass ++ "'")

showRofi options = run ("echo -n " ++ options ++ " | rofi -dmenu")

login = do
  pass <- getPassword
  result <- signIn pass
  run ("" ++ result)

main = login

