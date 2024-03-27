module Lib where

import Data.Time (getCurrentTime)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
  ( Element,
    UI,
    set,
    (#),
  )

--import Data.List.Split (splitOn)

data Environment = Environment
  { numberLs :: Int,
    standartList :: [String],
    comparedLists :: [[String]]
  }

environment :: Environment
environment = Environment 0 [] []

createButton :: String -> UI Element
createButton buttonName =
  UI.button
    # set UI.text buttonName
    # set
      (UI.attr "style")
      "text-align: center; font-size: 30px; min-height: 50px; width: 410px; \
      \ margin-bottom: 30px;"

createLabel :: UI Element
createLabel =
  UI.input
    # set UI.value ""
    # set
      (UI.attr "placeholder")
      "Choos file"
    # set
      (UI.attr "style")
      "text-align: center; font-size: 30px; \
      \ min-height: 50px; width: 410px; margin: 5px; \
      \ background: #6cf07d;"

searchIdenticalElems :: [String] -> [String] -> [String]
searchIdenticalElems standart = filter (`elem` standart)

name :: String -> UI Element
name str =
  UI.bold
    # set UI.text str
    # set
      (UI.attr "style")
      "display: flex; justify-content: center; font-size: 30px; margin: 20px; "

outputFileName :: IO String
outputFileName = do
  time <- map (\c -> if c == ' ' then '_' else c) . take 19 . show <$> getCurrentTime
  pure $ "result_" ++ time ++ ".txt"

writingListsToFile :: FilePath -> [[String]] -> Int -> IO ()
writingListsToFile _ [] _ = pure ()
writingListsToFile file (x : xs) count = do
  appendFile file $ show count ++ ". " ++ unwords x ++ "\n\n"
  writingListsToFile file xs $ count + 1

inputFile :: String -> UI Element
inputFile nameId =
  UI.input
    # set (UI.attr "type") "file"
    # set (UI.attr "id") nameId
    # set UI.value ""
    # set
      (UI.attr "style")
      "text-align: center; font-size: 20px; min-height: 35px; "

messages :: [String]
messages = 
  [ "Select the file with \n the reference list"
  , "Load the selected file"
  , "Select the file with \n the list to compare"
  , "Click the Compare button \n or select another file"
  , "The file with the comparison \n results has been generated"
  ]