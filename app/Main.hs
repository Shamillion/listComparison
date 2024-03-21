module Main where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Time (getCurrentTime)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
  ( Element,
    MonadIO (liftIO),
    UI,
    Window,
    column,
    defaultConfig,
    --  get,
    getBody,
    on,
    set,
    startGUI,
    title,
    (#),
    (#+),
  )
import System.Environment (getArgs)
import System.IO
  ( BufferMode (LineBuffering),
    hSetBuffering,
    stdout,
  )

--import Data.List.Split (splitOn)

setup :: Window -> UI ()
setup window = do
  _ <- return window # set title "Comparison of lists"

  ref <- liftIO $ newIORef environment

  getStandardButton <- createButton "Load standart"
  getListButton <- createButton "Load list"
  compareButton <- createButton "Compare"
  cleanButton <- createButton "Clean"

  -- standartLabel <- createLabel
  -- comparedLabel <- createLabel

  countLabel <-
    UI.label # set UI.text "0"
      # set (UI.attr "style") "font-size: 30px;"

  inputStandard <- inputFile "fileStandard"
  inputCompared <- inputFile "fileCompared"

  on UI.valueChange inputStandard $
    const $ do
      liftIO $ print "Hello"

  on UI.click getStandardButton $
    const $ do
      file <- UI.callFunction $ do UI.ffi "document.getElementById(\"fileStandard\").files[0].path"
      ls <- liftIO $ words <$> readFile file
      env <- liftIO $ readIORef ref
      liftIO $ writeIORef ref (env {standartList = ls}) >> print file >> print ls
      mapM_ (# set UI.enabled False . pure) [inputStandard, getStandardButton]
  -- x <-  UI.callFunction $ do UI.ffi "require('electron').dialog.showOpenDialog(mainWindow, { \
  --                        \  properties: ['openFile', 'openDirectory'] })"

  on UI.click getListButton $
    const $ do
      file <- UI.callFunction $ do UI.ffi "document.getElementById(\"fileCompared\").files[0].path"
      ls <- liftIO $ words <$> readFile file
      env <- liftIO $ readIORef ref
      let num = numberLs env + 1
      liftIO $ writeIORef ref (Environment num (standartList env) (ls : comparedLists env)) >> print file >> print ls
      _ <- pure countLabel # set UI.text (show num)
      pure inputCompared # set UI.value ""

  on UI.click compareButton $
    const $ do
      env <- liftIO $ readIORef ref
      let standartLs = standartList env
          comparedLss = comparedLists env
          ls = reverse $ map (searchIdenticalElems standartLs) comparedLss
      outputFileName' <- liftIO outputFileName
      _ <- liftIO $ writingListsToFile outputFileName' ls 1
      _ <- pure compareButton # set UI.text "Done!"
      mapM_ (# set UI.enabled False . pure) [inputStandard, getStandardButton, inputCompared, getListButton, compareButton]

  on UI.click cleanButton $
    const $ do
      liftIO $ writeIORef ref environment
      _ <- pure compareButton # set UI.text "Compare"
      _ <- pure countLabel # set UI.text "0"
      mapM_ (# set UI.enabled True . pure) [inputStandard, getStandardButton, inputCompared, getListButton, compareButton]
      mapM_ (# set UI.value "" . pure) [inputStandard, inputCompared]

  let columnTop =
        UI.column [name "Standart list", pure inputStandard, pure getStandardButton]
          # set (UI.attr "style") "background-color: #FFD700; padding: 0 20px; "

      rowComparedName =
        UI.row [name "Compared lists. Loaded", pure countLabel]
          # set (UI.attr "style") "display: flex; justify-content: center;"

      columnMiddle =
        UI.column [rowComparedName, pure inputCompared, pure getListButton]
          # set (UI.attr "style") "background-color: #00CED1; padding: 0 20px; "

      columnBottom =
        UI.column [pure compareButton, pure cleanButton]
          # set (UI.attr "style") "background-color: #9BC2F9; padding: 40px 20px 0; "

      gameBody = column [columnTop, columnMiddle, columnBottom]
  -- # set (UI.attr "style") "justify-content: center;"

  _ <- getBody window #+ [gameBody]

  pure ()

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  ls <- getArgs
  case ls of
    [port] -> start (read port)
    _ -> pure ()

--  startGUI defaultConfig setup

start :: Int -> IO ()
start port = do
  startGUI
    defaultConfig
      { UI.jsPort = Just port
      }
    setup

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
      "text-align: center; font-size: 50px; \
      \ min-height: 75px; width: 450px; margin-bottom: 5px; \
      \ background: #6cf07d;"

searchIdenticalElems :: [String] -> [String] -> [String]
searchIdenticalElems standart = filter (`elem` standart)

name :: String -> UI Element
name str =
  UI.p
    # set UI.text str
    # set
      (UI.attr "style")
      "text-align: center; font-size: 30px; margin-bottom: 20px; margin-right: 10px;"

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
