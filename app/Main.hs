
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
    get,
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
    ( hSetBuffering, stdout, BufferMode(LineBuffering) )


--import Data.List.Split (splitOn)

setup :: Window -> UI ()
setup window = do
  _ <- return window # set title "Comparison of lists"

  ref <- liftIO $ newIORef environment

  getStandardButton <- createButton "Load standart"
  getListButton <- createButton "Load list"
  compareButton <- createButton "Compare"
  cleanButton <- createButton "Clean"

  standartLabel <- createLabel
  comparedLabel <- createLabel

  countLabel <-
    UI.label # set UI.text "0"
      # set (UI.attr "style") "font-size: 30px;"

  on UI.click getStandardButton $
    const $ do
      ls <- words <$> get UI.value standartLabel
      env <- liftIO $ readIORef ref
      liftIO $ writeIORef ref (env {standartList = ls}) >> print ls
      mapM_ (# set UI.enabled False . pure) [standartLabel, getStandardButton]

  on UI.click getListButton $
    const $ do
      ls <- words <$> get UI.value comparedLabel
      env <- liftIO $ readIORef ref
      let num = numberLs env + 1
      liftIO $ writeIORef ref (Environment num (standartList env) (ls : comparedLists env)) >> print ls
      _ <- pure countLabel # set UI.text (show num)
      pure comparedLabel # set UI.value ""

  on UI.click compareButton $
    const $ do
      env <- liftIO $ readIORef ref
      let standartLs = standartList env
          comparedLss = comparedLists env
          ls = reverse $ map (searchIdenticalElems standartLs) comparedLss
      fileName' <- liftIO fileName
      _ <- liftIO $ writingListsToFile fileName' ls 1
      _ <- pure compareButton # set UI.text "Done!"
      mapM_ (# set UI.enabled False . pure) [standartLabel, getStandardButton, comparedLabel, getListButton, compareButton]

  on UI.click cleanButton $
    const $ do
      liftIO $ writeIORef ref environment
      _ <- pure compareButton # set UI.text "Compare"
      _ <- pure countLabel # set UI.text "0"
      mapM_ (# set UI.enabled True . pure) [standartLabel, getStandardButton, comparedLabel, getListButton, compareButton]
      mapM_ (# set UI.value "" . pure) [standartLabel, comparedLabel]

  let rowComparedName =
        UI.row [name "Compared lists. Loaded", pure countLabel]
          # set (UI.attr "style") "display: flex; justify-content: center;"

      gameBody =
        column
          [ name "Standart list",
            pure standartLabel,
            pure getStandardButton,
            rowComparedName,
            pure comparedLabel,
            pure getListButton,
            pure compareButton,
            pure cleanButton
          ]
      --    # set (UI.attr "style") "position: fixed; left: 30%; top : 5%;"

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
  startGUI defaultConfig
    { UI.jsPort = Just port
    } setup





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
      "text-align: center; font-size: 50px; min-height: 75px; width: 450px; \
      \ margin-bottom: 50px;"

createLabel :: UI Element
createLabel =
  UI.input
    # set UI.value ""
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
      "text-align: center; font-size: 30px; min-height: 75px; margin-bottom: -40px; margin-right: 10px;"

fileName :: IO String
fileName = do
  time <- map (\c -> if c == ' ' then '_' else c) . take 19 . show <$> getCurrentTime
  pure $ "result_" ++ time ++ ".txt"

writingListsToFile :: FilePath -> [[String]] -> Int -> IO ()
writingListsToFile _ [] _ = pure ()
writingListsToFile file (x : xs) count = do
  appendFile file $ show count ++ ". " ++ unwords x ++ "\n\n"
  writingListsToFile file xs $ count + 1
