module Tomboy where

import Text.XML.HXT.Arrow.ReadDocument
import Text.XML.HXT.Core
import Text.XML.HXT.DOM.FormatXmlTree
import System.Directory
import System.FilePath
import Control.Monad
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import System.Environment
import Data.Maybe
import Data.Char
import System.Process
import System.IO.Temp
import System.IO
import Data.List
import Data.UUID
import Data.Word
import System.Random

readline :: String -> IO (Maybe String)
readline p = do
    putStr p
    l <- getLine
    return $ Just l

addHistory :: String -> IO ()
addHistory _ = return ()

test_file = "/home/chime/.local/share/tomboy/c1e9d00e-2bdf-4135-a3de-475fcc4e49b9.note"
test_dir = "/home/chime/dcode/ignesco/tomboycl/testArea/test01"

endsWith :: String -> String -> Bool
endsWith what s = if (reverse . (take (length what)) . reverse) s == what then True else False

documentRoot :: String -> IOSLA (XIOState s) a XmlTree
documentRoot xml = readDocument [] xml >>> getChildren >>> hasName "note"


getTitles xml = documentRoot xml >>> getChildren >>> hasName "title" >>> getChildren >>> getText

getNoteContent xml = documentRoot xml >>> getChildren >>> hasName "text" >>> getChildren >>> hasName "note-content" >>> getChildren

getAllText xml = getNoteContent xml >>> listA ( getText )
getAllLinks xml = getNoteContent xml >>> listA ( hasName "link:url" >>> getChildren >>> getText )

getAll xml = getNoteContent xml >>> ( listA getText ) &&& listA ( hasName "link:url" >>> getChildren >>> getText ) 

detailAsOutputString :: ([String], [String]) -> String
detailAsOutputString (texts, links)  = concat [concat texts, concat links]

loadNotesTitle xmlFile = do
    info <- runX (getTitles xmlFile)
    return $ concat info

loadNotesDetail :: FilePath -> IO String
loadNotesDetail xmlFile = do
    info <- runX (getAll xmlFile)
    return $ concat $ map detailAsOutputString info

loadAllTitles :: FilePath -> IO [(String, String)]
loadAllTitles directory = do
    files <- getDirectoryContents directory
    let filteredFiles = filter isNotesFile files
    titles <-  mapM (\f -> loadNoteTitle (directory </> f)) filteredFiles
    return titles

loadNoteTitle :: FilePath -> IO (FilePath, String)
loadNoteTitle f = do
    n <- loadNotesTitle f
    return (f, n)

isNotesFile f = endsWith ".note" f

test_loadNotesTitle = do
    o <- loadNotesTitle test_file
    putStrLn o

test_loadNotesDetail = do
    o <- loadNotesDetail test_file
    putStrLn o

-----

data Env = Env {
        envDirectory :: FilePath,
        envEditor :: String
    }

data TomboyState  = TomboyState {
        tomboyTitles :: [(FilePath, String)],
        searchStrings :: [String]
    }

type Tomboy a = StateT TomboyState (ReaderT Env IO) a

readEvalPrintLoop :: Tomboy ()
readEvalPrintLoop = do
    searchCount <- (liftM (length . searchStrings))  get
    let prompt = case searchCount of
                    0 -> "(help for commands) tomboy> "
                    otherwise -> "(help for commands) tomboy[SEARCH]> "
    maybeLine <- liftIO $ readline prompt
    case maybeLine of 
        Nothing     -> return () -- EOF / control-d
        Just "exit" -> return ()
        Just line -> do
            liftIO $ addHistory line
            con <- processCommand line
            if con then readEvalPrintLoop else return ()

processCommand :: String -> Tomboy Bool
processCommand "help" = liftIO showUsage >> return True
processCommand "?" = liftIO showUsage >> return True
processCommand "list" = listTitles >> return True
processCommand "l" = listTitles >> return True
processCommand ('e':'d':'i':'t':n) = safeProcess (stripSpaces n) editNote >> return True
processCommand ('n':'e':'w':[]) = newNote >> return True
processCommand ('s':'h':'o':'w':n) = safeProcess (stripSpaces n) showNote >> return True
processCommand ('i':'s':'e':'a':'r':'c':'h':':':n) = searchNotes toUpper (stripSpaces n) >> return True
processCommand ('s':'e':'a':'r':'c':'h':':':n) = searchNotes id (stripSpaces n) >> return True
processCommand ('c':'l':'e':'a':'r':_) = clearSearch >> return True
processCommand ('r':'e':'l':'o':'a':'d':[]) = reloadAllNotes >> return True
processCommand "quit" = return False
processCommand "q" = return False
processCommand ":q" = return False
processCommand arg = safeProcess (stripSpaces arg) showEditNote >> return True

showUsage :: IO ()
showUsage = mapM_ putStrLn
                [
                    "list/l - list all current notes",
                    "showNN - quick show note with number NN",
                    "new - create a new note",
                    "editNN - edit the note with number NN",
                    "search:NEW_SEARCH_TERM - add new search term (incremental)",
                    "isearch:NEW_SEARCH_TERM - add new case insensitive search term (incremental)",
                    "clear - clear search list",
                    "reload - clear search list and reload all notes",
                    "quit/q - quit the application"
                ]

safeProcess :: String -> (Int -> Tomboy ()) -> Tomboy Bool
safeProcess s act = let
    safeNum = safeNumber s
    in
        case safeNum of
            Just n -> act n >> return True
            Nothing -> return True

safeNumber :: String -> Maybe Int
safeNumber s = if s == "" then Nothing else safeDecimalString s

clearSearch :: Tomboy ()
clearSearch = do
    Env directory _ <- lift ask
    titles <- liftIO $ loadAllTitles directory
    modify (\s -> s {tomboyTitles = titles, searchStrings = []})

searchNotes :: (Char -> Char) -> String -> Tomboy ()
searchNotes casef searchFor = do
    st <- get
    let newSearchStrings = (map casef searchFor):(searchStrings st)
    liftIO $ putStrLn $ "search for: " ++ (intercalate "," newSearchStrings)
    newTitles <- filterM (containsSearchStrings casef newSearchStrings) (tomboyTitles st)
    put st {tomboyTitles = newTitles, searchStrings = newSearchStrings}
    listTitles
    
containsSearchStrings :: (Char -> Char) -> [String] -> (FilePath, String) -> Tomboy Bool
containsSearchStrings casef searches (fp, title) = do
    let isInTitle = and $ map (\s -> isInfixOf s (map casef title)) searches
    if isInTitle
        then return True
        else do
            body <- liftIO $ loadNotesDetail fp
            let isInBody = and $ map (\s -> isInfixOf s (map casef body)) searches
            return isInBody

newNote :: Tomboy ()
newNote = do
    env <- lift ask
    titles <- get
    let (title, body) = ("<NOTE_TITLE>", "<NOTE_BODY>")
    (tempfp1, h1) <- liftIO $ openTempFile "/tmp" "cshtomboyXXX.note"
    liftIO $ writeTempNote h1 (Right body) title
    (tempfp2, h2) <- liftIO $ openTempFile "/tmp" "cshtomboyXXX.note"
    liftIO $ writeTempNote h2 (Right body) title
    liftIO $ system $ envEditor env ++ " " ++ tempfp1
    overwriteInfo <- liftIO $ testAndPromptForOverwrite tempfp1 tempfp2
    liftIO $ writeNewNote (envDirectory env) (overwriteInfo, "")
    liftIO $ removeFile tempfp1
    liftIO $ removeFile tempfp2
    reloadAllNotes

editNote :: Int -> Tomboy ()
editNote n = do
    env <- lift ask
    titles <- (liftM tomboyTitles) get
    let (notef,title) = titles !! (n - 1)
    (tempfp1, h1) <- liftIO $ openTempFile "/tmp" "cshtomboyXXX.note"
    liftIO $ writeTempNote h1 (Left notef) title
    (tempfp2, h2) <- liftIO $ openTempFile "/tmp" "cshtomboyXXX.note"
    liftIO $ writeTempNote h2 (Left notef) title
    liftIO $ system $ envEditor env ++ " " ++ tempfp1
    overwriteInfo <- liftIO $ testAndPromptForOverwrite tempfp1 tempfp2
    liftIO $ writeEditNote (envDirectory env) (overwriteInfo, notef)
    liftIO $ removeFile tempfp1
    liftIO $ removeFile tempfp2
    reloadAllNotes

writeEditNote :: FilePath -> (Maybe (String, String), FilePath) -> IO ()
writeEditNote _ (Nothing, _) = return ()
writeEditNote directory (Just (title, body), notefile) = do
    writeNote (title, body) notefile

writeNewNote :: FilePath -> (Maybe (String, String), FilePath) -> IO ()
writeNewNote _ (Nothing, _) = return ()
writeNewNote directory (Just (title, body), _) = do
    notefile <- getNewNoteFilename directory
    writeNote (title, body) notefile

writeNote :: (String, String) -> FilePath -> IO ()
writeNote (title, body) filename = do
    let filebody = [
                "<?xml version=\"1.0\" encoding=\"utf-8\"?>",
                "<note version=\"0.3\" xmlns:link=\"http://beatniksoftware.com/tomboy/link\" xmlns:size=\"http://beatniksoftware.com/tomboy/size\" xmlns=\"http://beatniksoftware.com/tomboy\">",
                "  <title>" ++ xmlEscapeChars title ++ "</title>",
                "  <text xml:space=\"preserve\"><note-content version=\"0.1\">" ++ xmlEscapeChars body ++ "</note-content></text>",
                "  <last-change-date>2014-01-10T09:10:49.4469220+00:00</last-change-date>",
                "  <last-metadata-change-date>2014-01-10T09:10:49.4469220+00:00</last-metadata-change-date>",
                "  <create-date>2014-01-09T12:21:50.0373540+00:00</create-date>",
                "  <cursor-position>19</cursor-position>",
                "  <width>994</width>",
                "  <height>590</height>",
                "  <x>0</x>",
                "  <y>0</y>",
                "  <open-on-startup>False</open-on-startup>",
                "</note>"
            ]
    let fileContents = intercalate "\n" filebody
    putStrLn $ "writing to file: " ++ filename
    writeFile filename fileContents

xmlEscapeChars :: String -> String
xmlEscapeChars str = let
    xmlEscapeChars' (ans, []) = (ans, "")
    xmlEscapeChars' (ans, (c:cs)) =
        case c of
            '<' -> xmlEscapeChars'(";tl&":ans, cs)
            '>' -> xmlEscapeChars'(";tg&":ans, cs)
            '&' -> xmlEscapeChars'(";pma&":ans, cs)
            otherwise -> xmlEscapeChars'([c]:ans, cs)
    in reverse . concat . fst $ xmlEscapeChars' ([], str)


getNewNoteFilename directory = do
    guidString <- mkRandomUUID
    let filename = directory </> guidString ++ ".note"
    isFile <- doesFileExist filename
    isDir <- doesDirectoryExist filename
    if isFile || isDir then getNewNoteFilename directory else return filename

testAndPromptForOverwrite :: FilePath -> FilePath -> IO (Maybe (String, String))
testAndPromptForOverwrite editedFile referenceFile = do
    editedString <- readFile editedFile
    referenceString <- readFile referenceFile
    if
        editedString == referenceString
    then return Nothing
    else do
        putStr "Note has changed do you want to commit the changes to this note? (y/n)" >> hFlush stdout
        ans <- getLine
        if ans == "n" then return Nothing else return (Just $ splitTitleAndNote editedString)

splitTitleAndNote :: String -> (String, String)
splitTitleAndNote editedString = let
        title = takeWhile (/= '\n') editedString
        (_, notitle) = splitAt (length title) editedString
        toNextLine = takeWhile (=='\n') notitle
        (_, potentialEqualLine) = splitAt (length toNextLine) notitle
        body = if take 1 potentialEqualLine == "=" then skipToBody potentialEqualLine else potentialEqualLine
        skipToBody potentialEqualLine = let
            striped = takeWhile (\c -> c=='\n' || c=='=') potentialEqualLine
            (_, body') = splitAt (length striped) potentialEqualLine
         in body'
    in (title, body)

showEditNote :: Int -> Tomboy ()
showEditNote n = do
        liftIO $ putStr "do you want to edit or show this note? (e/s)" >> hFlush stdout
        ans <- liftIO $ getLine
        case ans of
            "e" -> editNote n
            "s" -> showNote n
            otherwise -> showEditNote n

showNote :: Int -> Tomboy ()
showNote n = do
    titles <- (liftM tomboyTitles) get
    let (notef,title) = titles !! (n - 1)
    body <- liftIO $ loadNotesDetail notef
    liftIO $ mapM_ putStrLn [title,take (length title) (repeat '='), body]
    return ()

writeTempNote :: Handle -> Either FilePath String -> String -> IO ()
writeTempNote h (Left f) title = do
    hPutStrLn h title
    hPutStrLn h $ take (length title) (repeat '=')
    body <- loadNotesDetail f
    hPutStr h body
    hClose h

writeTempNote h (Right body) title = do
    hPutStrLn h title
    hPutStrLn h $ take (length title) (repeat '=')
    hPutStr h body
    hClose h


listTitles :: Tomboy ()
listTitles = do
    titles <- (liftM tomboyTitles) get
    if length titles == 0
        then liftIO $ putStrLn "[No titles found]"
        else mapM_ (\(n, (f, s)) -> liftIO $ putStrLn $ show n ++ " - "  ++ s ) (zip [1..] titles)

reloadAllNotes :: Tomboy ()
reloadAllNotes = do
    Env directory _ <- lift ask
    titles <- liftIO $ loadAllTitles directory
    modify (\s -> s {tomboyTitles = titles, searchStrings = []})
    
safeDecimalString :: String -> Maybe Int
safeDecimalString s = let
    a = safeDecimalString' (Just (0, s))
    safeDecimalString' (Just (n, [])) = Just (n, "")
    safeDecimalString' (Just (n, (s:ss))) = do
        sn <- safeChar s
        safeDecimalString' $ Just (n*10 + sn, ss)
    safeDecimalString' Nothing = Nothing
    in case a of
        Just (n,_) -> Just n
        _ -> Nothing

safeChar :: Char -> Maybe Int
safeChar c = if posdig >=0 && posdig <=9 then Just posdig else Nothing
    where
        posdig = ord c - ord '0'

stripSpaces (' ':s) = stripSpaces s
stripSpaces s = s

mkRandomUUID :: IO String
mkRandomUUID = do
    w1 <- randomIO
    w2 <- randomIO
    w3 <- randomIO
    w4 <- randomIO
    return $ toString $ fromWords w1 w2 w3 w4


main' :: IO ()
main' = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    mainHelper args

mainHelper :: [String] -> IO ()
mainHelper ("-d":directory:"-e":editor:[]) = do
    titles <- loadAllTitles directory
    runReaderT (runStateT readEvalPrintLoop (TomboyState titles [])) (Env directory editor) >> return ()
mainHelper _ = putStrLn "Usage: -d NOTES_DIRECTORY -e EDITOR"

