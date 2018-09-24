{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Exception hiding (Handler)
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text, pack, splitOn)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

import Data.Typeable
import Database.SQLite.Simple hiding (close, bind)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ

import System.Environment
import System.Exit
import System.IO

data User = User
    { userId        :: Integer
    , username      :: Text
    , shell         :: Text
    , homeDirectory :: Text
    , realName      :: Text
    , phone         :: Text
    } deriving (Eq, Show)

instance FromRow User where
    fromRow = User
        <$> field
        <*> field
        <*> field
        <*> field
        <*> field
        <*> field

instance ToRow User where
    toRow (User id_ username shell homeDir realName phone) =
        toRow (id_, username, shell, homeDir, realName, phone)

data DuplicateData = DuplicateData deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow = (Null, Text, Text, Text, Text, Text)

type Handler = (Connection -> Socket -> ByteString -> IO ())

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--server"] -> server 79 (\c s msg -> case msg of
            "\r\n" -> returnUsers c s
            name   -> returnUser c s (decodeUtf8 name))

        ["--control"] -> server 1079 (\_ s msg -> case splitOn ":" . decodeUtf8 $ msg of
            [username, shell, homeDirectory, realName, phone] -> do
                addUser
                    ( Null
                    , username
                    , shell
                    , homeDirectory
                    , realName
                    , phone
                    )
                sendAll s . encodeUtf8 $ "Added!\r\n"
                return ()
            _ -> do
                sendAll s . encodeUtf8 $ "Invalid input\r\n"
                return ())

        ["--add", username, shell, homeDirectory, realName, phone] ->
            addUser
                ( Null
                , pack username
                , pack shell
                , pack homeDirectory
                , pack realName
                , pack phone
                )
        _            -> hPutStrLn stderr "Unrecognised args" >> exitFailure

server :: Int -> Handler -> IO ()
server port h = withSocketsDo $ do
    addrinfos <- getAddrInfo
        (Just (defaultHints
            {addrFlags = [AI_PASSIVE]}))
        Nothing (Just . show $ port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr)
        Stream defaultProtocol
    bind sock (addrAddress serveraddr)
    listen sock 1 -- only one connection open at a time
    conn <- open "finger.db"
    handleQueries conn sock h
    SQLite.close conn
    close sock

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
    (id INTEGER PRIMARY KEY AUTOINCREMENT,
     username TEXT UNIQUE,
     shell TEXT, homeDirectory TEXT,
     realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser =
    "INSERT INTO users\
    \ VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers =
    "SELECT * from users"

getUserQuery :: Query
getUserQuery =
    "SELECT * from users where username = ?"

getUser :: Connection -> Text -> IO (Maybe User)
getUser conn username = do
    results <- query conn getUserQuery (Only username)
    case results of
        []     -> return $ Nothing
        [user] -> return $ Just user
        _      -> throwIO DuplicateData

addUser :: UserRow -> IO ()
addUser u = do
    bracket
        (open "finger.db")
        SQLite.close
        (\conn -> execute conn insertUser u)

createDatabase :: IO ()
createDatabase = do
    conn <- open "finger.db"
    execute_ conn createUsers
    execute conn insertUser meRow
    rows <- query_ conn allUsers
    mapM_ print (rows :: [User])
    SQLite.close conn
    where
            meRow :: UserRow
            meRow = (Null, "callen", "/bin/zsh", "/home/callen", "Chris Allen", "555-123-4567")

returnUsers :: Connection -> Socket -> IO ()
returnUsers dbConn soc = do
    rows <- query_ dbConn allUsers
    let usernames = map username rows
        newlineSeparated = T.concat $ intersperse "\n" usernames
    sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell homeDir realName _) = BS.concat
    [ "Login: ",     e username, "\t\t\t\t"
    , "Name: ",      e realName, "\n"
    , "Directory: ", e homeDir,  "\t\t\t"
    , "Shell: ",     e shell,    "\n"
    ]
      where e = encodeUtf8

returnUser :: Connection -> Socket -> Text -> IO ()
returnUser dbConn soc username = do
    maybeUser <- getUser dbConn (T.strip username)
    case maybeUser of
        Nothing -> do
            putStrLn
                ("Couldn't find matching user\
                \ for username: "
                ++ (show username))
            return ()
        Just user ->
            sendAll soc (formatUser user)

handleQuery :: Connection -> Socket -> Handler -> IO ()
handleQuery dbConn soc h = do
    msg <- recv soc 1024
    h dbConn soc msg

handleQueries :: Connection -> Socket -> Handler -> IO ()
handleQueries dbConn sock h = forever $ do
    (soc, _) <- accept sock
    putStrLn "Got connection, handling query"
    handleQuery dbConn soc h
    close soc
