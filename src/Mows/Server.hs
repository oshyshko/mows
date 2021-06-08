module Mows.Server
    ( Port, Micros, MicrosDelta, mkServer, nowMicros
    ) where

import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.IntMap.Strict    as MI
import qualified Data.Map.Strict       as M
import           Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.WebSockets    as WS

import           Mows.Db
import           Mows.Prelude
import           Mows.Protocol

type Port        = Int
type Micros      = Int
type MicrosDelta = Int

mapWidth, mapHeight :: Float
mapWidth = 1000
mapHeight = 500

{-# ANN mkServer ("HLint: ignore Avoid lambda" :: String) #-}
mkServer :: MicrosDelta -> IO (IO (), WS.PendingConnection -> IO ())
mkServer tickInterval = do
    db <- mkDb

    return
        ( forever $ handle
            (\(e :: SomeException) -> info $ "[!] Got an exeption inside broadcaster: " ++ show e)
            (do before <- nowMicros
                tickBroadcaster db
                after <- nowMicros
                threadDelay $ tickInterval - (after - before))

        , \p -> handleAny
            (\_e -> return ())
            -- (\e -> info $ "... Got an exeption with a pending connection: " ++ show e)
            (onConnect db p)
        )

tickBroadcaster :: Db -> IO ()
tickBroadcaster db = do
    ejectRooms db $ \r@Room{roomPlayerId2posOutbox} ->
        unless (MI.null roomPlayerId2posOutbox) $
            let kv = MI.assocs roomPlayerId2posOutbox
            in broadcast r $ Update (fst <$> kv) (x . snd <$> kv) (y . snd <$> kv)

onConnect :: Db -> WS.PendingConnection -> IO ()
onConnect db pending = do
    let WS.RequestHead{requestPath} = WS.pendingRequest pending
        roomIdBytes = B.drop (B.length "/rooms/") requestPath -- TODO use dir/path API

    if not ("/rooms/" `B.isPrefixOf` requestPath)
        then do
            WS.rejectRequest pending $ B.append "Expected requestPath to start with '/room/', but got: " roomIdBytes
        else do
            let roomId = if B.null roomIdBytes
                    then 0  -- default room
                    else fst $ fromMaybe
                        (error $ "Expected roomId to be an integer, but got: " ++ show roomIdBytes)
                        (BC8.readInt roomIdBytes)

            playerId <- atomically $ nextPlayerId db
            tRoom <- atomically $ lookupOrMkRoom db roomId

            WS.acceptRequest pending >>= \c ->
                WS.withPingThread c 30
                    (return ())
                    (finally
                        -- run player
                        (do -- info $ "[i] player joined: " ++ show playerId
                            runPlayer tRoom (intToText roomId) playerId c)
                        -- release resources
                        (onLeave db tRoom playerId c ))

onLeave :: Db -> TRoom -> PlayerId -> WS.Connection -> IO ()
onLeave db tRoom playerId c = do
    r <- atomically $ removePlayer db tRoom playerId
    -- info $ "[i] Player left: " ++ show playerId ++ " (" ++ show roomsRemaining ++ " rooms remaining)"
    broadcast r $ Remove playerId

    handleAny
        (\_e -> return ())
        -- (\e -> info $ "... Couldn't send 'close' to a peer: " ++ show e )
        (do WS.sendClose c ("Goodbye!" :: Text))

runPlayer :: TRoom -> RoomIdText -> PlayerId -> WS.Connection -> IO ()
runPlayer tRoom roomIdText playerId c = do
    initialXy <- XY
        <$> (truncate . (* mapWidth)  <$> (randomIO :: IO Float))
        <*> (truncate . (* mapHeight) <$> (randomIO :: IO Float))

    atomically $ addPlayer tRoom playerId (Player c initialXy)
    atomically $ movePlayer tRoom playerId initialXy

    readTVarIO tRoom >>= \r@Room{roomPlayerId2player} -> do
        send c $ Login playerId
        send c $ FullUpdate roomIdText (
            MI.foldrWithKey (\k v -> M.insert k (xy v)) M.empty roomPlayerId2player)
        broadcast r $ Add playerId initialXy

    -- pump commands from player
    forever $ receive c >>= \case
        Move xy    -> atomically $ movePlayer tRoom playerId xy
        Ping seqId -> send c $ Pong seqId

send :: TJson a => WS.Connection -> a -> IO ()
send c a = do
    let x = encode a
    -- info $ ">>> " ++ show x
    WS.sendTextData c x

broadcast :: TJson a => Room -> a -> IO ()
broadcast Room{roomPlayerId2player} m = do
    let json = encode m
    -- info $ ">*> " ++ show json

    forM_ (MI.elems roomPlayerId2player) $
        \Player{connection} -> handleAny
            (\_e -> return ())
            -- (\e -> info $ "... Couldn't send a broadcast to a peer: " ++ show e )
            (do WS.sendTextData connection json)

receive :: forall a. (FJson a) => WS.Connection -> IO a
receive c =
    WS.receiveDataMessage c <&> \case
        WS.Text bs _ -> fromMaybe (error $ "Couldn't parse JSON: " ++ show bs) (decode bs)
        WS.Binary bs -> error $ "Didn't expect a binary message, but got one: " ++ show bs

nowMicros :: IO Micros
nowMicros = ceiling . (* 1000000) <$> getPOSIXTime
