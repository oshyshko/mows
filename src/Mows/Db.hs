module Mows.Db
    ( Db, Player(..), Room(..), TRoom
    , mkDb, nextPlayerId, lookupOrMkRoom, addPlayer, movePlayer, removePlayer, ejectRooms
    ) where

import qualified Data.IntMap.Strict as MI
import qualified Network.WebSockets as WS

import           Mows.Prelude
import           Mows.Protocol

data Db = Db
    { dbTNextPlayerId :: TVar PlayerId
    , dbTRoomId2tRoom :: TVar (IntMap TRoom)
    }

data Player = Player
    { connection :: WS.Connection
    , xy         :: XY
    }

data Room = Room
    { roomId                 :: RoomId
    , roomPlayerId2player    :: IntMap Player
    , roomPlayerId2posOutbox :: IntMap XY
    }

type TRoom = TVar Room

mkDb :: IO Db
mkDb = Db
    <$> newTVarIO 1
    <*> newTVarIO MI.empty

nextPlayerId :: Db -> STM PlayerId
nextPlayerId Db{dbTNextPlayerId} =
    stateTVar dbTNextPlayerId (\a -> (a, succ a))

lookupOrMkRoom :: Db -> RoomId -> STM TRoom
lookupOrMkRoom Db{dbTRoomId2tRoom} roomId = do
    roomId2tRoom <- readTVar dbTRoomId2tRoom
    case MI.lookup roomId roomId2tRoom of
        Just tRoom ->
            return tRoom
        Nothing -> do
            tRoom <- newTVar $ Room
                { roomId                 = roomId
                , roomPlayerId2player    = MI.empty
                , roomPlayerId2posOutbox = MI.empty
                }
            writeTVar dbTRoomId2tRoom $ MI.insert roomId tRoom roomId2tRoom
            return tRoom

addPlayer :: TRoom -> PlayerId -> Player -> STM ()
addPlayer tRoom playerId player = do
    modifyTVar' tRoom $ \r@Room{roomPlayerId2player} ->
        r { roomPlayerId2player = MI.insert playerId player roomPlayerId2player}

movePlayer :: TRoom -> PlayerId -> XY -> STM ()
movePlayer tRoom playerId xy =
    modifyTVar' tRoom $ \r@Room{roomPlayerId2posOutbox, roomPlayerId2player} ->
        r { roomPlayerId2posOutbox = MI.insert playerId xy roomPlayerId2posOutbox
          , roomPlayerId2player    = MI.adjust (\p -> p { xy = xy }) playerId roomPlayerId2player
          }

removePlayer :: Db -> TRoom -> PlayerId -> STM Room
removePlayer Db{dbTRoomId2tRoom} tRoom playerId = do
    r <- readTVar tRoom

    let newR = r { roomPlayerId2player    = MI.delete playerId (roomPlayerId2player r)
                 , roomPlayerId2posOutbox = MI.delete playerId (roomPlayerId2posOutbox r)
                 }

    if MI.null (roomPlayerId2player newR)
        then do
            modifyTVar' dbTRoomId2tRoom $ MI.delete (roomId r)
            return r
        else do
            writeTVar tRoom newR
            return newR

ejectRooms :: Db -> (Room -> IO ()) -> IO ()
ejectRooms Db{dbTRoomId2tRoom} f = do
    tRooms <- MI.elems <$> readTVarIO dbTRoomId2tRoom
    forM_ tRooms $ \tRoom -> do
        room <- atomically $ stateTVar tRoom $ \r -> (r,)
            r { roomPlayerId2posOutbox = MI.empty }
        f room
