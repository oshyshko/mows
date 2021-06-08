{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE TemplateHaskell #-}
module Mows.Protocol
    ( PlayerId, RoomId, RoomIdText, SeqId, XY(..), X, Y, FJson, TJson
    , ServerMessage(..), ClientMessage(..)
    , encode, decode
    ) where

import           Data.Aeson           (FromJSON, Options (..), SumEncoding (..),
                                       ToJSON, defaultOptions)
import qualified Data.Aeson           as Aeson
import           Data.Aeson.TH        (deriveJSON)
import qualified Data.ByteString.Lazy as L
import           Data.Char            (toLower)

import           Mows.Prelude

-- TODO https://github.com/haskell-waargonaut/waargonaut    -- interesting
-- TODO https://hackage.haskell.org/package/jsonifier       -- encoder only
-- TODO https://z.haskell.world/Z-Data/JSON.html            -- human/pretty? isolated ecosystem
-- TODO https://hackage.haskell.org/package/hw-json         -- verbose
-- TODO https://github.com/kccqzy/haskell-sajson            -- encoder only, C++
-- TODO https://wiki.haskell.org/Haskell_program_coverage

type PlayerId     = Int
type RoomId       = Int
type RoomIdText   = Text
type SeqId        = Int
type X            = Int
type Y            = Int

data XY = XY
    { x :: {-# UNPACK #-} X
    , y :: {-# UNPACK #-} Y
    } deriving (Generic, Show)

data ClientMessage
    = Move { pos :: XY }
    | Ping { id :: SeqId }
    deriving (Generic, Show)

data ServerMessage
    = Login         { id :: PlayerId }
    | FullUpdate    { roomIdText :: RoomIdText, players :: Map PlayerId XY }
    | Update        { ids :: [PlayerId], xs :: [X], ys :: [Y] }
    | Add           { id :: PlayerId, pos :: XY }
    | Remove        { id :: PlayerId }
    | Pong          { id :: SeqId }
    deriving (Generic)

-- wrapper
type FJson a = FromJSON a
type TJson a = ToJSON a

-- Aeson
encode :: TJson a => a -> L.ByteString
encode = Aeson.encode

decode :: FJson a => L.ByteString -> Maybe a
decode = Aeson.decode'

msum <$> forM
    [''XY, ''ClientMessage, ''ServerMessage]
        (deriveJSON $ defaultOptions
            { fieldLabelModifier = \case
                "roomIdText" -> "roomId"
                x            -> map toLower x
            , constructorTagModifier = \case
                "FullUpdate" -> "full_update"
                x            -> map toLower x
            , sumEncoding =
                TaggedObject { tagFieldName = "type", contentsFieldName = "contents" }
            })

