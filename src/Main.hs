module Main
    ( main
    ) where

import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.Warp       as Warp
import qualified Network.Wai.Handler.WebSockets as WAS
import qualified Network.WebSockets             as WS

import           Mows.Prelude
import           Mows.Server                    (mkServer)

main :: IO ()
main = getArgs >>= mainArgs

mainArgs :: [String] -> IO ()
mainArgs _args = withLogger $ do
    port         <- lookupEnv "WONDER_PORT"   <&> maybe 8080 read
    tickInterval <- lookupEnv "TICK_INTERVAL" <&> maybe  200 read <&> (* 1000)

    (broadcaster, onPendingConnection) <- mkServer tickInterval

    withAsync broadcaster $ \aBroadcaster -> do
        info "[i] Broadcaster is running"
        info $ "[i] Listening on 0.0.0.0:" ++ show port ++ " with tick interval " ++ show (fromIntegral tickInterval/1000::Float) ++ "ms"

        finally
            (Warp.runSettings
                (Warp.setPort port $ Warp.setTimeout 36000 Warp.defaultSettings)
                (WAS.websocketsOr
                    WS.defaultConnectionOptions
                    (\p -> do
                        -- info "[i] onConnect"
                        onPendingConnection p)
                    (Static.staticApp $ Static.defaultFileServerSettings "./output")))
            (do
                info "[i] Stopping boradcaster..."
                cancel aBroadcaster )
