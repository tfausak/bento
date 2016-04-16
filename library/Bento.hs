{-# LANGUAGE TypeFamilies #-}

-- | Bento manages stateful components. It is inspired by
-- <https://github.com/stuartsierra/component Stuart Sierra's Component>
-- library for Clojure.
module Bento where

-- | A stateful component. Usually you will define an instance of this class to
-- a data type that contains all of its runtime state.
--
-- @data Example = Example { handle :: 'System.IO.Handle' }
--instance 'Component' Example where
--    -- To be defined...@
class Component component where
    -- | Everything necessary to start the component. Typically this is a
    -- tuple, but you are free to use whichever data structure you want. If
    -- your component does not have any dependencies, use @()@, the empty
    -- tuple.
    --
    -- @type 'Dependencies' Example = ('FilePath', 'System.IO.IOMode')@
    type Dependencies component :: *

    -- | Starts the component. This is where you should do things like open
    -- file handles, set up connections, and generally acquire resources. If
    -- anything goes wrong, just throw an exception, preferrably with
    -- 'Control.Exception.throwIO'.
    --
    -- This function should not block forever. If you need to start something
    -- that should keep running, like a server, put it on another thread with
    -- 'Control.Concurrent.forkIO'.
    --
    -- @'start' (path, mode) = do
    --    h <- 'System.IO.openFile' path mode
    --    let component = Example { handle = h }
    --    'return' component@
    start :: Dependencies component -> IO component

    -- | Stops the component. Generally this will do the opposite of whatever
    -- you did in 'start'. The default implementation does nothing, which can
    -- be enough if you want the garbage collector to handle everything.
    --
    -- @'stop' component = do
    --    'System.IO.hClose' (handle component)@
    stop :: component -> IO ()
    stop _component = return ()

-- * Complete example

-- $
-- The follow is a complete example of using 'Component's to build a larger
-- system, which is itself a 'Component'.
--
-- For this simple example, we will be starting a web server. The only piece of
-- configuration we need is the port to listen on. We will get that from the
-- environment. If it's not available, we'll fall back to a default.
--
-- @data Config = Config { port :: Int }
--instance 'Component' Config where
--    type 'Dependencies' Config = ()
--    'start' () = do
--        p <- 'System.Environment.lookupEnv' \"PORT\"
--        let config = Config { port = 'Data.Maybe.fromMaybe' 8080 p }
--        'return' config@
--
-- Now that we have the config, we can go ahead and set up the server. It
-- doesn't have to care how the config gets the port. We just have to list the
-- config as a dependency. We'll also need the 'Network.Wai.Application' we
-- want to run on the server.
--
-- Since 'start' shouldn't block forever, we fire up the server on a separate
-- thread. We keep track of the thread ID so that we can shut down the server
-- when we 'stop' by killing the thread.
--
-- @data Server = Server { threadId :: 'Control.Concurrent.ThreadId' }
--instance 'Component' Server where
--    type 'Dependencies' Server = (Config, 'Network.Wai.Application')
--    'start' (config, application) = do
--        tid <- 'Control.Concurrent.forkIO' ('Network.Wai.Handler.Warp.run' (port config) application)
--        let server = Server { threadId = tid }
--        return server
--    'stop' server = do
--        'Control.Concurrent.killThread' (threadId server)@
--
-- With the config and server in hand, we can combine them into a larger
-- system. The system will need the 'Network.Wai.Application' we want to run,
-- but it will handle 'start'ing the config and passing it to the server.
--
-- To 'stop' the system, we 'stop' each 'Component' in the reverse order that
-- we 'start'ed them.
--
-- @data System = System { config :: Config, server :: Server }
--instance 'Component' System where
--    type 'Dependencies' System = ('Network.Wai.Application')
--    'start' (application) = do
--        c <- 'start' ()
--        s <- 'start' (c, application)
--        let system = System { config = c, server = s }
--        'return' system
--    'stop' system = do
--        'stop' (server system)
--        'stop' (config system)@
--
-- To actually run the system, we 'start' it just like the other 'Component's.
-- Once it's started, we want to wait forever until something sends us a
-- @SIGINT@. Then we 'stop' the system.
--
-- @main :: 'IO' ()
--main = do
--    let application _request respond = respond ('Network.Wai.responseLBS' 'Network.HTTP.Types.ok200' [] 'Data.ByteString.Lazy.empty')
--    system <- 'start' (application)
--    sentinel <- 'Control.Concurrent.newEmptyMVar'
--    let handler _signal = 'Control.Concurrent.putMVar' sentinel ()
--    'System.Signal.installHandler' 'System.Signal.sigINT' handler
--    'Control.Concurrent.takeMVar' sentinel
--    'stop' (system :: System)@
