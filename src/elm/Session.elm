module Session exposing (Session, defaultSession, navKey)

import Browser.Navigation as Nav

{-| Session management

This module is required to use Browser.application, which needs to store a Nav.Key
somewhere. This can evolve to have logged in Users

-}



-- TYPES


type Session = Guest Nav.Key



defaultSession : Nav.Key -> Session
defaultSession key =
    Guest key

-- INFO


navKey : Session -> Nav.Key
navKey (Guest key) =
    key
