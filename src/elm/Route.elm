module Route exposing (Route(..), fromUrl, href, pushUrl, replaceUrl)

import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url exposing (Url)
import Url.Parser as Parser exposing (Parser, oneOf, s, (</>), int)

-- ROUTING --


type Route
    = Root
      | About
      | ListPage
      | Solve Int
      | Proof



--    When needing parameters on the form base/item/id
--   | Item String


parser : Parser (Route -> a) a
parser =
    oneOf
        [ Parser.map Root Parser.top,
          Parser.map About (s "about"),
          Parser.map ListPage (s "list"),
          Parser.map Solve (s "solve" </> int),
          Parser.map Proof (s "proof")
        ]



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


pushUrl : Nav.Key -> Route -> Cmd msg
pushUrl key route =
    Nav.pushUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser

replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)
        
-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Root ->
                    []

                About ->
                    [ "about"]

                ListPage ->
                   [ "list" ]
               
                Solve x ->
                    [ "solve", String.fromInt x ]
                    
                Proof ->
                   [ "proof" ]
                
    in
    String.join "/" pieces

