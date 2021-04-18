module Page.List exposing (..)

import Graphics
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Utilities exposing (..)
import Problem exposing (..)
import Json.Decode
import Session exposing (Session)

type alias Model =
    { problems : List Problem
     , session : Session
    }


init : Session -> (Model, Cmd Msg)
init session =
    ( { problems = []
       , session = session     
    }, Cmd.none
    )


type Msg
    = GoToProblemMsg Int
    | GoToAboutMsg
    | OnScrollMsg


view : Model -> {title: String, content : Html Msg}
view {problems} =
    { title = "List"
      , content =
          div [ class "content-body" ]
        [ div [ id "scrolling", onScroll OnScrollMsg ]
            [ ol [ class "top-list" ] (List.indexedMap viewItem problems)
            , viewAbout
            ]
        ]
    }


viewItem : Int -> Problem -> Html Msg
viewItem i p =
    li [ class "clickable listing-problem-item", onClick <| GoToProblemMsg i ]
        [ div [ class "listing-tombstone" ]
            [ viewTombstone p.solved
            ]
        , div [ class "listing-description" ]
            [ div [ class "listing-problem-text" ] [ text <| "Problem " ++ String.fromInt (i + 1) ]
            , div [ class "listing-title-text" ] [ text p.description ]
            ]
        ]


viewTombstone : Bool -> Html Msg
viewTombstone isSolved =
    if isSolved then
        Graphics.whiteTombstone
    else
        Graphics.dashedTombstone

viewAbout : Html Msg
viewAbout =
    div [ class "clickable listing-about-item", onClick GoToAboutMsg ]
        [ text "About" ]


toSession : Model -> Session
toSession { session } =
    session
