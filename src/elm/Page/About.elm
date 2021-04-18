module Page.About exposing (..)

import Graphics
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Session exposing (Session)
import Browser.Navigation as Nav

type alias Model = {session : Session}

init : Session -> (Model, Cmd Msg)
init session =
    ({session = session}, Cmd.none)



view : Int -> { title : String, content : Html Msg }
view version =
    { title = "About"
          , content = div [ class "content-body" ]
        [ div [ class "nav-bar" ]
            [ button [ class "nav-bar-button", title "Back", onClick ClickedBackButton ] [ Graphics.leftAngle ] ]
        , div [ class "grow" ] []
        , div [ class "about-box" ]
            [ div [ class "about-title" ]
                [ text "Sympathy for the Machine" ]
            , div [ class "about-version" ]
                [ text <| "version " ++ String.fromInt version ]
            , div [ class "about-created" ]
                [ text "Made by Edwin Zacharias" ]
            , div [ class "about-email" ]
                [ a [ href "mailto:sftm@schlussweisen.com" ]
                    [ text "sftm@schlussweisen.com" ]
                ]
            ]
        , div [ class "grow-2" ] []
        , div [ class "about-copyright" ]
            [ text "Copyright © 2017 Edwin Zacharias. All rights reserved." ]
        ]

    }


type Msg =
    ClickedBackButton


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickedBackButton ->
            (model, Nav.back (Session.navKey model.session) 1)
    

toSession : Model -> Session
toSession {session} =
    session
