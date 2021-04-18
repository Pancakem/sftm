module Main exposing (..)

import Dict exposing (Dict)
import Html exposing (Html)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Page.About
import Page.List
import Page.Proof as Proof
import Page.Solve as Solve
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Ports
import Problem exposing (Problem)
import Problems
import Task
import Utilities exposing (..)
import Url exposing (Url)
import Session exposing (Session)
import Route exposing (Route)
import Page


type alias Model =
    { problems : List Problem
    , scrollY : Float
    , status : Status
    }


type Status
    = Redirect Session
      | NotFound Session
      | ListPage Page.List.Model
      | ProofPage Proof.Model
      | SolvePage Int Solve.Model
      | AboutPage Page.About.Model

type Msg
    = IgnoreMsg
    | ChangedUrl Url
    | ClickedLink Browser.UrlRequest
    | LocalStorageChangedMsg (Maybe String)
    | GoToProblemMsg Int
    | GoToAboutMsg
    | OnScrollMsg
    | SaveScrollMsg Float
    | GotAboutMsg Page.About.Msg
    | SolvePageMsg Solve.Msg


main : Program Encode.Value Model Msg
main =
    Browser.application
        { subscriptions = subscriptions
        , init = init
        , view = view
        , update = update
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangedUrl
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Ports.onLocalStorageChange LocalStorageChangedMsg

init : Encode.Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url navKey =
    changeRouteTo (Route.fromUrl url)
        { status = (Redirect (Session.defaultSession navKey))
          , scrollY = 0.0
          , problems = []
        }


view : Model -> Browser.Document Msg
view model =
    let
        viewPage page toMsg config =
            let
                { title, body } =
                    Page.view page config
            in
            { title = title
              , body = List.map (Html.map toMsg) body
            }
    in
    case model.status of
        ListPage listModel ->
           viewPage Page.All listMsg (Page.List.view listModel) -- or use model.problems

        AboutPage _ ->
            viewPage Page.All GotAboutMsg <| Page.About.view version

        SolvePage i solveModel ->
            viewPage Page.All SolvePageMsg <| (Solve.view (currentProblem model) solveModel)

        ProofPage proofModel ->
            viewPage Page.All (\_ -> IgnoreMsg) (Proof.view (GoToProblemMsg proofModel.index) proofModel)

        _ ->
            { title = ""
              , body = []
            }

toSession : Model -> Session
toSession { status } =
    case status of
        Redirect session ->
            session
                
        NotFound session ->
            session

        ListPage model ->
            Page.List.toSession model

        AboutPage model ->
            Page.About.toSession model

        ProofPage model ->
            Proof.toSession model

        SolvePage _ model ->
            Solve.toSession model
                
changeRouteTo : Maybe Route -> Model -> ( Model, Cmd Msg )
changeRouteTo maybeRoute model =
    let
        session =
            toSession model
    in
        (model, Cmd.none)
                

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IgnoreMsg ->
            ( model
            , Cmd.none
            )

        ChangedUrl url ->
           (model, Cmd.none)

        ClickedLink urlRequest ->
            (model, Cmd.none)

        LocalStorageChangedMsg value ->
            case value of
                Nothing ->
                    ( model, Cmd.none )

                Just str ->
                    case Decode.decodeString (decodeLocalStorage model) str of
                        Ok newModel ->
                            ( modelUpdated newModel model, Cmd.none )

                        Err errS ->
                            Debug.log (Decode.errorToString errS) ( model, Cmd.none )

        GoToProblemMsg i ->
            ( model
            , Route.replaceUrl (Session.navKey (toSession model)) Route.About
                  -- (fullPath ++ "/problem-" ++ String.fromInt (1 + i) ++ "/")
            )

        GoToAboutMsg ->
            ( model
            , Route.replaceUrl (Session.navKey (toSession model)) Route.About
            )

        SolvePageMsg msg1 ->
            case Solve.update (currentProblem model) msg1 (currentSolvePageModel model) of
                Solve.UnchangedOutMsg ->
                    ( model, Cmd.none )

                Solve.ModelChangedOutMsg solvePageModel ->
                    let
                        newModel =
                            setCurrentSolvePageModel solvePageModel model
                    in
                        ( newModel
                        , Cmd.none
                        )

                Solve.ProblemChangedOutMsg problem1 solvePageModel ->
                    let
                        newModel =
                            model
                                |> setCurrentProblem problem1
                                |> setCurrentSolvePageModel solvePageModel
                    in
                        ( newModel
                        , Ports.setLocalStorage (encodeLocalStorage newModel)
                        )

                Solve.SolvedOutMsg steps0 problem0 ->
                    let
                        steps =
                            case problem0.proof of
                                Nothing ->
                                    steps0

                                Just steps1 ->
                                    if List.length steps1 < List.length steps0 then
                                        steps1
                                    else
                                        steps0

                        problem1 =
                            Problem.reset problem0

                        problem2 =
                            { problem1 | proof = Just steps }

                        newModel1 =
                            setCurrentProblem problem2 model

                        index =
                            problemIndex model

                        isInitial =
                            case problem0.proof of
                                Nothing ->
                                    True

                                Just _ ->
                                    False
                    in
                        ( newModel1
                        , Cmd.batch
                            [ Ports.setLocalStorage (encodeLocalStorage newModel1)
                            , Ports.problemSolved { problemIndex = index, isInitial = isInitial, stepCount = List.length steps0 }
                            , Route.replaceUrl (Session.navKey (toSession model)) Route.Root
                            ]
                        )

                Solve.ModelCmdChangedOutMsg solvePageModel cmd ->
                    let
                        newModel =
                            model |> setCurrentSolvePageModel solvePageModel
                    in
                        ( newModel
                        , Cmd.map SolvePageMsg cmd
                        )

                Solve.ExitOutMsg ->
                    ( model, Route.replaceUrl (Session.navKey (toSession model)) Route.Root )

                Solve.ShowProofOutMsg ->
                    ( model, Route.replaceUrl (Session.navKey (toSession model)) <| Route.Solve (problemIndex model))
                          --(fullPath ++ "/problem-" ++ String.fromInt (problemIndex model + 1) ++ "/proof/") )

        GotAboutMsg _ ->
            ( model, Route.replaceUrl (Session.navKey (toSession model)) Route.Root )

        OnScrollMsg ->
            ( model
            , --Task.attempt (SaveScrollMsg >> Result.withDefault IgnoreMsg) (Dom.setViewportOf "scrolling" 0 0)
                Cmd.none
            )

        SaveScrollMsg y ->
            let
                newModel =
                    { model | scrollY = y }
            in
                ( newModel
                , Cmd.none
                )

           
{-| This occurs when localStorage changes.
-}
modelUpdated : Model -> Model -> Model
modelUpdated newModel currentModel =
    case currentModel.status of
        SolvePage i _ ->
            let
                currentPrblem =
                    unsafeGet i currentModel.problems

                newProblem =
                    unsafeGet i newModel.problems
            in
                if currentPrblem == newProblem then
                    newModel
                else
                    { newModel | status = SolvePage i (Solve.init i (toSession currentModel) newProblem) }

        _ ->
            newModel


{-| Updates the model based on the URL path. The isInit flag makes sure it does
not register a page change with GoogleAnalytics on init.
-}


initialModel : Nav.Key -> Model
initialModel key =
    { problems = Problem.init Problems.problems
    , status = Redirect (Session.defaultSession key)
    , scrollY = 0.0
    }


decodeModel : Decode.Value -> Nav.Key -> Model
decodeModel storage key =
    let
        decodeLocal str =
            case Decode.decodeValue (decodeLocalStorage <| initialModel key) str of
                Ok model0 ->
                    model0

                Err errS ->
                    Debug.log (Decode.errorToString errS) <| initialModel key

        model =
            Maybe.withDefault
                -- This occurs during the first run of the site (when there is nothing in localStorage)
                (initialModel key)
                -- This occurs during all other runs
                (Maybe.map decodeLocal <| Just storage)
    in
        model


listMsg : Page.List.Msg -> Msg
listMsg msg =
    case msg of
        Page.List.GoToProblemMsg i ->
            GoToProblemMsg i

        Page.List.GoToAboutMsg ->
            GoToAboutMsg

        Page.List.OnScrollMsg ->
            OnScrollMsg


currentProblem : Model -> Problem
currentProblem model =
    unsafeGet (problemIndex model) model.problems


problemIndex : Model -> Int
problemIndex {status} =
    case status of
        SolvePage i _ ->
            i

        _ ->
            Debug.todo "impossible"


setCurrentProblem : Problem -> Model -> Model
setCurrentProblem problem model =
    { model | problems = set (problemIndex model) problem model.problems }


currentSolvePageModel : Model -> Solve.Model
currentSolvePageModel {status} =
    case status of
        SolvePage i solvePageModel ->
            solvePageModel

        _ ->
            Debug.todo "impossible"


setCurrentSolvePageModel : Solve.Model -> Model -> Model
setCurrentSolvePageModel solvePageModel model =
    case model.status of
        SolvePage i _ ->
            { model | status = SolvePage i solvePageModel }

        _ ->
            Debug.todo "impossible"


encodeLocalStorage : Model -> String
encodeLocalStorage model =
    Encode.encode 4 (localStorageValue model)


localStorageValue : Model -> Encode.Value
localStorageValue model =
    Encode.object
        [ ( "version", Encode.int version )
        , ( "problems",Encode.list identity (List.map Problem.encoder model.problems) )
        ]


decodeLocalStorage : Model -> Decode.Decoder Model
decodeLocalStorage model =
    let
        f version1 ps =
            if version1 == version then
                { model | problems = List.map2 Problem.restoreProblem ps model.problems }
            else
                model
    in
        Decode.succeed f
            |> Pipeline.required "version" Decode.int
            |> Pipeline.required "problems" (Decode.list Problem.localStorageDecoder)


fullPath : String
fullPath =
    ""


mainPath : String
mainPath =
    fullPath ++ "/"


version : Int
version =
    34
