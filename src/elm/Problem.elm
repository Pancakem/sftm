module Problem exposing (..)

import Html exposing (Html)
import Json.Decode as Decode
import Json.Decode.Pipeline as Pipeline
import Json.Encode as Encode
import Path exposing (Path)
import Rule exposing (Rule)
import Step exposing (Step)
import Symbol exposing (Symbol)
import Term exposing (..)


{-| Problem descriptions use this record.
-}
type alias StaticProblem =
    { description : String
    , notes : List (Html Never)
    , start : Term
    , finish : Term
    , rules : List Rule
    , scratch : List Symbol
    }


{-| A problem as stored in the problem list.
-}
type alias Problem =
    { description : String
    , notes : List (Html Never)
    , rules : List Rule
    , scratch : List Term
    , scratchSymbols : List Symbol
    , solved : Bool
    , start : Term
    , finish : Term
    , history : List Step
    , future : List Step
    , proof : Maybe (List Step)
    }


{-| This information is saved to local storage.
-}
type alias LocalStorageProblem =
    { scratch : List Term
    , solved : Bool
    , history : List Step
    , future : List Step
    , proof : Maybe (List Step)
    }


init : List StaticProblem -> List Problem
init =
    List.map initProblem


initProblem : StaticProblem -> Problem
initProblem problemStatic =
    { description = problemStatic.description
    , notes = problemStatic.notes
    , rules = problemStatic.rules
    , scratchSymbols = problemStatic.scratch
    , scratch = []
    , solved = False
    , start = problemStatic.start
    , finish = problemStatic.finish
    , history = []
    , future = []
    , proof = Nothing
    }


pathEncoder : {} -> Encode.Value
pathEncoder _ =
    Encode.object []


panelEncoder : {} -> Encode.Value
panelEncoder _ =
    Encode.object []


encoder : Problem -> Encode.Value
encoder problem =
    localStorageEncoder (saveProblem problem)


localStorageEncoder : LocalStorageProblem -> Encode.Value
localStorageEncoder x =
    Encode.object
        [ ( "scratch", Encode.list identity (List.map Term.encoder x.scratch) )
        , ( "solved", Encode.bool x.solved )
        , ( "history", Encode.list identity (List.map Step.encoder x.history) )
        , ( "future", Encode.list identity (List.map Step.encoder x.future) )
        , ( "proof", Maybe.withDefault Encode.null (Maybe.map (Encode.list identity << List.map Step.encoder) x.proof) )
        ]


localStorageDecoder : Decode.Decoder LocalStorageProblem
localStorageDecoder =
    Decode.succeed LocalStorageProblem
        |> Pipeline.required "scratch" (Decode.list Term.decoder)
        |> Pipeline.required "solved" Decode.bool
        |> Pipeline.required "history" (Decode.list Step.decoder)
        |> Pipeline.required "future" (Decode.list Step.decoder)
        |> Pipeline.required "proof" (Decode.nullable (Decode.list Step.decoder))


saveProblem : Problem -> LocalStorageProblem
saveProblem x =
    { scratch = x.scratch
    , solved = x.solved
    , history = x.history
    , future = x.future
    , proof = x.proof
    }


restoreProblem : LocalStorageProblem -> Problem -> Problem
restoreProblem x p =
    { p
        | scratch = x.scratch
        , solved = x.solved
        , history = x.history
        , future = x.future
        , proof = x.proof
    }


reset : Problem -> Problem
reset problem =
    { problem
        | scratch = []
        , history = []
        , future = []
    }
