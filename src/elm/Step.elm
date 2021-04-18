module Step exposing (..)

import Term exposing (Term)
import Json.Encode exposing (Value)
import Json.Decode exposing (succeed, Decoder)
import Json.Decode.Pipeline as Pipeline


type alias Step =
    { rule : Int
    , isReversed : Bool
    , term : Term
    }


encoder : Step -> Value
encoder x =
    Json.Encode.object
        [ ( "rule", Json.Encode.int x.rule )
        , ( "isReversed", Json.Encode.bool x.isReversed )
        , ( "term", Term.encoder x.term )
        ]


decoder : Decoder Step
decoder =
    succeed Step
        |> Pipeline.required "rule" Json.Decode.int
        |> Pipeline.required "isReversed" Json.Decode.bool
        |> Pipeline.required "term" Term.decoder
