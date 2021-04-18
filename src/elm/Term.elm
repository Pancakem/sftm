module Term exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode


type Term
    = Atom Atom
    | Unary Unary Term
    | Binary Binary Term Term


type Binary
    = Conjunction
    | Disjunction
    | Implication
    | Equivalence


type Unary
    = Not


type Atom
    = Top
    | Bot
    | VarA
    | VarB
    | VarC
    | MetaVar MetaVar


type MetaVar
    = MetaA
    | MetaB
    | MetaC


encoder : Term -> Encode.Value
encoder term =
    case term of
        Atom Top ->
            Encode.string "top"

        Atom Bot ->
            Encode.string "bot"

        Atom VarA ->
            Encode.string "var-a"

        Atom VarB ->
            Encode.string "var-b"

        Atom VarC ->
            Encode.string "var-c"

        Atom (MetaVar MetaA) ->
            Encode.string "meta-a"

        Atom (MetaVar MetaB) ->
            Encode.string "meta-b"

        Atom (MetaVar MetaC) ->
            Encode.string "meta-c"

        Unary Not t ->
            Encode.list identity [ Encode.string "not", encoder t ]

        Binary Conjunction t1 t2 ->
            Encode.list identity [ Encode.string "and", encoder t1, encoder t2 ]

        Binary Disjunction t1 t2 ->
            Encode.list identity [ Encode.string "or", encoder t1, encoder t2 ]

        Binary Implication t1 t2 ->
            Encode.list identity [ Encode.string "implies", encoder t1, encoder t2 ]

        Binary Equivalence t1 t2 ->
            Encode.list identity [ Encode.string "equivalent", encoder t1, encoder t2 ]


decoder : Decode.Decoder Term
decoder =
    Decode.oneOf
        [ Decode.string
            |> Decode.andThen
                (\s ->
                    case s of
                        "top" ->
                            Decode.succeed (Atom Top)

                        "bot" ->
                            Decode.succeed (Atom Bot)

                        "var-a" ->
                            Decode.succeed (Atom VarA)

                        "var-b" ->
                            Decode.succeed (Atom VarB)

                        "var-c" ->
                            Decode.succeed (Atom VarC)

                        "meta-a" ->
                            Decode.succeed (Atom (MetaVar MetaA))

                        "meta-b" ->
                            Decode.succeed (Atom (MetaVar MetaB))

                        "meta-c" ->
                            Decode.succeed (Atom (MetaVar MetaC))

                        _ ->
                            Decode.fail "term"
                )
        , Decode.index 0 Decode.string
            |> Decode.andThen
                (\s ->
                    case s of
                        "not" ->
                            Decode.map (Unary Not) (Decode.index 1 decoder)

                        "and" ->
                            Decode.map2 (Binary Conjunction) (Decode.index 1 decoder) (Decode.index 2 decoder)

                        "or" ->
                            Decode.map2 (Binary Disjunction) (Decode.index 1 decoder) (Decode.index 2 decoder)

                        "implies" ->
                            Decode.map2 (Binary Implication) (Decode.index 1 decoder) (Decode.index 2 decoder)

                        "equivalent" ->
                            Decode.map2 (Binary Equivalence) (Decode.index 1 decoder) (Decode.index 2 decoder)

                        _ ->
                            Decode.fail "term"
                )
        ]


{-| Makes sure every metavariable in the term is in the dictionary, adding an
empty entry if needed.
-}
addMetavariables : Term -> List ( MetaVar, List Term ) -> List ( MetaVar, List Term )
addMetavariables term dict =
    let
        f a1 d =
            case d of
                [] ->
                    [ ( a1, [] ) ]

                ( a2, xs ) :: ds ->
                    if a1 == a2 then
                        ( a2, xs ) :: ds
                    else
                        ( a2, xs ) :: f a1 ds
    in
        case term of
            Atom (MetaVar m) ->
                f m dict

            Atom _ ->
                dict

            Unary _ t ->
                addMetavariables t dict

            Binary _ t1 t2 ->
                addMetavariables t2 (addMetavariables t1 dict)


occupy : List ( MetaVar, List Term ) -> List ( MetaVar, List Term )
occupy =
    List.map occupyEntry


occupyEntry : ( MetaVar, List Term ) -> ( MetaVar, List Term )
occupyEntry ( m, ts ) =
    case ts of
        [] ->
            ( m, [ Atom (MetaVar m) ] )

        _ :: _ ->
            ( m, ts )


tex : Term -> String
tex =
    reallyTex 0


reallyTex : Int -> Term -> String
reallyTex p term =
    case term of
        Atom Top ->
            "\\top"

        Atom Bot ->
            "\\bot"

        Atom VarA ->
            "A"

        Atom VarB ->
            "B"

        Atom VarC ->
            "C"

        Atom (MetaVar MetaA) ->
            "\\frak{A}"

        Atom (MetaVar MetaB) ->
            "\\frak{B}"

        Atom (MetaVar MetaC) ->
            "\\frak{C}"

        Unary Not t ->
            parens 5 p <| "\\lnot " ++ reallyTex 5 t

        Binary Conjunction t1 t2 ->
            parens 4 p <| reallyTex 4 t1 ++ " \\land " ++ reallyTex 4 t2

        Binary Disjunction t1 t2 ->
            parens 3 p <| reallyTex 3 t1 ++ " \\lor " ++ reallyTex 3 t2

        Binary Implication t1 t2 ->
            parens 2 p <| reallyTex 2 t1 ++ " \\to " ++ reallyTex 2 t2

        Binary Equivalence t1 t2 ->
            parens 1 p <| reallyTex 1 t1 ++ " \\leftrightarrow " ++ reallyTex 1 t2


parens : Int -> Int -> String -> String
parens i j s =
    if 0 < j then
        "(" ++ s ++ ")"
    else
        s
