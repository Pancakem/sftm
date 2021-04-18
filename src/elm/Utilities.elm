module Utilities exposing (..)

import Html exposing (Html, Attribute)
import Html.Events exposing (on)
import Html.Attributes exposing (attribute)
import Json.Decode exposing (succeed)
import Task exposing (attempt)
import Browser.Dom as Dom


castHtml : Html Never -> Html a
castHtml =
    Html.map never


get : Int -> List a -> Maybe a
get i xs =
    List.head (List.drop i xs)


unsafeGet : Int -> List a -> a
unsafeGet i xs =
    case List.head (List.drop i xs) of
        Nothing ->
            Debug.todo "impossible"

        Just x ->
            x


set : Int -> a -> List a -> List a
set i x xs =
    List.append (List.take i xs) (x :: List.drop (i + 1) xs)


isNothing : Maybe a -> Bool
isNothing m =
    case m of
        Nothing ->
            True

        Just _ ->
            False


fromJust : Maybe a -> a
fromJust m =
    case m of
        Nothing ->
            Debug.todo "impossible"

        Just x ->
            x


remove : Int -> List a -> List a
remove n xs =
    List.take n xs ++ List.drop (n + 1) xs


onScroll : msg -> Attribute msg
onScroll f =
    on "scroll" (succeed f)


scrollToTopCmd : msg -> Cmd msg
scrollToTopCmd msg =
    Task.perform (\_ -> msg) (Dom.setViewport 0 0)


scrollToBottomCmd : msg -> Cmd msg
scrollToBottomCmd msg =
    Task.perform (\_ -> msg) (Dom.setViewport 0 0) -- should set to bottom of page


unsafeTail : List a -> List a
unsafeTail xs =
    case xs of
        [] ->
            Debug.todo "impossible"

        _ :: r ->
            r


ariaHidden : Bool -> Attribute msg
ariaHidden b =
    if b then
        attribute "aria-hidden" "true"
    else
        attribute "aria-hidden" "false"
