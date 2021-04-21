module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Navigation
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Random
import Url
import Url.Parser


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


type alias Model =
    { depth : Int
    , key : Navigation.Key
    }


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { depth = url.fragment |> Maybe.andThen String.toInt |> Maybe.withDefault 12
      , key = key
      }
    , Cmd.none
    )


type Msg
    = Increment
    | Decrement
    | ClickedRandom
    | NewDepth Int
    | UrlChanged Url.Url
    | UrlRequested UrlRequest


roll : Random.Generator Int
roll =
    Random.int 0 24


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model
            , Navigation.pushUrl model.key ("#" ++ String.fromInt (model.depth + 1))
            )

        Decrement ->
            ( model
            , Navigation.pushUrl model.key ("#" ++ String.fromInt (model.depth - 1))
            )

        ClickedRandom ->
            ( model, Random.generate NewDepth roll )

        NewDepth d ->
            ( model
            , Navigation.pushUrl model.key ("#" ++ String.fromInt d)
            )

        UrlChanged url ->
            ( { model | depth = url.fragment |> Maybe.andThen String.toInt |> Maybe.withDefault 12 }, Cmd.none )

        UrlRequested request ->
            case request of
                External url ->
                    ( model, Navigation.load url )

                Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )


border =
    style "border" "1px solid black"


height h =
    style "height" (String.fromInt (h - 2) ++ "px")


width w =
    style "width" (String.fromInt (w - 2) ++ "px")


block w h =
    div
        [ border
        , width w
        , height h
        , style "display" "flex"
        , style "flex" "0 0 auto"
        , style "flex-flow" "row wrap"
        , style "box-sizing" "border-box"
        ]


fibonacci depth w h isVerticalSplit =
    if depth <= 0 then
        block w h []

    else
        let
            wNew =
                if isVerticalSplit then
                    w // 2

                else
                    w

            hNew =
                if isVerticalSplit then
                    h

                else
                    h // 2
        in
        block w
            h
            ((if modBy 4 depth <= 1 then
                identity

              else
                List.reverse
             )
             <|
                [ block wNew hNew []
                , fibonacci (depth - 1) wNew hNew (not isVerticalSplit)
                ]
            )


view : Model -> Document Msg
view model =
    { title = "Fibonacci Layout Hannover"
    , body =
        [ fibonacci model.depth 512 512 True
        , button [ onClick Decrement ] [ text "-" ]
        , button [ onClick Increment ] [ text "+" ]
        , button [ onClick ClickedRandom ] [ text "random" ]
        ]
    }
