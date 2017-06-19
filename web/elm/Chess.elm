module Chess exposing (..)

import Debug exposing (..)
import Maybe exposing (..)
import Html exposing (..)
import Html.Attributes exposing (attribute, id, class, draggable)
import DragEvents exposing (..)
import Array exposing (..)
import Dict exposing (..)
import Char exposing (toCode)
import Board exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)
import Phoenix
import Phoenix.Socket as Socket
import Phoenix.Channel as Channel
import Phoenix.Push as Push


-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { board : Board
    , movingPiece : Maybe TeamPiece
    , movingFrom : Maybe String
    , socketUrl : String
    }


init : ( Model, Cmd Msg )
init =
    ( model, Cmd.none )


model : Model
model =
    { movingPiece = Nothing
    , movingFrom = Nothing
    , board = Board.init
    , socketUrl = "ws://localhost:4001/socket/websocket"
    }



-- UPDATE


charToNum : Maybe Char -> Int
charToNum num =
    case num of
        Just num ->
            num
                |> String.fromChar
                |> String.toInt
                |> Result.toMaybe
                |> Maybe.withDefault -1

        Nothing ->
            -1


convertPosition : String -> ( Int, Int )
convertPosition position =
    let
        positionChars =
            String.toList position |> Array.fromList
    in
        ( charToNum (Array.get 0 positionChars)
        , charToNum (Array.get 1 positionChars)
        )


type Msg
    = NewGame
    | Move TeamPiece String
    | DropOn String
    | UpdateBoard Encode.Value


updateModel : List ( String, TeamPiece ) -> Model -> Model
updateModel pieces model =
    List.foldl
        (\a b ->
            { b
                | board =
                    b.board
                        |> Dict.insert (Tuple.first a) (Tuple.second a)
            }
        )
        model
        pieces


movePiece : String -> TeamPiece -> Model -> Model
movePiece position currentPiece model =
    let
        sameBoard =
            { model
                | movingPiece = Nothing
                , movingFrom = Nothing
            }
    in
        case ( currentPiece, model.movingPiece, model.movingFrom ) of
            ( Empty, Just newPiece, Just oldPosition ) ->
                let
                    canMove =
                        Board.canMoveTo newPiece (convertPosition oldPosition) Empty (convertPosition position)
                in
                    if canMove then
                        updateModel [ ( position, newPiece ), ( oldPosition, Empty ) ] model
                    else
                        sameBoard

            ( currentPiece, Just newPiece, Just oldPosition ) ->
                let
                    canMove =
                        Board.canMoveTo newPiece (convertPosition oldPosition) currentPiece (convertPosition position)
                in
                    if canMove then
                        updateModel [ ( position, newPiece ), ( oldPosition, Empty ) ] model
                    else
                        sameBoard

            _ ->
                sameBoard


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, Cmd.none )

        Move teamPiece position ->
            ( { model
                | movingPiece = Just teamPiece
                , movingFrom = Just position
              }
            , Cmd.none
            )

        DropOn position ->
            let
                currentPiece =
                    Dict.get position model.board

                payload =
                    Encode.object []

                message =
                    Push.init "game" "update_board" |> Push.withPayload payload

                logMessage =
                    Debug.log "message" message
            in
                case currentPiece of
                    Just piece ->
                        movePiece position piece model ! [ Phoenix.push model.socketUrl message ]

                    Nothing ->
                        movePiece position Empty model ! [ Phoenix.push model.socketUrl message ]

        UpdateBoard raw ->
            let
                logRaw =
                    Debug.log "board" raw
            in
                case raw of
                    _ ->
                        ( model, Cmd.none )


encodeSocketMessage topic event payload ref =
    Encode.object
        [ ( "topic", Encode.string topic )
        , ( "event", Encode.string event )
        , ( "payload", Encode.string payload )
        , ( "ref", Encode.string ref )
        ]
        |> Encode.encode 0



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        channel =
            Channel.init "game" |> Channel.on "update_board" UpdateBoard
    in
        Phoenix.connect (Socket.init model.socketUrl) <| [ channel ]



-- VIEW


view : Model -> Html Msg
view model =
    let
        boardPieces =
            List.sortBy Tuple.first (Dict.toList model.board)
                |> Array.fromList

        endRowNumbers =
            Array.initialize 8 (\n -> (n + 1) * 8)
                |> Array.toList

        rowSlice n =
            Array.slice (n - 8) n boardPieces |> Array.toList
    in
        div [ class "board" ]
            (List.map rowSlice endRowNumbers |> List.map placeRow)


pieceDisplay teampiece =
    case teampiece of
        TeamPiece White King ->
            "♔"

        TeamPiece White Queen ->
            "♕"

        TeamPiece White Rook ->
            "♖"

        TeamPiece White Bishop ->
            "♗"

        TeamPiece White Knight ->
            "♘"

        TeamPiece White Pawn ->
            "♙"

        TeamPiece Black King ->
            "♚"

        TeamPiece Black Queen ->
            "♛"

        TeamPiece Black Rook ->
            "♜"

        TeamPiece Black Bishop ->
            "♝"

        TeamPiece Black Knight ->
            "♞"

        TeamPiece Black Pawn ->
            "♟"

        Empty ->
            ""


placePiece ( position, piece ) =
    div
        [ class "square"
        , id position
        , attribute "ondragover" "return false"
        , onDrop <| DropOn position
        ]
        [ div
            [ class "piece"
            , draggable "true"
            , onDragStart <| Move piece position
            ]
            [ text (pieceDisplay piece) ]
        ]


placeRow pieces =
    div [ class "row" ]
        (List.map placePiece pieces)
