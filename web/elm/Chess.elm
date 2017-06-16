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


-- MAIN


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



-- MODEL


type alias Model =
    { board : Board
    , movingPiece : Maybe TeamPiece
    , movingFrom : Maybe String
    }


model : Model
model =
    { movingPiece = Nothing
    , movingFrom = Nothing
    , board = Board.init
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


movePiece position currentPiece model =
    case ( currentPiece, model.movingPiece, model.movingFrom ) of
        ( Just Empty, Just newPiece, Just oldPosition ) ->
            if Board.canMoveTo newPiece (convertPosition oldPosition) (convertPosition position) then
                { model
                    | board =
                        model.board
                            |> (Dict.insert position newPiece)
                            |> (Dict.insert oldPosition Empty)
                    , movingPiece = Nothing
                    , movingFrom = Nothing
                }
            else
                { model
                    | movingPiece = Nothing
                    , movingFrom = Nothing
                }

        _ ->
            { model
                | movingPiece = Nothing
                , movingFrom = Nothing
            }


update : Msg -> Model -> Model
update msg model =
    case msg of
        NewGame ->
            model

        Move teamPiece position ->
            { model
                | movingPiece = Just teamPiece
                , movingFrom = Just position
            }

        DropOn position ->
            let
                currentPiece =
                    Dict.get position model.board
            in
                movePiece position currentPiece model



-- VIEW


view : Model -> Html Msg
view model =
    let
        boardPieces =
            List.sortBy Tuple.first (Dict.toList model.board)
                |> Array.fromList

        rows =
            Array.initialize 8 (\n -> (n + 1) * 8)
                |> Array.toList
    in
        div [ class "board" ]
            (List.map
                placeRow
                (List.map
                    (\a ->
                        (Array.slice (a - 8) a boardPieces)
                            |> Array.toList
                    )
                    rows
                )
            )


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
