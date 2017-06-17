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
                case currentPiece of
                    Just piece ->
                        movePiece position piece model

                    Nothing ->
                        movePiece position Empty model



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
