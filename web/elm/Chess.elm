module Chess exposing (..)

import Debug exposing (..)
import Maybe exposing (..)
import Html exposing (..)
import Html.Attributes exposing (attribute, id, class, draggable)
import DragEvents exposing (..)
import Array exposing (..)
import Dict exposing (..)


type Team
    = White
    | Black


type Piece
    = King
    | Queen
    | Rook
    | Knight
    | Bishop
    | Pawn


type TeamPiece
    = TeamPiece Team Piece
    | Empty


type alias Position =
    String


type Msg
    = NewGame
    | Move TeamPiece Position
    | DropOn Position



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
    { board : Dict String TeamPiece
    , movingPiece : Maybe TeamPiece
    , movingFrom : Maybe Position
    }


model : Model
model =
    { movingPiece = Nothing
    , movingFrom = Nothing
    , board =
        Dict.fromList
            [ ( "A1", TeamPiece Black Rook )
            , ( "A2", TeamPiece Black Knight )
            , ( "A3", TeamPiece Black Bishop )
            , ( "A4", TeamPiece Black Queen )
            , ( "A5", TeamPiece Black King )
            , ( "A6", TeamPiece Black Bishop )
            , ( "A7", TeamPiece Black Knight )
            , ( "A8", TeamPiece Black Rook )
            , ( "B1", TeamPiece Black Pawn )
            , ( "B2", TeamPiece Black Pawn )
            , ( "B3", TeamPiece Black Pawn )
            , ( "B4", TeamPiece Black Pawn )
            , ( "B5", TeamPiece Black Pawn )
            , ( "B6", TeamPiece Black Pawn )
            , ( "B7", TeamPiece Black Pawn )
            , ( "B8", TeamPiece Black Pawn )
            , ( "C1", Empty )
            , ( "C2", Empty )
            , ( "C3", Empty )
            , ( "C4", Empty )
            , ( "C5", Empty )
            , ( "C6", Empty )
            , ( "C7", Empty )
            , ( "C8", Empty )
            , ( "D1", Empty )
            , ( "D2", Empty )
            , ( "D3", Empty )
            , ( "D4", Empty )
            , ( "D5", Empty )
            , ( "D6", Empty )
            , ( "D7", Empty )
            , ( "D8", Empty )
            , ( "E1", Empty )
            , ( "E2", Empty )
            , ( "E3", Empty )
            , ( "E4", Empty )
            , ( "E5", Empty )
            , ( "E6", Empty )
            , ( "E7", Empty )
            , ( "E8", Empty )
            , ( "F1", Empty )
            , ( "F2", Empty )
            , ( "F3", Empty )
            , ( "F4", Empty )
            , ( "F5", Empty )
            , ( "F6", Empty )
            , ( "F7", Empty )
            , ( "F8", Empty )
            , ( "G1", TeamPiece White Pawn )
            , ( "G2", TeamPiece White Pawn )
            , ( "G3", TeamPiece White Pawn )
            , ( "G4", TeamPiece White Pawn )
            , ( "G5", TeamPiece White Pawn )
            , ( "G6", TeamPiece White Pawn )
            , ( "G7", TeamPiece White Pawn )
            , ( "G8", TeamPiece White Pawn )
            , ( "H1", TeamPiece White Rook )
            , ( "H2", TeamPiece White Knight )
            , ( "H3", TeamPiece White Bishop )
            , ( "H4", TeamPiece White Queen )
            , ( "H5", TeamPiece White King )
            , ( "H6", TeamPiece White Bishop )
            , ( "H7", TeamPiece White Knight )
            , ( "H8", TeamPiece White Rook )
            ]
    }



-- UPDATE


canMoveTo : Piece -> Position -> Position -> Bool
canMoveTo piece from to =
    let
        fromPositionChars =
            String.toList from |> Array.fromList

        toPositionChars =
            String.toList to |> Array.fromList

        fromRow =
            Array.get 0 fromPositionChars

        fromColumn =
            Array.get 1 fromPositionChars

        toRow =
            Array.get 0 toPositionChars

        toColumn =
            Array.get 1 toPositionChars
    in
        case piece of
            King ->
                False

            Queen ->
                False

            Bishop ->
                False

            Knight ->
                False

            Rook ->
                False

            Pawn ->
                False


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
                case ( currentPiece, model.movingPiece, model.movingFrom ) of
                    ( Just Empty, Just newPiece, Just oldPosition ) ->
                        { model
                            | board =
                                model.board
                                    |> (Dict.insert position newPiece)
                                    |> (Dict.insert oldPosition Empty)
                            , movingPiece = Nothing
                        }

                    _ ->
                        { model
                            | movingPiece = Nothing
                        }



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
