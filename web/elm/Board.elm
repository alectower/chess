module Board exposing (..)

import Dict exposing (Dict)


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
    ( Int, Int )


type alias Board =
    Dict String TeamPiece


init : Board
init =
    Dict.fromList
        [ ( "11", TeamPiece Black Rook )
        , ( "12", TeamPiece Black Knight )
        , ( "13", TeamPiece Black Bishop )
        , ( "14", TeamPiece Black Queen )
        , ( "15", TeamPiece Black King )
        , ( "16", TeamPiece Black Bishop )
        , ( "17", TeamPiece Black Knight )
        , ( "18", TeamPiece Black Rook )
        , ( "21", TeamPiece Black Pawn )
        , ( "22", TeamPiece Black Pawn )
        , ( "23", TeamPiece Black Pawn )
        , ( "24", TeamPiece Black Pawn )
        , ( "25", TeamPiece Black Pawn )
        , ( "26", TeamPiece Black Pawn )
        , ( "27", TeamPiece Black Pawn )
        , ( "28", TeamPiece Black Pawn )
        , ( "31", Empty )
        , ( "32", Empty )
        , ( "33", Empty )
        , ( "34", Empty )
        , ( "35", Empty )
        , ( "36", Empty )
        , ( "37", Empty )
        , ( "38", Empty )
        , ( "41", Empty )
        , ( "42", Empty )
        , ( "43", Empty )
        , ( "44", Empty )
        , ( "45", Empty )
        , ( "46", Empty )
        , ( "47", Empty )
        , ( "48", Empty )
        , ( "51", Empty )
        , ( "52", Empty )
        , ( "53", Empty )
        , ( "54", Empty )
        , ( "55", Empty )
        , ( "56", Empty )
        , ( "57", Empty )
        , ( "58", Empty )
        , ( "61", Empty )
        , ( "62", Empty )
        , ( "63", Empty )
        , ( "64", Empty )
        , ( "65", Empty )
        , ( "66", Empty )
        , ( "67", Empty )
        , ( "68", Empty )
        , ( "71", TeamPiece White Pawn )
        , ( "72", TeamPiece White Pawn )
        , ( "73", TeamPiece White Pawn )
        , ( "74", TeamPiece White Pawn )
        , ( "75", TeamPiece White Pawn )
        , ( "76", TeamPiece White Pawn )
        , ( "77", TeamPiece White Pawn )
        , ( "78", TeamPiece White Pawn )
        , ( "81", TeamPiece White Rook )
        , ( "82", TeamPiece White Knight )
        , ( "83", TeamPiece White Bishop )
        , ( "84", TeamPiece White Queen )
        , ( "85", TeamPiece White King )
        , ( "86", TeamPiece White Bishop )
        , ( "87", TeamPiece White Knight )
        , ( "88", TeamPiece White Rook )
        ]


canMoveTo : TeamPiece -> Position -> Position -> Bool
canMoveTo piece from to =
    let
        fromRow =
            Tuple.first from

        toRow =
            Tuple.first to

        fromCol =
            Tuple.second from

        toCol =
            Tuple.second to

        rowDelta =
            abs (toRow - fromRow)

        colDelta =
            abs (toCol - fromCol)
    in
        case piece of
            TeamPiece _ King ->
                if (rowDelta == 1 || rowDelta == 0) && (colDelta == 1 || colDelta == 0) then
                    True
                else
                    False

            TeamPiece _ Queen ->
                if (rowDelta == colDelta) || (rowDelta == 0 || colDelta == 0) then
                    True
                else
                    False

            TeamPiece _ Bishop ->
                if (rowDelta == colDelta) then
                    True
                else
                    False

            TeamPiece _ Knight ->
                if (rowDelta == 1 && colDelta == 2) || (rowDelta == 2 && colDelta == 1) then
                    True
                else
                    False

            TeamPiece _ Rook ->
                if rowDelta == 0 || colDelta == 0 then
                    True
                else
                    False

            TeamPiece team Pawn ->
                if
                    rowDelta
                        == 1
                        && colDelta
                        == 0
                        || (team == White && fromRow == 7 && rowDelta == 2 && colDelta == 0)
                        || (team == Black && fromRow == 2 && rowDelta == 2 && colDelta == 0)
                then
                    True
                else
                    False

            Empty ->
                False
