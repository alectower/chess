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


type alias Position =
    ( Int, Int )


type alias Board =
    Dict String (Maybe TeamPiece)


init : Board
init =
    Dict.fromList
        [ ( "11", Just (TeamPiece Black Rook) )
        , ( "12", Just (TeamPiece Black Knight) )
        , ( "13", Just (TeamPiece Black Bishop) )
        , ( "14", Just (TeamPiece Black Queen) )
        , ( "15", Just (TeamPiece Black King) )
        , ( "16", Just (TeamPiece Black Bishop) )
        , ( "17", Just (TeamPiece Black Knight) )
        , ( "18", Just (TeamPiece Black Rook) )
        , ( "21", Just (TeamPiece Black Pawn) )
        , ( "22", Just (TeamPiece Black Pawn) )
        , ( "23", Just (TeamPiece Black Pawn) )
        , ( "24", Just (TeamPiece Black Pawn) )
        , ( "25", Just (TeamPiece Black Pawn) )
        , ( "26", Just (TeamPiece Black Pawn) )
        , ( "27", Just (TeamPiece Black Pawn) )
        , ( "28", Just (TeamPiece Black Pawn) )
        , ( "31", Nothing )
        , ( "32", Nothing )
        , ( "33", Nothing )
        , ( "34", Nothing )
        , ( "35", Nothing )
        , ( "36", Nothing )
        , ( "37", Nothing )
        , ( "38", Nothing )
        , ( "41", Nothing )
        , ( "42", Nothing )
        , ( "43", Nothing )
        , ( "44", Nothing )
        , ( "45", Nothing )
        , ( "46", Nothing )
        , ( "47", Nothing )
        , ( "48", Nothing )
        , ( "51", Nothing )
        , ( "52", Nothing )
        , ( "53", Nothing )
        , ( "54", Nothing )
        , ( "55", Nothing )
        , ( "56", Nothing )
        , ( "57", Nothing )
        , ( "58", Nothing )
        , ( "61", Nothing )
        , ( "62", Nothing )
        , ( "63", Nothing )
        , ( "64", Nothing )
        , ( "65", Nothing )
        , ( "66", Nothing )
        , ( "67", Nothing )
        , ( "68", Nothing )
        , ( "71", Just (TeamPiece White Pawn) )
        , ( "72", Just (TeamPiece White Pawn) )
        , ( "73", Just (TeamPiece White Pawn) )
        , ( "74", Just (TeamPiece White Pawn) )
        , ( "75", Just (TeamPiece White Pawn) )
        , ( "76", Just (TeamPiece White Pawn) )
        , ( "77", Just (TeamPiece White Pawn) )
        , ( "78", Just (TeamPiece White Pawn) )
        , ( "81", Just (TeamPiece White Rook) )
        , ( "82", Just (TeamPiece White Knight) )
        , ( "83", Just (TeamPiece White Bishop) )
        , ( "84", Just (TeamPiece White Queen) )
        , ( "85", Just (TeamPiece White King) )
        , ( "86", Just (TeamPiece White Bishop) )
        , ( "87", Just (TeamPiece White Knight) )
        , ( "88", Just (TeamPiece White Rook) )
        ]


oppositeTeams : Maybe TeamPiece -> TeamPiece -> Bool
oppositeTeams teamOne teamTwo =
    case teamOne of
        Just (TeamPiece White _) ->
            case teamTwo of
                TeamPiece White _ ->
                    False

                TeamPiece Black _ ->
                    True

        Just (TeamPiece Black _) ->
            case teamTwo of
                TeamPiece White _ ->
                    True

                TeamPiece Black _ ->
                    False

        Nothing ->
            True


canMoveTo : TeamPiece -> Position -> Maybe TeamPiece -> Position -> Bool
canMoveTo movingPiece from currentPiece to =
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

        onOppositeTeams =
            oppositeTeams currentPiece movingPiece
    in
        case movingPiece of
            TeamPiece _ King ->
                if
                    (rowDelta == 1 || rowDelta == 0)
                        && (colDelta == 1 || colDelta == 0)
                        && onOppositeTeams
                then
                    True
                else
                    False

            TeamPiece _ Queen ->
                if
                    ((rowDelta == colDelta && from /= to)
                        || (rowDelta == 0 || colDelta == 0)
                    )
                        && onOppositeTeams
                then
                    True
                else
                    False

            TeamPiece _ Bishop ->
                if (rowDelta == colDelta) && onOppositeTeams then
                    True
                else
                    False

            TeamPiece _ Knight ->
                if
                    ((rowDelta == 1 && colDelta == 2)
                        || (rowDelta == 2 && colDelta == 1)
                    )
                        && onOppositeTeams
                then
                    True
                else
                    False

            TeamPiece _ Rook ->
                if (rowDelta == 0 || colDelta == 0) && onOppositeTeams then
                    True
                else
                    False

            TeamPiece team Pawn ->
                if
                    ((fromRow < toRow && team == Black) || (fromRow > toRow && team == White))
                        && (rowDelta == 1 && colDelta == 0 && currentPiece == Nothing)
                        || (team == White && fromRow == 7 && rowDelta == 2 && colDelta == 0)
                        || (team == Black && fromRow == 2 && rowDelta == 2 && colDelta == 0)
                        || (rowDelta == 1 && colDelta == 1 && onOppositeTeams && currentPiece /= Nothing)
                then
                    True
                else
                    False
