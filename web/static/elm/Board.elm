module Board exposing (..)

import Dict exposing (Dict)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)
import Array exposing (get)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)


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


encodePiece : Maybe TeamPiece -> Encode.Value
encodePiece piece =
    case piece of
        Nothing ->
            Encode.string ""

        Just (TeamPiece a b) ->
            Encode.string ((toString a) ++ " " ++ (toString b))


encodeBoard : Board -> Encode.Value
encodeBoard board =
    board
        |> Dict.toList
        |> List.map
            (\k ->
                ( Tuple.first k
                , encodePiece (Tuple.second k)
                )
            )
        |> Encode.object


encodeMoves : List String -> Encode.Value
encodeMoves moves =
    moves
        |> List.map (\m -> Encode.string m)
        |> Encode.list


decodeStringToPiece : String -> Maybe TeamPiece
decodeStringToPiece pieceString =
    case pieceString of
        "" ->
            Nothing

        a ->
            let
                teamPiece =
                    String.split " " a |> Array.fromList

                teamString =
                    Array.get 0 teamPiece

                pieceString =
                    Array.get 1 teamPiece

                team =
                    if teamString == Just "White" then
                        White
                    else
                        Black
            in
                case pieceString of
                    Just "Pawn" ->
                        Just (TeamPiece team Pawn)

                    Just "Rook" ->
                        Just (TeamPiece team Rook)

                    Just "Knight" ->
                        Just (TeamPiece team Knight)

                    Just "Bishop" ->
                        Just (TeamPiece team Bishop)

                    Just "Queen" ->
                        Just (TeamPiece team Queen)

                    Just "King" ->
                        Just (TeamPiece team King)

                    _ ->
                        Nothing


decodeTurn : Result error String -> Team
decodeTurn result =
    case result of
        Ok a ->
            if a == "White" then
                White
            else
                Black

        Err a ->
            White


decodeBoard : Result error String -> Board
decodeBoard result =
    case result of
        Ok a ->
            let
                board =
                    (Decode.decodeString (Decode.keyValuePairs Decode.string) a)

                boardWithPieces =
                    case board of
                        Ok a ->
                            List.map (\p -> ( Tuple.first p, (decodeStringToPiece (Tuple.second p)) )) a

                        Err a ->
                            []
            in
                Dict.fromList boardWithPieces

        Err a ->
            Dict.fromList []


decodeMoves : Result error String -> List String
decodeMoves result =
    case result of
        Ok a ->
            let
                moves =
                    (Decode.decodeString (Decode.list Decode.string) a)

                movesResult =
                    case moves of
                        Ok a ->
                            a

                        Err a ->
                            []
            in
                movesResult

        Err a ->
            []


init : Board
init =
    Dict.fromList
        [ ( "81", Just (TeamPiece Black Rook) )
        , ( "82", Just (TeamPiece Black Knight) )
        , ( "83", Just (TeamPiece Black Bishop) )
        , ( "84", Just (TeamPiece Black Queen) )
        , ( "85", Just (TeamPiece Black King) )
        , ( "86", Just (TeamPiece Black Bishop) )
        , ( "87", Just (TeamPiece Black Knight) )
        , ( "88", Just (TeamPiece Black Rook) )
        , ( "71", Just (TeamPiece Black Pawn) )
        , ( "72", Just (TeamPiece Black Pawn) )
        , ( "73", Just (TeamPiece Black Pawn) )
        , ( "74", Just (TeamPiece Black Pawn) )
        , ( "75", Just (TeamPiece Black Pawn) )
        , ( "76", Just (TeamPiece Black Pawn) )
        , ( "77", Just (TeamPiece Black Pawn) )
        , ( "78", Just (TeamPiece Black Pawn) )
        , ( "61", Nothing )
        , ( "62", Nothing )
        , ( "63", Nothing )
        , ( "64", Nothing )
        , ( "65", Nothing )
        , ( "66", Nothing )
        , ( "67", Nothing )
        , ( "68", Nothing )
        , ( "51", Nothing )
        , ( "52", Nothing )
        , ( "53", Nothing )
        , ( "54", Nothing )
        , ( "55", Nothing )
        , ( "56", Nothing )
        , ( "57", Nothing )
        , ( "58", Nothing )
        , ( "41", Nothing )
        , ( "42", Nothing )
        , ( "43", Nothing )
        , ( "44", Nothing )
        , ( "45", Nothing )
        , ( "46", Nothing )
        , ( "47", Nothing )
        , ( "48", Nothing )
        , ( "31", Nothing )
        , ( "32", Nothing )
        , ( "33", Nothing )
        , ( "34", Nothing )
        , ( "35", Nothing )
        , ( "36", Nothing )
        , ( "37", Nothing )
        , ( "38", Nothing )
        , ( "21", Just (TeamPiece White Pawn) )
        , ( "22", Just (TeamPiece White Pawn) )
        , ( "23", Just (TeamPiece White Pawn) )
        , ( "24", Just (TeamPiece White Pawn) )
        , ( "25", Just (TeamPiece White Pawn) )
        , ( "26", Just (TeamPiece White Pawn) )
        , ( "27", Just (TeamPiece White Pawn) )
        , ( "28", Just (TeamPiece White Pawn) )
        , ( "11", Just (TeamPiece White Rook) )
        , ( "12", Just (TeamPiece White Knight) )
        , ( "13", Just (TeamPiece White Bishop) )
        , ( "14", Just (TeamPiece White Queen) )
        , ( "15", Just (TeamPiece White King) )
        , ( "16", Just (TeamPiece White Bishop) )
        , ( "17", Just (TeamPiece White Knight) )
        , ( "18", Just (TeamPiece White Rook) )
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
                let
                    forwardMove =
                        (fromRow > toRow && team == Black) || (fromRow < toRow && team == White)

                    singleStep =
                        rowDelta == 1 && colDelta == 0 && currentPiece == Nothing

                    firstMoveWhite =
                        team == White && fromRow == 2 && rowDelta == 2 && colDelta == 0

                    firstMoveBlack =
                        team == Black && fromRow == 7 && rowDelta == 2 && colDelta == 0

                    attack =
                        rowDelta == 1 && colDelta == 1 && onOppositeTeams && currentPiece /= Nothing
                in
                    if forwardMove && (singleStep || firstMoveWhite || firstMoveBlack || attack) then
                        True
                    else
                        False
