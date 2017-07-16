port module Chess exposing (..)

--import Debug exposing (..)

import Maybe exposing (..)
import Html exposing (..)
import Html.Attributes exposing (attribute, id, class, draggable)
import DragEvents exposing (..)
import Array exposing (..)
import Dict exposing (..)
import Board exposing (..)
import Json.Encode as Encode exposing (..)
import Json.Decode as Decode exposing (..)
import Phoenix
import Phoenix.Socket as Socket
import Phoenix.Channel as Channel
import Phoenix.Push as Push


-- PORTS


port updateGameId : String -> Cmd msg



-- MAIN


main : Program Flags Model Msg
main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { board : Board
    , moves : List String
    , movingPiece : Maybe TeamPiece
    , movingFrom : Maybe String
    , teamTurn : Team
    , socketUrl : String
    , gameId : String
    }


type alias Flags =
    { gameId : String, host : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { model
        | gameId = flags.gameId
        , socketUrl = flags.host ++ "/socket/websocket"
      }
    , Cmd.none
    )


model : Model
model =
    { board = Board.init
    , moves = []
    , movingPiece = Nothing
    , movingFrom = Nothing
    , teamTurn = White
    , socketUrl = ""
    , gameId = ""
    }



-- UPDATE


type Msg
    = NewGame
    | Move (Maybe TeamPiece) String
    | DropOn String
    | UpdateBoard Encode.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( model, Cmd.none )

        Move teamPiece position ->
            ( { model
                | movingPiece = teamPiece
                , movingFrom = Just position
              }
            , Cmd.none
            )

        DropOn position ->
            let
                currentPiece =
                    Dict.get position model.board
            in
                case currentPiece of
                    Just piece ->
                        let
                            newModel =
                                movePiece position piece model
                        in
                            newModel ! [ Phoenix.push model.socketUrl (newModelPayload newModel) ]

                    Nothing ->
                        let
                            newModel =
                                movePiece position Nothing model
                        in
                            newModel ! [ Phoenix.push model.socketUrl (newModelPayload newModel) ]

        UpdateBoard raw ->
            let
                respGameId =
                    Decode.decodeValue (Decode.field "game_id" Decode.string) raw

                gameId =
                    case respGameId of
                        Ok id ->
                            id

                        Err a ->
                            ""

                turn =
                    Board.decodeTurn (Decode.decodeValue (Decode.field "turn" Decode.string) raw)

                board =
                    Board.decodeBoard (Decode.decodeValue (Decode.field "board" Decode.string) raw)

                moves =
                    Board.decodeMoves (Decode.decodeValue (Decode.field "moves" Decode.string) raw)
            in
                if List.length (Dict.toList board) > 0 then
                    ( { model
                        | board = board
                        , teamTurn = turn
                        , gameId = gameId
                        , moves = moves
                      }
                    , Cmd.none
                    )
                else
                    ( { model | gameId = gameId }, updateGameId gameId )


movePiece : String -> Maybe TeamPiece -> Model -> Model
movePiece position currentPiece model =
    let
        sameBoardModel =
            { model
                | movingPiece = Nothing
                , movingFrom = Nothing
            }
    in
        case ( currentPiece, model.movingPiece, model.movingFrom ) of
            ( Nothing, Just (TeamPiece movingTeam movingPiece), Just oldPosition ) ->
                let
                    newPiece =
                        TeamPiece movingTeam movingPiece

                    fromPosition =
                        convertPosition oldPosition

                    toPosition =
                        convertPosition position

                    validPieceMove =
                        Board.canMoveTo newPiece fromPosition Nothing toPosition

                    canMove =
                        model.teamTurn == movingTeam && validPieceMove

                    updatedBoard =
                        updateBoard [ ( position, Just newPiece ), ( oldPosition, Nothing ) ] model.board
                in
                    if canMove then
                        { model
                            | board = updatedBoard
                            , teamTurn = oppositeTeamTurn movingTeam
                            , moves = model.moves ++ [ positionNotation newPiece toPosition ]
                        }
                    else
                        sameBoardModel

            ( currentPiece, Just (TeamPiece movingTeam movingPiece), Just oldPosition ) ->
                let
                    newPiece =
                        TeamPiece movingTeam movingPiece

                    fromPosition =
                        convertPosition oldPosition

                    toPosition =
                        (convertPosition position)

                    validPieceMove =
                        Board.canMoveTo newPiece fromPosition currentPiece toPosition

                    canMove =
                        model.teamTurn == movingTeam && validPieceMove

                    updatedBoard =
                        updateBoard [ ( position, Just newPiece ), ( oldPosition, Nothing ) ] model.board
                in
                    if canMove then
                        { model
                            | board = updatedBoard
                            , teamTurn = oppositeTeamTurn movingTeam
                            , moves = model.moves ++ [ positionNotation newPiece toPosition ]
                        }
                    else
                        sameBoardModel

            _ ->
                sameBoardModel


convertPosition : String -> ( Int, Int )
convertPosition position =
    let
        positionChars =
            String.toList position |> Array.fromList
    in
        ( charToNum (Array.get 0 positionChars)
        , charToNum (Array.get 1 positionChars)
        )


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


updateBoard : List ( String, Maybe TeamPiece ) -> Board -> Board
updateBoard pieces board =
    List.foldl updatePiece board pieces


updatePiece : ( String, Maybe TeamPiece ) -> Board -> Board
updatePiece piecePosition board =
    let
        square =
            Tuple.first piecePosition

        piece =
            Tuple.second piecePosition
    in
        board |> Dict.insert square piece


oppositeTeamTurn : Team -> Team
oppositeTeamTurn team =
    case team of
        White ->
            Black

        Black ->
            White


fileNumToLetter : Int -> String
fileNumToLetter file =
    case file of
        1 ->
            "a"

        2 ->
            "b"

        3 ->
            "c"

        4 ->
            "d"

        5 ->
            "e"

        6 ->
            "f"

        7 ->
            "g"

        8 ->
            "h"

        _ ->
            ""


pieceToLetter : TeamPiece -> String
pieceToLetter piece =
    case piece of
        TeamPiece _ Pawn ->
            ""

        TeamPiece _ Rook ->
            "R"

        TeamPiece _ Knight ->
            "K"

        TeamPiece _ Bishop ->
            "B"

        TeamPiece _ King ->
            "K"

        TeamPiece _ Queen ->
            "Q"


positionNotation : TeamPiece -> ( Int, Int ) -> String
positionNotation piece position =
    let
        rank =
            toString (Tuple.first position)

        file =
            Tuple.second position
    in
        (pieceToLetter piece) ++ (fileNumToLetter file) ++ rank


newModelPayload : Model -> Push.Push msg
newModelPayload model =
    let
        payload =
            Encode.object
                [ ( "board", Board.encodeBoard model.board )
                , ( "moves", Board.encodeMoves model.moves )
                , ( "turn", Encode.string (toString model.teamTurn) )
                ]
    in
        Push.init ("game:" ++ model.gameId) "update_board"
            |> Push.withPayload payload



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        channel =
            Channel.init ("game:" ++ model.gameId)
                |> Channel.onJoin UpdateBoard
                |> Channel.on "update_board" UpdateBoard
    in
        Phoenix.connect (Socket.init model.socketUrl) <| [ channel ]



-- VIEW


stringToInt : String -> Int
stringToInt s =
    case String.toInt s of
        Ok a ->
            a

        Err a ->
            -1


positionComparison : ( String, a ) -> ( String, b ) -> Order
positionComparison a b =
    case stringToInt (Tuple.first a) > stringToInt (Tuple.first b) of
        True ->
            LT

        False ->
            GT


view : Model -> Html Msg
view model =
    let
        boardPieces =
            Dict.toList model.board
                |> List.sortWith positionComparison
                |> Array.fromList

        endRowNumbers =
            Array.initialize 8 (\n -> (n + 1) * 8)
                |> Array.toList

        rowSlice n =
            Array.slice (n - 8) n boardPieces
                |> Array.toList
    in
        div [ class "board" ]
            (boardHeaderElement model
                :: (List.map rowSlice endRowNumbers |> List.map rowElement)
            )


boardHeaderElement : Model -> Html msg
boardHeaderElement model =
    div
        [ class "header row" ]
        [ (teamTurnElement model.teamTurn)
        , (lastMoveElement model.moves)
        ]


teamTurnElement : Team -> Html msg
teamTurnElement team =
    div
        [ class "team-turn col-xs-6" ]
        [ div [] [ text ("Turn: " ++ toString team) ] ]


lastMoveElement : List String -> Html msg
lastMoveElement moves =
    let
        numMoves =
            List.length moves

        moveNumber =
            (numMoves // 2) + (numMoves % 2)

        recentMoves =
            List.reverse moves

        lastMoves =
            if numMoves % 2 == 0 then
                List.take 2 recentMoves
            else
                List.take 1 recentMoves

        lastMove =
            List.foldr (\a b -> a ++ " " ++ b) "" (List.reverse lastMoves)
    in
        div
            [ class "last-move col-xs-6 text-right" ]
            [ div [] [ text ("Moves: " ++ (toString moveNumber) ++ ". " ++ lastMove) ] ]


rowElement : List ( String, Maybe TeamPiece ) -> Html Msg
rowElement pieces =
    div [ class "row" ]
        (pieces
            |> List.reverse
            |> List.map squareElement
        )


squareElement : ( String, Maybe TeamPiece ) -> Html Msg
squareElement ( position, piece ) =
    div
        [ class "square"
        , id position
        , attribute "ondragover" "return false"
        , onDrop <| DropOn position
        ]
        [ pieceElement position piece ]


pieceElement : String -> Maybe TeamPiece -> Html Msg
pieceElement position piece =
    div
        [ class "piece"
        , draggable "true"
        , onDragStart <| Move piece position
        ]
        [ text (pieceDisplay piece) ]


pieceDisplay : Maybe TeamPiece -> String
pieceDisplay teampiece =
    case teampiece of
        Just (TeamPiece White King) ->
            "♔"

        Just (TeamPiece White Queen) ->
            "♕"

        Just (TeamPiece White Rook) ->
            "♖"

        Just (TeamPiece White Bishop) ->
            "♗"

        Just (TeamPiece White Knight) ->
            "♘"

        Just (TeamPiece White Pawn) ->
            "♙"

        Just (TeamPiece Black King) ->
            "♚"

        Just (TeamPiece Black Queen) ->
            "♛"

        Just (TeamPiece Black Rook) ->
            "♜"

        Just (TeamPiece Black Bishop) ->
            "♝"

        Just (TeamPiece Black Knight) ->
            "♞"

        Just (TeamPiece Black Pawn) ->
            "♟"

        Nothing ->
            ""
