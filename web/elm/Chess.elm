port module Chess exposing (..)

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
    , movingPiece : Maybe TeamPiece
    , movingFrom : Maybe String
    , teamTurn : Team
    , socketUrl : String
    , gameId : String
    }


type alias Flags =
    { gameId : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { model | gameId = flags.gameId }, Cmd.none )


model : Model
model =
    { movingPiece = Nothing
    , movingFrom = Nothing
    , board = Board.init
    , teamTurn = White
    , socketUrl = "ws://localhost:4001/socket/websocket"
    , gameId = ""
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
    | Move (Maybe TeamPiece) String
    | DropOn String
    | UpdateBoard Encode.Value


updateModel : List ( String, Maybe TeamPiece ) -> Model -> Model
updateModel pieces model =
    List.foldl
        (\a b ->
            { b
                | board = b.board |> Dict.insert (Tuple.first a) (Tuple.second a)
            }
        )
        model
        pieces


oppositeTeamTurn : Team -> Team
oppositeTeamTurn team =
    case team of
        White ->
            Black

        Black ->
            White


movePiece : String -> Maybe TeamPiece -> Model -> Model
movePiece position currentPiece model =
    let
        sameBoard =
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

                    canMove =
                        model.teamTurn
                            == movingTeam
                            && Board.canMoveTo
                                newPiece
                                (convertPosition oldPosition)
                                Nothing
                                (convertPosition position)

                    updatedModel =
                        updateModel [ ( position, Just newPiece ), ( oldPosition, Nothing ) ] model
                in
                    if canMove then
                        { updatedModel | teamTurn = oppositeTeamTurn movingTeam }
                    else
                        sameBoard

            ( currentPiece, Just (TeamPiece movingTeam movingPiece), Just oldPosition ) ->
                let
                    newPiece =
                        TeamPiece movingTeam movingPiece

                    canMove =
                        model.teamTurn
                            == movingTeam
                            && Board.canMoveTo
                                newPiece
                                (convertPosition oldPosition)
                                currentPiece
                                (convertPosition position)

                    updatedModel =
                        updateModel [ ( position, Just newPiece ), ( oldPosition, Nothing ) ] model
                in
                    if canMove then
                        { updatedModel | teamTurn = oppositeTeamTurn movingTeam }
                    else
                        sameBoard

            _ ->
                sameBoard


newModelPayload model =
    let
        payload =
            Encode.object
                [ ( "board", (Board.encodeBoard model.board) )
                , ( "turn", Encode.string (toString model.teamTurn) )
                ]
    in
        Push.init ("game:" ++ model.gameId) "update_board"
            |> Push.withPayload payload


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
            in
                if List.length (Dict.toList board) > 0 then
                    ( { model | board = board, teamTurn = turn, gameId = gameId }, Cmd.none )
                else
                    ( { model | gameId = gameId }, updateGameId gameId )



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
            ((div
                [ class "team-turn" ]
                [ div [] [ text (String.concat [ "Turn: ", (toString model.teamTurn) ]) ] ]
             )
                :: (List.map rowSlice endRowNumbers |> List.map placeRow)
            )


placeRow : List ( String, Maybe TeamPiece ) -> Html Msg
placeRow pieces =
    div [ class "row" ]
        (List.map placePiece pieces)


placePiece : ( String, Maybe TeamPiece ) -> Html Msg
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
