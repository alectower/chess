defmodule Chess.GameChannel do
  use Chess.Web, :channel
  alias Chess.Repo

  def join("game:" <> game_id, payload, socket) do
    new_game_id = :crypto.strong_rand_bytes(30)
              |> Base.url_encode64
              |> binary_part(0, 30)

    new_game = &(
      Repo.insert %Chess.Game{
        game_id: &1,
        board: %{},
        turn: "White"
      }
    )

    game = if game_id == ""  do
      {:ok, game} = new_game.(new_game_id)

      game
    else
      game = Repo.get_by(Chess.Game, game_id: game_id)

      if game do
        game
      else
        {:ok, game} = new_game.(game_id)

        game
      end
    end

    {:ok, %{
      game_id: game.game_id,
      board: game.board |> Poison.encode!,
      turn: game.turn
    }, socket}
  end

  def handle_in("update_board", data, socket) do
    [_, game_id] = String.split socket.topic, ":"

    game = Repo.get_by(Chess.Game, game_id: game_id)

    game = Ecto.Changeset.change game,
      board: data |> Map.get("board"),
      turn: data |> Map.get("turn")

    case Repo.update game do
      {:ok, struct}       ->
        Chess.Endpoint.broadcast! socket.topic,
          "update_board", %{
            game_id: struct.game_id,
            board: struct.board |> Poison.encode!,
            turn: struct.turn
          }

      {:error, changeset} ->
        Chess.Endpoint.broadcast! socket.topic,
          "update_board", %{
            game_id: data |> Map.get("game_id"),
            board: data |> Map.get("board") |> Poison.encode!,
            turn: data |> Map.get("turn")
          }
    end

    {:noreply, socket}
  end

  defp authorized?(_payload) do
    true
  end
end
