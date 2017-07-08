defmodule Chess.GameTest do
  use Chess.ModelCase

  alias Chess.Game

  @valid_attrs %{board: "some content", game_id: "some content", team_turn: "some content"}
  @invalid_attrs %{}

  test "changeset with valid attributes" do
    changeset = Game.changeset(%Game{}, @valid_attrs)
    assert changeset.valid?
  end

  test "changeset with invalid attributes" do
    changeset = Game.changeset(%Game{}, @invalid_attrs)
    refute changeset.valid?
  end
end
