defmodule Chess.Game do
  use Chess.Web, :model

  schema "games" do
    field :board, :map
    field :moves, { :array, :string }
    field :game_id, :string
    field :turn, :string

    timestamps()
  end

  @doc """
  Builds a changeset based on the `struct` and `params`.
  """
  def changeset(struct, params \\ %{}) do
    struct
    |> cast(params, [:game_id, :turn])
    |> validate_required([:board, :moves, :game_id, :turn])
  end
end
