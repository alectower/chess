defmodule Chess.Repo.Migrations.CreateGame do
  use Ecto.Migration

  def change do
    create table(:games) do
      add :board, :map
      add :game_id, :string
      add :turn, :string

      timestamps()
    end

  end
end
