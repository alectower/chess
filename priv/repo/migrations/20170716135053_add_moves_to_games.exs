defmodule Chess.Repo.Migrations.AddMovesToGames do
  use Ecto.Migration

  def change do
    alter table(:games) do
      add :moves, { :array, :string }
    end
  end
end
