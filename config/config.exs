# This file is responsible for configuring your application
# and its dependencies with the aid of the Mix.Config module.
#
# This configuration file is loaded before any dependency and
# is restricted to this project.
use Mix.Config

# General application configuration
config :chess,
  ecto_repos: [Chess.Repo]

# Configures the endpoint
config :chess, Chess.Endpoint,
  url: [host: "localhost"],
  secret_key_base: "lUDthaDZhTaLvApZfsULjzqyQPuPy1lAWLd7feD/BuRD9yACWxCEHHL3bLZqBYAB",
  render_errors: [view: Chess.ErrorView, accepts: ~w(html json)],
  pubsub: [name: Chess.PubSub,
           adapter: Phoenix.PubSub.PG2]

# Configures Elixir's Logger
config :logger, :console,
  format: "$time $metadata[$level] $message\n",
  metadata: [:request_id]

# Import environment specific config. This must remain at the bottom
# of this file so it overrides the configuration defined above.
import_config "#{Mix.env}.exs"