// Brunch automatically concatenates all files in your
// watched paths. Those paths can be configured at
// config.paths.watched in "brunch-config.js".
//
// However, those files will only be executed if
// explicitly imported. The only exception are files
// in vendor, which are never wrapped in imports and
// therefore are always executed.

// Import dependencies
//
// If you no longer want to use a dependency, remember
// to also remove its path from "config.paths.watched".
//import "phoenix"
//import "phoenix_html"

// Import local files
//
// Local files can be imported directly using relative
// paths "./socket" or full ones "web/static/js/socket".

import socket from "./socket"
import dragndrop from "./ios-drag-drop"

dragndrop({
  enableEnterLeave: true
})

const paramsMap = new Map(
  window.location.search
    .replace("?", "")
    .split("&")
    .filter((z) => { return z != "" })
    .map((s) => { return s.split("=") })
)

const elmDiv = document.getElementById("elm-main"),
  protocol = location.protocol.search("s") != -1 ? "wss://" : "ws://",
  elmApp = Elm.Chess.embed(elmDiv, {
    host:  protocol + location.host,
    gameId:  paramsMap.get("game_id") || ""
  })

elmApp.ports.updateGameId.subscribe(function(gameId) {
  if (window.location.search.indexOf("game_id") == -1) {
    window.location.href = window.location.origin + "?game_id=" + gameId
  }
});
