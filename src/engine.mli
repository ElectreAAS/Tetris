open Common

val init : unit -> game_state

val update :
  io:Gamelle.io ->
  game_state ->
  control_state ->
  animation_state ->
  game_state * animation_state
