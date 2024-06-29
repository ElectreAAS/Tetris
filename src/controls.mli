open Common

val long_press_delay : float
(** In seconds. Delay after which a press is understood as a 'hold' instead of a short press. *)

type action =
  | Quit
  | Pause
  | Nothing
  | Go_left
  | Hold_left
  | Go_right
  | Hold_right
  | Go_down
  | Hold_down
  | Rotate

val quit : action
val pause : action
val nothing : action
val go_left : action
val hold_left : action
val go_right : action
val hold_right : action
val go_down : action
val hold_down : action
val rotate : action
val init : unit -> control_state
val is_on : io:Gamelle.io -> control_state -> action -> bool
val poll : io:Gamelle.io -> control_state -> action
