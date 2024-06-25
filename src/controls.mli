val long_press_delay : float
(** In seconds. Delay after which a press is understood as a 'hold' instead of a short press. *)

type action

val quit : action
val go_left : action
val hold_left : action
val go_right : action
val hold_right : action
val go_down : action
val hold_down : action
val rotate : action
val pause : action
val is_on : io:Gamelle.io -> action -> bool
