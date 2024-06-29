open Common

val long_press_delay : int
(** In number of frames. Delay after which a press is understood as a 'hold' instead of a short press. *)

type action =
  | Quit
  | Pause
  | Nothing
  | Go_left
  | Go_right
  | Instant_fall
  | Quick_fall
  | Rotate

val init : unit -> control_state
val poll : io:Gamelle.io -> control_state -> action * control_state
