open Common
open Gamelle

let long_press_delay = 8

type action =
  | Quit
  | Pause
  | Nothing
  | Go_left
  | Go_right
  | Instant_fall
  | Quick_fall
  | Rotate

let init () = { holding_timer = 0 }

let poll ~io controls =
  let open Input in
  if is_pressed ~io `escape then (Quit, controls)
  else if is_down ~io `space then (Pause, controls)
  else if is_down ~io `arrow_up then (Rotate, controls)
  else if is_down ~io `arrow_left then (Go_left, controls)
  else if is_down ~io `arrow_right then (Go_right, controls)
  else if is_pressed ~io `arrow_down then
    (Quick_fall, { holding_timer = controls.holding_timer + 1 })
  else if is_up ~io `arrow_down && controls.holding_timer < long_press_delay
  then (Instant_fall, { holding_timer = 0 })
  else (Nothing, { holding_timer = 0 })
