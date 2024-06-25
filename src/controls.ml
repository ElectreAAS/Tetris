open Gamelle

let long_press_delay = 0.25

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

let quit = Quit
let pause = Pause
let nothing = Nothing
let go_left = Go_left
let hold_left = Hold_left
let go_right = Go_right
let hold_right = Hold_right
let go_down = Go_down
let hold_down = Hold_down
let rotate = Rotate

let is_on ~io = function
  | Quit -> Input.is_down ~io `escape
  | Go_left -> Input.is_down ~io `arrow_left
  | Go_right -> Input.is_down ~io `arrow_right
  | Go_down -> Input.is_pressed ~io `arrow_down
  | Rotate -> Input.is_down ~io `arrow_up
  | Pause -> Input.is_down ~io `space
  | _ -> failwith "not implemented!" (* TODO *)

let poll ~io =
  if Input.is_down ~io `escape then Quit
  else if Input.is_down ~io `arrow_left then Go_left
  else if Input.is_down ~io `arrow_right then Go_right
  else if Input.is_pressed ~io `arrow_down then Go_down
  else if Input.is_down ~io `arrow_up then Rotate
  else if Input.is_down ~io `space then Pause
  else failwith "not implemented!" (* TODO *)
