open Common
open Gamelle

(*
  Left pane:
  Next up:
  #####
  # some
  # shape
  # here
  #
  #####

  Score
  lvl?
  paused?
  finished?
*)

let left_width = 11
let right_width = 1

(** in pixels *)
let cell_size = 40.0

let window_width = float (left_width + width + right_width) *. cell_size
let window_height = float (height + 1) *. cell_size
let bg_color = Color.rgb 255 237 208
let wall_color = Color.rgb 13 17 47
let flash_color = Color.white
let text_color = Color.black

let color_of_shape shape =
  let open Color in
  match shape with
  | I -> rgb 0x9b 0xf6 0xff (* cyan *)
  | O -> rgb 254 215 97 (* yellow *)
  | T -> rgb 0xbd 0xb2 0xff (* purple *)
  | L -> rgb 250 193 102 (* orange *)
  | J -> rgb 0xa0 0xc4 0xff (* blue *)
  | Z -> rgb 0xff 0xad 0xad (* red *)
  | S -> rgb 0xca 0xff 0xbf (* green *)
  (* FIXME choose a better color. Maybe a glitching/changing effect? *)
  | Freckles -> white

let border_color_of_shape shape =
  let open Color in
  match shape with
  | I -> rgb 31 132 141
  | O -> rgb 186 186 71
  | T -> rgb 115 103 174
  | L -> rgb 193 154 107
  | J -> rgb 83 116 169
  | Z -> rgb 169 95 96
  | S -> rgb 126 175 116
  | Freckles -> rgb 174 174 174

(** Translate from game coordinates to rendering coordinates. *)
let translate_xy (x, y) =
  Size.v (cell_size *. float (left_width + x)) (cell_size *. float y)

let d_box ~io (x, y) ?border color =
  let grid_coord = translate_xy (x, y) in
  let inner_box =
    Box.v grid_coord
      (Size.v
         (* These -1 are there because gamelle renders a box of size 10 on pixels 0 to 10 INCLUDED. *)
         (cell_size -. 1.)
         (cell_size -. 1.))
  in
  Box.fill ~io ~color inner_box;
  let b_color, b_up, b_right, b_down, b_left =
    Option.value border ~default:(color, false, false, false, false)
  in
  (if b_up then
     let up =
       Segment.v grid_coord
         (Size.v (grid_coord.x +. cell_size -. 1.) grid_coord.y)
     in
     Segment.draw ~io ~color:b_color up);
  (if b_right then
     let right =
       Segment.v
         (Size.v (grid_coord.x +. cell_size -. 1.) grid_coord.y)
         (Size.v
            (grid_coord.x +. cell_size -. 1.)
            (grid_coord.y +. cell_size -. 1.))
     in
     Segment.draw ~io ~color:b_color right);
  (if b_down then
     let down =
       Segment.v
         (Size.v
            (grid_coord.x +. cell_size -. 1.)
            (grid_coord.y +. cell_size -. 1.))
         (Size.v grid_coord.x (grid_coord.y +. cell_size -. 1.))
     in
     Segment.draw ~io ~color:b_color down);
  if b_left then
    let left =
      Segment.v
        (Size.v grid_coord.x (grid_coord.y +. cell_size -. 1.))
        grid_coord
    in
    Segment.draw ~io ~color:b_color left

let d_cell ~io cell =
  let color = color_of_shape cell.from_shape in
  let b_color = border_color_of_shape cell.from_shape in
  let up, right, down, left = cell.border in
  d_box ~io cell.position ~border:(b_color, up, right, down, left) color

let d_block ~io block =
  let cells = cells_of_block block in
  List.iter (d_cell ~io) cells

let d_shadow ~io shadow =
  let cells = cells_of_block shadow in
  let inner_color = color_of_shape shadow.shape |> Color.with_alpha 0.5 in
  let border_color =
    border_color_of_shape shadow.shape |> Color.with_alpha 0.75
  in
  List.iter
    (fun c ->
      let up, right, down, left = c.border in
      d_box ~io c.position
        ~border:(border_color, up, right, down, left)
        inner_color)
    cells

let d_walls ~io =
  (* Left & right walls *)
  for i = 0 to height do
    d_box ~io (-1, i) wall_color;
    d_box ~io (width, i) wall_color
  done;
  (* Under the playspace *)
  for i = 0 to width - 1 do
    d_box ~io (i, height) wall_color
  done

let d_background ~io =
  for x = 0 to width - 1 do
    for y = 0 to height - 1 do
      d_box ~io (x, y) bg_color
    done
  done

let d_next_block ~io future =
  let txt = "Next block:" in
  Text.draw ~io ~color:text_color ~at:(translate_xy (-8, 1)) txt;
  for i = 0 to 6 do
    d_box ~io (-4 - i, 2) wall_color;
    d_box ~io (-4 - i, 9) wall_color
  done;
  for i = 0 to 7 do
    d_box ~io (-4, 2 + i) wall_color;
    d_box ~io (-10, 2 + i) wall_color
  done;
  let cells = cells_of_block future in
  List.iter
    (fun c ->
      let x, y = c.position in
      d_box ~io (x - 13, y + 4) (color_of_shape c.from_shape))
    cells

let a_flash_row ~io y =
  for x = 0 to width - 1 do
    d_box ~io (x, y) flash_color
  done
