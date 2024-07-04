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
let bg_color = Color.rgb 254 203 144
let wall_color = Color.rgb 13 17 47
let flash_color = Color.white
let text_color = Color.black

let color_of_shape shape =
  let open Color in
  match shape with
  | I -> rgb 0x9b 0xf6 0xff (* cyan *)
  | O -> rgb 0xfd 0xff 0xb6 (* yellow *)
  | T -> rgb 0xbd 0xb2 0xff (* purple *)
  | L -> rgb 0xff 0xd6 0xa5 (* orange *)
  | J -> rgb 0xa0 0xc4 0xff (* blue *)
  | Z -> rgb 0xff 0xad 0xad (* red *)
  | S -> rgb 0xca 0xff 0xbf (* green *)
  (* FIXME choose a better color. Maybe a glitching/changing effect? *)
  | Freckles -> white

let border_color_of_shape _shape =
  let open Color in
  black

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
  (* up *)
  (if b_up then
     let up =
       Segment.v grid_coord
         (Size.v (grid_coord.x +. cell_size -. 1.) grid_coord.y)
     in
     Segment.draw ~io ~color:b_color up);
  (if b_right then
     (* right *)
     let right =
       Segment.v
         (Size.v (grid_coord.x +. cell_size -. 1.) grid_coord.y)
         (Size.v
            (grid_coord.x +. cell_size -. 1.)
            (grid_coord.y +. cell_size -. 1.))
     in
     Segment.draw ~io ~color:(if b_right then b_color else color) right);
  (* down *)
  (if b_down then
     let down =
       Segment.v
         (Size.v
            (grid_coord.x +. cell_size -. 1.)
            (grid_coord.y +. cell_size -. 1.))
         (Size.v grid_coord.x (grid_coord.y +. cell_size -. 1.))
     in
     Segment.draw ~io ~color:(if b_down then b_color else color) down);
  (* left *)
  if b_left then
    let left =
      Segment.v
        (Size.v grid_coord.x (grid_coord.y +. cell_size -. 1.))
        grid_coord
    in
    Segment.draw ~io ~color:(if b_left then b_color else color) left

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
  let color = color_of_shape shadow.shape |> Color.with_alpha 0.5 in
  List.iter (fun c -> d_box ~io c.position color) cells

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
