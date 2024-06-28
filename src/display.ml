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
let bg_color = Color.black
let wall_color = Color.gray

(* FIXME: these colors work well with a black background, but with a white one not so much. *)
let color_of_shape shape =
  let open Color in
  match shape with
  | I -> cyan
  | O -> yellow
  | T -> rgb 0x8f 0x1c 0xe2 (* purple *)
  | L -> orange
  | J -> blue
  | Z -> red
  | S -> green
  (* FIXME choose a better color. Maybe a glitching/changing effect? *)
  | Freckles -> white

(** Translate from game coordinates to rendering coordinates. *)
let translate_xy (x, y) =
  Size.v (cell_size *. float (left_width + x)) (cell_size *. float y)

let d_box ~io (x, y) color =
  let box =
    Box.v
      (translate_xy (x, y))
      (Size.v
         (* These -1 are there because gamelle renders a box of size 10 on pixels 0 to 10 INCLUDED. *)
         (cell_size -. 1.)
         (cell_size -. 1.))
  in
  Box.fill ~io ~color box

let d_cell ~io cell =
  let color = color_of_shape cell.from_shape in
  d_box ~io cell.position color

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
  Text.draw ~io ~color:Color.black ~at:(translate_xy (-8, 1)) txt;
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

let display ~io state =
  Window.set_size ~io (Size.v window_width window_height);
  Box.fill ~io ~color:Color.white (Window.box ~io);
  d_walls ~io;
  d_background ~io;
  d_next_block ~io (List.hd state.next_blocks);
  List.iter (d_cell ~io) state.cells;
  d_shadow ~io (shadow state.cells state.block);
  d_block ~io state.block
