open Common
open Gamelle

let origin = Size.v 0.0 0.0

(** Added grey blocks under the gamespace *)
let oob_height = 4

(** Added on both sides *)
let oob_width = 4

(** in pixels *)
let cell_size = 40.0

let window_width = float (width + (2 * oob_width)) *. cell_size
let window_height = float (height + oob_height) *. cell_size

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

(** Translate from game coordinates to rendering coordinates. *)
let render_coords (x, y) =
  Size.v (cell_size *. float (oob_width + x)) (cell_size *. float y)

let render_cell ~io cell =
  let color = color_of_shape cell.from_shape in
  let x, y = cell.position in
  let box =
    Box.v
      (render_coords (x, y))
      (Size.v
         (* These -1 are there because gamelle renders a box of size 10 on pixels 0 to 10 INCLUDED. *)
         (cell_size -. 1.)
         (cell_size -. 1.))
  in
  Box.fill ~io ~color box

let render_block ~io block =
  let cells = cells_of_block block in
  List.iter (render_cell ~io) cells

let render_shadow ~io shadow =
  let cells = cells_of_block shadow in
  let color = color_of_shape shadow.shape |> Color.with_alpha 0.5 in
  List.iter
    (fun c ->
      let x, y = c.position in
      let box =
        Box.v
          (render_coords (x, y))
          (Size.v (cell_size -. 1.) (cell_size -. 1.))
      in
      Box.fill ~io ~color box)
    cells

let render_walls ~io =
  let walls =
    let left =
      Box.v origin (Size.v (float oob_width *. cell_size) window_height)
    in
    let under =
      Box.v
        (Size.v (float oob_width *. cell_size) (float height *. cell_size))
        (Size.v (float width *. cell_size) (float oob_height *. cell_size))
    in
    let right =
      Box.v
        (Size.v (float (width + oob_width) *. cell_size) 0.0)
        (Size.v (float oob_width *. cell_size) window_height)
    in
    [ left; under; right ]
  in
  List.iter (Box.fill ~io ~color:Color.gray) walls

let render ~io state =
  Window.set_size ~io (Size.v window_width window_height);
  Box.fill ~io ~color:Color.black (Window.box ~io);
  render_walls ~io;
  List.iter (render_cell ~io) state.board.cells;
  render_shadow ~io (shadow state.board.cells state.board.block);
  render_block ~io state.board.block
