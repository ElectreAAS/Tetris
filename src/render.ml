open Common
open Display

let display_static ~io =
  let open Gamelle in
  Window.set_size ~io (Size.v window_width window_height);
  Box.fill ~io ~color:Color.white (Window.box ~io);
  d_background ~io;
  d_walls ~io

let display ~io game anim =
  display_static ~io;

  (* Dynamic stuff *)
  List.iter (d_cell ~io) game.cells;
  d_shadow ~io (shadow game.cells game.block);
  d_block ~io game.block;
  d_next_block ~io (List.hd game.next_blocks);
  (* Animations *)
  Anim.go ~io anim
