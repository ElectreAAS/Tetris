open Gamelle

module Constants = struct
  let width = 12
  let height = 24
  let number_of_shapes = 7
end

open Constants

module Game = struct
  type coord = int * int
  (** x, y.
    We count from the top left, going down and right. *)

  type shape =
    | I
    | O
    | T
    | L
    | J
    | Z
    | S
        (** Origin point is marked with X or ·
          shape up     right     down     left
          --------------------------------------
                X       ·
            I   #      ####
                #
                #
          --------------------------------------
                  X#
            O     ##
          --------------------------------------
                   ·#    ·#         ·        ·#
            T      ###    ##        ###      ##
                          #          #        #
          --------------------------------------
                 ·#      ·         X#       · #
            L     #      ###        #       ###
                  ##     #          #
          --------------------------------------
                                  ·
                 · #    ·          ##       ·
            J      #     #         #        ###
                  ##     ###       #          #
          --------------------------------------
                 ·#     ·
            Z    ##     ##
                 #       ##
          --------------------------------------
                 X      ·
            S    ##      ##
                  #     ##
  *)

  type direction = Up | Down | Left | Right
  type cell = { from_shape : shape; position : coord }
  type block = { shape : shape; pos : coord; orientation : direction }
  type board = { cells : cell list; block : block }

  type state = {
    board : board;
    is_finished : bool;
    is_paused : bool;
    timer : int;  (** Number of frames that have passed since last move. *)
    speed : int;
        (** Blocks move every [speed] frames. Lower this number to make them go faster. *)
  }

  let random_shape () =
    match Random.int number_of_shapes with
    | 0 -> I
    | 1 -> O
    | 2 -> T
    | 3 -> L
    | 4 -> J
    | 5 -> Z
    | _ -> S

  (* FIXME: They're all geometrically correct, but some rotations feel wrong. *)
  let cells_of_block block =
    let x, y = block.pos in
    let coords =
      match (block.shape, block.orientation) with
      (* From the top-left, going right then down, normal reading order. *)
      | I, (Up | Down) -> [ (x, y); (x, y + 1); (x, y + 2); (x, y + 3) ]
      | I, (Left | Right) ->
          [ (x - 1, y + 1); (x, y + 1); (x + 1, y + 1); (x + 2, y + 1) ]
      | O, _ -> [ (x, y); (x + 1, y); (x, y + 1); (x + 1, y + 1) ]
      | T, Up -> [ (x + 1, y); (x, y + 1); (x + 1, y + 1); (x + 2, y + 1) ]
      | T, Right ->
          [ (x + 1, y); (x + 1, y + 1); (x + 2, y + 1); (x + 1, y + 2) ]
      | T, Down ->
          [ (x, y + 1); (x + 1, y + 1); (x + 2, y + 1); (x + 1, y + 2) ]
      | T, Left -> [ (x + 1, y); (x, y + 1); (x + 1, y + 1); (x + 1, y + 2) ]
      | L, Up -> [ (x + 1, y); (x + 1, y + 1); (x + 1, y + 2); (x + 2, y + 2) ]
      | L, Right -> [ (x, y + 1); (x + 1, y + 1); (x + 2, y + 1); (x, y + 2) ]
      | L, Down -> [ (x, y); (x + 1, y); (x + 1, y + 1); (x + 1, y + 2) ]
      | L, Left -> [ (x + 2, y); (x, y + 1); (x + 1, y + 1); (x + 2, y + 1) ]
      | J, Up -> [ (x + 2, y); (x + 2, y + 1); (x + 1, y + 2); (x + 2, y + 2) ]
      | J, Right ->
          [ (x + 1, y + 1); (x + 1, y + 2); (x + 2, y + 2); (x + 3, y + 2) ]
      | J, Down ->
          [ (x + 1, y + 1); (x + 2, y + 1); (x + 1, y + 2); (x + 1, y + 3) ]
      | J, Left ->
          [ (x, y + 1); (x + 1, y + 1); (x + 2, y + 1); (x + 2, y + 2) ]
      | Z, (Up | Down) -> [ (x + 1, y); (x, y + 1); (x + 1, y + 1); (x, y + 2) ]
      | Z, (Left | Right) ->
          [ (x, y + 1); (x + 1, y + 1); (x + 1, y + 2); (x + 2, y + 2) ]
      | S, (Up | Down) -> [ (x, y); (x, y + 1); (x + 1, y + 1); (x + 1, y + 2) ]
      | S, (Left | Right) ->
          [ (x + 1, y + 1); (x + 2, y + 1); (x, y + 2); (x + 1, y + 2) ]
    in
    List.map (fun position -> { from_shape = block.shape; position }) coords

  let init_block () =
    let shape = random_shape () in
    let pos = (width / 2, 0) in
    { shape; pos; orientation = Up }

  let init_board () =
    {
      cells = [ { from_shape = J; position = (0, height - 1) } ];
      block = init_block ();
    }

  let init_state () =
    {
      board = init_board ();
      is_finished = false;
      is_paused = false;
      timer = 0;
      speed = 60;
    }

  let is_valid_block state new_block =
    List.for_all
      (fun c1 ->
        let x1, y1 = c1.position in
        x1 >= 0 && x1 < width && y1 >= 0 && y1 < height
        && List.for_all
             (fun { position = p2; _ } -> (x1, y1) <> p2)
             state.board.cells)
      (cells_of_block new_block)

  (** This function is called on each frame, before considering player actions, to move the timer by 1. *)
  let pre_update state =
    if state.is_finished || state.is_paused then state
    else
      let new_timer = state.timer + 1 in
      if new_timer < state.speed then { state with timer = new_timer }
      else
        let new_timer = 0 in
        (* Attempt to move block down one cell *)
        let old_block = state.board.block in
        let x, y = old_block.pos in
        let candidate_block = { old_block with pos = (x, y + 1) } in
        if is_valid_block state candidate_block then
          {
            state with
            timer = new_timer;
            board = { state.board with block = candidate_block };
          }
        else
          (* sediment block + destroy check -> (?) + spawn new one *)
          let new_cells = cells_of_block old_block @ state.board.cells in
          (* TODO: check for destruction *)
          let spawned_block = init_block () in
          let board = { cells = new_cells; block = spawned_block } in
          { state with board; timer = new_timer }

  let io_update ~io state =
    if Input.is_pressed ~io `escape then raise Exit;

    if Input.is_down ~io `space then
      { state with is_paused = not state.is_paused }
    else
      let old_block = state.board.block in
      let candidate_block =
        if Input.is_down ~io `arrow_up then
          let orientation =
            match old_block.orientation with
            | Up -> Right
            | Right -> Down
            | Down -> Left
            | Left -> Up
          in
          { old_block with orientation }
        else if Input.is_down ~io `arrow_left then
          let old_x, old_y = old_block.pos in
          { old_block with pos = (old_x - 1, old_y) }
        else if Input.is_down ~io `arrow_right then
          let old_x, old_y = old_block.pos in
          { old_block with pos = (old_x + 1, old_y) }
        else (* TODO: add speed down *)
          old_block
      in
      let new_block =
        if is_valid_block state candidate_block then candidate_block
        else old_block
      in
      { state with board = { state.board with block = new_block } }

  let update ~io state = state |> pre_update |> io_update ~io
end

module Display = struct
  let origin = Size.v 0.0 0.0

  (** Added grey blocks under the gamespace *)
  let oob_height = 4

  (** Added on both sides *)
  let oob_width = 4

  (** in pixels *)
  let cell_size = 40.0

  let window_width = float (width + (2 * oob_width)) *. cell_size
  let window_height = float (height + oob_height) *. cell_size

  let color_of_shape (shape : Game.shape) =
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

  let render_cell ~io (cell : Game.cell) =
    let color = color_of_shape cell.from_shape in
    let x, y = cell.position in
    let box = Box.v (render_coords (x, y)) (Size.v cell_size cell_size) in
    Box.fill ~io ~color box

  let render_block ~io (block : Game.block) =
    let cells = Game.cells_of_block block in
    List.iter (render_cell ~io) cells

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

  let render ~io (state : Game.state) =
    Window.set_size ~io (Size.v window_width window_height);
    Box.fill ~io ~color:Color.black (Window.box ~io);
    render_walls ~io;
    List.iter (render_cell ~io) state.board.cells;
    render_block ~io state.board.block
end

let () =
  Random.self_init ();
  Gamelle.run (Game.init_state ()) @@ fun ~io state ->
  let new_state = Game.update ~io state in
  Display.render ~io new_state;
  new_state
