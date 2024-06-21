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
        (**
          shape up     right     down     left
          --------------------------------------
                #
            I   #      ####
                #
                #
          --------------------------------------
                  ##
            O     ##
          --------------------------------------
                    #     #                   #
            T      ###    ##        ###      ##
                          #          #        #
          --------------------------------------
                  #                ##         #
            L     #      ###        #       ###
                  ##     #          #
          --------------------------------------
                   #     #         ##
            J      #     ###       #        ###
                  ##               #          #
          --------------------------------------
                  #     ##
            Z    ##      ##
                 #
          --------------------------------------
                 #       ##
            S    ##     ##
                  #
  *)

  type direction = Up | Down | Left | Right
  type cell = { shape : shape; position : coord }
  type block = { shape : shape; position : coord; orientation : direction }
  type board = { cells : cell list; block : block; score : int }

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

  let init_board () =
    {
      cells = [ { shape = J; position = (0, height - 1) } ];
      block =
        {
          shape = random_shape ();
          position = (width / 2, height);
          orientation = Up;
        };
      score = 0;
    }

  let init () =
    {
      board = init_board ();
      is_finished = false;
      is_paused = false;
      timer = 0;
      speed = 60;
    }

  let update ~io state =
    if Input.is_pressed ~io `escape then raise Exit;

    if
      state.is_finished || (state.is_paused && not (Input.is_pressed ~io `space))
    then state
    else
      let new_timer = succ state.timer in
      let new_state =
        if new_timer >= state.speed then
          (* TODO: the only thing we do is move the block downwards forever. *)
          let x, y = state.board.block.position in
          {
            state with
            timer = 0;
            board =
              {
                state.board with
                block = { state.board.block with position = (x, y - 1) };
              };
          }
        else state
      in
      if Input.is_pressed ~io `space then
        (* let new_state = react to other inputs! TODO *)
        { new_state with is_paused = true }
      else new_state
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

  let render_cell ~io (cell : Game.cell) =
    let color = color_of_shape cell.shape in
    let x, y = cell.position in
    let box =
      Box.v
        (Size.v (cell_size *. float (oob_width + x)) (cell_size *. float y))
        (Size.v cell_size cell_size)
    in
    Box.fill ~io ~color box

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
    List.iter (render_cell ~io) state.board.cells
end

let () =
  Random.self_init ();
  Gamelle.run (Game.init ()) @@ fun ~io state ->
  let new_state = Game.update ~io state in
  Display.render ~io new_state;
  new_state
