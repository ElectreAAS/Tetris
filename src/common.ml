let width = 12
let height = 24
let number_of_shapes = 7
let base_speed = 60

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
    | T, Right -> [ (x + 1, y); (x + 1, y + 1); (x + 2, y + 1); (x + 1, y + 2) ]
    | T, Down -> [ (x, y + 1); (x + 1, y + 1); (x + 2, y + 1); (x + 1, y + 2) ]
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
    | J, Left -> [ (x, y + 1); (x + 1, y + 1); (x + 2, y + 1); (x + 2, y + 2) ]
    | Z, (Up | Down) -> [ (x + 1, y); (x, y + 1); (x + 1, y + 1); (x, y + 2) ]
    | Z, (Left | Right) ->
        [ (x, y + 1); (x + 1, y + 1); (x + 1, y + 2); (x + 2, y + 2) ]
    | S, (Up | Down) -> [ (x, y); (x, y + 1); (x + 1, y + 1); (x + 1, y + 2) ]
    | S, (Left | Right) ->
        [ (x + 1, y + 1); (x + 2, y + 1); (x, y + 2); (x + 1, y + 2) ]
  in
  List.map (fun position -> { from_shape = block.shape; position }) coords

let is_valid_block board new_block =
  List.for_all
    (fun c1 ->
      let x1, y1 = c1.position in
      x1 >= 0 && x1 < width && y1 >= 0 && y1 < height
      && List.for_all (fun { position = p2; _ } -> (x1, y1) <> p2) board.cells)
    (cells_of_block new_block)

let rec shadow state block =
  (* We assume the block is valid *)
  let x, y = block.pos in
  let candidate_block = { block with pos = (x, y + 1) } in
  if is_valid_block state candidate_block then shadow state candidate_block
  else block
