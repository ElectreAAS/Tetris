let width = 12
let height = 24
let base_speed = 60
let acceleration = 4

(* TUNE: this min speed feels rightly difficult, but maybe the acceleration isn't correct. *)
let min_speed = 10

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
                X        ·
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
                  X       ·        #X        ·#
            L     #      ###        #       ###
                  ##     #          #
          --------------------------------------
                   X               X#        ·
            J      #     #·        #        ###
                  ##     ###       #          #
          --------------------------------------
                 ·#      ·
            Z    ##     ##
                 #       ##
          --------------------------------------
                 X      ·
            S    ##      ##
                  #     ##
          --------------------------------------
       Freckles  ·#     X
                 #       #
  *)
(* | Freckles *)

type direction = Up | Down | Left | Right
type cell = { from_shape : shape; position : coord }
type block = { shape : shape; pos : coord; orientation : direction }

type state = {
  cells : cell list;
  block : block;
  is_finished : bool;
  is_paused : bool;
  timer : int;  (** Number of frames that have passed since last move. *)
  speed : int;
      (** Blocks move every [speed] frames. Lower this number to make them go faster. *)
}

let weighted_shapes =
  [|
    (I, 10);
    (O, 10);
    (T, 10);
    (L, 10);
    (J, 10);
    (Z, 10);
    (S, 10);
    (* (Freckles, 1); *)
  |]

let total_weights =
  Array.fold_left (fun sum (_, n) -> sum + n) 0 weighted_shapes

let random_shape () =
  let n = Random.int total_weights in
  let rec aux cursor i =
    let shape, weight = weighted_shapes.(i) in
    let new_cursor = cursor + weight in
    if new_cursor >= n then shape else aux new_cursor (i + 1)
  in
  aux 0 0

let cells_of_block block =
  let x, y = block.pos in
  let coords =
    match (block.shape, block.orientation) with
    (* From the top-left, going right then down, normal reading order. *)
    | I, (Up | Down) -> [ (x, y); (x, y + 1); (x, y + 2); (x, y + 3) ]
    | I, (Left | Right) ->
        [ (x - 2, y + 1); (x - 1, y + 1); (x, y + 1); (x + 1, y + 1) ]
    | O, _ -> [ (x, y); (x + 1, y); (x, y + 1); (x + 1, y + 1) ]
    | T, Up -> [ (x + 1, y); (x, y + 1); (x + 1, y + 1); (x + 2, y + 1) ]
    | T, Right -> [ (x + 1, y); (x + 1, y + 1); (x + 2, y + 1); (x + 1, y + 2) ]
    | T, Down -> [ (x, y + 1); (x + 1, y + 1); (x + 2, y + 1); (x + 1, y + 2) ]
    | T, Left -> [ (x + 1, y); (x, y + 1); (x + 1, y + 1); (x + 1, y + 2) ]
    | L, Up -> [ (x, y); (x, y + 1); (x, y + 2); (x + 1, y + 2) ]
    | L, Right -> [ (x - 1, y + 1); (x, y + 1); (x + 1, y + 1); (x - 1, y + 2) ]
    | L, Down -> [ (x - 1, y); (x, y); (x, y + 1); (x, y + 2) ]
    | L, Left -> [ (x + 1, y); (x - 1, y + 1); (x, y + 1); (x + 1, y + 1) ]
    | J, Up -> [ (x, y); (x, y + 1); (x, y + 2); (x - 1, y + 2) ]
    | J, Right -> [ (x - 1, y); (x - 1, y + 1); (x, y + 1); (x + 1, y + 1) ]
    | J, Down -> [ (x, y); (x + 1, y); (x, y + 1); (x, y + 2) ]
    | J, Left -> [ (x - 1, y + 1); (x, y + 1); (x + 1, y + 1); (x + 1, y + 2) ]
    | Z, (Up | Down) -> [ (x + 1, y); (x, y + 1); (x + 1, y + 1); (x, y + 2) ]
    | Z, (Left | Right) ->
        [ (x - 1, y + 1); (x, y + 1); (x, y + 2); (x + 1, y + 2) ]
    | S, (Up | Down) -> [ (x, y); (x, y + 1); (x + 1, y + 1); (x + 1, y + 2) ]
    | S, (Left | Right) ->
        [ (x + 1, y + 1); (x + 2, y + 1); (x, y + 2); (x + 1, y + 2) ]
    (* | Freckles, (Up | Down) -> [ (x + 1, y); (x, y + 1) ]
       | Freckles, (Left | Right) -> [ (x, y); (x + 1, y + 1) ] *)
  in
  List.map (fun position -> { from_shape = block.shape; position }) coords

let is_valid_block cells new_block =
  List.for_all
    (fun c1 ->
      let x1, y1 = c1.position in
      x1 >= 0 && x1 < width && y1 >= 0 && y1 < height
      && List.for_all (fun { position = p2; _ } -> (x1, y1) <> p2) cells)
    (cells_of_block new_block)

let rec shadow state block =
  (* We assume the block is valid *)
  let x, y = block.pos in
  let candidate_block = { block with pos = (x, y + 1) } in
  if is_valid_block state candidate_block then shadow state candidate_block
  else block
