open Common

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

let demolish cells =
  (* OPTIM: this function is wildly inefficient, but the search space is so small it shouldn't matter. *)
  let rec aux i cells =
    if i < 0 then cells
    else
      let line, others =
        List.partition
          (fun c ->
            let _, y = c.position in
            y = i)
          cells
      in
      if List.length line = width then
        (* DESTROY *)
        let new_cells =
          List.map
            (fun c ->
              let x, y = c.position in
              let new_y = if y < i then y + 1 else y in
              { c with position = (x, new_y) })
            others
        in
        aux i new_cells
      else aux (i - 1) cells
  in
  aux height cells

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
      if is_valid_block state.board candidate_block then
        {
          state with
          timer = new_timer;
          board = { state.board with block = candidate_block };
        }
      else
        let new_cells =
          cells_of_block old_block @ state.board.cells |> demolish
        in
        let spawned_block = init_block () in
        let board = { cells = new_cells; block = spawned_block } in
        { state with board; timer = new_timer }

let io_update ~io state =
  if Controls.(is_on ~io quit) then raise Exit;

  if Controls.(is_on ~io pause) then
    { state with is_paused = not state.is_paused }
  else
    let old_block = state.board.block in
    let candidate_block =
      if Controls.(is_on ~io rotate) then
        let orientation =
          match old_block.orientation with
          | Up -> Right
          | Right -> Down
          | Down -> Left
          | Left -> Up
        in
        { old_block with orientation }
      else if Controls.(is_on ~io go_left) then
        let old_x, old_y = old_block.pos in
        { old_block with pos = (old_x - 1, old_y) }
      else if Controls.(is_on ~io go_right) then
        let old_x, old_y = old_block.pos in
        { old_block with pos = (old_x + 1, old_y) }
      else if Controls.(is_on ~io go_down) then
        let old_x, old_y = old_block.pos in
        { old_block with pos = (old_x, old_y + 1) }
      else old_block
    in
    let new_block =
      if is_valid_block state.board candidate_block then candidate_block
      else old_block
    in
    { state with board = { state.board with block = new_block } }

let update ~io state = state |> pre_update |> io_update ~io
