open Common

let new_shuffle () = List.shuffle all_shapes

let init_block shape =
  let pos = (width / 2, 0) in
  let orientation = Up in
  { shape; pos; orientation }

let rec pop_shape l =
  let shuffle = List.map init_block (new_shuffle ()) in
  match l with
  | [] -> pop_shape shuffle
  | [ block ] -> (block, shuffle)
  | block :: rest -> (block, rest)

let pop_shape_from state =
  let block, next_blocks = pop_shape state.next_blocks in
  { state with block; next_blocks }

let init_state () =
  let block, next_blocks = pop_shape [] in
  {
    cells = [];
    block;
    next_blocks;
    is_finished = false;
    is_paused = false;
    timer = 0;
    speed = 60;
  }

let sediment block cells =
  let block_cells = cells_of_block block in
  let indices =
    List.map (fun c -> snd c.position) block_cells |> List.sort_uniq compare
  in
  let new_cells = block_cells @ cells in
  let full_rows =
    List.filter
      (fun y ->
        let row_count =
          List.fold_left
            (fun n c ->
              let _, y' = c.position in
              if y = y' then n + 1 else n)
            0 new_cells
        in
        row_count = width)
      indices
  in
  (new_cells, full_rows)

let rec destroy_rows cells = function
  | [] -> cells
  (* We assume here that the list is sorted in increasing order,
     so we destroy rows from top to bottom. *)
  | y :: ys ->
      let filtered_cells =
        List.filter_map
          (fun c ->
            let x, y' = c.position in
            if y' = y then (* cell is on the row to destroy*)
              None
            else if y' > y then
              (* lower than destroyed row, nothing to do *)
              Some c
            else
              (* higher than destroyed row, move downwards *)
              Some { c with position = (x, y' + 1) })
          cells
      in
      destroy_rows filtered_cells ys

(** This function is called on each frame, before considering player actions, to move the timer by 1. *)
let pre_update state =
  if state.is_finished || state.is_paused then state
  else
    let new_timer = state.timer + 1 in
    if new_timer < state.speed then { state with timer = new_timer }
    else
      let state = { state with timer = 0 } in
      (* Attempt to move block down one cell *)
      let old_block = state.block in
      let x, y = old_block.pos in
      let candidate_block = { old_block with pos = (x, y + 1) } in
      if is_valid_block state.cells candidate_block then
        { state with block = candidate_block }
      else
        let new_cells, full_rows = sediment old_block state.cells in
        (* TODO: animation triggering here *)
        let new_cells = destroy_rows new_cells full_rows in
        let state = pop_shape_from state in
        let new_speed =
          (* Acceleration *)
          (* FIXME: acceleration is linear in that speed goes down regularly, but that results in exponential felt speed. *)
          if not (List.is_empty full_rows) then
            max min_speed (state.speed - acceleration)
          else state.speed
        in
        if is_valid_block new_cells state.block then
          { state with cells = new_cells; speed = new_speed }
        else (* Defeat *)
          { state with cells = new_cells; is_finished = true }

let io_update ~io state =
  if Controls.(is_on ~io quit) then raise Exit;

  if state.is_finished then state
  else if Controls.(is_on ~io pause) then
    { state with is_paused = not state.is_paused }
  else
    let old_block = state.block in
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
      if is_valid_block state.cells candidate_block then candidate_block
      else old_block
    in
    { state with block = new_block }

let update ~io state = state |> pre_update |> io_update ~io
