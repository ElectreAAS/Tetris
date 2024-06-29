open Common

let init_block shape =
  let pos = (width / 2, 0) in
  let orientation = Up in
  { shape; pos; orientation }

let rec pop_shape = function
  | [] ->
      let shuffle = List.map init_block (shuffled_shapes ()) in
      pop_shape shuffle
  | [ block ] ->
      let shuffle = List.map init_block (shuffled_shapes ()) in
      (block, shuffle)
  | block :: rest -> (block, rest)

let pop_shape_from state =
  let block, next_blocks = pop_shape state.next_blocks in
  { state with block; next_blocks }

let init () =
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

let spawn_block game =
  let game = pop_shape_from game in
  if is_valid_block game.cells game.block then game
  else (* Defeat *)
    { game with is_finished = true }

(** This function is called on each frame, before considering player actions, to move the timer by 1. *)
let pre_update game anim =
  if game.is_finished || game.is_paused then (game, anim)
  else
    let new_timer = game.timer + 1 in
    if new_timer < game.speed then ({ game with timer = new_timer }, anim)
    else
      let game = { game with timer = 0 } in
      (* Attempt to move block down one cell. *)
      let old_block = game.block in
      let x, y = old_block.pos in
      let candidate_block = { old_block with pos = (x, y + 1) } in
      if is_valid_block game.cells candidate_block then
        ({ game with block = candidate_block }, anim)
      else
        (* Can't move downwards, block sediments. *)
        let new_cells, full_rows = sediment old_block game.cells in
        if List.is_empty full_rows then
          (* No full rows, a new block is spawned. *)
          (spawn_block { game with cells = new_cells }, anim)
        else
          (* At least one row is full, the animation triggers. *)
          let anim = Anim.register_flash anim full_rows in
          ({ game with cells = new_cells }, anim)

let try_move game old_block new_block =
  let new_block =
    if is_valid_block game.cells new_block then new_block else old_block
  in
  { game with block = new_block }

let io_update ~io controls game =
  (* if game.is_finished then (game, controls)
     else *)
  match Controls.poll ~io controls with
  | Quit, _ -> raise Exit
  | Pause, _ -> ({ game with is_paused = not game.is_paused }, controls)
  | Rotate, _ ->
      let old_block = game.block in
      let new_block =
        let orientation =
          match old_block.orientation with
          | Up -> Right
          | Right -> Down
          | Down -> Left
          | Left -> Up
        in
        { old_block with orientation }
      in
      (try_move game old_block new_block, controls)
  | Go_left, controls ->
      let old_block = game.block in
      let old_x, old_y = old_block.pos in
      let new_block = { old_block with pos = (old_x - 1, old_y) } in
      (try_move game old_block new_block, controls)
  | Go_right, controls ->
      let old_block = game.block in
      let old_x, old_y = old_block.pos in
      let new_block = { old_block with pos = (old_x + 1, old_y) } in
      (try_move game old_block new_block, controls)
  | Quick_fall, controls ->
      let old_block = game.block in
      let old_x, old_y = old_block.pos in
      let new_block = { old_block with pos = (old_x, old_y + 1) } in
      (try_move game old_block new_block, controls)
  | Instant_fall, controls ->
      let new_block = shadow game.cells game.block in
      ({ game with block = new_block; timer = game.speed }, controls)
  | Nothing, controls -> (game, controls)

(** This function is only called on the exact frame after flashing row animation finishes *)
let post_flash game rows =
  let new_cells = destroy_rows game.cells rows in
  let new_speed = max min_speed (game.speed - acceleration) in
  spawn_block { game with cells = new_cells; speed = new_speed }

(* We always assume animations are not busy on [update] start. *)
let update ~io game controls anim =
  let game, anim = pre_update game anim in
  match anim with
  | Nothing ->
      let game, controls = io_update ~io controls game in
      (game, controls, anim)
  | Ongoing _ -> (game, controls, anim)
  | Finished rows -> (post_flash game rows, controls, Nothing)
