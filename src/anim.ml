open Common
module D = Display

let init () = Nothing

let register_flash anim rows =
  match anim with
  | Nothing -> Ongoing { time_elapsed = 0; rows }
  | _ -> failwith "Attempted to register a flash while one was going on"

let pop_flash = function
  | Nothing | Ongoing _ -> None
  | Finished rows -> Some (Nothing, rows)

let is_busy = function Ongoing _ -> true | _ -> false

let go ~io anim =
  match anim with
  | Nothing | Finished _ -> anim
  | Ongoing ({ time_elapsed; rows } as data) -> (
      let data = { data with time_elapsed = time_elapsed + 1 } in
      match time_elapsed with
      | time_elapsed when time_elapsed < 8 ->
          List.iter (D.a_flash_row ~io) rows;
          Ongoing data
      | time_elapsed when time_elapsed < 16 -> Ongoing data
      | time_elapsed when time_elapsed < 24 ->
          List.iter (D.a_flash_row ~io) rows;
          Ongoing data
      | time_elapsed when time_elapsed < 32 -> Ongoing data
      | time_elapsed when time_elapsed < 40 ->
          List.iter (D.a_flash_row ~io) rows;
          Ongoing data
      | _ -> Finished rows)
