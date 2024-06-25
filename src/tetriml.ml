open Common
open Gamelle

let () =
  Random.self_init ();
  Gamelle.run (Engine.init_state ()) @@ fun ~io state ->
  let new_state = Engine.update ~io state in
  Display.render ~io new_state;
  new_state
