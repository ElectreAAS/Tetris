let () =
  Random.self_init ();
  Gamelle.run (Engine.init_state ()) @@ fun ~io state ->
  let new_state = Engine.update ~io state in
  Display.display ~io new_state;
  new_state
