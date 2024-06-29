let full_init () =
  Random.self_init ();
  (Engine.init (), Controls.init (), Anim.init ())

let () =
  Random.self_init ();
  Gamelle.run (full_init ()) @@ fun ~io (game, controls, anim) ->
  let game, anim =
    if Anim.is_busy anim then (game, anim)
    else Engine.update ~io game controls anim
  in
  let anim = Render.display ~io game anim in
  (game, controls, anim)
