.PHONY: run html

run:
	dune build --root=. --watch _build/default/src/tetriml.cmxs & \
		export DUNE_PID=$$! ; \
		gamelle hotreload _build/default/src/tetriml.cmxs ; \
		kill $${DUNE_PID}

html:
	dune build --root=. --profile=release
	xdg-open _build/default/tetriml.html
