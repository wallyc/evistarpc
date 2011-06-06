all: compile

compile: 
	(erl -make)
	cp src/evistarpc.app.src ebin/evistarpc.app

clean:
	rm -f ./ebin/*.beam ./ebin/*.app ./doc/*.html ./doc/edoc-info \
	./doc/*.html ./doc/stylesheet.css ./doc/erlang.png

docs: 
	erl -noshell -run edoc_run application "evistarpc" '"."' [] -s init stop

