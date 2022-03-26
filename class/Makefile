
all: Hello.class parseHello

Hello.class: Hello.java
	javac @<

Hello: Hello.class
	java Hello

clean:
	rm -f *.class *.dump *.beam

parse_class.beam: parse_class.erl
	erlc $<

parseHello: parse_class.beam
	erl -noshell -s parse_class main Hello.class -s init stop
