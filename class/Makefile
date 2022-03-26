
all: Hello.class inspectHello

Hello.class: Hello.java
	javac $<

Hello: Hello.class
	java Hello

clean:
	rm -f *.class *.dump *.beam

inspect_class.beam: inspect_class.erl
	erlc $<

inspectHello: inspect_class.beam
	erl -noshell -s inspect_class main Hello.class -s init stop
