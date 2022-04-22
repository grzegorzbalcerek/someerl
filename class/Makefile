
F=Hello.class

all: $(F) inspectClass

%.class: %.java
	javac $<

Hello: Hello.class
	java Hello

clean:
	rm -f *.class *.dump *.beam

inspect_class.beam: inspect_class.erl
	erlc $<

inspectClass: inspect_class.beam
	erl -noshell -s inspect_class main $(F) -s init stop
