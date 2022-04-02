
CLASSFILE=Hello.class

all: $(CLASSFILE) inspectClass

Hello.class: Hello.java
	javac $<

Hello: Hello.class
	java Hello

clean:
	rm -f *.class *.dump *.beam

inspect_class.beam: inspect_class.erl
	erlc $<

inspectClass: inspect_class.beam
	erl -noshell -s inspect_class main $(CLASSFILE) -s init stop
