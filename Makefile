FC = mpif90

TARGET = sample
OBJS = main.o subroutine_ex.o

.SUFFIXES: .f90

%.o: %.f90
	$(COMPILE.f) $(OUTPUT_OPTION) $<

#%.mod: %.f90 %.o
#	@:

$(TARGET): $(OBJS)
	$(LINK.f) $^ $(LOADLIBES) $(LDLIBS) -o $@

#main.o: submod.mod 

clean:
	rm -f *.o sample