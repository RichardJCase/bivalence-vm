WARNINGS=-Werror -Wfatal-errors -Wall -Wextra -Wpedantic -Wunused-macros -Wunsafe-loop-optimizations -Wunsuffixed-float-constants -Wtrampolines -Wswitch-enum -Wswitch-default -Wstrict-prototypes -Wstack-protector -Wsign-conversion -Wnull-dereference -Wmultichar -Winline -Wduplicated-cond -Wconversion -Walloc-zero -Walloca $(NOWARNS)

DEFINES=-DMAX_THREADS=$(MAX_THREADS) -DPAGE_SIZE=$(PAGE_SIZE)

LIBS=lib/*.a
INCLUDES=-Iinclude
CFLAGS=$(OPLVL) -c $(WARNINGS) $(DEFINES) $(INCLUDES) -fsplit-stack
TARGET=vm

.PHONEY: $(LIBS)

OBJ=vm.o args.o bytecode.o cpu.o execute.o settings.o

%.o: %.c
	$(CC) -o $@ $< $(CFLAGS)

$(TARGET): $(OBJ) $(LIBS)
	$(CC) -o $@ $^ $(LIBS)

$(LIBS):
	make -C datastructures

clean:
	rm -f *.o *~ $(TARGET)
	rm -f lib/*.a

re: clean $(TARGET)

install: $(TARGET)
	mv $(TARGET) $(INSTALL_DIR)/$(TARGET)
	rm -f *.o *~

test: $(INSTALL_DIR)/$(TARGET)
	valgrind -q $(TARGET)
