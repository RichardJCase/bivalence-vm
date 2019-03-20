NOWARNS=
WARNINGS=-Wall -Wextra -Wpedantic -Wunused-macros -Wunsafe-loop-optimizations -Wunsuffixed-float-constants -Wtrampolines -Wswitch-enum -Wswitch-default -Wstrict-prototypes -Wstack-protector -Wsign-conversion -Wnull-dereference -Wmultichar -Winline -Wduplicated-cond -Wconversion -Walloc-zero -Walloca $(NOWARNS)

DEFINES=-DMAX_THREADS=$(MAX_THREADS) -DPAGE_SIZE=$(PAGE_SIZE)

CFLAGS=$(OPLVL) -c $(WARNINGS) -Werror -Wfatal-errors $(DEFINES) -Iinclude
LINK=
TARGET=vm

OBJ=vm.o args.o bytecode.o cpu.o execute.o settings.o

%.o: %.c
	$(CC) -o $@ $< $(CFLAGS)

$(TARGET): $(OBJ)
	$(CC) -o $@ $^ $(LINK)

clean:
	rm -f *.o *~

re: clean $(TARGET)

install: $(TARGET)
	mv $(TARGET) $(INSTALL_DIR)/$(TARGET)
	rm -f *.o *~

test: $(INSTALL_DIR)/$(TARGET)
	valgrind -q $(TARGET) #todo unit tests
