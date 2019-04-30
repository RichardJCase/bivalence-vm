WARNINGS=-Werror -Wfatal-errors -Wall -Wextra -Wpedantic -Wunused-macros -Wunsafe-loop-optimizations -Wunsuffixed-float-constants -Wtrampolines -Wswitch-enum -Wswitch-default -Wstrict-prototypes -Wstack-protector -Wsign-conversion -Wnull-dereference -Wmultichar -Winline -Wduplicated-cond -Wconversion -Walloc-zero -Walloca $(NOWARNS)

DEFINES=-DMAX_THREADS=$(MAX_THREADS) -DPAGE_SIZE=$(PAGE_SIZE) -DNUM_PAGE=$(NUM_PAGE) -DNUM_GP_REG=$(NUM_GP_REG) -DPRA=$(PRA) -DLOG_FILE=$(LOG_FILE)

#LIBS=lib/*.a
LNK=-ldl
INCLUDES=-Iinclude
CFLAGS=$(OPLVL) -c $(WARNINGS) $(DEFINES) $(INCLUDES) -fsplit-stack
TARGET=bvm

.PHONEY: $(LIBS) $(TEST)

OBJ=vm.o args.o paging.o execute.o

%.o: %.c
	$(CC) -o $@ $< $(CFLAGS)

$(TARGET): $(OBJ) $(LIBS)
	$(CC) -o $@ $^ $(LIBS) $(LNK)

$(LIBS):

$(TEST):
	make -C test

clean:
	rm -f *.o *~ $(TARGET)
	rm -f lib/*.a
	make -C test clean
	make -C datastructures clean

re: clean $(TARGET)

install: $(TARGET)
	mv $(TARGET) $(INSTALL_DIR)/$(TARGET)
	rm -f *.o *~

test: $(TARGET)
	valgrind -q ./$(TARGET)
