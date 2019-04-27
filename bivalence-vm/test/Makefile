WARNINGS=-Wall -Wextra -Wpedantic -Wunused-macros -Wunsafe-loop-optimizations -Wunsuffixed-float-constants -Wtrampolines -Wswitch-enum -Wswitch-default -Wstrict-prototypes -Wstack-protector -Wsign-conversion -Wnull-dereference -Wmultichar -Winline -Wduplicated-cond -Wconversion -Walloc-zero -Walloca $(NOWARNS)

DEFINES=-DMAX_THREADS=$(MAX_THREADS) -DPAGE_SIZE=$(PAGE_SIZE) -DNUM_PAGE=$(NUM_PAGE) -DNUM_GP_REG=$(NUM_GP_REG) -DPRA=$(PRA)

LIBS=../lib/datastructures.a
LNK=-ldl
INCLUDE=-I../include
CFLAGS=-c $(WARNINGS) -Werror -Wfatal-errors $(DEFINES) $(INCLUDE)

TARGET=unit_main

.PHONEY: $(LIBS)

OBJ=unit_main.o set_test.o page_test.o execute_test.o ../paging.o ../execute.o

%.o: %.c
	$(CC) -o $@ $< $(CFLAGS)

$(TARGET): $(OBJ) $(TESTOBJ) $(LIBS) $(LNK)
	$(CC) -o $@ $^

$(LIBS):
	make -C ../datastructures

clean:
	rm -f *.o *~ $(TARGET) $(TEST_TARGET) test.b

re: clean $(TARGET)

test: $(TARGET)
	./create_program.py
	valgrind -q ./$(TARGET)
