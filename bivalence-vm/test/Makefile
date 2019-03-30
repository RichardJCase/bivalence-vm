WARNINGS=-Wall -Wextra -Wpedantic -Wunused-macros -Wunsafe-loop-optimizations -Wunsuffixed-float-constants -Wtrampolines -Wswitch-enum -Wswitch-default -Wstrict-prototypes -Wstack-protector -Wsign-conversion -Wnull-dereference -Wmultichar -Winline -Wduplicated-cond -Wconversion -Walloc-zero -Walloca $(NOWARNS)

DEFINES=-DMAX_THREADS=$(MAX_THREADS) -DPAGE_SIZE=$(PAGE_SIZE) -DNUM_PAGE=$(NUM_PAGE) -DNUM_GP_REG=$(NUM_GP_REG)

LIBS=../lib/datastructures.a
INCLUDE=-I../include
CFLAGS=-c $(WARNINGS) -Werror -Wfatal-errors $(DEFINES) $(INCLUDE)

TARGET=unit_main

OBJ=unit_main.o set_test.o page_test.o execute_test.o

%.o: %.c
	$(CC) -o $@ $< $(CFLAGS)

$(TARGET): $(OBJ) $(TESTOBJ) $(LIBS)
	$(CC) -o $@ $^

clean:
	rm -f *.o *~ $(TARGET) $(TEST_TARGET)

re: clean $(TARGET)

test: $(TARGET)
	valgrind -q ./$(TARGET)
