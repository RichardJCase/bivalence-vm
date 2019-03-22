WARNINGS=-Wall -Wextra -Wpedantic -Wunused-macros -Wunsafe-loop-optimizations -Wunsuffixed-float-constants -Wtrampolines -Wswitch-enum -Wswitch-default -Wstrict-prototypes -Wstack-protector -Wsign-conversion -Wnull-dereference -Wmultichar -Winline -Wduplicated-cond -Wconversion -Walloc-zero -Walloca $(NOWARNS)

INCLUDE=-I../include
CFLAGS=-c $(WARNINGS) -Werror -Wfatal-errors $(DEFINES) $(INCLUDE)

TARGET=../lib/datastructures.a
TEST_TARGET=unit_main

OBJ=set.o
TESTOBJ=unit_main.o set_test.o

%.o: %.c
	$(CC) -o $@ $< $(CFLAGS)

$(TARGET): $(OBJ)
	ar rc $@ $<
	ranlib $@

$(TEST_TARGET): $(OBJ) $(TESTOBJ)
	$(CC) -o $@ $^

clean:
	rm -f *.o *~ $(TARGET) $(TEST_TARGET)

re: clean $(TARGET)

install: $(TARGET)
	mv $(TARGET) $(INSTALL_DIR)/$(TARGET)
	rm -f *.o *~

test: $(TARGET)
	valgrind -q ./$(TARGET)
