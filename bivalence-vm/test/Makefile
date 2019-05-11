LNK=-ldl
INCLUDES=-I$(VM_INC)
VMOBJS=../paging.o ../execute.o ../args.o

TARGET=unit_main

OBJ=unit_main.o page_test.o execute_test.o $(VMOBJS)

%.o: %.c
	$(CC) -o $@ $< $(CFLAGS) $(INCLUDES)

$(TARGET): $(OBJ) $(TESTOBJ) $(LIBS) $(LNK)
	$(CC) -o $@ $^

clean:
	rm -f *.o *~ $(TARGET) $(TEST_TARGET) test.b

re: clean $(TARGET)

test: $(TARGET)
	./create_program.py
	valgrind -q ./$(TARGET)
