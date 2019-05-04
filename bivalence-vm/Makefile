LNK=-ldl
INCLUDES=-Iinclude
TARGET=bvm

.PHONEY: $(LIBS) $(TEST)

OBJ=vm.o args.o paging.o execute.o

%.o: %.c
	$(CC) -o $@ $< $(CFLAGS) $(INCLUDES)

$(TARGET): $(OBJ) $(LIBS)
	$(CC) -o $@ $^ $(LIBS) $(LNK)

$(LIBS):

$(TEST):
	make -C test

clean:
	rm -f *.o *~ $(TARGET)
	rm -f lib/*.a
	make -C test clean

re: clean $(TARGET)

install: $(TARGET)
	mv $(TARGET) $(INSTALL_DIR)/$(TARGET)
	rm -f *.o *~

test: $(TARGET)
	valgrind -q ./$(TARGET)
