INCLUDES=-I$(VM_INC) -I$(COMP_INC)
TARGET=bas

.PHONEY: $(LIBS) $(TEST)

OBJ=as.o parsefuncs.o ../compiler/strings.o

%.o: %.c
	$(CC) -o $@ $< $(CFLAGS) $(INCLUDES)

$(TARGET): $(OBJ) $(LIBS)
	$(CC) -o $@ $^ $(LIBS) $(LNK)

$(LIBS):

$(TEST):
	make -C test

clean:
	rm -f $(OBJ) *~ $(TARGET) test*

re: clean $(TARGET)

install: $(TARGET)
	mv $(TARGET) $(INSTALL_DIR)/$(TARGET)
	rm -f *.o *~

test: $(TARGET)
	valgrind -q ./$(TARGET)
