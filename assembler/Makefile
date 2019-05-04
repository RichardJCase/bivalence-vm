INCLUDES=-I$(VM_INC)
TARGET=bas

.PHONEY: $(LIBS) $(TEST)

OBJ=as.o

%.o: %.c
	$(CC) -o $@ $< $(CFLAGS) $(INCLUDES)

$(TARGET): $(OBJ) $(LIBS)
	$(CC) -o $@ $^ $(LIBS) $(LNK)

$(LIBS):

$(TEST):
	make -C test

clean:
	rm -f *.o *~ $(TARGET)

re: clean $(TARGET)

install: $(TARGET)
	mv $(TARGET) $(INSTALL_DIR)/$(TARGET)
	rm -f *.o *~

test: $(TARGET)
	valgrind -q ./$(TARGET)
