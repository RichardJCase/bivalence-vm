LNK=-ldl -lm -lpthread
INCLUDES=-I$(VM_INC)
TARGET=bivalence_lib.a

OBJ=mem.o

%.o: %.c
	$(CC) -o $@ $< $(CFLAGS) $(INCLUDES)

$(TARGET): $(OBJ)
	ar -rcs $@ $^
	ranlib $@

clean:
	rm -f *.o *~ $(TARGET)

re: clean $(TARGET)

install: $(TARGET)
	mv $(TARGET) $(INSTALL_DIR)/$(TARGET)
	rm -f *.o *~
