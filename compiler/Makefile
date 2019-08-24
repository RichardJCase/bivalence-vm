TARGET=bivalence

$(TARGET):
	ghc bivalence.hs -O2

all: $(TARGET)

install: $(TARGET)
	mv $(TARGET) $(INSTALL_DIR)/$(TARGET)
	rm -f *.o *.hi

clean:
	rm -f *.o *.hi bivalence
