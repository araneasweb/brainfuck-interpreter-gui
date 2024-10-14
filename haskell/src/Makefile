SRC = Main.hs
TARGET = brainfuck
GHC_FLAGS = -O2 --make

all: $(TARGET)

$(TARGET): $(SRC)
	ghc $(GHC_FLAGS) -o $(TARGET) $(SRC)

clean:
	rm -f $(TARGET) *.hi *.o
