.PHONY: all clean

PNAME=./src/modules/Sudoku/Sudoku
DLLTARGET=./src/Sudoku
GAMEFILE=./src/game
CC=fsharpc
CFLAGS=-a --warnaserror+ --nologo

all: $(PNAME).dll

$(PNAME).dll: $(PNAME).fsi $(PNAME).fs
	$(CC) $(CFLAGS) -o $(DLLTARGET).dll $(PNAME).fsi $(PNAME).fs && $(CC) --warnaserror+ --nologo -o $(GAMEFILE).exe -r $(DLLTARGET).dll $(GAMEFILE).fsx


build: $(PNAME).dll $(GAMEFILE).fsx
	$(CC) --warnaserror+ --nologo -o $(GAMEFILE).exe -r $(PNAME).dll $(GAMEFILE).fsx

dll: $(PNAME).fsi $(PNAME).fs 
	$(CC) $(CFLAGS) -o $(DLLTARGET).dll $(PNAME).fsi $(PNAME).fs 

withdocs: $(PNAME).fsi $(PNAME).fs
	$(CC) $(CFLAGS) --doc:./src/SudokuDocs.xml -o $(DLLTARGET).dll $(PNAME).fsi $(PNAME).fs && $(CC) --warnaserror+ --nologo -o $(GAMEFILE).exe -r $(DLLTARGET).dll $(GAMEFILE).fsx

clean:
	rm -f $(DLLTARGET).dll && rm -f $(GAMEFILE).exe
