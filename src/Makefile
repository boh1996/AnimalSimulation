.PHONY: all clean

PNAME=./
TESTFILE=./test
GAMEFILE=./simulation
CC=fsharpc
CFLAGS= --warnaserror+ --nologo

all: $(PNAME).dll

$(PNAME).dll: 
	$(CC) $(CFLAGS) $(GAMEFILE).fsx && $(CC) $(CFLAGS) $(TESTFILE).fsx


withdocs: 
	$(CC) $(CFLAGS) --doc:./animalDocs.xml $(GAMEFILE).fsx  &&  $(CC) $(CFLAGS) $(TESTFILE).fsx

clean:
	rm -f $(TESTFILE).exe && rm -f $(GAMEFILE).exe
