CFLAGS := -I./ -lreadline
EXEC := spidey-sh.out

default: build

build:
	gcc -o $(EXEC) main.c $(CFLAGS)
	./$(EXEC)

clean:
	rm -f $(EXEC)
