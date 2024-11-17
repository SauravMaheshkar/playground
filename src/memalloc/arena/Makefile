CFLAGS := -I./
ARENA_OBJ_FILE := arena.o
ARENA_EXAMPLE := example

default: run-arena-example clean

run-arena-example: $(ARENA_OBJ_FILE)
	gcc -o $(ARENA_EXAMPLE) $(ARENA_OBJ_FILE) $(ARENA_EXAMPLE).c $(CFLAGS)
	./$(ARENA_EXAMPLE)

$(ARENA_OBJ_FILE): arena.c
	gcc -o $(ARENA_OBJ_FILE) -c arena.c

clean:
	rm -f $(ARENA_OBJ_FILE) $(ARENA_EXAMPLE)
