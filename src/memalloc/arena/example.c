#include <arena.h>
#include <stdio.h>

int main() {
  // Initialise a uint8 arena region
  printf("Initialising a Arena with capacity 1024\n");
  Arena arena = uint8_arena_init(1024);
  uint8_arena_print(&arena);

  // Set / Increase it's size
  printf("Making space for 42 objects\n");
  void *init_data = uint8_arena_alloc(&arena, 42);
  uint8_arena_print(&arena);
  printf("Making space for 2 more objects\n");
  void *more_data = uint8_arena_alloc(&arena, 2);
  uint8_arena_print(&arena);

  // Incase of overflow create a new arena
  printf("Address of next arena pre-overflow:[%p]\n", arena.next);
  void *overflow = uint8_arena_alloc(&arena, 2048);
  uint8_arena_print(&arena);
  printf("Address of next arena post-overflow:[%p]\n", arena.next);

  // Reset Arena
  printf("Resetting Arena this will set the size to 0\n");
  uint8_arena_reset(&arena);
  uint8_arena_print(&arena);

  // Free Arena
  printf("Freeing Arena this will set size and capacity to 0 and free memory "
         "in data\n");
  uint8_arena_free(&arena);
  uint8_arena_print(&arena);
  return 0;
}
