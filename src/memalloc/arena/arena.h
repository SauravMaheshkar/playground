#ifndef ARENA_H
#define ARENA_H
#include <stddef.h>
#include <stdint.h>

typedef struct Arena {
  struct Arena *next;
  size_t capacity;
  size_t size;
  uint8_t *data;
} Arena;

extern Arena uint8_arena_init(size_t capacity);

extern void *uint8_arena_malloc(size_t size);

extern void *uint8_arena_alloc(Arena *arena, size_t size);

extern void uint8_arena_reset(Arena *arena);

extern void uint8_arena_free(Arena *arena);

extern void uint8_arena_print(Arena *arena);

#endif
