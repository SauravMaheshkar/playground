#include <assert.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "arena.h"

Arena uint8_arena_init(size_t capacity) {
  void *data = malloc(sizeof(uint8_t) * capacity);
  Arena arena = {
      .capacity = capacity,
      .size = 0,
      .data = data,
  };
  return arena;
}

void *uint8_arena_malloc(size_t size) { return malloc(sizeof(uint8_t) * size); }

void *uint8_arena_alloc(Arena *arena, size_t size) {
  Arena *current = arena;
  while (!(current->size + size <= current->capacity)) {
    if (current->next == NULL) {
      Arena *next = uint8_arena_malloc(sizeof(Arena));
      Arena clone = uint8_arena_init(arena->capacity + size);
      memcpy(next, &clone, sizeof(Arena));
      current->next = next;
    }
    current = current->next;
  }

  uint8_t *data = &current->data[current->size];
  current->size += size;
  return data;
}

void uint8_arena_reset(Arena *arena) {
  Arena *current = arena;
  while (current != NULL) {
    current->size = 0;
    current = current->next;
  }
}

void uint8_arena_free(Arena *arena) {
  arena->capacity = 0;
  arena->size = 0;
  free(arena->data);
  Arena *current = arena->next;
  while (current != NULL) {
    Arena *tmp = current->next;
    free(current->data);
    free(current);
    current = tmp;
  }
  arena->next = NULL;
}

void uint8_arena_print(Arena *arena) {
  printf("Capacity %zu, Size: %zu, Memory Location:[%p]\n", arena->capacity,
         arena->size, &arena);
}
