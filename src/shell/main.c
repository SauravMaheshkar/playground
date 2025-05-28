#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <readline/readline.h>
#include <readline/history.h>

#define PROMPT "[spidey-sh]λ "

int main() {
  bool QUIT = false;
  char *cmd = NULL;
  size_t line = 0;

  using_history();

  while (!QUIT) {
    char prompt[32];
    snprintf(prompt, sizeof(prompt), PROMPT);
    cmd = readline(prompt);
    if (!cmd) {
      break; // EOF (Ctrl-D)
    }
    if (strlen(cmd) > 0) {
      add_history(cmd);
    }
    if (strcmp(cmd, "exit") == 0) {
      QUIT = true;
    }
    free(cmd);
    line++;
  }

  return 0;
}
