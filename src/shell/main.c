#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdbool.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <sys/wait.h>
#include <unistd.h>

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
    } else if (strlen(cmd) > 0) {
      char *argv[64];
      int argc = 0;
      char *token = strtok(cmd, " \t\n");
      while (token && argc < 63) {
        argv[argc++] = token;
        token = strtok(NULL, " \t\n");
      }
      argv[argc] = NULL;
      if (argc > 0) {
        pid_t pid = fork();
        if (pid == 0) { // Kind
          execvp(argv[0], argv);
          perror("ERROR");
          exit(EXIT_FAILURE);
        } else if (pid > 0) { // Eltern
          int status;
          waitpid(pid, &status, 0);
        } else {
          perror("fork");
        }
      }
    }
    free(cmd);
    line++;
  }

  return 0;
}
