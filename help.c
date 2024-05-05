#include <stdarg.h>
#include <stdio.h>
#include <string.h>

void log_printf(int level, const char *format, va_list args) {
  char buf[1024];
  strcat(buf, "[LIBSEAT]: ");
  strcat(buf, format);
  strcat(buf, "\n");
  vfprintf(stderr, buf, args);
}


// TODO: Feeble attempt at the variadics parsing
// Gave up - and left it to rot

// #include <stdlib.h>
// #include <stdarg.h>
// #include <string.h>
// #include <regex.h>

// #define ARRAY_SIZE(arr) (sizeof((arr)) / sizeof((arr)[0]))

// static void (*libseat_log_func)(int level, const char *format, void *data);
// static void extract_va(int level, const char *format, ...) {
  // va_list args;
  // size_t matches = 0;
  // regex_t regex;
  // regmatch_t pmatch[1];
  // const char *s = format;

  // if (regcomp(&regex, "(%s|%d)", 0)) exit(1);

  // // NOTE: Count the number of matches.
  // for (int i = 0; ; i++) {
    // if (regexec(&regex, s, ARRAY_SIZE(pmatch), pmatch, 0)) break;

    // matches++;
    // s += pmatch[0].rm_eo;
  // }

  // void *values = malloc(matches * sizeof(void *));
  // va_start(args, format);
  // for (int i=0; i<matches; i++) {
    // values[i] = &va_arg(args, void *);
  // }
  // va_end(args);
  // return libseat_log_func(level, format, values);
// }
