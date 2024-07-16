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

void parse_and_callback(int level, const char *format, va_list args) {
  char buf[1024];
  int *argtypes;
  size_t num_args = parse_printf_format(format, 0, argtypes);

  callback(format, num_args, argtypes, args);
}

void (*callback)(const char *, size_t, int *, void *) = NULL;

void set_callback(void (*cb)(const char *, size_t, int *, void *)) {
  callback = cb;
}
