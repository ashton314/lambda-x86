#include <stdio.h>

#define fixnum_mask  3
#define fixnum_tag   0
#define fixnum_shift 2

#define empty_list   47

#define char_tag     15
#define char_mask    255
#define char_shift   8

#define bool_tag     31
#define bool_mask    127
#define bool_shift   7

int main(int argc, char** argv) {
  int val = scheme_entry();

  if ((val & fixnum_mask) == fixnum_tag) {
    printf("%d\n", val >> fixnum_shift);
  }
  else if (val == empty_list) {
    printf("()\n");
  }
  else if ((val & char_mask) == char_tag) {
    printf("%c\n", val >> char_shift);
  }
  else if ((val & bool_mask) == bool_tag) {
    printf((val >> bool_shift) ? "#t" : "#f");
  }
  else {
    printf("#<unknown value>\n");
  }
  return 0;
}
