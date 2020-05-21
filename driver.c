#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define fixnum_mask  3
#define fixnum_tag   0
#define fixnum_shift 2

#define cons_tag     1
#define vector_tag   2
#define string_tag   3
#define symb_tag     5
#define closure_tag  6

#define empty_list   47

#define char_tag     15
#define char_mask    255
#define char_shift   8

#define bool_tag     31
#define bool_mask    127
#define bool_shift   7

#define heap_size    8192

size_t scheme_entry(size_t *heap);
void format_val(size_t val);

int main(int argc, char** argv) {
  size_t *heap = malloc(heap_size);
  size_t val = scheme_entry(heap);

  format_val(val);
  return 0;
}

void format_val(size_t val) {
  if ((val & bool_mask) == bool_tag) {
    printf((val >> bool_shift) ? "#t" : "#f");
  }
  else if ((val & fixnum_mask) == fixnum_tag) {
    printf("%zu", val >> fixnum_shift);
  }
  else if ((val & fixnum_mask) == closure_tag) {
    printf("#<closure %zx>", val);
  }
  else if ((val & fixnum_mask) == cons_tag) {
    val--;
    size_t car = *((size_t*)val);
    size_t cdr = *((size_t*)val + 1);
    printf("("); format_val(car); printf(" . "); format_val(cdr); printf(")");
  }
  else if (val == empty_list) {
    printf("()");
  }
  /* else if ((val & char_mask) == char_tag) { */
  /*   printf("%c", val >> char_shift); */
  /* } */
  else {
    printf("#<unknown value: %zx>", val);
  }
}
