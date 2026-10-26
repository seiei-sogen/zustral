/* libdfp on top of GCC _Decimal128. Inputs are parsed at run time with
   strtod128; values are printed with the same BID decoder as d128.c so the
   three decimal128 implementations can be compared line by line. */
#include <fenv.h>
#include <float.h>
#include <math.h>
#include <stdlib.h>
#include "bid128.h"

static _Decimal128 num(const char *s) { return strtod128(s, NULL); }

int main(void) {
  static const struct { int mode; const char *name; } modes[] = {
    {FE_DEC_TONEAREST, "TONEAREST"}, {FE_DEC_TOWARDZERO, "TOWARDZERO"},
    {FE_DEC_UPWARD, "UPWARD"}, {FE_DEC_DOWNWARD, "DOWNWARD"},
    {FE_DEC_TONEARESTFROMZERO, "TONEARESTFROMZERO"},
  };
  _Decimal128 r, sum = num("0"), step = num("0.01"), ten = num("10");

  printf("sizeof=%zu\n", sizeof(_Decimal128));
  r = num("1") / num("3"); print_bid128("div_1_3", &r);
  r = num("2") / num("3"); print_bid128("div_2_3", &r);
  r = num("0.1") + num("0.2"); print_bid128("add_01_02", &r);
  for (size_t i = 0; i < sizeof modes / sizeof modes[0]; i++) {
    char key[64];
    printf("fe_dec_setround_%s=%d\n", modes[i].name, fe_dec_setround(modes[i].mode));
    r = quantized128(num("1.005"), num("0.01"));
    snprintf(key, sizeof key, "quantize_1005_%s", modes[i].name);
    print_bid128(key, &r);
  }
  fe_dec_setround(FE_DEC_TONEAREST);
  r = num("12345678901234567890123456789012345") * ten; print_bid128("mul_wide", &r);
  feclearexcept(FE_ALL_EXCEPT);
  r = DEC128_MAX * ten; print_bid128("overflow", &r);
  printf("overflow_fe_overflow=%d\n", fetestexcept(FE_OVERFLOW) != 0);
  for (int k = 0; k < 1000; k++) sum += step;
  print_bid128("sum_001_x1000", &sum);
  return 0;
}
