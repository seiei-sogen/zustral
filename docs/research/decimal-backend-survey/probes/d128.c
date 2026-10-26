/* GCC _Decimal128 without libdfp: the arithmetic comes from libgcc. */
#define __STDC_WANT_IEC_60559_DFP_EXT__ 1
#include <fenv.h>
#include <float.h>
#include "bid128.h"

/* libgcc keeps the BID exception flags in its own thread-local variable, which
   glibc's fetestexcept does not read. These are libgcc's internal accessors. */
extern void __dfp_clear_except(void);
extern int __dfp_test_except(int);

int main(void) {
  /* volatile keeps every operation a runtime libgcc call at -O2 as well. */
  volatile _Decimal128 one = 1.DL, two = 2.DL, three = 3.DL, p1 = 0.1DL, p2 = 0.2DL,
                       step = 0.01DL, ten = 10.DL, max = DEC128_MAX,
                       wide = 12345678901234567890123456789012345.DL;
  _Decimal128 r, sum = 0.DL;

  printf("sizeof=%zu\n", sizeof(_Decimal128));
  printf("DECIMAL_BID_FORMAT=%d\n", __DECIMAL_BID_FORMAT__);
  printf("DEC128_MANT_DIG=%d\n", DEC128_MANT_DIG);
  printf("DEC128_MIN_EXP=%d\n", DEC128_MIN_EXP);
  printf("DEC128_MAX_EXP=%d\n", DEC128_MAX_EXP);
  r = one / three; print_bid128("div_1_3", &r);
  r = two / three; print_bid128("div_2_3", &r);
  r = p1 + p2; print_bid128("add_01_02", &r);
  r = wide * ten; print_bid128("mul_wide", &r);
  feclearexcept(FE_ALL_EXCEPT);
  __dfp_clear_except();
  r = max * ten; print_bid128("overflow", &r);
  printf("overflow_fe_overflow=%d\n", fetestexcept(FE_OVERFLOW) != 0);
  printf("overflow_libgcc_dfp_test_except=%d\n", __dfp_test_except(FE_OVERFLOW) != 0);
  for (int k = 0; k < 1000; k++) sum += step;
  print_bid128("sum_001_x1000", &sum);
  return 0;
}
