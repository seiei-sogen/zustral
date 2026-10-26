/* Intel DFP (libbid) built with CALL_BY_REF=0 GLOBAL_RND=0 GLOBAL_FLAGS=0:
   values are passed by value, rounding mode and flags are explicit arguments. */
#include <stdio.h>
#include "bid_conf.h"
#include "bid_functions.h"

static _IDEC_flags ignored;

static BID_UINT128 num(const char *s) {
  return bid128_from_string((char *)s, BID_ROUNDING_TO_NEAREST, &ignored);
}

static void print(const char *key, BID_UINT128 v) {
  char s[64];
  bid128_to_string(s, v, &ignored);
  printf("%s=%s\n", key, s);
}

static void print_flags(const char *key, _IDEC_flags f) {
  static const struct { _IDEC_flags bit; const char *name; } names[] = {
    {BID_INVALID_EXCEPTION, "invalid"}, {BID_ZERO_DIVIDE_EXCEPTION, "zero_divide"},
    {BID_OVERFLOW_EXCEPTION, "overflow"}, {BID_UNDERFLOW_EXCEPTION, "underflow"},
    {BID_INEXACT_EXCEPTION, "inexact"},
  };
  printf("%s=", key);
  const char *sep = "";
  for (size_t i = 0; i < sizeof names / sizeof names[0]; i++)
    if (f & names[i].bit) { printf("%s%s", sep, names[i].name); sep = ","; }
  printf("\n");
}

int main(void) {
  static const struct { _IDEC_round mode; const char *name; } modes[] = {
    {BID_ROUNDING_TO_NEAREST, "TO_NEAREST"}, {BID_ROUNDING_DOWN, "DOWN"},
    {BID_ROUNDING_UP, "UP"}, {BID_ROUNDING_TO_ZERO, "TO_ZERO"},
    {BID_ROUNDING_TIES_AWAY, "TIES_AWAY"},
  };
  const _IDEC_round rn = BID_ROUNDING_TO_NEAREST;
  BID_UINT128 one = num("1"), two = num("2"), three = num("3"), ten = num("10");
  _IDEC_flags f;

  printf("sizeof=%zu\n", sizeof(BID_UINT128));
  print("div_1_3", bid128_div(one, three, rn, &ignored));
  print("div_2_3", bid128_div(two, three, rn, &ignored));
  print("add_01_02", bid128_add(num("0.1"), num("0.2"), rn, &ignored));
  for (size_t i = 0; i < sizeof modes / sizeof modes[0]; i++) {
    char key[64];
    snprintf(key, sizeof key, "quantize_1005_%s", modes[i].name);
    print(key, bid128_quantize(num("1.005"), num("0.01"), modes[i].mode, &ignored));
  }
  print("mul_wide", bid128_mul(num("12345678901234567890123456789012345"), ten, rn, &ignored));
  f = 0;
  print("overflow", bid128_mul(num("9999999999999999999999999999999999E+6111"), ten, rn, &f));
  print_flags("overflow_flags", f);
  BID_UINT128 sum = num("0"), step = num("0.01");
  for (int k = 0; k < 1000; k++) sum = bid128_add(sum, step, rn, &ignored);
  print("sum_001_x1000", sum);
  return 0;
}
