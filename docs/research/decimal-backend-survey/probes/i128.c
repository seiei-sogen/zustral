/* Fixed point on __int128 with scale 10^18. Rounding is C integer division
   (toward zero); any other rounding mode would be written by the implementer. */
#include <stdio.h>
#include <string.h>

typedef __int128 fx;
#define SCALE_DIGITS 18
static const fx SCALE = (fx)1000000000000000000LL;
static const fx FX_MAX = (fx)(((unsigned __int128)1 << 127) - 1);

static void print_int(const char *key, fx v) {
  char buf[64];
  int i = sizeof buf - 1, neg = v < 0;
  unsigned __int128 u = neg ? -(unsigned __int128)v : (unsigned __int128)v;
  buf[i] = '\0';
  do { buf[--i] = (char)('0' + (int)(u % 10)); u /= 10; } while (u);
  printf("%s=%s%s\n", key, neg ? "-" : "", buf + i);
}

static void print_fx(const char *key, fx v) {
  int neg = v < 0;
  unsigned __int128 u = neg ? -(unsigned __int128)v : (unsigned __int128)v;
  unsigned __int128 ip = u / (unsigned __int128)SCALE, fp = u % (unsigned __int128)SCALE;
  char ibuf[64], fbuf[SCALE_DIGITS + 1];
  int i = sizeof ibuf - 1;
  ibuf[i] = '\0';
  do { ibuf[--i] = (char)('0' + (int)(ip % 10)); ip /= 10; } while (ip);
  for (int k = SCALE_DIGITS - 1; k >= 0; k--) { fbuf[k] = (char)('0' + (int)(fp % 10)); fp /= 10; }
  fbuf[SCALE_DIGITS] = '\0';
  printf("%s=%s%s.%s\n", key, neg ? "-" : "", ibuf + i, fbuf);
}

/* Parses a non-negative decimal string. Returns 0 if it does not fit. */
static int parse(const char *s, fx *out) {
  fx v = 0;
  int frac = -1;
  for (; *s; s++) {
    if (*s == '.') { frac = 0; continue; }
    if (frac >= SCALE_DIGITS) continue;
    if (__builtin_mul_overflow(v, 10, &v) || __builtin_add_overflow(v, *s - '0', &v)) return 0;
    if (frac >= 0) frac++;
  }
  for (int k = frac < 0 ? 0 : frac; k < SCALE_DIGITS; k++)
    if (__builtin_mul_overflow(v, 10, &v)) return 0;
  *out = v;
  return 1;
}

static fx fx_div(fx a, fx b) { return a * SCALE / b; }

static fx quantize(fx v, int places) {
  fx unit = 1;
  for (int k = 0; k < SCALE_DIGITS - places; k++) unit *= 10;
  return v / unit * unit;
}

int main(void) {
  fx one, two, three, a, b, x, sum = 0, step, wide, r;
  parse("1", &one); parse("2", &two); parse("3", &three);

  printf("sizeof=%zu\n", sizeof(fx));
  print_int("int128_max", FX_MAX);
  print_fx("max_value", FX_MAX);
  print_fx("div_1_3", fx_div(one, three));
  print_fx("div_2_3", fx_div(two, three));
  parse("0.1", &a); parse("0.2", &b);
  print_fx("add_01_02", a + b);
  parse("1.005", &x);
  print_fx("quantize_1005_c_division", quantize(x, 2));
  if (parse("12345678901234567890123456789012345", &wide)) print_fx("mul_wide", wide * 10);
  else printf("mul_wide=overflow_detected\n");
  print_fx("overflow_wrapped", FX_MAX * 10);
  printf("overflow_builtin_detected=%d\n", __builtin_mul_overflow(FX_MAX, 10, &r));
  parse("0.01", &step);
  for (int k = 0; k < 1000; k++) sum += step;
  print_fx("sum_001_x1000", sum);
  return 0;
}
