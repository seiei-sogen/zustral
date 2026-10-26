/* Prints a 16-byte BID-encoded decimal128 as <sign><coefficient>E<exponent>.
   glibc printf has no _Decimal128 conversion, so the probes decode the bits. */
#include <stdio.h>
#include <string.h>

static void print_bid128(const char *key, const void *p) {
  unsigned __int128 v, coef;
  int exp;
  memcpy(&v, p, sizeof v);
  char sign = (v >> 127) ? '-' : '+';
  unsigned top5 = (unsigned)(v >> 122) & 0x1f;
  if (top5 == 0x1e) { printf("%s=%cinf\n", key, sign); return; }
  if (top5 == 0x1f) { printf("%s=%cnan\n", key, sign); return; }
  const unsigned __int128 max_coef = (unsigned __int128)10000000000000000ULL * 1000000000000000000ULL - 1;
  if (((v >> 125) & 3) == 3) {
    exp = (int)((v >> 111) & 0x3fff);
    coef = 0;
  } else {
    exp = (int)((v >> 113) & 0x3fff);
    coef = v & (((unsigned __int128)1 << 113) - 1);
    if (coef > max_coef) coef = 0;
  }
  char buf[48];
  int i = sizeof buf - 1;
  buf[i] = '\0';
  do { buf[--i] = (char)('0' + (int)(coef % 10)); coef /= 10; } while (coef);
  printf("%s=%c%sE%+d\n", key, sign, buf + i, exp - 6176);
}
