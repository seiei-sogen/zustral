/* C side of Probe.aum. It is a separate translation unit because the library
   headers conflict with the declarations in the generated C. Decimal values
   cross the FFI only as pointers to 16-byte slots or to mpd_t. */
#include <mpdecimal.h>
#include <stdint.h>
#include "../bid128.h"

static mpd_context_t *ctx(void) {
  static mpd_context_t c;
  static int ready;
  if (!ready) { mpd_ieee_context(&c, MPD_DECIMAL128); ready = 1; }
  return &c;
}

int32_t shim_mpd_set(mpd_t *d, const char *s) {
  uint32_t status = 0;
  mpd_qset_string(d, s, ctx(), &status);
  return 0;
}

int32_t shim_mpd_add(mpd_t *r, const mpd_t *a, const mpd_t *b) {
  uint32_t status = 0;
  mpd_qadd(r, a, b, ctx(), &status);
  return 0;
}

int32_t shim_mpd_print(const char *key, const mpd_t *d) {
  char *s = mpd_to_sci(d, 1);
  printf("%s=%s\n", key, s);
  mpd_free(s);
  return 0;
}

/* Builds the BID bit pattern directly: biased exponent above the coefficient. */
int32_t shim_d128_set(void *slot, int64_t coefficient, int32_t exponent) {
  unsigned __int128 bits = ((unsigned __int128)(exponent + 6176) << 113) | (unsigned __int128)coefficient;
  memcpy(slot, &bits, sizeof bits);
  return 0;
}

int32_t shim_d128_add(void *r, const void *a, const void *b) {
  _Decimal128 x, y, z;
  memcpy(&x, a, sizeof x);
  memcpy(&y, b, sizeof y);
  z = x + y;
  memcpy(r, &z, sizeof z);
  return 0;
}

int32_t shim_d128_print(const char *key, const void *slot) {
  print_bid128(key, slot);
  return 0;
}

static const __int128 SCALE = 1000000000000000000LL;

int32_t shim_i128_set(void *slot, int64_t coefficient, int32_t exponent) {
  __int128 v = coefficient;
  for (int k = 0; k < 18 + exponent; k++) v *= 10;
  memcpy(slot, &v, sizeof v);
  return 0;
}

int32_t shim_i128_add(void *r, const void *a, const void *b) {
  __int128 x, y, z;
  memcpy(&x, a, sizeof x);
  memcpy(&y, b, sizeof y);
  z = x + y;
  memcpy(r, &z, sizeof z);
  return 0;
}

/* shortcut: prints only non-negative values below 2^63 * 10^18; i128.c has the full printer. */
int32_t shim_i128_print(const char *key, const void *slot) {
  __int128 v;
  memcpy(&v, slot, sizeof v);
  printf("%s=%lld.%018lld\n", key, (long long)(v / SCALE), (long long)(v % SCALE));
  return 0;
}
