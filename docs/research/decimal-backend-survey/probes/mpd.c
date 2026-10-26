#include <mpdecimal.h>
#include <stdio.h>
#include <stdlib.h>

static long allocs;
static void *count_malloc(size_t n) { allocs++; return malloc(n); }
static void *count_calloc(size_t m, size_t n) { allocs++; return calloc(m, n); }
static void *count_realloc(void *p, size_t n) { allocs++; return realloc(p, n); }

static mpd_context_t ctx;

static mpd_t *num(const char *s) {
  uint32_t status = 0;
  mpd_t *d = mpd_qnew();
  mpd_qset_string(d, s, &ctx, &status);
  return d;
}

static void print(const char *key, const mpd_t *d) {
  char *s = mpd_to_sci(d, 1);
  printf("%s=%s\n", key, s);
  mpd_free(s);
}

int main(void) {
  mpd_mallocfunc = count_malloc;
  mpd_callocfunc = count_calloc;
  mpd_reallocfunc = count_realloc;
  mpd_ieee_context(&ctx, MPD_DECIMAL128);

  uint32_t status = 0;
  char flags[MPD_MAX_FLAG_LIST];
  printf("mpd_version=%s\n", mpd_version());
  printf("sizeof_mpd_t=%zu\n", sizeof(mpd_t));
  printf("MPD_MAX_PREC=%lld\n", (long long)MPD_MAX_PREC);
  printf("MPD_MAX_EMAX=%lld\n", (long long)MPD_MAX_EMAX);
  printf("MPD_MIN_EMIN=%lld\n", (long long)MPD_MIN_EMIN);
  printf("ctx128_prec=%lld\n", (long long)ctx.prec);
  printf("ctx128_emax=%lld\n", (long long)ctx.emax);
  printf("ctx128_emin=%lld\n", (long long)ctx.emin);
  printf("ctx128_round=%s\n", mpd_round_string[ctx.round]);

  mpd_t *one = num("1"), *two = num("2"), *three = num("3");
  allocs = 0;
  mpd_t *r = mpd_qnew();
  printf("alloc_qnew=%ld\n", allocs);
  allocs = 0;
  mpd_qdiv(r, one, three, &ctx, &status);
  printf("alloc_qdiv_1_3=%ld\n", allocs);
  print("div_1_3", r);

  mpd_uint_t data[MPD_MINALLOC_MAX];
  mpd_t stack_r = {MPD_STATIC | MPD_STATIC_DATA, 0, 0, 0, MPD_MINALLOC_MAX, data};
  allocs = 0;
  mpd_qdiv(&stack_r, one, three, &ctx, &status);
  printf("alloc_qdiv_1_3_static_result=%ld\n", allocs);

  mpd_qdiv(r, two, three, &ctx, &status);
  print("div_2_3", r);
  mpd_t *p1 = num("0.1"), *p2 = num("0.2");
  mpd_qadd(r, p1, p2, &ctx, &status);
  print("add_01_02", r);

  mpd_t *x = num("1.005"), *q = num("0.01");
  for (int mode = 0; mode < MPD_ROUND_GUARD; mode++) {
    mpd_context_t c = ctx;
    char key[64];
    c.round = mode;
    mpd_qquantize(r, x, q, &c, &status);
    snprintf(key, sizeof key, "quantize_1005_%s", mpd_round_string[mode]);
    print(key, r);
  }

  mpd_t *wide = num("12345678901234567890123456789012345"), *ten = num("10");
  mpd_qmul(r, wide, ten, &ctx, &status);
  print("mul_wide", r);

  mpd_t *max = num("9.999999999999999999999999999999999E+6144");
  status = 0;
  mpd_qmul(r, max, ten, &ctx, &status);
  print("overflow", r);
  mpd_snprint_flags(flags, sizeof flags, status);
  printf("overflow_status=%s\n", flags);

  mpd_t *sum = num("0"), *step = num("0.01");
  for (int k = 0; k < 1000; k++) mpd_qadd(sum, sum, step, &ctx, &status);
  print("sum_001_x1000", sum);

  mpd_del(one); mpd_del(two); mpd_del(three); mpd_del(r); mpd_del(p1); mpd_del(p2);
  mpd_del(x); mpd_del(q); mpd_del(wide); mpd_del(ten); mpd_del(max); mpd_del(sum); mpd_del(step);
  mpd_del(&stack_r);
  return 0;
}
