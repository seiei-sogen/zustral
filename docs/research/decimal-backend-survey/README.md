# 10進バックエンドの実像（issue #1）

Zustral に Decimal を持ち込むとき、実行時表現の候補が実際にどう違うかを並べた調査レポートである。
対象は mpdecimal、IEEE 754-2008 decimal128（GCC の `_Decimal128`、libdfp、Intel DFP）、`__int128` 固定小数点の3系統である。
観点は、確保と解放、精度と丸め、C ABI と Zustral からの呼び出し、依存とライセンスとビルドと移植性、決定性の5つである。

このレポートはバックエンドを選ばない。
選定は issue #4（0004）、#5（0005）、#8（0008）の仕事で、ここでは各問いに効く事実を名指しするところまでにとどめる。

## 読み方

事実を述べる文には、次のどれかの根拠を付ける。

- `〔probe: <記録> / <key>〕` は、`expected/<記録>.txt` の `<key>=` の行を指す。この行は `run.sh` が再実行して照合する。
- `〔run: <行>〕` は、`run.sh` が表示する結果行を指す。未実行になったプローブとその理由は、この形で引く。
- `〔src: <ID>〕` は、末尾の「一次資料」の表の行を指す。表に URL と版がある。
- 「出典のみ・未実測」は、このマシン（x86_64 Linux）では観測していない主張に付ける。

根拠を示せない内容は、本文ではなく「未確認事項」に置いた。

## 再実行の方法

```sh
docs/research/decimal-backend-survey/run.sh
```

どのディレクトリから起動してもよい。
前提は次のとおりである。

- x86_64 Linux と、flakes を有効にした nix。調査に使ったマシンの nix は 2.34.6 である。
- ホストの bash、coreutils、diffutils、grep、sed、curl、tar、gzip。
- ネットワーク接続。nixpkgs のパッケージは nix のストアに残るので取得は初回だけだが、libdfp と Intel DFP の tarball は一時ディレクトリに置くので実行のたびに取得する。

ツールチェーンとライブラリは、すべて nixpkgs の rev `674a2028661e550a97400c1cc992b9cca9a668a1` から取る。
libdfp と Intel DFP は、版と sha256 を固定した tarball を一時ディレクトリでビルドする。
Zustral コンパイラは、リポジトリの dev shell で `make` してビルドする。

`run.sh` は各プローブを2回実行し、出力を `expected/` の記録と照合する。
結果は `一致`、`不一致`、`未実行` のどれかで、最後に `一致 N / 不一致 M / 未実行 K` を表示する。
不一致が1つでもあれば終了状態は 1 になる。
依存を取得できなかったプローブは理由付きで `未実行` と表示し、成功に数えず、終了状態も変えない。
`run.sh --record` は記録を書き直す。プローブを変えたら記録を作り直す。

記録を作ったときの結果は `一致 18 / 不一致 0 / 未実行 4` だった。
未実行の4件は `d128@clang-O0`、`d128@clang-O2`、`libdfp@clang-O0`、`libdfp@clang-O2` で、理由は「決定性」の節に書く。

## 比較表

decimal128 系統は、GCC の `_Decimal128`、libdfp、Intel DFP の3列に分けた。
gcc は nixpkgs の gcc 16.2.0、clang は nixpkgs の clang 21.1.8 である 〔src: nixpkgs〕。

| 観点 | mpdecimal 4.0.1 | GCC `_Decimal128` | libdfp 1.0.17 | Intel DFP v2.5 | `__int128` 固定小数点 |
| --- | --- | --- | --- | --- | --- |
| 値の生成にヒープ確保 | 要る。`mpd_qnew` 1回で2回確保する 〔probe: mpd / alloc_qnew〕 | C の16バイトの値 〔probe: d128 / sizeof〕 | C の16バイトの値 〔probe: libdfp / sizeof〕 | 16バイトの構造体の値 〔probe: intel / sizeof〕 | C の16バイトの値 〔probe: i128 / sizeof〕 |
| 明示的な解放 | 要る。`mpd_del` 〔src: mpd-header〕 | 解放する API はない 〔src: gcc-dfp〕 | 解放する API はない 〔src: gcc-dfp〕 | 解放する API はない 〔src: intel-header〕 | 解放する API はない 〔src: gcc-int128〕 |
| 有効桁数 | 最大 999999999999999999 桁 〔probe: mpd / MPD_MAX_PREC〕。decimal128 の文脈では34桁 〔probe: mpd / ctx128_prec〕 | 34桁 〔probe: d128 / DEC128_MANT_DIG〕 | 34桁 〔probe: libdfp / div_1_3〕 | 34桁 〔probe: intel / div_1_3〕 | 整数部は 170141183460469231731 まで（21桁）、小数部18桁 〔probe: i128 / max_value〕 |
| 丸めモード | 9種 〔src: mpd-context〕 | 切り替える公開 API が GCC にない 〔src: gcc-dfp〕 | 5種 〔src: libdfp-fenv〕 | 5種 〔src: intel-header〕 | C の整数除算（0への切り捨て）だけ 〔probe: i128 / div_2_3〕 |
| 上限を超えたとき | `Infinity` と `Overflow` フラグ 〔probe: mpd / overflow_status〕 | `+inf`。`fetestexcept` では見えない 〔probe: d128 / overflow_fe_overflow〕 | `+inf`。`fetestexcept` では見えない 〔probe: libdfp / overflow_fe_overflow〕 | `+Inf` と overflow フラグ 〔probe: intel / overflow_flags〕 | `-fwrapv` で黙って折り返す 〔probe: i128 / overflow_wrapped〕 |
| Zustral から値のまま渡す | できない。`Address[Nat8]` で受ける 〔probe: ffi / mpd_qnew_direct〕 | できない 〔src: zustral-type〕 | できない 〔src: zustral-type〕 | できない 〔src: zustral-codegen〕 | できない 〔src: zustral-type〕 |
| 入手手段 | nixpkgs のパッケージ 〔run: 一致 mpd@gcc-O0〕 | nixpkgs の gcc に入っている 〔run: 一致 d128@gcc-O0〕 | ソースからのビルド 〔run: 一致 libdfp@gcc-O0〕 | ソースからのビルド 〔run: 一致 intel@gcc-O0〕 | コンパイラに入っている 〔run: 一致 i128@clang-O0〕 |
| ライセンス | BSD-2-Clause 〔src: mpd-copyright〕 | libgcc は GPL-3.0-or-later と GCC Runtime Library Exception 〔src: gcc-rle〕 | LGPL-2.1 〔src: libdfp-copying〕 | 3条項の BSD 形式 〔src: intel-eula〕 | なし（コンパイラの機能） |
| clang 21.1.8 | 使える 〔run: 一致 mpd@clang-O2〕 | 使えない 〔run: 未実行 d128@clang-O0〕 | 使えない 〔run: 未実行 libdfp@clang-O0〕 | 使える 〔run: 一致 intel@clang-O2〕 | 使える 〔run: 一致 i128@clang-O2〕 |
| 決定性（このマシン） | 4変種と2回実行が一致 | gcc の2変種と2回実行が一致。clang は未実行 | gcc の2変種と2回実行が一致。clang は未実行 | 4変種と2回実行が一致 | 4変種と2回実行が一致 |

## 確保と解放

mpdecimal の値 `mpd_t` は、構造体と係数の配列からなる。
`mpd_t` の構造体は48バイトである 〔probe: mpd / sizeof_mpd_t〕。
`mpd_qnew` は構造体と係数をそれぞれ確保し、1回の呼び出しで確保関数を2回呼ぶ 〔probe: mpd / alloc_qnew〕 〔src: mpd-alloc〕。
使い終わった値は `mpd_del` で解放する 〔src: mpd-header〕。
`mpd_qnew` で作った値に 1/3 の結果を書き込むと、確保は0回だった 〔probe: mpd / alloc_qdiv_1_3〕。
構造体と係数をどちらも静的な領域に置いた値（`MPD_STATIC` と `MPD_STATIC_DATA`）に同じ 1/3 の結果を書き込んでも、確保は0回だった 〔probe: mpd / alloc_qdiv_1_3_static_result〕。
観測したのはこの34桁の除算1つだけで、桁の多い演算や別の演算の確保の回数は測っていない。
静的な係数は `MPD_MINALLOC_MAX` 語の大きさが要り、それを超えて伸びる値は自動で動的な領域に切り替わる 〔src: mpd-memory〕。
確保関数は `mpd_mallocfunc`、`mpd_callocfunc`、`mpd_reallocfunc`、`mpd_free` の変数で差し替えられ、ドキュメントはプログラムの開始時に設定するものとしている 〔src: mpd-memory〕。
プローブはこの差し替えで確保の回数を数えている。

GCC の `_Decimal128`、libdfp、`__int128` の値は、どれも16バイトの C の値である 〔probe: d128 / sizeof〕 〔probe: libdfp / sizeof〕 〔probe: i128 / sizeof〕。
Intel DFP の値 `BID_UINT128` は、2つの `BID_UINT64` を持つ16バイト境界の構造体である 〔src: intel-header〕 〔probe: intel / sizeof〕。
4つとも値を解放する API を持たない 〔src: gcc-dfp〕 〔src: intel-header〕 〔src: gcc-int128〕。
演算ライブラリが内部でヒープを使うかは観測していない（「未確認事項」）。

## 精度、桁の上限、丸めモード

### mpdecimal

64ビット環境での上限は、精度が 999999999999999999 桁、指数が -999999999999999999 から 999999999999999999 である 〔probe: mpd / MPD_MAX_PREC〕 〔probe: mpd / MPD_MAX_EMAX〕 〔probe: mpd / MPD_MIN_EMIN〕 〔src: mpd-context〕。
32ビット環境の上限は 425000000 で、これは出典のみ・未実測である 〔src: mpd-context〕。
`mpd_ieee_context(&ctx, MPD_DECIMAL128)` は、精度34桁、emax 6144、emin -6143、最近接偶数丸めの文脈を作る 〔probe: mpd / ctx128_prec〕 〔probe: mpd / ctx128_emax〕 〔probe: mpd / ctx128_emin〕 〔probe: mpd / ctx128_round〕。
プローブの演算は、この decimal128 の文脈で行った。

丸めモードは `ROUND_UP`、`ROUND_DOWN`、`ROUND_CEILING`、`ROUND_FLOOR`、`ROUND_HALF_UP`、`ROUND_HALF_DOWN`、`ROUND_HALF_EVEN`、`ROUND_05UP`、`ROUND_TRUNC` の9種である 〔src: mpd-context〕。
1.005 を小数2桁に量子化すると、`ROUND_UP`、`ROUND_CEILING`、`ROUND_HALF_UP`、`ROUND_05UP` で 1.01、残りの5種で 1.00 になる 〔probe: mpd / quantize_1005_ROUND_UP〕 〔probe: mpd / quantize_1005_ROUND_HALF_EVEN〕 〔probe: mpd / quantize_1005_ROUND_05UP〕。

35桁の 12345678901234567890123456789012345 に 10 を掛けると、34桁に丸めた `1.234567890123456789012345678901234E+35` になる 〔probe: mpd / mul_wide〕。
decimal128 の最大値に 10 を掛けると `Infinity` になり、状態フラグに `Inexact Overflow Rounded` が立つ 〔probe: mpd / overflow〕 〔probe: mpd / overflow_status〕。
フラグは演算の引数 `status` に返る 〔src: mpd-header〕。

### GCC の `_Decimal128`

nixpkgs の gcc 16.2.0 の `_Decimal128` は BID 形式で 〔probe: d128 / DECIMAL_BID_FORMAT〕、有効桁数は34桁である 〔probe: d128 / DEC128_MANT_DIG〕。
`float.h` の指数の範囲は `DEC128_MIN_EXP=-6142`、`DEC128_MAX_EXP=6145` である 〔probe: d128 / DEC128_MIN_EXP〕 〔probe: d128 / DEC128_MAX_EXP〕。
GCC は `fenv.h` などの10進のライブラリ機能を提供せず、別の C ライブラリから得る必要がある 〔src: gcc-dfp〕。
このため GCC 単体には丸めモードを切り替える公開 API がなく、プローブは既定のモードの結果だけを出す 〔src: gcc-dfp〕。

`d128.c` は35桁の値をリテラルで与えている。それは34桁に丸められ、10 を掛けた結果は `+1234567890123456789012345678901234E+2` になる 〔probe: d128 / mul_wide〕。
落ちた桁が5で残る最後の桁が4なので、この出力からは丸めが最近接偶数か切り捨てかを見分けられない。
`DEC128_MAX` に 10 を掛けると `+inf` になる 〔probe: d128 / overflow〕。
このとき glibc の `fetestexcept(FE_OVERFLOW)` は 0 を返し 〔probe: d128 / overflow_fe_overflow〕、libgcc 内部の `__dfp_test_except(FE_OVERFLOW)` は 1 を返した 〔probe: d128 / overflow_libgcc_dfp_test_except〕。
つまり10進のオーバーフローは libgcc 内部では記録されるが、標準の `fenv.h` の関数からは見えない。

### libdfp

libdfp は `fe_dec_setround` で5種の丸めモード（`FE_DEC_TONEAREST`、`FE_DEC_TOWARDZERO`、`FE_DEC_UPWARD`、`FE_DEC_DOWNWARD`、`FE_DEC_TONEARESTFROMZERO`）を切り替える 〔src: libdfp-fenv〕。
5種とも `fe_dec_setround` は 0 を返した 〔probe: libdfp / fe_dec_setround_UPWARD〕。
1.005 を `quantized128` で小数2桁にすると、`UPWARD` と `TONEARESTFROMZERO` で `+101E-2`、残りの3種で `+100E-2` になる 〔probe: libdfp / quantize_1005_UPWARD〕 〔probe: libdfp / quantize_1005_TONEARESTFROMZERO〕 〔probe: libdfp / quantize_1005_TONEAREST〕。
演算の結果は GCC の `_Decimal128` と同じ値を出した 〔probe: libdfp / div_1_3〕 〔probe: libdfp / mul_wide〕 〔probe: libdfp / sum_001_x1000〕。
`DEC128_MAX` に 10 を掛けると `+inf` になり、`fetestexcept(FE_OVERFLOW)` は 0 を返した 〔probe: libdfp / overflow〕 〔probe: libdfp / overflow_fe_overflow〕。
libdfp の公開 API で10進の例外フラグを読めるかは確かめていない（「未確認事項」）。

### Intel DFP

Intel DFP は5種の丸めモード（`BID_ROUNDING_TO_NEAREST`、`BID_ROUNDING_DOWN`、`BID_ROUNDING_UP`、`BID_ROUNDING_TO_ZERO`、`BID_ROUNDING_TIES_AWAY`）を持つ 〔src: intel-header〕。
今回のビルド（`GLOBAL_RND=0`）では、丸めモードを各関数の引数で渡す 〔src: intel-readme〕。
1.005 を小数2桁にすると、`UP` と `TIES_AWAY` で `+101E-2`、残りの3種で `+100E-2` になる 〔probe: intel / quantize_1005_UP〕 〔probe: intel / quantize_1005_TIES_AWAY〕 〔probe: intel / quantize_1005_TO_NEAREST〕。
有効桁数は34桁で、1/3 は3が34個並ぶ 〔probe: intel / div_1_3〕。
最大値 `9999999999999999999999999999999999E+6111` に 10 を掛けると `+Inf` になり、引数で渡したフラグに overflow と inexact が立つ 〔probe: intel / overflow〕 〔probe: intel / overflow_flags〕。

### `__int128` 固定小数点

`__int128` の最大値は 170141183460469231731687303715884105727 である 〔probe: i128 / int128_max〕。
プローブはスケールを 10^18 に固定したので、表せる最大値は 170141183460469231731.687303715884105727 になる 〔probe: i128 / max_value〕。
丸めは C の整数除算（0への切り捨て）だけで、2/3 は 0.666666666666666666 になる 〔probe: i128 / div_2_3〕。
1.005 を小数2桁にすると 1.00 になる 〔probe: i128 / quantize_1005_c_division〕。
ほかの丸めモードは、実装者が書く。
35桁の入力はスケールを掛ける段階で128ビットを超え、`__builtin_mul_overflow` が検出する 〔probe: i128 / mul_wide〕。
最大値に 10 を掛けると、`-fwrapv` のもとでは黙って `-0.000000000000000010` に折り返し 〔probe: i128 / overflow_wrapped〕、`__builtin_mul_overflow` はそれを検出する 〔probe: i128 / overflow_builtin_detected〕。

### 共通の演算

0.1 + 0.2 は、10進の4つの実装でちょうど 0.3 になる 〔probe: mpd / add_01_02〕 〔probe: d128 / add_01_02〕 〔probe: libdfp / add_01_02〕 〔probe: intel / add_01_02〕。
`__int128` 固定小数点でも 0.300000000000000000 になる 〔probe: i128 / add_01_02〕。
0.01 を1000回足すと、5つとも 10 になる 〔probe: mpd / sum_001_x1000〕 〔probe: d128 / sum_001_x1000〕 〔probe: libdfp / sum_001_x1000〕 〔probe: intel / sum_001_x1000〕 〔probe: i128 / sum_001_x1000〕。
2/3 の最後の桁は、decimal128 の3実装と mpdecimal で7に丸まる 〔probe: d128 / div_2_3〕 〔probe: libdfp / div_2_3〕 〔probe: intel / div_2_3〕 〔probe: mpd / div_2_3〕。

## C ABI と Zustral からの呼び出し

### Zustral の FFI が受け付ける型

Zustral の整数の幅は 8、16、32、64 ビットと `ByteSize`、`Index` だけで、128ビットの整数型はない 〔src: zustral-type〕。
`Foreign_Import` の引数と戻り値に使えるのは、真偽値、整数、単精度と倍精度の浮動小数点、`Span`、`Address`、`Pointer`、関数ポインタである 〔src: zustral-codegen〕。
`Unit`、名前の付いた型（レコードなど）、参照、リージョンは `Not allowed` として拒まれる 〔src: zustral-codegen〕。
戻り値も同じ規則で変換されるので、`void` の C 関数を戻り値 `Unit` で束縛するとコンパイルが失敗する 〔src: zustral-codegen〕。
`UnitReturn.aum` のコンパイルは `Not allowed` を出し、終了状態 255 で失敗した 〔probe: ffi-unit-return / exit〕。

### Zustral プログラムからの呼び出し

このマシンでは Zustral コンパイラをビルドでき、`Foreign_Import` で各系統を呼ぶプログラム `probes/ffi/Probe.aum` を実行できた 〔run: 一致 ffi〕。
mpdecimal の `mpd_qnew` は、シムなしで直接 `Address[Nat8]` として受け取れた 〔probe: ffi / mpd_qnew_direct〕。
`void` を返す `mpd_qadd` などは、`int32_t` を返すシム関数を通して呼び、0.1 + 0.2 = 0.3 を得た 〔probe: ffi / mpd_add_01_02〕。
GCC の `_Decimal128` は、Zustral 側で確保した16バイトの領域へのポインタをシムに渡して計算し、`+3E-1` を得た 〔probe: ffi / d128_add_01_02〕。
`__int128` も同じく16バイトの領域越しに計算し、0.300000000000000000 を得た 〔probe: ffi / i128_add_01_02〕。
どちらも、10進の値そのものは FFI の境界を越えていない。

libdfp と Intel DFP は Zustral のプローブでは呼んでおらず、次の2文はヘッダと上のプローブから述べる。
libdfp の関数は `_Decimal128` を値で受け渡しするので 〔src: libdfp-math〕、128ビットの型がない Zustral からは値のまま呼べず、シムが要る 〔src: zustral-type〕。
Intel DFP は、`CALL_BY_REF=0` のビルドでは構造体 `BID_UINT128` を値で受け渡しし、`CALL_BY_REF=1` のビルドではポインタを受けて `void` を返す 〔src: intel-header〕。どちらの形も Zustral の `Foreign_Import` では直接束縛できず、シムが要る 〔src: zustral-codegen〕。

### コンパイラをビルドできなかった場合

`run.sh` は Zustral コンパイラのビルドを、上限 600 秒の `timeout` の下で行う（`ZUSTRAL_BUILD_TIMEOUT` で変えられる）。
ビルドが失敗するか上限を超えると、`ffi` と `ffi-unit-return` は `未実行` になる。
そのときは、この節の Zustral プログラムの結果は使えず、C だけのプローブ（`d128`、`libdfp`、`intel`、`i128`、`mpd`）とコンパイラのソースの引用 〔src: zustral-type〕 〔src: zustral-codegen〕 が根拠になる。
このマシンでは、ビルドは上限内に終わり、2つとも一致した 〔run: 一致 ffi〕 〔run: 一致 ffi-unit-return〕。

## 依存、ライセンス、ビルド、移植性

### mpdecimal

nixpkgs のパッケージ `mpdecimal`（4.0.1）で入り、ソースからのビルドは要らない 〔probe: mpd / mpd_version〕 〔src: nixpkgs〕。
ライセンスは BSD-2-Clause である 〔src: mpd-copyright〕 〔src: nixpkgs〕。
公式には Linux、FreeBSD、NetBSD、OpenBSD、Windows、macOS、AIX、Solaris で定期的に試験され、x86_64、i386、aarch64、ppc64、ppc64le、risc-v などで試験が回っている。これは出典のみ・未実測である 〔src: mpd-index〕。
Python の `_decimal` モジュールは libmpdec の包みである 〔src: mpd-index〕。

### GCC の `_Decimal128`

nixpkgs の gcc 16.2.0 に入っていて、追加の依存は要らない 〔run: 一致 d128@gcc-O0〕。
プローブは、-O2 でも演算をコンパイル時に畳まず実行時に行わせるため、入力を `volatile` にしている。
libgcc のライセンスは GPL-3.0-or-later に GCC Runtime Library Exception が付いたもので、例外は libgcc や libdecnumber に及ぶ 〔src: gcc-rle〕 〔src: nixpkgs〕。
GCC の `--enable-decimal-float` は、AArch64、PowerPC、i386、x86_64 の GNU/Linux で既定で有効で、AArch64、i386、x86_64 では BID、PowerPC では DPD が既定の形式である。これは出典のみ・未実測である 〔src: gcc-configure〕。
GCC は、10進の浮動小数点型をすべてのターゲットが持つわけではないとしている 〔src: gcc-dfp〕。
nixpkgs の clang 21.1.8 は `_Decimal128` をコンパイルできない 〔run: 未実行 d128@clang-O0〕。

### libdfp

nixpkgs にパッケージがなく、GitHub の tarball（commit `e898f1868e3f`、版 1.0.17）をソースからビルドする 〔run: 一致 libdfp@gcc-O0〕。
ビルドには gcc、gnumake、binutils、gawk、python3 を使った。どれが不要かは確かめていない。
ライセンスは LGPL-2.1 で、内部で使う libdecnumber は GCC の一部として GPL-3.0 と Runtime Library Exception の下にある 〔src: libdfp-copying〕。
libdfp は GNU/Linux だけで動くようにされており、glibc 2.10 以上と、10進の浮動小数点を有効にした GCC を要する 〔src: libdfp-readme〕。
ソースには aarch64、powerpc64le、s390x、x86_64 向けの `sysdeps` がある。x86_64 以外での動作は出典のみ・未実測である 〔src: libdfp-source〕。
`_Decimal128` を使うので、clang 21.1.8 ではプローブをコンパイルできない 〔run: 未実行 libdfp@clang-O0〕。

### Intel DFP

nixpkgs にパッケージがなく、netlib の tarball（v2.5、sha256 `85dafd70…01037`）をソースからビルドする 〔run: 一致 intel@gcc-O0〕。
ビルドには gcc と gnumake を使った。
ライセンスは、再配布の条件3つと無保証を定めた BSD 形式の条文である 〔src: intel-eula〕。
README によると、Intel 64 アーキテクチャの Linux、Windows、macOS で試験されている。これは出典のみ・未実測である 〔src: intel-readme〕。
`_Decimal128` を使わない C の API なので、clang 21.1.8 でもビルドして実行できた 〔run: 一致 intel@clang-O0〕。

### `__int128` 固定小数点

コンパイラの拡張で、依存はない。
GCC は `__int128` を、128ビットを保持できる整数モードを持つターゲットで提供する。他のターゲットについては出典のみ・未実測である 〔src: gcc-int128〕。
`long long` が128ビットより狭いターゲットでは、`__int128` の整数定数を書く手段がない 〔src: gcc-int128〕。
nixpkgs の gcc 16.2.0 と clang 21.1.8 の両方でビルドして実行できた 〔run: 一致 i128@gcc-O0〕 〔run: 一致 i128@clang-O0〕。

## 決定性

各系統の C プローブを、gcc と clang、`-O0` と `-O2` の4変種でビルドし、それぞれ2回実行して同じ記録と照合した。
実行できた変種は、すべて2回とも系統の記録と一致した。変種だけの記録（`expected/<系統>@<変種>.txt`）は1つもない。

| 系統 | 一致した変種 | 未実行の変種と理由 |
| --- | --- | --- |
| mpdecimal | `mpd@gcc-O0`、`mpd@gcc-O2`、`mpd@clang-O0`、`mpd@clang-O2` | なし |
| GCC `_Decimal128` | `d128@gcc-O0`、`d128@gcc-O2` | `d128@clang-O0`、`d128@clang-O2`。`clang-dfp: error: GNU decimal type extension not supported` 〔run: 未実行 d128@clang-O0〕 |
| libdfp | `libdfp@gcc-O0`、`libdfp@gcc-O2` | `libdfp@clang-O0`、`libdfp@clang-O2`。理由は同じ 〔run: 未実行 libdfp@clang-O0〕 |
| Intel DFP | `intel@gcc-O0`、`intel@gcc-O2`、`intel@clang-O0`、`intel@clang-O2` | なし |
| `__int128` | `i128@gcc-O0`、`i128@gcc-O2`、`i128@clang-O0`、`i128@clang-O2` | なし |

最適化レベルを変えたのはプローブ本体のビルドだけである。
mpdecimal のライブラリは nixpkgs のビルドを、libdfp は `-O2`、Intel DFP は付属の Makefile の既定で、それぞれ1度だけビルドしたものを使った。
clang での GCC `_Decimal128` と libdfp の決定性は、確かめていない。
プラットフォームをまたいだ一致は、このマシンでは測っていない（「未確認事項」）。

## 下流の問いに効く事実

ここでは事実を名指しするだけで、どれを選ぶべきかは書かない。

### 0004「スケールは型に入るか、値に入るか」

- mpdecimal と decimal128 の3実装では、指数が値の中にある。同じ 1.005 の量子化でも結果は `+100E-2` のように指数を持って出てくる 〔probe: intel / quantize_1005_TO_NEAREST〕 〔probe: libdfp / quantize_1005_TONEAREST〕 〔probe: mpd / quantize_1005_ROUND_HALF_EVEN〕。
- 0.1 + 0.2 の結果は、mpdecimal と decimal128 では `0.3`、`+3E-1` と1桁の係数で出る 〔probe: mpd / add_01_02〕 〔probe: d128 / add_01_02〕。値の指数は演算ごとに変わる。
- `__int128` 固定小数点では、スケールは値の外にあり、プローブは 10^18 に固定した。結果はつねに小数18桁で出る 〔probe: i128 / add_01_02〕。
- `__int128` のスケールを 10^18 にすると、整数部は21桁までになる 〔probe: i128 / max_value〕。

### 0005「Decimal は線形リソースか、値型か」

- mpdecimal の値は、`mpd_qnew` で作るとヒープ確保が2回要り 〔probe: mpd / alloc_qnew〕、`mpd_del` で解放する 〔src: mpd-header〕。
- mpdecimal の値は、構造体と係数を静的な領域に置いても作れる 〔src: mpd-memory〕。ただし係数が伸びると自動で動的な領域に移る 〔src: mpd-memory〕。
- 34桁の 1/3 の結果を書き込む演算は、結果が `mpd_qnew` で作った値でも静的な領域の値でも、確保が0回だった 〔probe: mpd / alloc_qdiv_1_3〕 〔probe: mpd / alloc_qdiv_1_3_static_result〕。ほかの演算の確保の回数は測っていない。
- decimal128 の3実装と `__int128` は16バイト固定の値で、解放の API を持たない 〔probe: d128 / sizeof〕 〔probe: intel / sizeof〕 〔probe: i128 / sizeof〕。
- Zustral の FFI はどの系統の10進の値も値のまま渡せない。mpdecimal は `Address` で、decimal128 と `__int128` は16バイトの領域へのポインタで渡した 〔probe: ffi / mpd_qnew_direct〕 〔probe: ffi / d128_add_01_02〕 〔probe: ffi / i128_add_01_02〕。
- `void` を返す C 関数を戻り値 `Unit` で束縛するとコンパイルが失敗する 〔probe: ffi-unit-return / exit〕。プローブは mpdecimal の演算関数を、`int32_t` を返すシムで包んで呼んだ 〔probe: ffi / mpd_add_01_02〕。

### 0008「桁あふれを例外なしでどう扱うか」

- mpdecimal は上限を超えると `Infinity` を返し、引数の `status` に `Overflow` を立てる 〔probe: mpd / overflow〕 〔probe: mpd / overflow_status〕。
- Intel DFP は `+Inf` を返し、引数で渡したフラグに overflow を立てる 〔probe: intel / overflow_flags〕。
- GCC の `_Decimal128` と libdfp は `+inf` を返すが、glibc の `fetestexcept` ではオーバーフローが見えない 〔probe: d128 / overflow_fe_overflow〕 〔probe: libdfp / overflow_fe_overflow〕。GCC では libgcc 内部の関数で見える 〔probe: d128 / overflow_libgcc_dfp_test_except〕。
- `__int128` は `-fwrapv` のもとで黙って折り返し、検出は `__builtin_mul_overflow` などを書いたときだけ起きる 〔probe: i128 / overflow_wrapped〕 〔probe: i128 / overflow_builtin_detected〕。Zustral の生成コードは `-fwrapv` 付きでビルドされる 〔src: zustral-util〕。
- 35桁の係数の入力は、decimal128 の3実装と mpdecimal の decimal128 文脈のどれでもエラーにならず、34桁に丸められた 〔probe: d128 / mul_wide〕 〔probe: libdfp / mul_wide〕 〔probe: intel / mul_wide〕 〔probe: mpd / mul_wide〕。プローブの入力は、GCC の `_Decimal128` ではコンパイル時に変換されるリテラル、ほかの3つでは実行時に文字列から読んだ値である。

## 未確認事項

根拠を示せなかったので、事実としては書いていないことである。

- 他のプラットフォーム（AArch64、macOS、Windows など）で、各プローブがこのマシンと同じ出力を出すか。プラットフォームをまたいだ結果の一致を示す一次資料も、今回は確かめていない。
- clang での GCC `_Decimal128` と libdfp の決定性。clang 21.1.8 が型を拒むので測れない。
- 各演算ライブラリ（libgcc、libdfp、Intel DFP）が演算の内部でヒープを使うか。プローブは mpdecimal の確保関数だけを数えている。
- libdfp の公開 API で10進の例外フラグを読めるか。x86_64 で `fetestexcept` が 0 を返したことだけを観測した。
- gcc 16.2.0 の `include` ディレクトリに `fe_dec_setround` などの宣言がないことと、libgcc に内部関数 `__dfp_set_round` があること。`run.sh` の外で `grep` と `nm` で見ただけで、再実行できる形にしていない。GCC 単体に丸めモードを切り替える公開 API がないことは、GCC のマニュアルを根拠に本文で述べた 〔src: gcc-dfp〕。
- 35桁の入力の丸めが最近接偶数か切り捨てか。プローブの入力では2つの結果が同じになる。
- `void` を返す C 関数を、戻り値を偽った型（`Int32` など）で束縛した場合の振る舞い。Zustral は `Foreign_Import` ごとに自前の C の宣言を出す 〔src: zustral-codegen〕 ので、コンパイルは通る見込みだが、試していない。
- 生成 C とライブラリのヘッダを同じ翻訳単位に入れると型が衝突すること。設計文書の実験2で観測したが、`run.sh` では再実行しない。シムを別の翻訳単位に置いているのはこのためである。
- libdfp のビルドに使ったツールの組（gcc、gnumake、binutils、gawk、python3）のうち、どれが不要か。

## 一次資料

取得日はすべて 2026-10-26 である。

| ID | 資料 | URL または場所 | 版 |
| --- | --- | --- | --- |
| nixpkgs | nixpkgs の `gcc.version`、`clang.version`、`mpdecimal.version`、`gcc.cc.meta.license`、`mpdecimal.meta.license` | `nix eval github:NixOS/nixpkgs/674a2028661e550a97400c1cc992b9cca9a668a1#<属性>` | rev `674a2028661e550a97400c1cc992b9cca9a668a1`。gcc 16.2.0、clang 21.1.8、mpdecimal 4.0.1 |
| mpd-context | libmpdec のドキュメント「Context」 | https://www.bytereef.org/mpdecimal/doc/libmpdec/context.html | mpdecimal 4.0.1 のサイト |
| mpd-memory | libmpdec のドキュメント「Advanced Memory Handling」 | https://www.bytereef.org/mpdecimal/doc/libmpdec/memory.html | mpdecimal 4.0.1 のサイト |
| mpd-index | mpdecimal のトップページ | https://www.bytereef.org/mpdecimal/index.html | 最新版 4.0.1 と記載 |
| mpd-header | `mpdecimal.h`（`mpd_t` は 324〜331 行、`mpd_qadd` は 498 行、確保関数の変数は 783〜798 行） | https://www.bytereef.org/software/mpdecimal/releases/mpdecimal-4.0.1.tar.gz | 4.0.1、tarball の sha256 `96d33abb4bb0070c7be0fed4246cd38416188325f820468214471938545b1ac8` |
| mpd-alloc | `libmpdec/mpalloc.c` の `mpd_qnew_size` と `mpd_qnew`（150〜184 行） | 同上 | 同上 |
| mpd-copyright | `COPYRIGHT.txt` | 同上 | 同上 |
| gcc-dfp | GCC マニュアル「Decimal Floating Types」 | https://gcc.gnu.org/onlinedocs/gcc-16.2.0/gcc/Decimal-Float.html | GCC 16.2.0 |
| gcc-int128 | GCC マニュアル「128-bit Integers」 | https://gcc.gnu.org/onlinedocs/gcc-16.2.0/gcc/_005f_005fint128.html | GCC 16.2.0 |
| gcc-configure | GCC のインストール手順「Configuration」の `--enable-decimal-float` | https://gcc.gnu.org/install/configure.html | 開発版の文書、2026-10-25 更新 |
| gcc-rle | GCC Runtime Library Exception FAQ | https://www.gnu.org/licenses/gcc-exception-3.1-faq.html | 例外の版 3.1 |
| libdfp-fenv | libdfp の `dfp/fenv.h`（`FE_DEC_*` と `fe_dec_setround`） | https://github.com/libdfp/libdfp/blob/e898f1868e3fb0b703e508857bae0df08d4268d5/dfp/fenv.h | commit `e898f1868e3f`、版 1.0.17 |
| libdfp-math | libdfp の `dfp/math.h` と、それが読み込む `dfp/bits/dfp_dmathcalls.h` 64 行（`quantize` を10進の値2つを受けて10進の値を返す形で宣言する） | https://github.com/libdfp/libdfp/blob/e898f1868e3fb0b703e508857bae0df08d4268d5/dfp/bits/dfp_dmathcalls.h | 同上 |
| libdfp-readme | libdfp の `README.user` の 5 章「Dependencies」 | https://github.com/libdfp/libdfp/blob/e898f1868e3fb0b703e508857bae0df08d4268d5/README.user | 同上 |
| libdfp-copying | libdfp の `COPYING.txt`、`COPYING.libdfp.txt`、`COPYING.libdecnumber.txt` | https://github.com/libdfp/libdfp/tree/e898f1868e3fb0b703e508857bae0df08d4268d5 | 同上 |
| libdfp-source | libdfp の `sysdeps/` ディレクトリ | https://github.com/libdfp/libdfp/tree/e898f1868e3fb0b703e508857bae0df08d4268d5/sysdeps | 同上 |
| intel-header | Intel DFP の `LIBRARY/src/bid_functions.h`（`BID_UINT128` は 91〜94 行、丸めモードは 140〜144 行、`bid128_add` は 438 行と 3033 行） | https://netlib.org/misc/intel/IntelRDFPMathLib20U5.tar.gz | v2.5（README の日付 2026-09-28）、sha256 `85dafd70f0fe2a8da218ade4233fca9d3228b0b04cd6d8527f7499926be01037` |
| intel-readme | Intel DFP の `README`（ビルドオプションの説明と「6. TESTING」） | 同上 | 同上 |
| intel-eula | Intel DFP の `eula.txt` | 同上 | 同上 |
| zustral-type | Zustral の `lib/Type.ml` 22〜29 行（整数の幅） | このリポジトリ | このブランチの基点 `master` |
| zustral-codegen | Zustral の `lib/CodeGen.ml` 554〜591 行（`Foreign_Import` の型の変換） | このリポジトリ | 同上 |
| zustral-util | Zustral の `lib/Util.ml` 202〜203 行（生成 C を `-fwrapv` 付きでビルドする） | このリポジトリ | 同上 |
