/-!
# Zustral `Decimal[P, S]` 仕様の形式化

`docs/decimal.md` の算術の意味論を Lean 4 に写したもの。Mathlib には頼らない。

* 定義はすべて実行できる。`#guard` で仕様書の表（3.2節、7.2節）を計算し直している。
* 定理は文だけで、証明は `sorry` である。定理6の数値の2つだけは `decide` で証明してある。
* Lean 4.35.0 で `lean Decimal.lean` が通る。

値は係数（値を 10^S 倍した整数）で持つ（2節）。型 `Decimal[P, S]` の値は、
`InRange P c` を満たす係数 `c : Int` である。
-/

namespace Zustral.Decimal

/-! ## 型（1節、2節） -/

/-- `Decimal[P, S]` が型として書けること（1節）。 -/
def ValidType (P S : Nat) : Prop := 1 ≤ P ∧ P ≤ 38 ∧ S ≤ P

/-- 係数 `c` が精度 `P` の範囲にあること（6節の手順3）。範囲は正負で対称である。 -/
def InRange (P : Nat) (c : Int) : Prop := c.natAbs < 10 ^ P

instance (P : Nat) (c : Int) : Decidable (InRange P c) := by
  unfold InRange; infer_instance

/-! ## 丸め（7節） -/

/-- 丸めモード（7.1節）。 -/
inductive RoundingMode where
  | towardGreater
  | towardLesser
  | towardZero
  | awayFromZero
  | nearestEven
  | nearestAwayFromZero
  | nearestTowardZero
  deriving DecidableEq, Repr

/--
正確な値 `n / d`（`0 < d`）を整数へ丸める。6節の手順2にあたる。
`round` も `divide` も、正確な値を分数にしてから、これを1回だけ呼ぶ。

`d > 0` のとき、Lean の `Int` の `/` は床、`%` は 0 以上 `d` 未満の余りである。
`q` が小さい方の目盛り、`q + 1` が大きい方の目盛りになる。
-/
def roundDiv (m : RoundingMode) (n d : Int) : Int :=
  let q := n / d
  let r := n % d
  if r = 0 then q
  else
    let up := q + 1
    match m with
    | .towardGreater => up
    | .towardLesser => q
    | .towardZero => if n < 0 then up else q
    | .awayFromZero => if n < 0 then q else up
    | .nearestEven =>
        if 2 * r < d then q else if d < 2 * r then up
        else if q % 2 = 0 then q else up
    | .nearestAwayFromZero =>
        if 2 * r < d then q else if d < 2 * r then up
        else if n < 0 then q else up
    | .nearestTowardZero =>
        if 2 * r < d then q else if d < 2 * r then up
        else if n < 0 then up else q

/-- スケール `S` の係数 `c` を、スケール `S'` へ丸める（`round`、9.3節）。範囲の検査は別。 -/
def rescale (m : RoundingMode) (S S' : Nat) (c : Int) : Int :=
  if S ≤ S' then c * 10 ^ (S' - S) else roundDiv m c (10 ^ (S - S'))

/--
`divide(a, b, mode)` の、範囲の検査の前の係数（5.3節）。
`a` はスケール `Sa`、`b` はスケール `Sb`、結果はスケール `S`。`b = 0` なら `none`。
正確な値は `(a / 10^Sa) / (b / 10^Sb) = (a * 10^(Sb + S)) / (b * 10^Sa) / 10^S` である。
-/
def divideCoeff (m : RoundingMode) (Sa Sb S : Nat) (a b : Int) : Option Int :=
  if b = 0 then none
  else
    let n := a * 10 ^ (Sb + S)
    let d := b * 10 ^ Sa
    some (if 0 < d then roundDiv m n d else roundDiv m (-n) (-d))

/-! ### 7.2節の表 -/

section Table72
open RoundingMode

#guard roundDiv nearestEven 2345 10 = 234
#guard roundDiv nearestEven 2355 10 = 236
#guard roundDiv nearestEven (-2345) 10 = -234
#guard roundDiv nearestEven 2344 10 = 234

#guard roundDiv nearestAwayFromZero 2345 10 = 235
#guard roundDiv nearestAwayFromZero 2355 10 = 236
#guard roundDiv nearestAwayFromZero (-2345) 10 = -235
#guard roundDiv nearestAwayFromZero 2344 10 = 234

#guard roundDiv nearestTowardZero 2345 10 = 234
#guard roundDiv nearestTowardZero 2355 10 = 235
#guard roundDiv nearestTowardZero (-2345) 10 = -234
#guard roundDiv nearestTowardZero 2344 10 = 234

#guard roundDiv awayFromZero 2345 10 = 235
#guard roundDiv awayFromZero 2355 10 = 236
#guard roundDiv awayFromZero (-2345) 10 = -235
#guard roundDiv awayFromZero 2344 10 = 235

#guard roundDiv towardZero 2345 10 = 234
#guard roundDiv towardZero 2355 10 = 235
#guard roundDiv towardZero (-2345) 10 = -234
#guard roundDiv towardZero 2344 10 = 234

#guard roundDiv towardGreater 2345 10 = 235
#guard roundDiv towardGreater 2355 10 = 236
#guard roundDiv towardGreater (-2345) 10 = -234
#guard roundDiv towardGreater 2344 10 = 235

#guard roundDiv towardLesser 2345 10 = 234
#guard roundDiv towardLesser 2355 10 = 235
#guard roundDiv towardLesser (-2345) 10 = -235
#guard roundDiv towardLesser 2344 10 = 234

-- 6節：999.96 を Decimal[4, 1] へ丸めると 1000.0 になり、桁あふれである。
#guard rescale nearestAwayFromZero 2 1 99996 = 10000
#guard ¬ InRange 4 (rescale nearestAwayFromZero 2 1 99996)

-- 16節：100 円を3人で分けると 33、33、34。
#guard divideCoeff towardZero 0 0 0 100 3 = some 33

end Table72

/-! ## 定理1：乗算は桁あふれしない（5.2節） -/

/-- 積の係数は、必ず `Decimal[P1 + P2, S1 + S2]` の範囲に収まる。 -/
theorem mul_inRange (P1 P2 : Nat) (a b : Int)
    (ha : InRange P1 a) (hb : InRange P2 b) :
    InRange (P1 + P2) (a * b) := by
  sorry

/-! ## 定理2：`widen` は失敗しない（9.2節） -/

/-- `Decimal[P, S]` から `Decimal[P', S']` へ `widen` が通る条件。 -/
def WidenOk (P S P' S' : Nat) : Prop := S ≤ S' ∧ P - S ≤ P' - S'

/-- 条件を満たせば、元の型のどの値も、変換先で正確に表せる。 -/
theorem widen_inRange (P S P' S' : Nat) (c : Int)
    (hS : S ≤ P) (hS' : S' ≤ P') (hw : WidenOk P S P' S')
    (hc : InRange P c) :
    InRange P' (c * 10 ^ (S' - S)) := by
  sorry

/-- 条件は必要でもある。満たさなければ、表せない値が元の型にある。 -/
theorem widen_complete (P S P' S' : Nat)
    (hP : 1 ≤ P) (hS : S ≤ P) (hS' : S' ≤ P')
    (hw : ¬ WidenOk P S P' S') :
    ∃ c : Int, InRange P c ∧
      ¬ ∃ c' : Int, InRange P' c' ∧ c' * 10 ^ S = c * 10 ^ S' := by
  sorry

/-- 整数型（9.1節）。`index` と `byteSize` は 64 ビットの符号なしとして数える。 -/
inductive IntType where
  | nat8 | nat16 | nat32 | nat64
  | int8 | int16 | int32 | int64
  | index | byteSize
  deriving DecidableEq, Repr

namespace IntType

def minValue : IntType → Int
  | int8 => -(2 ^ 7) | int16 => -(2 ^ 15) | int32 => -(2 ^ 31) | int64 => -(2 ^ 63)
  | _ => 0

def maxValue : IntType → Int
  | nat8 => 2 ^ 8 - 1 | nat16 => 2 ^ 16 - 1 | nat32 => 2 ^ 32 - 1 | nat64 => 2 ^ 64 - 1
  | int8 => 2 ^ 7 - 1 | int16 => 2 ^ 15 - 1 | int32 => 2 ^ 31 - 1 | int64 => 2 ^ 63 - 1
  | index => 2 ^ 64 - 1 | byteSize => 2 ^ 64 - 1

/-- 9.2節の桁数の表。 -/
def digits : IntType → Nat
  | nat8 | int8 => 3
  | nat16 | int16 => 5
  | nat32 | int32 => 10
  | int64 => 19
  | nat64 | index | byteSize => 20

end IntType

/-- 桁数の表は正しい。その型のどの値も `Decimal[桁数, 0]` に収まる。 -/
theorem intType_digits_sound (t : IntType) (n : Int)
    (hlo : t.minValue ≤ n) (hhi : n ≤ t.maxValue) :
    InRange t.digits n := by
  sorry

/-- 桁数の表は最小である。1桁減らすと、収まらない値がある。 -/
theorem intType_digits_minimal (t : IntType) :
    ¬ InRange (t.digits - 1) t.maxValue := by
  sorry

/-- 整数型からの `widen` は、桁数が `P' - S'` 以下なら失敗しない。 -/
theorem widen_int_inRange (t : IntType) (P' S' : Nat) (n : Int)
    (hS' : S' ≤ P') (hd : t.digits ≤ P' - S')
    (hlo : t.minValue ≤ n) (hhi : n ≤ t.maxValue) :
    InRange P' (n * 10 ^ S') := by
  sorry

/-! ## 定理3：丸めは隣り合う目盛りのどちらかを取る（7.1節） -/

/-- 正確な値が目盛りの上にあれば、どのモードでも結果はその値である。 -/
theorem roundDiv_exact (m : RoundingMode) (k d : Int) (hd : 0 < d) :
    roundDiv m (k * d) d = k := by
  sorry

/-- 結果は、小さい方の目盛り `n / d` か、大きい方の目盛り `n / d + 1` である。 -/
theorem roundDiv_neighbors (m : RoundingMode) (n d : Int) (hd : 0 < d) :
    roundDiv m n d = n / d ∨ roundDiv m n d = n / d + 1 := by
  sorry

/-- 誤差は1目盛り未満である。 -/
theorem roundDiv_error_lt (m : RoundingMode) (n d : Int) (hd : 0 < d) :
    (roundDiv m n d * d - n).natAbs < d.natAbs := by
  sorry

theorem roundDiv_towardLesser (n d : Int) (hd : 0 < d) :
    roundDiv .towardLesser n d * d ≤ n := by
  sorry

theorem roundDiv_towardGreater (n d : Int) (hd : 0 < d) :
    n ≤ roundDiv .towardGreater n d * d := by
  sorry

/-- 切り捨ては、絶対値を増やさない。 -/
theorem roundDiv_towardZero (n d : Int) (hd : 0 < d) :
    (roundDiv .towardZero n d * d).natAbs ≤ n.natAbs := by
  sorry

/-- 切り上げは、絶対値を減らさない。 -/
theorem roundDiv_awayFromZero (n d : Int) (hd : 0 < d) :
    n.natAbs ≤ (roundDiv .awayFromZero n d * d).natAbs := by
  sorry

/-- 3つの「近い方」のモードは、誤差が半目盛り以下である。 -/
theorem roundDiv_nearest (m : RoundingMode) (n d : Int) (hd : 0 < d)
    (hm : m = .nearestEven ∨ m = .nearestAwayFromZero ∨ m = .nearestTowardZero) :
    2 * (roundDiv m n d * d - n).natAbs ≤ d.natAbs := by
  sorry

/-- 丸めは符号について対称である。対称でないのは `towardGreater` と `towardLesser` だけ。 -/
theorem roundDiv_neg (m : RoundingMode) (n d : Int) (hd : 0 < d)
    (hm : m ≠ .towardGreater ∧ m ≠ .towardLesser) :
    roundDiv m (-n) d = -roundDiv m n d := by
  sorry

theorem roundDiv_neg_towardGreater (n d : Int) (hd : 0 < d) :
    roundDiv .towardGreater (-n) d = -roundDiv .towardLesser n d := by
  sorry

/-- 丸めは単調である。 -/
theorem roundDiv_mono (m : RoundingMode) (n n' d : Int) (hd : 0 < d) (h : n ≤ n') :
    roundDiv m n d ≤ roundDiv m n' d := by
  sorry

/-! ## 定理4：正準の表記は必ず読める（11節） -/

/-- 左を `0` で埋めて、ちょうど `n` 文字にする。 -/
def padZeros (n : Nat) (s : String) : String :=
  String.ofList (List.replicate (n - s.length) '0') ++ s

/-- 正準の表記（11.1節）。 -/
def render (S : Nat) (c : Int) : String :=
  let a := c.natAbs
  let sign := if c < 0 then "-" else ""
  let intPart := toString (a / 10 ^ S)
  if S = 0 then sign ++ intPart
  else sign ++ intPart ++ "." ++ padZeros S (toString (a % 10 ^ S))

/-- 1桁以上の ASCII の数字の列を読む。空の列と、数字でない文字は `none`。 -/
def digitsValue (cs : List Char) : Option Nat :=
  if cs.isEmpty then none
  else cs.foldlM
    (fun acc ch => if ch.isDigit then some (acc * 10 + (ch.toNat - '0'.toNat)) else none) 0

/-- `parse`（11.3節）。値が `Decimal[P, S]` で正確に表せるときだけ `some`。丸めない。 -/
def parse (P S : Nat) (s : String) : Option Int :=
  let cs := s.toList
  let (neg, body) : Bool × List Char :=
    match cs with
    | '-' :: t => (true, t)
    | '+' :: t => (false, t)
    | t => (false, t)
  let (intDigits, rest) := body.span (fun ch => ch != '.')
  -- 小数点があるなら、後ろに1桁以上の数字が要る。
  let frac? : Option (List Char) :=
    match rest with
    | [] => some []
    | _ :: f => if f.isEmpty then none else some f
  match frac?, digitsValue intDigits with
  | some frac, some i =>
    match (if frac.isEmpty then some 0 else digitsValue frac) with
    | none => none
    | some f =>
      let k := frac.length
      let num := i * 10 ^ k + f   -- 値は num / 10^k
      let coeff? : Option Nat :=
        if k ≤ S then some (num * 10 ^ (S - k))
        else if num % 10 ^ (k - S) = 0 then some (num / 10 ^ (k - S))
        else none
      match coeff? with
      | none => none
      | some c =>
        if c < 10 ^ P then some (if neg then -(c : Int) else (c : Int)) else none
  | _, _ => none

-- 11.1節の例
#guard render 2 150 = "1.50"
#guard render 2 (-5) = "-0.05"
#guard render 2 0 = "0.00"
#guard render 0 (-42) = "-42"

-- 11.3節の例（Decimal[7, 2]）
#guard parse 7 2 "1.5" = some 150
#guard parse 7 2 "1.500" = some 150
#guard parse 7 2 "007.50" = some 750
#guard parse 7 2 "+1.50" = some 150
#guard parse 7 2 "-0" = some 0
#guard parse 7 2 "-0.00" = some 0
#guard parse 7 2 "1.234" = none
#guard parse 7 2 "100000.00" = none
#guard parse 7 2 ".5" = none
#guard parse 7 2 "5." = none
#guard parse 7 2 "" = none
#guard parse 7 2 "-" = none
#guard parse 7 2 " 1.50" = none
#guard parse 7 2 "1'000.25" = none
#guard parse 7 2 "1.5e3" = none
#guard parse 7 2 "1.2.3" = none

/-- 同じ型で、出力したものを `parse` すると、元の値の `some` になる。 -/
theorem parse_render (P S : Nat) (c : Int) (hS : S ≤ P) (hc : InRange P c) :
    parse P S (render S c) = some c := by
  sorry

/-- 正準の表記の長さは、最大で `P + 3` バイトである（11.1節）。 -/
theorem render_length (P S : Nat) (c : Int) (hP : 1 ≤ P) (hS : S ≤ P) (hc : InRange P c) :
    (render S c).length ≤ P + 3 := by
  sorry

/-- `parse` が返す値は、必ず型の範囲にある。 -/
theorem parse_inRange (P S : Nat) (s : String) (c : Int) (h : parse P S s = some c) :
    InRange P c := by
  sorry

/-! ## 定理5：リテラルの型は最小の精度である（3.2節） -/

/-- 10進の桁数。`0` は 1 桁と数える。 -/
def numDigits (n : Nat) : Nat := (Nat.toDigits 10 n).length

/--
型が届かない位置のリテラルの精度。`coeff` は係数の絶対値、`S` は小数点より後ろに書いた桁数。
係数の桁数、`S`、1 のうち、いちばん大きい数である。
-/
def literalPrecision (coeff S : Nat) : Nat := max (max (numDigits coeff) S) 1

-- 3.2節の表
#guard literalPrecision 2 0 = 1      -- `2`      : Decimal[1, 0]
#guard literalPrecision 100 0 = 3    -- `100`    : Decimal[3, 0]
#guard literalPrecision 825 4 = 4    -- `0.0825` : Decimal[4, 4]
#guard literalPrecision 15 1 = 2     -- `1.5`    : Decimal[2, 1]
#guard literalPrecision 150 2 = 3    -- `1.50`   : Decimal[3, 2]
#guard literalPrecision 10 2 = 2     -- `0.10`   : Decimal[2, 2]
#guard literalPrecision 0 2 = 2      -- `0.00`   : Decimal[2, 2]
#guard literalPrecision 10825 4 = 5  -- `1 + 0.0825` : Decimal[5, 4]（3.3節）

/-- リテラルの値は、その型で表せる。38 以下かどうかは別に検査する。 -/
theorem literalPrecision_sound (coeff S : Nat) :
    let P := literalPrecision coeff S
    1 ≤ P ∧ S ≤ P ∧ InRange P (coeff : Int) := by
  sorry

/-- その値をスケール `S` で表せる精度のうち、最小である。 -/
theorem literalPrecision_minimal (coeff S P : Nat)
    (hP : 1 ≤ P) (hS : S ≤ P) (hc : InRange P (coeff : Int)) :
    literalPrecision coeff S ≤ P := by
  sorry

/-! ## 定理6：係数は符号付き128ビット整数に入る（2節） -/

theorem pow38_lt_pow127 : (10 : Nat) ^ 38 < 2 ^ 127 := by
  decide

/-- `P ≤ 38` なら、範囲にある係数は符号付き128ビット整数に入る。 -/
theorem inRange_fits_int128 (P : Nat) (c : Int) (hP : P ≤ 38) (hc : InRange P c) :
    -(2 ^ 127 : Int) < c ∧ c < 2 ^ 127 := by
  sorry

/-- `P1 + P2 ≤ 38` なら、積も符号付き128ビット整数に入る（5.2節）。 -/
theorem mul_fits_int128 (P1 P2 : Nat) (a b : Int) (hP : P1 + P2 ≤ 38)
    (ha : InRange P1 a) (hb : InRange P2 b) :
    -(2 ^ 127 : Int) < a * b ∧ a * b < 2 ^ 127 := by
  sorry

set_option exponentiation.threshold 1100 in
/-- `approximate` は失敗しない。値の絶対値は 10^38 未満で、`Float64` の範囲に収まる（9.4節）。 -/
theorem pow38_lt_float64_max : (10 : Nat) ^ 38 < (2 ^ 53 - 1) * 2 ^ 971 := by
  exact Nat.lt_of_lt_of_le pow38_lt_pow127 (by decide)

end Zustral.Decimal