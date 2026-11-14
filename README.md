# Zustralのコンセプト

ZustralはAustralをフォークした、業務バッチ処理のためのプログラミング言語である。次の4つを柱とする。

## 1. コンパクトなCOBOLの代替

Zustralは、Rustのような汎用言語を目指さない。大艦巨砲主義も目指さない。用途をCOBOLの代替、すなわち金融をはじめとする業務の10進計算とバッチ処理に特化する。

理想はコンパクトな言語である。ただし、COBOLの代替として最善を尽くした結果、中規模程度の大きさになる可能性はある。それでも大艦巨砲主義に陥らないよう、常に努める。

## 2. メモリ安全

Australの線形型を受け継ぎ、ガベージコレクションに頼らずにメモリ安全性を保証する。

## 3. 明示的であること（驚き最小の原則）

コードに書かれていないことは起きない。暗黙の型変換、隠れた制御フロー、見えないメモリ確保を排し、読んだとおりに動く言語にする。この点ではAustralの設計を土台に、Zigの良いところを取り入れる。

## 4. 金銭計算のバグを言語で防ぐ

金銭の計算、とりわけ10進計算で起きがちなバグを、プログラマの注意力ではなく言語と型システムによって防ぐ。誤った計算は実行前に弾く。これがZustralの核であり、1〜3はそれを支える土台である。

## 機能

- **線形型**：線形型によって、資源を安全だと証明できるやり方で扱える。メモリを
  安全に、実行時のオーバーヘッドなしに管理でき、`free()` の二重呼び出し、
  `free` したあとの使用、二重フェッチのエラーを避けられる。ファイルや
  データベースのハンドルのような、ほかの資源も安全に扱える。

- **ケイパビリティ**：線形の[ケイパビリティ][cap]によって、低水準の機能への
  アクセスに、細かく権限を与えられる。サードパーティの依存ライブラリが、
  どの種類の資源にアクセスできるかを制限できる。これにより、言語が
  [サプライチェーン攻撃][sca]を受けにくくなる。

- **型クラス**：Haskell から借りた型クラスによって、有界なアドホック多相が
  使える。

- **安全な算術**：Zustral は、数値型に対するすべての算術演算に、明確に定義
  された意味論を持つ。Ada と同じく、オーバーフローでトラップする算術と、
  剰余算術とで、別々の演算がある。

- **代数的データ型**：ML や Haskell と同じ代数的データ型を、網羅性の検査つきで
  備える。

## 持たない機能

- ガベージコレクションはない。
- デストラクタはない。
- 例外はない（不意打ちの制御フローもない）。
- 暗黙の関数呼び出しはない。
- 暗黙の型変換はない。
- 暗黙のコピーはない。
- グローバルな状態はない。
- サブタイピングはない。
- マクロはない。
- リフレクションはない。
- Java 風の @アノテーションはない。
- 型推論はない。型の情報は一方向に流れる。
- 初期化されていない変数はない。
- 前置・後置のインクリメントとデクリメント（C の `x++`）はない。
- 言語に組み込みの非同期（first-class async）はない。
- 関数のオーバーロードはない（型クラスによるものは除く。そちらは有界である）。
- 算術演算子の優先順位はない。
- 変数のシャドーイングはない。

## 例

10番目のフィボナッチ数を計算して表示する。

```ml
module body Fib is
    function fib(n: Nat64): Nat64 is
        if n < 2 then
            return n;
        else
            return fib(n - 1) + fib(n - 2);
        end if;
    end;

    function main(): ExitCode is
        print("fib(10) = ");
        printLn(fib(10));
        return ExitSuccess();
    end;
end module body.
```

ビルドして実行する。

```bash
$ austral compile fib.aum --entrypoint=Fib:main --output=fib
$ ./fib
fib(10) = 55
```

## Nix でビルドする

[Nix][nix] があれば、ずっと簡単になる。次を実行するだけである。

[nix]: https://nixos.org/

```bash
$ nix-shell
$ make
```

これで終わりである。

## Nix を使わずにビルドする

Zustral のコンパイラのビルドには、`make` と、OCaml のビルドシステム `dune` が
要る。出力されたものをビルドするには、C コンパイラが要る。OCaml は 4.13.0 以上を
入れること。コンパイラの実行ファイルの名前は、Austral から受け継いだ `austral` の
ままである。

まず、次を実行する。

```bash
$ git clone git@github.com:seiei-sogen/zustral.git
$ cd zustral
```

次に、[opam][opam] を入れる。Debian や Ubuntu なら、次のとおりでよい。

```bash
$ sudo apt-get install opam
$ opam init
```

それから、`austral` という名前の opam の switch を作り、opam で依存パッケージを入れる。

```bash
opam switch create austral 4.13.0
eval $(opam env --switch=austral)
opam install --deps-only -y .
```

最後に、次を実行する。
```bash
make
```

テストを実行するには、次のとおりにする。

```bash
$ ./run-tests.sh
```

標準ライブラリをビルドするには、次のとおりにする。

```bash
$ cd standard
$ make
```

## 使い方

モジュール `A`、`B`、`C` からなるプログラムが、次のファイルにあるとする。

```
src/A.aui
src/A.aum

src/B.aui
src/B.aum

src/C.aui
src/C.aum
```

これをコンパイルするには、次を実行する。

```bash
$ austral compile \
    src/A.aui,src/A.aum \
    src/B.aui,src/B.aum \
    src/C.aui,src/C.aum \
    --entrypoint=C:main \
    --output=program
```

`--entrypoint` オプションには、モジュールの名前、コロン、公開関数の名前を
続けて書く。その関数のシグネチャは、次のどちらかでなければならない。

1. `function main(): ExitCode;`
2. `function main(root: RootCapability): ExitCode;`

`ExitCode` 型には、`ExitSuccess()` と `ExitFailure()` の2つのコンストラクタがある。

最後に、`--output` オプションは、コンパイルした C を書き出すパスである。

既定では、コンパイラは C のコードを出力し、自動で `cc` を呼び出して実行ファイルを
作る。C のコードだけを作るには、次のとおりにする。

```bash
$ austral compile --target-type=c [modules...] --entrypoint=Foo:main --output=program.c
```

エントリポイントが要らない場合（ライブラリをコンパイルするときなど）は、
`--entrypoint` の代わりに `--no-entrypoint` を渡す必要がある。

```bash
$ austral compile --target-type=c [modules...] --no-entrypoint --output=program.c
```

コンパイルせずに型検査だけをしたい場合は、ターゲットの種類 `tc` を使う。

```bash
$ austral compile --target-type=tc [modules...]
```

生成された C のコードは、次のようにコンパイルすること。

```bash
$ gcc -fwrapv generated.c -lm
```

## 現状

1. OCaml で書かれたブートストラップ用のコンパイラは、実装済みである。主な制約は、
   分割コンパイルに対応していないことである。実際には問題にならない。それが効いて
   くるほどの量の Austral のコードが、まだないからである。

2. コンパイラは、仕様のすべての機能を実装している。

3. いくつかの基本的なデータ構造と、ケイパビリティに基づくファイルシステムへの
   アクセスを備えた標準ライブラリを、設計しているところである。

## 貢献

[`CONTRIBUTING.md`](CONTRIBUTING.md) を参照のこと。

## コミュニティ

- [Discord](https://discord.gg/8cEuAcD8pM)（本家 Austral のコミュニティ）

## ロードマップ

現在の作業：

- [標準ライブラリ](standard)を
  広げる。

近いうちの作業：

- ビルドの道具と、パッケージマネージャ。

# ライセンス

Copyright 2018–2023 [Fernando Borretti][fernando].

[Apache 2.0 ライセンス][apache]に、[LLVM 例外][llvmex]を付けて提供する。詳しくは [LICENSE ファイル][license]を参照のこと。

[opam]: https://opam.ocaml.org/doc/Install.html
[cap]: https://en.wikipedia.org/wiki/Capability-based_security
[sca]: https://en.wikipedia.org/wiki/Supply_chain_attack
[fernando]: https://borretti.me/

[apache]: https://www.apache.org/licenses/LICENSE-2.0
[llvmex]: https://spdx.org/licenses/LLVM-exception.html
[license]: LICENSE
