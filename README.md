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

## Example

Calculate and print the 10th Fibonacci number:

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

Build and run:

```bash
$ austral compile fib.aum --entrypoint=Fib:main --output=fib
$ ./fib
fib(10) = 55
```

## Building with Nix

If you have [Nix][nix], this will be much simpler. Just:

[nix]: https://nixos.org/

```bash
$ nix-shell
$ make
```

And you're done.

## Building without Nix

Building the `austral` compiler requires `make` and the `dune` build system for
OCaml, and a C compiler for building the resulting output. You should install
OCaml 4.13.0 or above.

First:

```bash
$ git clone git@github.com:austral/austral.git
$ cd austral
```

Next, install [opam][opam]. On Debian/Ubuntu you can just do:

```bash
$ sudo apt-get install opam
$ opam init
```

Then, create an opam switch for austral and install dependencies via opam:

```bash
opam switch create austral 4.13.0
eval $(opam env --switch=austral)
opam install --deps-only -y .
```

Finally:
```bash
make
```

To run the tests:

```bash
$ ./run-tests.sh
```

To build the standard library:

```bash
$ cd standard
$ make
```

## Usage

Suppose you have a program with modules `A`, `B`, and `C`, in the following
files:

```
src/A.aui
src/A.aum

src/B.aui
src/B.aum

src/C.aui
src/C.aum
```

To compile this, run:

```bash
$ austral compile \
    src/A.aui,src/A.aum \
    src/B.aui,src/B.aum \
    src/C.aui,src/C.aum \
    --entrypoint=C:main \
    --output=program
```

The `--entrypoint` option must be the name of a module, followed by a colon,
followed by the name of a public function with either of the following
signatures:

1. `function main(): ExitCode;`
2. `function main(root: RootCapability): ExitCode;`

The `ExitCode` type has two constructors: `ExitSuccess()` and `ExitFailure()`.

Finally, the `--output` option is just the path to dump the compiled C to.

By default, the compiler will emit C code and invoke `cc` automatically to
produce an executable. To just produce C code, use:

```bash
$ austral compile --target-type=c [modules...] --entrypoint=Foo:main --output=program.c
```

If you don't need an entrypoint (because you're compiling a library), instead of
`--entrypoint` you have to pass `--no-entrypoint`:

```bash
$ austral compile --target-type=c [modules...] --no-entrypoint --output=program.c
```

If you just want to typecheck without compiling, use the `tc` target type:

```bash
$ austral compile --target-type=tc [modules...]
```

Generated C code should be compiled with:

```bash
$ gcc -fwrapv generated.c -lm
```

## Status

1. The bootstrapping compiler, written in OCaml, is implemented. The main
   limitation is it does not support separate compilation. In practice this is
   not a problem: there's not enough Austral code for this to matter.

2. The compiler implements every feature of the spec.

3. A standard library with a few basic data structures and capability-based
   filesystem access is being designed.

## Contributing

See: [`CONTRIBUTING.md`](https://github.com/austral/austral/blob/master/CONTRIBUTING.md)

## Community

- [Discord](https://discord.gg/8cEuAcD8pM)

## Roadmap

Currently:

- Expanding the [standard
  library](https://github.com/austral/austral/tree/master/standard).

Near-future work:

- Build tooling and package manager.

# License

Copyright 2018–2023 [Fernando Borretti][fernando].

Licensed under the [Apache 2.0 license][apache] with the [LLVM exception][llvmex]. See the [LICENSE file][license] for details.

[opam]: https://opam.ocaml.org/doc/Install.html
[cap]: https://en.wikipedia.org/wiki/Capability-based_security
[sca]: https://en.wikipedia.org/wiki/Supply_chain_attack
[fernando]: https://borretti.me/

[apache]: https://www.apache.org/licenses/LICENSE-2.0
[llvmex]: https://spdx.org/licenses/LLVM-exception.html
[license]: https://github.com/austral/austral/blob/master/LICENSE
