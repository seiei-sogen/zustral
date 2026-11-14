Programs that execute lines of the compiler that no other test program executes:
the checks of the declaration stages (lib/ExtractionPass.ml, lib/CombiningPass.ml,
lib/CstUtil.ml), the checks of the entrypoint (lib/Entrypoint.ml), and paths of
the late stages (lib/Monomorphize.ml, lib/CodeGen.ml, lib/MtastUtil.ml). Each
test's README.md names the lines it is there for.
