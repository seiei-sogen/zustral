{ nixpkgs ? import <nixpkgs> { }}:

let
  nixpkgsTag = "23.05";
  # A checkout of the same tag, made with git by
  # tools/coding-agent-setup/fetch-pinned-nixpkgs-with-git.sh. Where the tarball below cannot
  # be downloaded (the Claude Code cloud environment blocks it, issue #42), the checkout is used.
  nixpkgsCheckoutMadeWithGit = builtins.getEnv "HOME" + "/.cache/zustral/nixpkgs-${nixpkgsTag}";
  # The commit of the tag. A checkout at another commit is not used: the tarball is
  # pinned by its hash, and the checkout is pinned by this.
  nixpkgsTagCommit = "4ecab3273592f27479a583fb6d975d4aba3486fe";
  checkoutIsAtTheTagCommit =
    builtins.pathExists (nixpkgsCheckoutMadeWithGit + "/.git/HEAD")
    && builtins.readFile (nixpkgsCheckoutMadeWithGit + "/.git/HEAD") == nixpkgsTagCommit + "\n";
  pinnedPkgs =
    if checkoutIsAtTheTagCommit
    then nixpkgsCheckoutMadeWithGit
    else builtins.fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/${nixpkgsTag}.tar.gz";
      sha256 = "10wn0l08j9lgqcw8177nh2ljrnxdrpri7bp0g7nvrsn9rkawvlbf";
    };
  pkgs = import pinnedPkgs {};
in

pkgs.mkShell {
  buildInputs = with pkgs; [
    pyright
    black
    isort
  ];
}
