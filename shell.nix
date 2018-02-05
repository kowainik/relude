{ghc}:
with (import <nixpkgs> {});

haskell.lib.buildStackProject {
  inherit ghc;
  name = "myEnv";
  buildInputs = [ zlib binutils gmp];
  buildPhase = ''
    export LANG=en_US.UTF-8
    '';
}
