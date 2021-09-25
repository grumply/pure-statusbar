{ mkDerivation, stdenv, ghc, base, pure-elm, pure-txt
}:
mkDerivation {
  pname = "pure-statusbar";
  version = "0.8.0.0";
  src = ./.;
  libraryHaskellDepends = [ base pure-elm pure-txt ];
  homepage = "github.com/grumply/pure-statusbar";
  license = stdenv.lib.licenses.bsd3;
}
