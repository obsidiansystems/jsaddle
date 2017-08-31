{ mkDerivation, ghc, aeson, attoparsec, base, base64-bytestring
, bytestring, containers, deepseq, filepath, ghc-prim, http-types
, lens, primitive, process, random, ref-tf, scientific, stdenv, stm
, text, time, transformers, unordered-containers, vector, ghcjs-base
}:
mkDerivation {
  pname = "jsaddle";
  version = "0.9.2.1";
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [ ".git" "dist" ])) ./.;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring bytestring containers
    deepseq filepath ghc-prim http-types lens primitive process random
    ref-tf scientific stm text time transformers unordered-containers
    vector
  ] ++ (if ghc.isGhcjs or false then [
      ghcjs-base
  ] else []);
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
