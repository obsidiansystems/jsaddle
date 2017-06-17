{ mkDerivation, stdenv, aeson, base, bytestring, jsaddle, data-default
, buildPackages
}:

mkDerivation {
  pname = "jsaddle-wkwebview";
  version = "0.9.0.0";
  src = ./.;
  # should use `librarySystemDepends` but it is not propagated
  libraryHaskellDepends = [
    aeson
    base
    bytestring
    jsaddle
    data-default
  ] ++ (with buildPackages; [
    darwin.libobjc
    darwin.apple_sdk.libs.xpc
  ] ++ (if buildPackages.osx_sdk or null == null then [ # macOS
    darwin.apple_sdk.frameworks.Foundation
    darwin.apple_sdk.frameworks.Cocoa
    darwin.apple_sdk.frameworks.WebKit
  ] else [ # iOS
    osx_sdk
  ]));
  description = "Interface for JavaScript that works with GHCJS and GHC";
  license = stdenv.lib.licenses.mit;
}
