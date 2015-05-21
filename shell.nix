with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            ({ mkDerivation, aeson, base, base64-bytestring, bytestring
             , http-client, http-client-tls, lens, lens-action, mtl, network
             , pipes, pipes-aeson, pipes-http, pipes-parse, stdenv
             , template-haskell, text, unordered-containers, uuid
             }:
             mkDerivation {
               pname = "flowdock";
               version = "0.2.0.2";
               src = ./.;
               buildDepends = [
                 aeson base base64-bytestring bytestring http-client http-client-tls
                 lens lens-action mtl network pipes pipes-aeson pipes-http
                 pipes-parse template-haskell text unordered-containers uuid
               ];
               homepage = "https://github.com/brewtown/hs-flowdock";
               description = "Flowdock client library for Haskell";
               license = stdenv.lib.licenses.mit;
             }) {};
in
  pkg.env
