{ mkDerivation
, stdenv
, lib

, opencv3

# library dependencies
, aeson
, base
, base64-bytestring
, bindings-DSL
, bytestring
, containers
, deepseq
, inline-c
, inline-c-cpp
, linear
, primitive
, repa
, template-haskell
, text
, transformers
, vector

# test dependencies
, directory
, Glob
, haskell-src-exts
, QuickCheck
, tasty
, tasty-hunit
, tasty-quickcheck

# benchmark dependencies
, criterion
}:
mkDerivation ({
  pname = "opencv";
  version = "0.0.0";
  src = builtins.filterSource (path: type:
          # Filter out .cpp files under ./src generated by inline-c:
            !(lib.hasPrefix (toString (./. + "/src")) (dirOf path)
              && lib.hasSuffix ".cpp" (baseNameOf path))

          # Filter out generated images:
          && !(  type != "directory"
              && (  (lib.hasSuffix "doc/generated"          (dirOf path))
                 || (lib.hasSuffix "doc/generated/examples" (dirOf path))
                 )
              )

          # Filter out .nix files so that changing them doesn't necessarily cause a rebuild:
          && !(lib.hasSuffix ".nix" (baseNameOf path))

          # Filter out some other files or directories not needed for a build:
          && !(builtins.elem (toString path) (map (p: toString (./. + "/${p}")) [
                "cabal.config"
                "dist"
                "examples"
                ".git"
                ".gitignore"
                "Makefile"
                "opencv-extra"
                "README.md"
              ]))
        ) ./.;

  libraryHaskellDepends = [
    aeson
    base
    base64-bytestring
    bindings-DSL
    bytestring
    containers
    deepseq
    inline-c
    inline-c-cpp
    linear
    primitive
    repa
    template-haskell
    text
    transformers
    vector
  ];

  testHaskellDepends = [
    base
    containers
    directory
    Glob
    haskell-src-exts
    QuickCheck
    tasty
    tasty-hunit
    tasty-quickcheck

    criterion
  ];

  libraryPkgconfigDepends = [ opencv3 ];

  configureFlags =
    [ "--with-gcc=g++"
      "--with-ld=g++"
    ];

  homepage = "https://github.com/LumiGuide/haskell-opencv";
  license = stdenv.lib.licenses.bsd3;
  maintainers = [ "engineering@lumi.guide" ];
} // (lib.optionalAttrs (lib.hasPrefix "16.09" lib.nixpkgsVersion) {
        hardeningDisable = [ "bindnow" ];
        shellHook = ''
          export hardeningDisable=bindnow
        '';
     }))
