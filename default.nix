{
  mkDerivation,
  base,
  containers,
  file-embed,
  haskeline,
  lib,
  megaparsec,
  mtl,
  template-haskell,
  text,
}:
let
  version = "0.1.0.0";
  core = mkDerivation {
    pname = "opus-core";
    inherit version;
    src = ./.;
    postUnpack = "sourceRoot+=/core; echo source root reset to $sourceRoot";
    isLibrary = true;
    libraryHaskellDepends = [
      base
      containers
      file-embed
      megaparsec
      mtl
      template-haskell
      text
    ];
  };
in
mkDerivation {
  pname = "opus";
  version = "0.1.0.0";
  src = ./.;
  postUnpack = "sourceRoot+=/cli; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  executableHaskellDepends = [
    base
    containers
    haskeline
    text
    core
  ];
  license = lib.licenses.bsd3;
  mainProgram = "opus";
  homepage = "https://github.com/yaoshiu/scheme";
  description = "Opus language interpreter";
  maintainers = [
    {
      name = "Fay Ash";
      github = "yaoshiu";
      githubId = 56054933;
    }
  ];
}
