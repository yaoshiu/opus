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
mkDerivation {
  pname = "opus";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base
    containers
    file-embed
    megaparsec
    mtl
    template-haskell
    text
  ];
  executableHaskellDepends = [
    base
    containers
    haskeline
    text
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
