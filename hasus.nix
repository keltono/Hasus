{ mkDerivation, base, lib, mtl, parsec }:
mkDerivation {
  pname = "hasus";
  version = "0.1.0.0";
  src = ./src;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base mtl parsec ];
  license = "AGPLv3";
  hydraPlatforms = lib.platforms.none;
}
