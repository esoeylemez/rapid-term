{ mkDerivation, base, clock, kan-extensions, process, stdenv
, transformers, unix
}:
mkDerivation {
  pname = "rapid-term";
  version = "0.1.1";
  src = ./.;
  libraryHaskellDepends = [
    base clock kan-extensions process transformers unix
  ];
  homepage = "https://github.com/esoeylemez/rapid-term";
  description = "External terminal support for rapid";
  license = stdenv.lib.licenses.bsd3;
}
