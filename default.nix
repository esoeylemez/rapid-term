{ mkDerivation, ansi-terminal, base, kan-extensions, process
, stdenv, unix
}:
mkDerivation {
  pname = "rapid-term";
  version = "0.1.0";
  src = ./.;
  libraryHaskellDepends = [
    ansi-terminal base kan-extensions process unix
  ];
  homepage = "https://github.com/esoeylemez/rapid-term";
  description = "External terminal support for rapid";
  license = stdenv.lib.licenses.bsd3;
}
