{ mkDerivation, base, bytestring, containers, HUnit, lib
, template-haskell, text, time
}:
mkDerivation {
  pname = "witch";
  version = "0.3.4.0";
  src = ./..;
  libraryHaskellDepends = [
    base bytestring containers template-haskell text time
  ];
  testHaskellDepends = [
    base bytestring containers HUnit text time
  ];
  description = "Convert values from one type into another";
  license = lib.licenses.isc;
}
