{ mkDerivation, attoparsec, base, brick, bytestring, containers
, file-embed, foldl, geoip2, iproute, lens, log-parser, pipes
, pipes-attoparsec, pipes-bytestring, pipes-group, pipes-parse
, protolude, QuickCheck, quickcheck-instances, safe-exceptions
, stdenv, stm, stringsearch, tailfile-hinotify, time
, unordered-containers, uri-bytestring, vty
}:
mkDerivation {
  pname = "ngx-top";
  version = "0.3.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base brick bytestring containers file-embed foldl geoip2
    iproute lens log-parser pipes pipes-attoparsec pipes-bytestring
    pipes-group pipes-parse protolude QuickCheck quickcheck-instances
    safe-exceptions stm stringsearch tailfile-hinotify time
    unordered-containers uri-bytestring vty
  ];
  executableHaskellDepends = [ base protolude ];
  testHaskellDepends = [ base protolude ];
  homepage = "https://github.com/pbogdan/ngx-top#readme";
  description = "Real-time stats for nginx logs";
  license = stdenv.lib.licenses.bsd3;
}
