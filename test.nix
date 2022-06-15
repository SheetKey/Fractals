{ mkDerivation, base, Chart, Chart-cairo, colour
, data-default-class, extra, gtk, gtk2hs-buildtools, lens, lib
, random, transformers
}:
mkDerivation {
  pname = "Fractals";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base Chart Chart-cairo colour data-default-class extra gtk
    gtk2hs-buildtools lens random transformers
  ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
