{ stdenv, lib, buildPythonPackage, fetchPypi, pytest, mpg123, fuzzywuzzy
, pycryptodomex, requests-cache, importlib-metadata
, withCache ? true, aria2, withNotify ? stdenv.isLinux, libnotify
, withDesktopWords ? withNotify, dbus, pyqt5, qt5, dbus-python, withFuzzySearch ? true
, python-Levenshtein
}:

assert withCache -> aria2 != null;
assert withNotify -> libnotify != null;
assert withDesktopWords -> lib.all (p: p != null) [ qt5 pyqt5 dbus-python dbus ];
assert withFuzzySearch -> python-Levenshtein != null;

let
  wrapBinPath = with lib; makeBinPath ([ mpg123 ]
    ++ optional withNotify libnotify
    ++ optional withDesktopWords dbus
    ++ optional withCache aria2);
in buildPythonPackage rec {
  pname = "netease-musicbox";
  version = "0.3.0";

  src = fetchPypi {
    inherit pname version;
    sha256 = "0nvbwi266k7vw1411k7n3r2170ycpp53q9zx02akzqqgdc36x3gx";
  };

  # TODO add more dependencies
  checkInputs = [ pytest ];

  nativeBuildInputs = [ qt5.wrapQtAppsHook ];

  # TODO importlib-metadata is old on nixpkgs
  buildInputs = with lib;
    [ mpg123 fuzzywuzzy pycryptodomex requests-cache importlib-metadata ]
    ++ optional withFuzzySearch python-Levenshtein
    ++ optional withCache aria2
    ++ optionals withDesktopWords [ pyqt5 dbus-python dbus ]
    ++ optional withNotify libnotify;

  dontWrapQtApps = true;
  preFixup = ''
    for i in ${placeholder "out"}/bin/*; do
      wrapQtApp $i --prefix PATH : "${wrapBinPath}"
    done
  '';

  meta = with lib; {
    homepage = "https://github.com/darknessomi/musicbox";
    description = "Command line version for NetEase Music.";
    license = licenses.mit;
  };
}
