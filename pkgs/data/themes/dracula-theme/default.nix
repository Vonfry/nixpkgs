{ stdenv, fetchurl, gtk-engine-murrine }:

let
  themeName = "Dracula";
in
stdenv.mkDerivation rec {
  pname = "dracula-theme";
  version = "1.3.0"; # TODO wait for new release

  src = fetchFromGithub {
    owner = "dracula";
    repo = "gtk";
    rev = "v${version}";
    sha256 = "";
  };

  propagatedUserEnvPkgs = [
    gtk-engine-murrine
  ];

  installPhase = ''
    runHook preInstall
    mkdir -p $out/share/themes/${themeName}
    cp -a * $out/share/themes/${themeName}
    rm -r $out/share/themes/${themeName}/{Art,LICENSE,README.md,gtk-2.0/render-assets.sh}
    ln -s $out/share/themes/${themeName}/kde/sddm $out/share/themes/${themeName}/sddm
    runHook postInstall
  '';

  meta = with stdenv.lib; {
    description = "This theme provides support for GTK-3 and GTK-2 based desktop environments like Gnome, Unity, Budgie, Pantheon, XFCE, Mate, etc. Also provides support for KDE plasma.";
    homepage = "https://github.com/dracula/gtk";
    license = licenses.gpl3;
    platforms = platforms.all;
    maintainers = with maintainers; [ alexarice vonfry ];
  };
}
