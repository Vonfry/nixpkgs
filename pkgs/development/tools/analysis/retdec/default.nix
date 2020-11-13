{ stdenv
, fetchFromGitHub
, fetchpatch
, fetchzip
, lib
, callPackage
, cmake
, autoconf
, automake
, libtool
, pkgconfig
, bison
, flex
, groff
, perl
, python3
, time
, upx
, ncurses
, libffi
, libxml2
, gnum4
, zlib
, withPEPatterns ? false
}:

let
  deps = callPackage ./deps.nix { inherit withPEPatterns; };
in stdenv.mkDerivation rec {
  pname = "retdec";

  # If you update this you will also need to adjust the versions of the updated dependencies. You can do this by first just updating retdec
  # itself and trying to build it. The build should fail and tell you which dependencies you have to upgrade to which versions.
  # I've notified upstream about this problem here:
  # https://github.com/avast-tl/retdec/issues/412
  version = "4.0";

  src = fetchFromGitHub {
    owner = "avast";
    repo = "retdec";
    name = "retdec-${version}";
    rev = "refs/tags/v${version}";
    sha256 = "0s2rhd7xaa4qxnxa0b0h1jvkx47m53mz02zb1qarvg4d1vld972j";
  };

  nativeBuildInputs = [
    cmake
    autoconf
    automake
    libtool
    pkgconfig
    bison
    flex
    groff
    perl
    python3
    gnum4
  ];

  buildInputs = [
    ncurses
    libffi
    libxml2
    zlib
  ];

  cmakeFlags = with lib;
    let
      depsName = [ "capstone" "googletest" "keystone" "llvm" "yara" "yaramod"
                   "openssl"
                 ];

      filterredDeps =
        filterAttrs (n: v: any (e: e == n) depsName) deps;

      depToLocal = name: value:
        "-D${toUpper name}_LOCAL_DIR=${value}";

      depsLocal = mapAttrsToList depToLocal filterredDeps;
    in [
      "-DRETDEC_TESTS=ON" # build tests
    ] ++ depsLocal;

  patches = [ ./deps.patch ];

  postPatch = with deps; ''
    # install retdec-support
    echo "Checking version of retdec-support"
    expected_version="$( sed -n -e "s|^ *\"https://github.com/avast/retdec-support/releases/download/\(.*\)/retdec-support_.*.tar.xz\"$|\1|p" 'cmake/deps.cmake' )"
    if [ "$expected_version" != '${retdec-support.version}' ]; then
      echo "The retdec-support dependency has the wrong version: ${retdec-support.version} while $expected_version is expected."
      exit 1
    fi
    mkdir -p "$out/share/retdec"
    cp -r ${retdec-support} "$out/share/retdec/support" # write permission needed during install
    chmod -R u+w "$out/share/retdec/support"

    # call correct `time` and `upx` programs
    substituteInPlace scripts/retdec-config.py --replace /usr/bin/time ${time}/bin/time
    substituteInPlace scripts/retdec-unpacker.py --replace "'upx'" "'${upx}/bin/upx'"
  '';

  enableParallelBuilding = true;

  doInstallCheck = true;
  installCheckPhase = ''
    ${python3.interpreter} "$out/bin/retdec-tests-runner.py"

    rm -rf $out/bin/__pycache__
  '';

  meta = with lib; {
    description = "A retargetable machine-code decompiler based on LLVM";
    homepage = "https://retdec.com";
    license = licenses.mit;
    maintainers = with maintainers; [ dtzWill timokau ];
    platforms = ["x86_64-linux" "i686-linux"];
  };
}
