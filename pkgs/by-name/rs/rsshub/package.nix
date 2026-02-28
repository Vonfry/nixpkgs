{
  lib,
  fetchFromGitHub,
  makeBinaryWrapper,
  nix-update-script,
  nodejs,
  pnpm,
  fetchPnpmDeps,
  pnpmConfigHook,
  replaceVars,
  stdenv,
}:
stdenv.mkDerivation (finalAttrs: {
  pname = "rsshub";
  version = "0-unstable-2026-02-28";

  src = fetchFromGitHub {
    owner = "DIYgod";
    repo = "RSSHub";
    rev = "1acb8057995a446574827b6e3e756de462e8f6be";
    hash = "sha256-I89mEL93rktDZdeSCQp6N6JCp7k93jvKS74ALXz6GUs=";
  };

  patches = [
    (replaceVars ./0001-fix-git-hash.patch {
      GIT_HASH = finalAttrs.src.rev;
    })
  ];

  pnpmDeps = fetchPnpmDeps {
    inherit (finalAttrs) pname version src;
    fetcherVersion = 3;
    hash = "sha256-/Rc8yb0To9rNs5XavFBjEImMk32/zFX03RHvI+Uj4gc=";
  };

  nativeBuildInputs = [
    makeBinaryWrapper
    nodejs
    pnpmConfigHook
    pnpm
  ];

  # Patch lib/registry.ts to add a BUILD_ROUTES_MODE branch that uses
  # directoryImport to collect route metadata without executing module-level
  # code, which would fail in the network-isolated Nix sandbox.
  # See: https://github.com/DIYgod/RSSHub/blob/master/flake.nix
  postPatch = ''
    substituteInPlace lib/registry.ts \
      --replace-fail 'if (config.isPackage)' \
                     'if (process.env.BUILD_ROUTES_MODE) {
        modules = directoryImport({
            targetDirectoryPath: path.join(__dirname, "./routes"),
            importPattern: /\.tsx?$/,
        }) as typeof modules;
    } else if (config.isPackage)'
  '';

  buildPhase = ''
    runHook preBuild
    # First build route metadata using directoryImport (avoids executing
    # module-level code that would trigger network requests)
    BUILD_ROUTES_MODE=1 pnpm run build:routes
    # Then build the application
    pnpm run build
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall
    mkdir -p $out/bin $out/lib/rsshub/lib
    cp -r dist node_modules $out/lib/rsshub
    cp -r lib/assets $out/lib/rsshub/lib
    runHook postInstall
  '';

  preFixup = ''
    makeWrapper ${lib.getExe nodejs} $out/bin/rsshub \
      --set "NODE_ENV" "production" \
      --set "NO_LOGFILES" "true" \
      --add-flags "$out/lib/rsshub/dist/index.mjs"
  '';

  passthru.updateScript = nix-update-script { extraArgs = [ "--version=branch=master" ]; };

  meta = {
    description = "RSS feed generator";
    longDescription = ''
      RSSHub is an open source, easy to use, and extensible RSS feed generator.
      It's capable of generating RSS feeds from pretty much everything.

      RSSHub delivers millions of contents aggregated from all kinds of sources,
      our vibrant open source community is ensuring the deliver of RSSHub's new routes,
      new features and bug fixes.
    '';
    homepage = "https://docs.rsshub.app";
    license = lib.licenses.agpl3Only;
    maintainers = with lib.maintainers; [ xinyangli ];
    mainProgram = "rsshub";
    platforms = lib.platforms.all;
  };
})
