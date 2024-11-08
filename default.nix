{ stdenvNoCC, haskellPackages }:
stdenvNoCC.mkDerivation {
  name = "";
  src = ./.;
  nativeBuildInputs = [
    (haskellPackages.ghcWithPackages (
      ps: with ps; [
        shake
        yaml
      ]
    ))
    haskellPackages.haskell-language-server
  ];
  installPhase = ''
    mkdir -p $out
  '';
}
