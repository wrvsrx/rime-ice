{ stdenvNoCC, haskellPackages }:
stdenvNoCC.mkDerivation {
  name = "";
  src = ./.;
  nativeBuildInputs = [
    (haskellPackages.ghcWithPackages (
      ps: with ps; [
        shake
        yaml
        utf8-string
      ]
    ))
    haskellPackages.haskell-language-server
  ];
}
