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
        raw-strings-qq
      ]
    ))
    haskellPackages.haskell-language-server
  ];
}
