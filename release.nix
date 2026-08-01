let
  inherit (pkgs) lib;

  sources = import ./npins;
  pkgs = import sources.nixpkgs { };

  workspace =
    let
      root = ./.;
      src = lib.cleanSourceWith {
        src = lib.cleanSource root;
        filter = path: _type:
          !lib.hasSuffix ".nix" path; # Avoid rebuilds when changing nix files
      };
    in
    {
      inherit root src;
      formattingOptions = {
        projectRootFile = "termtab.cabal";
        programs = {
          nixpkgs-fmt.enable = true;
          yamlfmt.enable = true;
          fourmolu.enable = true;
        };
      };
    };

  # TODO: Is this still necessary?
  # Bravura ships under a lilypond-version-dependent subpath; pin the .otf to a
  # stable store path so the build/runtime never hardcodes the lilypond version.
  bravuraOtf = pkgs.runCommand "bravura-otf" { } ''
    cp "$(find ${pkgs.openlilylib-fonts.bravura} -name Bravura.otf | head -1)" "$out"
  '';

  termtab = pkgs.haskellPackages.callCabal2nix "termtab" workspace.src {
    inherit (pkgs) fluidsynth freetype;
  };

  checks = {
    inherit termtab;

    hlint =
      pkgs.runCommand "hlint" { buildInputs = [ pkgs.haskellPackages.hlint ]; } ''
        cd ${workspace.src}
        hlint app
        touch $out
      '';

    treefmt-check =
      let
        treefmt =
          let treefmt = import sources.treefmt; in
          treefmt.evalModule pkgs workspace.formattingOptions;
      in
      treefmt.config.build.check workspace.src;
  };
in
{
  inherit termtab checks;

  devshell = pkgs.mkShell {
    inputsFrom = [ termtab.env ] ++ builtins.attrValues checks;
    buildInputs = with pkgs; [
      fluidsynth
      pkg-config
      soundfont-fluid
      jujutsu
      nil
      npins
    ] ++ (with pkgs.haskellPackages; [
      cabal-install
      cabal-add
      haskell-language-server
    ]);
    TERMTAB_SOUNDFONT = "${pkgs.soundfont-fluid}/share/soundfonts/FluidR3_GM2-2.sf2";
    TERMTAB_BRAVURA_FONT = "${bravuraOtf}";
  };
}
