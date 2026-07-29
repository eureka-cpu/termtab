let
  inherit (pkgs) lib;

  sources = import ./npins;
  pkgs = import sources.nixpkgs { };

  workspace =
    let
      root = ./.;
      src = lib.cleanSourceWith {
        src = lib.cleanSource root;
        filter =
          path: _type:
          # Avoid rebuilds when changing nix files
            !lib.hasSuffix ".nix" path;
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

  termtab = pkgs.haskellPackages.callCabal2nix "termtab" workspace.src { };

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
          let
            treefmt = import sources.treefmt;
          in
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
      jujutsu
      nil
      npins
    ] ++ (with pkgs.haskellPackages; [
      cabal-install
      cabal-add
      haskell-language-server
    ]);
  };
}
