{
  description = "My Lean package";

  inputs.lean.url = github:leanprover/lean4;
  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.std4.url = github:leanprover/std4;
  inputs.std4.flake = false;

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      leanPkgs = inputs.lean.packages.${system};
      std4 = leanPkgs.buildLeanPackage {
        name = "Std";
        src = inputs.std4;
      };
      pkg = leanPkgs.buildLeanPackage {
        name = "Aoc";
        deps = [ std4 ];
        src = ./.;
        roots = [ { mod = "Aoc"; glob = "submodules"; } ];
        overrideBuildModAttrs = x: _: with leanPkgs.nixpkgs; {
          src = builtins.filterSource (e: _: lib.hasPrefix (baseNameOf x.relpath) (baseNameOf e)) ./Aoc;
        };
      };
    in {
      packages = pkg // {
        inherit (leanPkgs) lean;
      };

      defaultPackage = pkg.modRoot;
    });
}
