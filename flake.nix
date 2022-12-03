{
  description = "My Lean package";

  inputs.lean.url = github:leanprover/lean4;
  inputs.lean-doc.url = github:leanprover/lean4?dir=doc;
  inputs.lean-doc.inputs.lean.follows = "lean";
  inputs.lean-doc.inputs.flake-utils.follows = "lean/flake-utils";
  inputs.flake-utils.url = github:numtide/flake-utils;
  inputs.std4.url = github:leanprover/std4;
  inputs.std4.flake = false;

  outputs = inputs: inputs.flake-utils.lib.eachDefaultSystem (system:
    let
      leanPkgs = inputs.lean.packages.${system};
      lean-doc = inputs.lean-doc.packages.${system};
      std4 = leanPkgs.buildLeanPackage {
        name = "Std";
        src = inputs.std4;
      };
      pkg = leanPkgs.buildLeanPackage {
        name = "Aoc";
        deps = [ std4 ];
        src = ./.;
        roots = [ { mod = "Aoc"; glob = "submodules"; } ];
        overrideBuildModAttrs = final: prev: with leanPkgs.nixpkgs; {
          LEAN_FILENAME = "${final.src}/${baseNameOf prev.relpath}";
          src = builtins.filterSource (e: _: lib.hasPrefix (baseNameOf prev.relpath) (baseNameOf e)) ./Aoc;
        };
      };
    in {
      packages = pkg // (with leanPkgs.nixpkgs; rec {
        inherit (leanPkgs) lean;
        renders = lean-doc.renderPackage pkg;

        book = stdenv.mkDerivation {
          name = "do-doc";
          buildInputs = [ lean-doc.lean-mdbook ];
          src = ./doc;
          buildPhase = ''
            mkdir $out
            # necessary for `additional-css`...?
            cp -r --no-preserve=mode ${inputs.lean-doc}/doc/*.{js,css} .
            cp -r ${renders}/* .
            for f in Aoc/*.lean.md; do
              [[ $f =~ .*([0-9]+).* ]] && echo "- [Day ''${BASH_REMATCH[1]}]($f)" >> SUMMARY.md
            done
            mdbook build -d $out
        '';
          dontInstall = true;
        };
      });

      defaultPackage = pkg.modRoot;
    });
}
