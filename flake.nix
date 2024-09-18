{
  # This flake is not intended to provide an application or target
  # library---it provides nix flake support code for writing other
  # flakes.
  #
  # Flakes have several restrictions:
  #
  #  * inputs must be simple declarative specifications and cannot be
  #    functions or computations.
  #
  #  * inputs must be url references (paths or http accesses) and
  #    cannot be other nix types (strings, integers, bools, etc.)
  #
  # This means that although you can override the location of various
  # input url references, it is not possible to specify other
  # parameters... like compiler versions, debug v.s. non-debug, etc.
  #
  # In order to overcome those issues, the code here provides a matrix
  # of those elements *below* the output packages instead of as inputs.
  #
  # For example, if a C package could be built with gcc8, gcc9,
  # clang9, or clang10, these parameters could be provided to the
  # declaration code here to allow:
  #
  #    $ nix build .#package.gcc8
  #    $ nix build .#package.gcc9
  #    $ nix build .#package.clang9
  #    $ nix build .#package.clang10
  #
  # If the above is now extended to two different build types: debug
  # and optimized, this parameter can be provided as well, resulting
  # in targets that can specify one or both parameters, in any order:
  #
  #    $ nix build .#package.gcc8.default
  #    $ nix build .#package.gcc8.debug
  #    $ nix build .#package.debug.gcc8
  #    $ nix build .#package.clang10.opt
  #
  # Note that at any point the "default" keyword can be used to
  # specify that for all remaining parameters not explicitly
  # specified, the default value should be used.  The "default"
  # keyword must be the _last_ component specified.
  #
  # This flake also provides support for creating packages for various languages.
  #
  #   mkHaskellPkg - creates a cabal2nix-derived haskell package build specification.
  #

  description = "Generates parameterized flake configurations for language packages";

  inputs.nixpkgs.url = github:nixos/nixpkgs;

  outputs = { self, nixpkgs }: rec
    {
      supportedSystems = [
        "x86_64-linux"
        # "i686-linux"
        # "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      eachSystem = nixpkgs.lib.genAttrs supportedSystems;

      # For a given input list, return a list of all permutations of that
      # list.
      #
      # Example:
      #   permutations [1 2 3]
      #   => [ [ 1 2 3 ] [ 2 1 3 ] [ 2 3 1 ] [ 1 3 2 ] [ 3 1 2 ] [ 3 2 1 ] ]
      #
      permutations = list:
        with builtins;
        let subl = tail list;
            hd = head list;
            s = length list;
            insertAt = lst: pos:
              let b = genList (p: elemAt lst p) pos;
                  a = genList (p: elemAt lst (p + pos)) (s - 1 - pos);
              in b ++ [hd] ++ a;
            insertEachPos = lst: genList (insertAt lst) s;
        in if s == 0 then [[]]
           else concatLists (map insertEachPos (permutations subl));

      # Merges two attr sets.  Where an attribute's value in both
      # attrsets is also an attrset, those sub-attrsets are
      # recursively merged.  When an entry is not an attrset in both
      # input attrsets, the value from the second attrset is used.
      # Unique attrs in both inputs are directly copied through to the
      # output.
      mergeAttrSets = a: b:
        with builtins;
        let bNames = attrNames b;
            aNames = attrNames a;
            jointNames = filter (x: elem x bNames) aNames;
            aNamesLeft = filter (x: ! elem x jointNames) aNames;
            bNamesLeft = filter (x: ! elem x jointNames) bNames;
            solo = set: names: map(n: { name = n; value = set.${n}; }) names;
            aLeft = solo a aNamesLeft;
            bLeft = solo b bNamesLeft;
            merged = map mergeAt jointNames;
            mergeAt = n:
              if isAttrs a.${n} && isAttrs b.${n}
              then { name = n; value = mergeAttrSets a.${n} b.${n}; }
              else { name = n; value = b.${n}; };
        in listToAttrs (merged ++ aLeft ++ bLeft);

      # Variation targets are used to generate multi-layer nested
      # attribute sets with list-directed path variations, as well as
      # a default path.  The value at the leaves of each path are
      # supplied by calling the func with the various attribute
      # values.  Note that all path permutations are generated so that
      # the path values can be specified in any order (this implies no
      # duplication across listAttrs values).  The first value entry
      # for each attr's list is also recorded as the "default".
      #
      # This is probably best explained by an example:
      #   variedTargets { "cc": ["gcc" "clang" "icc"],
      #                   "cfg": ["json" "toml"],
      #                   "ui": ["curses"]
      #                 } (x: x)
      #   => { gcc = { json = { "curses"= { cc="gcc"; cfg="json"; ui="curses"; },
      #                         "default"= { cc="gcc"; cfg="json"; ui="curses"; } },
      #              , toml = { "curses"= { cc="gcc"; cfg="toml"; ui="curses"; },
      #                         "default"= { cc="gcc"; cfg="toml"; ui="curses"; } },
      #              , curses = { "json"= { cc="gcc"; cfg="json"; ui="curses"; },
      #                           "toml"= { cc="gcc"; cfg="toml"; ui="curses"; },
      #                           "default"= { cc="gcc"; cfg="json"; ui="curses"; } },
      #              , default = {  cc="gcc"; cfg="json"; ui="curses"; } },
      #      , clang = { json = { "curses"= { cc="clang"; cfg="json"; ui="curses"; },
      #                           "default"= { cc="clang"; cfg="json"; ui="curses"; },
      #                , toml = { "curses"= { cc="clang"; cfg="toml"; ui="curses"; },
      #                           "default"= { cc="clang"; cfg="toml"; ui="curses; },
      #                , curses = { "json"= { cc="clang"; cfg="json"; ui="curses"; },
      #                             "toml"= { cc="clang"; cfg="toml"; ui="curses"; },
      #                             "default"= { cc="clang"; cfg="json"; ui="curses"; } },
      #                , default = {  cc="clang"; cfg="json"; ui="curses"; },
      #      , icc = { json = { "curses"= { cc="icc"; cfg="json"; ui="curses"; },
      #                       , "default"= { cc="icc"; cfg="json"; ui="curses"; },
      #              , toml = { "curses"= { cc="icc"; cfg="toml"; ui="curses"; },
      #                       , "default"= { cc="icc"; cfg="toml"; ui="curses; },
      #              , curses = { "json"= { cc="icc"; cfg="json"; ui="curses"; },
      #                         , "toml"= { cc="icc"; cfg="toml"; ui="curses"; },
      #                         , "default"= { cc="icc"; cfg="json"; ui="curses"; } },
      #              , default = {  cc="icc"; cfg="json"; ui="curses"; },
      #      , json = { gcc = { "curses"= { cc="gcc"; cfg="json"; ui="curses"; },
      #                       , "default"= { cc="gcc"; cfg="json"; ui="curses"; },
      #               , clang = { "curses"= { cc="clang"; cfg="json"; ui="curses"; },
      #                         , "default"= { cc="clang"; cfg="json"; ui="curses; },
      #               , icc = { "curses"= { cc="icc"; cfg="json"; ui="curses"; },
      #                       , "default"= { cc="icc"; cfg="json"; ui="curses; },
      #              , curses = { gcc = { cc="gcc"; cfg="json"; ui="curses; },
      #                           clang = { cc="clang"; cfg="json"; ui="curses" },
      #                           icc = { cc="icc"; cfg="json"; ui="curses" },
      #                           default = { cc="gcc"; cfg="json"; ui="curses"; } },
      #              , default = {  cc="gcc"; cfg="json"; ui="curses"; },
      #      , toml = { gcc = { "curses"= { cc="gcc"; cfg="toml"; ui="curses"; },
      #                       , "default"= { cc="gcc"; cfg="toml"; ui="curses"; },
      #               , clang = { "curses"= { cc="clang"; cfg="toml"; ui="curses"; },
      #                         , "default"= { cc="clang"; cfg="toml"; ui="curses; },
      #               , icc = { "curses"= { cc="icc"; cfg="toml"; ui="curses"; },
      #                       , "default"= { cc="icc"; cfg="toml"; ui="curses; },
      #              , default = {  cc="gcc"; cfg="toml"; ui="curses"; },
      #      , curses = { gcc = { "curses"= { cc="gcc"; cfg="toml"; ui="curses"; },
      #                       , "default"= { cc="gcc"; cfg="toml"; ui="curses"; },
      #               , clang = { "curses"= { cc="clang"; cfg="toml"; ui="curses"; },
      #                         , "default"= { cc="clang"; cfg="toml"; ui="curses; },
      #               , icc = { "curses"= { cc="icc"; cfg="toml"; ui="curses"; },
      #                       , "default"= { cc="icc"; cfg="toml"; ui="curses; },
      #              , default = {  cc="gcc"; cfg="toml"; ui="curses"; },
      #      ...
      #
      # The result of the above allows specification of the
      # discriminated portions of the path (in any attr order), and
      # the use of "default" to get the default values for any
      # portions that are not discrminated.
      #
      # This function is particularly useful for a flake-specified
      # derivation that has multiple parameters.  The "func" is the
      # mkDerivation that takes the parameters as inputs to generate
      # the specifically-targeted mkDerivation.  See mkHaskellPkg
      # below.
      #
      variedTargets = listAttrs: func:
        with builtins;
        let aspects = attrNames listAttrs;
            aseqs = permutations aspects;
            valSeqTarget = args: keys:
              if length keys == 0
              then let tgt = func args; in tgt // { default = tgt; }
              else
                let
                  k = head keys;
                  keys_ = tail keys;
                  args_ = v: args // { ${k} = v; };
                  recurse = v: valSeqTarget (args_ v) keys_;
                  l = map (v: { name = v; value = recurse v; }) listAttrs.${k};
                  primary = listToAttrs l;
                  def = foldl' op primary keys;
                  op = curset: k:
                    getAttr (head listAttrs.${k}) curset;
                in primary // { default = def; };
            defaultPath = listAttrs: keys:
              concatStringsSep "."
                (map (k: head (listAttrs.${k})) keys);
        in foldl' (r: a: mergeAttrSets r (valSeqTarget {} a)) {} aseqs;

      # When variedTargets is used to generate a matrix of possible
      # derivations, the use of the right derivation in the table for
      # as an input (dependency) for a different package requires
      # selecting the right variant-guided entry from that matrix.
      # This function can be passed the variant attribute values and a
      # variedTargets package and it will select the specific package
      # configuration that most closely matches the variant attributes.
      #
      # If the input package is *not* a variedTargets output but is a
      # simple package, this will default to directly using that
      # simple package, so this is safe to apply to *all* input
      # dependencies.
      #
      #   returns: a specific derivation to use (for the current
      #            'system', based on the 'variantArgValues').
      #
      inputVariation =
        system:  # the string name of the target system
        variantArgValues:  # attrset of supplied variants and their
                           # current value
        targetName:  # string name of this target (flake outputs attribute)
        variantTarget:  # derivation (or derivation matrix) to resolve to a
                        # specific target based on the variantArgValues
        let defloc = x: x.default or x;
            outloc = x: if (builtins.isAttrs x)
                        then let r = pathloc x names;
                                 names = builtins.attrNames variantArgValues;
                                 isFlake = builtins.isAttrs (x."outputs" or null);
                             in if isFlake
                                then outloc x.outputs.packages.${system}."${targetName}"
                                else r
                        else x;
            # pathloc recursively walks the remNames (names of the
            # vargs) to see if the same path element exists on the
            # input specification.
            pathloc = x: remNames:
              if remNames == [] then x
              else let thisNm  = builtins.head remNames;
                       remNm   = builtins.tail remNames;
                       thisVal = variantArgValues.${thisNm};
                       step    = x.${thisVal} or x;
                   in pathloc step remNm;
        in outloc (defloc (outloc variantTarget));

      # Determine the appropriate drv target for all dependencies
      # (convenience wrapper around 'inputVariation' that runs against
      # every dependency and not just a single one).
      #
      #   returns: an attrset whose keys match 'dependencies' and the
      #            values are the resolve derivation to use (for the
      #            current 'system', based on the 'variantArgValues').
      #
      variedInputs =
        system:  # the string name of the target system
        dependencies:  # the attrset of the dependency
                       # packages/inputs, each of which which may a
                       # variant matrix of derivations or a direct
                       # derivation.
        variantArgValues:  # attrset of supplied variants and their
                           # current value
        let argNames = builtins.attrNames dependencies;
            verInpAttr = n: {
              name = n;
              value = inputVariation system variantArgValues n dependencies.${n};
            };
        in builtins.listToAttrs (builtins.map verInpAttr argNames);

      # Wraps a number of packages.  This is commonly used as a high-level test
      # or check target.  It does not have any real substance or build process
      # itself, but it requires the provided list of dependency targets to have
      # been built successfully before it can succeed.
      pkg_wrapper =
        system:
        pkgs:
        name:
        pkglist:
        builtins.derivation
          {
            inherit name system;
            builder = "${pkgs.bash}/bin/bash";
            args = [ "-c" "echo OK > $out" ];
            buildInputs = pkglist;
          };


      # ----------------------------------------------------------------------
      # Haskell Package Management

      # Returns a list of the valid GHC compiler attribute names for
      # the compiler set passed, in sorted order of most recently
      # *released* first.
      #
      # Example return:
      # [ "ghc8102", "ghc8101", "ghc883", # "ghc883Binary", "ghcHead", "ghcjs" ]
      #
      validGHCVersions =
        s:  # should be pkgs.haskell.compiler attrset for the current pkgs
        let names = builtins.attrNames s;
            validGHCName = n: s.${n} ? "version";
            validVersions = builtins.filter validGHCName names;
            # comparingReleases = a: b: a < b;
            comparingReleases = a: b:
              # a and b are: ghcjs, ghcHead, ghc883, ghc8101, ghc8102,
              # ghcjs86, ...  want to sort this with the highest
              # version down to the lowest so that the highest version
              # (the default) is the first in the list.
              let av = getVer a;
                  bv = getVer b;
                  getVer = x: let y = builtins.splitVersion x ++ [ "[none]" "[empty]" ];
                              in builtins.elemAt y 1 + "--" + builtins.elemAt y 2;
              in builtins.compareVersions av bv > 0;
            sortedVersions = builtins.sort comparingReleases validVersions;
            ghcVersions = sortedVersions;
        in ghcVersions;

      # Given a list of nixpkgs instances, this will determine all the GHC
      # versions supported across those instances.  The input nixpkgs list
      # is a list of items that can be imported:
      #
      #    import X { inherit system; }
      #
      # returns [{ this_nixpkgs: nixpkgs, ghcvers: [ "ghcv1" "ghcv2" .. ]}
      # for all ghcver found across all nixpkgs in nixpkgs_lst.
      #
      all_ghcver_and_nixpkgs =
        system:
        nixpkgs_lst: # [ X Y ] --> import X { inherit system; }
        let get_ghcvers_for_nixpkgs_ref = cur_nixpkgs:
              let pkgs = import cur_nixpkgs { inherit system; };
              in validGHCVersions pkgs.haskell.compiler;
            build_ghcvers_for_nixpkgs = accum: this_nixpkgs:
              let ghcvers = builtins.filter notSeenYet
                            (get_ghcvers_for_nixpkgs_ref this_nixpkgs);
                  notSeenYet = v:
                    ! (builtins.elem v
                      (builtins.concatLists
                        (builtins.map (a: builtins.getAttr "ghcvers" a) accum)));
              in accum ++ [ { inherit this_nixpkgs ghcvers;} ];
        in builtins.foldl' build_ghcvers_for_nixpkgs [] nixpkgs_lst;

      # The mkHaskellPkg is a convenience function to generate a
      # Haskell package derivation for the specified set of GHC
      # versions, given only the name, source, and overrides for that
      # derivation.  The principle elements needed for the derivation
      # come from the output of a cabal2nix derivation that acts as
      # input to this derivation.
      #
      # The mkHaskellPkg is designed to be used with flake-specified
      # Haskell packages and generate a derivation for each possible
      # GHC compiler version.
      #
      # The ovrDrvOrArgs can be used to override any arguments to the
      # derivation in a specific fashion.  This argument can be any
      # of:
      #
      #  * a function which is passed the variant args (e.g. {
      #    ghcver="..." }) as an argument and returns an attrset of
      #    override inputs for the standard derivation.
      #
      #  * an attrset.  Each attr will be the
      #    "attr.outputs.packages.${system}.attrname.${ghcver} if that
      #    attribute path exists, otherwise just attr.
      #
      #  * Anything else, which is just passed to the haskell
      #    callPackage as the override arguments.
      #
      # The following special attrs can be included in the
      # ovrDrvOrArgs; they are removed from the arguments passed to
      # the override but control other aspects of the derivation
      # creation:
      #
      #  * "adjustDrv" value is a function which is passed the variant
      #    args and the derivation and returns the derivation.  This
      #    can be useful to apply modifiers like the
      #    haskell.lib.dontCheck adjustment.
      #
      #  * "configFlags" value is a list of flags that is provided to
      #    both the cabal2nix and applied to the package derivation.
      #
      # The return is a derivation for the default configuration (to
      # satisfy other flake expectations like `nix flake check`), but
      # it also contains attributes for each variant value that return
      # a tree of derivations for the corresponding variant values.
      #
      mkHaskellPkg =
        { nixpkgs
        , system ? "x86_64-linux"
        , pkgs ? import nixpkgs { inherit system; }
        , ghcver ? validGHCVersions pkgs.haskell.compiler
        , ...
        } @ hpkgargs:
        name: src: ovrDrvOrArgs:
        let configFlags = ovrDrvOrArgs.configFlags or [];
            # varargs is the list of variant arguments (remove the expected ones)
            varargs = removeAttrs hpkgargs [ "nixpkgs" "system" "pkgs" ];
            # ovrargs is the list after overlay control args are
            # removed.  This should end up being just the dependencies
            # that are being overridden.
            ovrargs = removeAttrs ovrDrvOrArgs [ "adjustDrv" "configFlags" ];
            #
            adjDrv = ovrDrvOrArgs.adjustDrv or (va: drv: drv);
            adjCfg = drv: pkgs.haskell.lib.appendConfigureFlags drv
              (ovrDrvOrArgs.configFlags or []);

            targets = variedTargets
              # Vary over additional user arguments plus the known GHC versions
              (varargs // { ghcver = ghcver; })
              # Calling this function with each argument combination:
              ( { ghcver, ... } @ vargs:
                with builtins;
                let args = if isFunction ovrargs
                           then ovrargs vargs
                           else (if isAttrs ovrargs
                                 then variedInputs system ovrargs vargs
                                 else ovrargs);
                    callPkg = if pkgs.haskell.packages ? "${ghcver}"
                              then pkgs.haskell.packages.${ghcver}.callPackage
                              else abort "Requested ${ghcver} not available in: ${builtins.toString (builtins.attrNames pkgs.haskell.packages)}";
                    pkgSpec = fromCabal { inherit pkgs name src ghcver configFlags; };
                    hpkg = callPkg pkgSpec args;
                in adjCfg (adjDrv vargs hpkg)
              );
        in targets.default // targets;

      # This function is a wrapper for mkHaskellPkg that additionally takes a
      # list of nixpkgs entries.  It will create a haskell package declaration
      # for all GHC versions that exist across all the provided nixpkgs entries.
      mkHaskellPkgs =
        { system ? "x86_64-linux"
        , ...
        } @ hpkgargs:
        nixpkgs_list: name: src: ovrDrvOrArgs:
        let nixghcs = all_ghcver_and_nixpkgs system nixpkgs_list;
            addpkgsfornixghc = accum:
              { this_nixpkgs, ghcvers }:
              let thisHaskellPkgs =
                    let args = hpkgargs // {
                          nixpkgs = this_nixpkgs;
                          ghcver = ghcvers;
                        };
                    in mkHaskellPkg args name src ovrDrvOrArgs;
              in thisHaskellPkgs // accum;
              # in accum // thisHaskellPkgs;
        in builtins.foldl' addpkgsfornixghc {} nixghcs;

      fromCabal = { pkgs, name, src, ghcver, configFlags }:
        with builtins;
        let cFlags = concatStringsSep " " (configFlags ++ ["--compiler ${compiler}"]);
            cabalFlags = if isString src
                         then cFlags
                         else if hasAttr "dir" src
                              then cFlags + " --subpath ${src.dir}"
                              else cFlags;
            # Convert from the nix attribute name to the name needed by cabal2nix
            # See: nix eval '(let pkgs = import <nixpkgs> {}; in builtins.attrNames pkgs.haskell.compiler)'
            #   "ghc8101" => "ghc-8.10"
            #   "ghc884" => "ghc-8.8"
            #   etc.
            compilerVer = pkgs.haskell.compiler.${ghcver}.version or "0.0";
            majorMinor = v:
              let vl = splitVersion v;
              in if length vl > 1
                 then [(elemAt vl 0) (elemAt vl 1)]
                 else [0 0];
            compiler = "ghc-${concatStringsSep "." (majorMinor compilerVer)}";
        in pkgs.stdenv.mkDerivation {
          pname = "${name}-cabal2nix";
          version = "1";
          src = src;
          # n.b. the LANG specification avoids commitBuffer errors
          # from cabal2nix when UTF characters are encountered
          # (viz. path-io).
          buildPhase = ''
            mkdir $out
            export LANG=C.UTF-8
            echo ${pkgs.cabal2nix}/bin/cabal2nix ${cabalFlags} ${src} ">" $out/default.nix
            ${pkgs.cabal2nix}/bin/cabal2nix ${cabalFlags} ${src} > $out/default.nix
          '';
          installPhase = ": unused";
        };

    };
}

# KWQ: allow specifying hackage package override by version alone?
