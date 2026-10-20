#!/usr/bin/env bash

show_usage() {
    ( echo "Usage: $(basename $0) CMD CMD-ARGS..."
      echo ""
      echo "Known CMD operations:"
      echo ""
      echo "  flake_update [OWNER]"
      echo "      - Updates the flake dependencies to the latest version available"
      echo "        at the flake input (usually a github/gitlab repository).  The"
      echo "        'OWNER' specification can be used to filter the"
      echo "        set of dependencies to only those provided by the specified"
      echo "        forge organization (e.g. galoisinc, taphos, etc.); note that the"
      echo "        OWNER must be specified in lowercase."
      echo ""
      echo "  cabal2nix OUTDIR FLAKE-INPUT [SUBDIR]"
      echo "      - Outputs a nix deriviation into the target directory for"
      echo "        each specified flake input (that is a valid hosted repo)"
      echo "        at the version in the flake.lock file.  The optional SUBDIR"
      echo "        indicates the subdirectory of the input that contains the"
      echo "        cabal file."
      echo ""
    ) 1>&2
}

visibly () {
    echo "$@"
    "$@"
}

run () {
    pkg=$1
    shift 1
    nix run nixpkgs#${pkg} -- "$@"
}


do_flake_update () {
    # Updates flake inputs.  The $1 parameter specifies a repo owner and all
    # flake inputs for whom that is the repo owner are updated.  If $1 is not
    # specified *all* flake inputs are updated.
    #
    if [ -z "$1" ]
    then visibly nix flake update --option tarball-ttl 60
    else
        # Get all dependencies that are in .root.inputs, hosted by github or
        # gitlab remotes, and owned by the input parameter.
        deps=$(nix flake metadata --json |
                   run jq -r --arg OWNER "$1" \
                       '.locks.nodes as $nodes
                       | $nodes.root.inputs
                       | to_entries[]
                       | select(.value | type == "string")
                       | .key as $input
                       | $nodes[.value].locked
                       | select((.type == "github" or .type == "gitlab")
                                and (.owner | ascii_downcase) == $OWNER)
                       | $input
                       ')
        if [ -z "$deps" ] ;
        then echo "No dependencies to update for the $1 organization." 1>&2
        else visibly nix flake update --option tarball-ttl 60 $deps
        fi
    fi
    return 0
}

do_cabal2nix_flake() {
    # Runs cabal2nix on the specified flake input ($2), writing the output to the
    # specified output directory ($1).  Parses the flake lockfile to identify the
    # input and its source and locked revision.  The name of the file written to
    # the output directory is determined by the Haskell package name in the
    # processed cabal file.  The optional subdirectory ($3) can specify which
    # subdirectory of the input source contains the project's cabal file (some
    # flake inputs contain multiple Haskell packages in different subdirectories;
    # a cabal2nix is performed for each package separately even though they share
    # a single flake input).
    outdir="${1}"
    INP="${2}"
    repoURL=$(nix flake metadata --json |
                  run jq -r --arg SRC "${INP}" \
                      '.locks.nodes.[$SRC].locked
                      | select (.type == "gitlab" or .type == "github")
                      | "https://\(.host // "github.com")/\(.owner)/\(.repo)"
                      ')
    if [ -z "${repoURL}" ]
    then echo "${INP} is not a valid flake input; skipping"
    else rev=$(nix flake metadata --json |
                   run jq -r --arg SRC "${INP}" '.locks.nodes.[$SRC].locked.rev')
         if [ -z "${3}" ]
         then
             echo run cabal2nix "${repoURL}" --revision="$rev"
             run cabal2nix "${repoURL}" --revision="$rev" > ${outdir}/.new.nix 2> ${outdir}/.new.log
         else
             echo run cabal2nix "${repoURL}" --revision="$rev" --subpath="$3"
             run cabal2nix "${repoURL}" --revision="$rev" --subpath="$3" > ${outdir}/.new.nix 2> ${outdir}/.new.log
         fi
         if [ $? -ne 0 ]
         then
             echo FAIL: Errors attempting to run cabal2nix 1>&2
             cat ${outdir}/.new.log
             return 1
         else
             rm -f ${outdir}/.new.log
             outname=$(sed -n -e '/pname =/s/ *pname *= *\"\(.*\)\";/\1/p' ${outdir}/.new.nix)
             if [ -z "${outname}" ]
             then echo Unable to determine cabal2nix target name for ${INP} 1>&2
                  rm -f ${outdir}/.new.nix
             else mv ${outdir}/.new.nix ${outdir}/${outname}.nix
             fi
         fi
    fi
}

case $1 in
    flake_update) do_flake_update "${2}";;
    cabal2nix)
        shift 1
        outdir="${1}"
        shift 1
        if [ -z "${outdir}" -o "$#" -eq 0 ]
        then echo "Missing arguments; check '$(basename $0) help' info" 1>&2
             exit 1
        else do_cabal2nix_flake "${outdir}" "$@"
        fi;;
    help) show_usage;;
    *) echo "UNRECOGNIZED command: '${1}'" 1>&2;
       echo "Use '$(basename $0) help' to get help" 1>&2;
       exit 1;;
esac

exit $?
