#!/usr/bin/env bash

show_usage() {
    ( echo "Usage: $(basename $0) CMD CMD-ARGS..."
      echo ""
      echo "Known CMD operations:"
      echo ""
      echo "  flake_update [organization]"
      echo "      - Updates the flake dependencies to the latest version available"
      echo "        at the flake input (usually a github/gitlab repository).  The"
      echo "        'organization' specification can be used to filter the"
      echo "        set of dependencies to only those provided by the specified"
      echo "        forge organization (e.g. Galois, taphos, etc.)."
      echo ""
      echo "  cabal2nix OUTDIR {FLAKE-INPUT ...}"
      echo "      - Outputs a nix deriviation into the target directory for"
      echo "        each specified flake input (that is a valid hosted repo)"
      echo "        at the version in the flake.lock file."
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

# mkDeriv () {
#     # Older version of cabal->nix which took an owner and repo but assumed github
#     echo :::: Github $1 / $2 derivation generation;
#     run cabal2nix https://github.com/${1}/${2} \
#         --revision=$(nix flake metadata --json |
#                      run jq -r --arg SRC "${2}-src" '.locks.nodes.[$SRC].locked.rev') \
#         > ${2}.nix;
# }

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
        then echo "No dependencies to update for the $1 organization."
        else visibly nix flake update --option tarball-ttl 60 $deps
        fi
    fi
    return 0
}

do_cabal2nix_flake() {
    outdir="${1}"
    shift 1
    for INP in "${*}"
    do
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
             echo run cabal2nix "${repoURL}" --revision="$rev"
             run cabal2nix "${repoURL}" --revision="$rev" > ${outdir}/.new.nix
             outname=$(sed -n -e '/pname =/s/ *pname *= *\"\(.*\)\";/\1/p' ${outdir}/.new.nix)
             if [ -z "${outname}" ]
             then echo Unable to determine cabal2nix target name for ${INP} 1>&2
                  rm ${outdir}/.new.nix
             else mv ${outdir}/.new.nix ${outdir}/${outname}.nix
                  echo "###### Updated ${outdir}/${outname}.nix"
             fi
        fi
    done
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

exit 0
