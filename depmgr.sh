#!/usr/bin/env bash

show_usage() {
    ( echo "Usage: $(basename $0) CMD CMD-ARGS..."
      echo ""
      echo "Known CMD operations:"
      echo ""
      echo "  flake_update [OWNER] ..."
      echo "      - Updates the flake dependencies to the latest version available"
      echo "        at the flake input (usually a github/gitlab repository).  The"
      echo "        'OWNER' specification can be used to filter the"
      echo "        set of dependencies to only those provided by the specified"
      echo "        forge organization (e.g. galoisinc, taphos, etc.); note that the"
      echo "        OWNER must be specified in lowercase.  Multiple owners may be"
      echo "        specified on the command-line: each will be updated."
      echo ""
      echo "  cabal2nix OUTDIR FLAKE-INPUT [SUBDIR]"
      echo "      - Outputs a nix deriviation into the target directory for"
      echo "        the specified flake input (if it is a valid hosted repo)"
      echo "        at the version in the flake.lock file.  The optional SUBDIR"
      echo "        indicates the subdirectory of the input that contains the"
      echo "        cabal file."
      echo ""
      echo "  cabal2nixMany OUTDIR FLAKE-INPUT[/SUBDIR] ..."
      echo "      - Outputs a nix deriviation into the target directory for"
      echo "        each specified flake input (that is a valid hosted repo)"
      echo "        at the version in the flake.lock file.  The optional SUBDIR"
      echo "        indicates the subdirectory of the input that contains the"
      echo "        cabal file."
      echo ""
      echo "  makebom FLAKE_OUTPUT"
      echo "      - Generates a bill of materials for the specified flake"
      echo "        output package."
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
    # Updates flake inputs.  The parameters specifies repo owners and all flake
    # inputs for whom that is the repo owner are updated.  If no parameters are
    # specified *all* flake inputs are updated.
    #
    fui=${FLAKE_UPDATE_INTERVAL:-240}
    if [ $# -eq 0 ]
    then visibly nix flake update --option tarball-ttl $fui
    else
        # Get all dependencies that are in .root.inputs, hosted by github or
        # gitlab remotes, and owned by the input parameter.
        deps=$(for I in $*; \
               do nix flake metadata --json |
                   run jq -r --arg OWNER "$I" \
                       '.locks.nodes as $nodes
                       | $nodes.root.inputs
                       | to_entries[]
                       | select(.value | type == "string")
                       | .key as $input
                       | $nodes[.value].locked
                       | select((.type == "github" or .type == "gitlab")
                                and (.owner | ascii_downcase) == $OWNER)
                       | $input
                       '; \
               done)
        if [ -z "$deps" ] ;
        then echo "No dependencies to update for the $* organization(s)." 1>&2
        else visibly nix flake update --option tarball-ttl $fui $deps
        fi
    fi
    return 0
}

do_cabal2nix_many() {
    # Runs do_cabal2nix_flake on the set of flake inputs ($*) in parallel,
    # writing the output to the specified output directory ($1).  Each flake
    # input argument isthe name of a flake input with an optional /SUBDIR suffix.
    # If the SUBDIR suffix is present, that is used as the subdirectory of the
    # input source in which to run the cabal2nix update from.
    outdir="${1}"
    shift 1
    nprocs=0
    declare -a pids
    for X in $*
    do
        IFS=/ read INP SUBDIR <<<${X}
        do_cabal2nix_flake ${outdir} ${INP} ${SUBDIR} &
        pids[$nprocs]=$!
        nprocs=$(( nprocs + 1))
    done
    nfail=0
    for N in $(seq 0 $((nprocs - 1)))
    do
        wait ${pids[$N]} || nfail=$(( nfail + 1 ))
    done
    echo Updated $((nprocs - nfail)) of ${nprocs} successfully
    return ${nfail}
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
    tdir="$(mktemp -d)"
    repoURL=$(nix flake metadata --json |
                  run jq -r --arg SRC "${INP}" \
                      '.locks.nodes.[$SRC].locked
                      | select (.type == "gitlab" or .type == "github")
                      | "https://\(.host // "github.com")/\(.owner)/\(.repo)"
                      ')
    if [ -z "${repoURL}" ]
    then echo "${INP} is not a valid flake input; skipping"
         return 1
    else rev=$(nix flake metadata --json |
                   run jq -r --arg SRC "${INP}" '.locks.nodes.[$SRC].locked.rev')
         newfile="${tdir}/$$.new.nix"
         logfile="${tdir}/$$.new.log"
         if [ -z "${3}" ]
         then
             echo run cabal2nix "${repoURL}" --revision="$rev"
             run cabal2nix "${repoURL}" --revision="$rev" > ${newfile} 2> ${logfile}
         else
             echo run cabal2nix "${repoURL}" --revision="$rev" --subpath="$3"
             run cabal2nix "${repoURL}" --revision="$rev" --subpath="$3" > ${newfile} 2> ${logfile}
         fi
         if [ $? -ne 0 ]
         then
             echo FAIL: Errors attempting to run cabal2nix 1>&2
             cat ${logfile}
             [ -d "${tdir}" ] && rm -rf "${tdir}" || :
             return 1
         else
             outname=$(sed -n -e '/pname =/s/ *pname *= *\"\(.*\)\";/\1/p' ${newfile})
             if [ -z "${outname}" ]
             then echo Unable to determine cabal2nix target name for ${INP} 1>&2
                  [ -d "${tdir}" ] && rm -rf "${tdir}" || :
                  return 1
             else mv ${newfile} ${outdir}/${outname}.nix
             fi
         fi
    fi
    [ -d "${tdir}" ] && rm -rf "${tdir}" || :
    return 0
}

do_makebom() {
    # Writes the bill-of-materials for the specified flake output package to stdout.
    #
    # There are three parts:
    #  1. Obtain the source URL for all derivation inputs that have such,
    #     recursively.
    #  2. Flake inputs do not create derivation inputs with a URL and hash,
    #     just plain source checkouts in the nix store.  Instead, the URL
    #     and hash are in the flake.lock, so extract from there (also
    #     recursively).
    #  3. de-duplicate the combined results
    UFILE=$(mktemp)
    echo $(echo ${1} | sed -e s/#.*//) > $UFILE
    (
        # Get the URL(s) of hashed outputs.  The URL(s) are in the .env.urls
        # field, although sometimes we see env:{__json:"escaped json string"}
        # instead of env:{..., urls: ""}, and the 'urls' (if any) in the former
        # are actually a JSON list.
        nix derivation show --no-update-lock-file -r ${1} | \
            nix run nixpkgs#jq -- -r \
                '.[]
                | select(.outputs.out.hash)
                | .env.urls
                  // (.env.__json // "{}" | fromjson | .urls // [] | add)
                  // empty
                '
        # Now recursively process flake inputs.
        collect_flakeinp_urls 0 ${UFILE} $(echo ${1} | sed -e s/#.*//)
    ) | sort -u
    if [ "Z${UFILE}" == "Z" ] ; then :; else rm -f ${UFILE}; fi
}

collect_flakeinp_urls() {
    nestlvl="${1}"
    ufile="${2}"
    flakesrc="${3}"
    # n.b. Do not re-examine the derivation inputs of this flake because it was
    # already recursively obtained in do_makebom, and the flake inputs are not
    # "follows"-locked from the calling flake's specification.
    #
    # Uses the ufile for two reasons:
    #
    # 1. Ensures that a package whose parent flake already specified this package
    #    determines the version of that package used.
    #
    #    It's possible that the parent did not specify that input as a "follows"
    #    for the current flake, but that means that either (a) serendipitously
    #    the flake will be at the same version or (b) the flake is only used in
    #    sideline outputs (e.g. tests) that do not propagate through the flake
    #    tree.  Both of these do not invalidate the sbom search.
    #
    # 2. De-duplication so that the same packages are not recursively examined
    #    multiple times (which can slow things down).
    
    # Get inputs that are public (i.e. github only, no internal gitlab)
    nix flake metadata --no-update-lock-file --json ${flakesrc} | \
        nix run nixpkgs#jq -- -r \
            '.locks.nodes.[].locked
            | select (.type == "github")
            | "https://\(.host // "github.com")/\(.owner)/\(.repo)/commit/\(.rev)"
            '
    # Recurse into inputs that are themselves flakes (whether public or private).
    # Skip nixpkgs though: don't need to recurse through *all* packages!
    subflakes=$(nix flake metadata --no-update-lock-file --json ${flakesrc} | \
                nix run nixpkgs#jq -- -r \
                    '.locks.nodes.[]
                    | select( (has("flake") | not) or (.flake == true))
                    | .locked
                    | select ((.type == "gitlab" or .type == "github") and
                              (.repo == "nixpkgs" | not) )
                    | "git+ssh://git@\(.host // "github.com")/\(.owner)/\(.repo)?rev=\(.rev)"
                    ')

    newsubs=$(for S in ${subflakes}; do
                  srcbase=$(echo ${S} | sed -e s/\?.*//)
                  if ! grep -q ${srcbase} ${ufile}
                  then echo ${S}
                       echo ${srcbase} >> ${ufile}
                  fi
              done)

    for S in ${newsubs}
    do
        collect_flakeinp_urls $((nestlvl+1)) "${ufile}" "${S}"
    done
}
                    

cmd="${1}"
shift 1

case "$cmd" in
    flake_update) do_flake_update "$@";;
    cabal2nix)
        outdir="${1}"
        shift 1
        if [ -z "${outdir}" -o "$#" -eq 0 ]
        then echo "Missing arguments; check '$(basename $0) help' info" 1>&2
             exit 1
        else do_cabal2nix_flake "${outdir}" "$@"
        fi;;
    cabal2nixMany)
        outdir="${1}"
        shift 1
        if [ -z "${outdir}" -o "$#" -eq 0 ]
        then echo "Missing arguments; check '$(basename $0) help' info" 1>&2
             exit 1
        else do_cabal2nix_many "${outdir}" "$@"
        fi;;
    makebom)
        if [ "$#" -ne 1 ]
        then echo "Invalid arguments; check '$(basename $0) help' for info" 1>&2
             exit 1
        else do_makebom "$1"
        fi;;
    help) show_usage;;
    *) echo "UNRECOGNIZED command: '${cmd}'" 1>&2;
       echo "Use '$(basename $0) help' to get help" 1>&2;
       exit 1;;
esac

exit $?
