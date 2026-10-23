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
         newfile="${outdir}/$$.new.nix"
         logfile="${outdir}/$$.new.log"
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
             return 1
         else
             rm -f ${logfile}
             outname=$(sed -n -e '/pname =/s/ *pname *= *\"\(.*\)\";/\1/p' ${newfile})
             if [ -z "${outname}" ]
             then echo Unable to determine cabal2nix target name for ${INP} 1>&2
                  rm -f ${newfile}
                  return 1
             else mv ${newfile} ${outdir}/${outname}.nix
             fi
         fi
    fi
    return 0
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
    help) show_usage;;
    *) echo "UNRECOGNIZED command: '${cmd}'" 1>&2;
       echo "Use '$(basename $0) help' to get help" 1>&2;
       exit 1;;
esac

exit $?
