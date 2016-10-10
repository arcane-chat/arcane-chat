
# fixupOutputHooks+=(_linkDLLs)

# For every *.{exe,dll} in $output/bin/ we try to find all (potential)
# transitive dependencies and symlink those DLLs into $output/bin
# so they are found on invocation.
# (DLLs are first searched in the directory of the running exe file.)
# The links are relative, so relocating whole /nix/store won't break them.
_linkDLLs() {
(
    if [ ! -d "$prefix/bin" ]; then exit; fi
    cd "$prefix/bin"

    # Compose path list where DLLs should be located:
    #   prefix $PATH by currently-built outputs
    local DLLPATH=""
    local outName
    for outName in $outputs; do
        addToSearchPath DLLPATH "${!outName}/bin"
    done
    DLLPATH="$DLLPATH:$PATH"

    echo DLLPATH="'$DLLPATH'"

    linkCount=0
    # Iterate over any DLL that we depend on.
    local dll
    for dll in $(objdump -p *.{exe,dll} | sed -n 's/.*DLL Name: \(.*\)/\1/p' | sort -u); do
        echo checking $dll
        if [ -e "./$dll" ]; then continue; fi
        # Locate the DLL - it should be an *executable* file on $DLLPATH.
        local dllPath="$(PATH="$DLLPATH" type -Pa "$dll" | tee tmp.log | grep -v tmp | head -n1)"
        cat tmp.log
        if [ -z "$dllPath" ]; then continue; fi
        # That DLL might have its own (transitive) dependencies,
        # so add also all DLLs from its directory to be sure.
        local dllPath2
        for dllPath2 in "$dllPath" "$(dirname "$dllPath")"/*.dll; do
            echo and checking $dllPath2
            if [ -e ./"$(basename "$dllPath2")" ]; then continue; fi
            ln -vsfr "$dllPath2" .
            linkCount=$(($linkCount+1))
        done
    done
    echo "Created $linkCount DLL link(s) in $prefix/bin"
)
}
