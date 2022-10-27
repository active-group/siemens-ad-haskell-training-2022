# a package (or: derivation) is a function
{ stdenv, gcc, cowsay }:

# a description of the build process and the result
stdenv.mkDerivation {
    # built-in attributes
    pname = "hello-siemens-package";
    version = "0.1.0";
    src = ./src;

    # the build of a package decomposes into several "phases"
    # unpackPhase -> irrelevant here
    # patchPhase -> irrelevant here
    # configurePhase -> irrelevant here

    # A list of packages you want to use during the build phase
    buildInputs = [ gcc cowsay ];
    # shell script with everything in scope
    buildPhase = ''
        cowsay hiiii
        gcc main.c
    '';

    # another script
    # have to create an output path at $out
    installPhase = ''
        mkdir -p $out/bin
        mv a.out $out/bin/hello-siemens-package
    '';
}