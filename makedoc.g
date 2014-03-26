#
# Generate the manual using AutoDoc
#
LoadPackage("AutoDoc", "2014.03.27");

SetPackagePath("Polenta", ".");
AutoDoc("Polenta" :
    scaffold := rec(
        bib := "polentabib.xml",
        includes := [
            "intro.xml",
            "methods.xml",
            "example.xml",
            "install.xml",
            "info.xml",
        ],
    )
 );

QUIT;
