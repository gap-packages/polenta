#
# Generate the manual using AutoDoc
#
if fail = LoadPackage("AutoDoc", ">= 2016.01.21") then
    Error("AutoDoc 2016.01.21 or newer is required");
fi;

AutoDoc(rec(
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
 ));
