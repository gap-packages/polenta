LoadPackage( "polenta" );
dirs := DirectoriesPackageLibrary( "polenta", "tst" );
tests := [
    "bugfix.tst",
    "polenta_finite.tst",
    "POLENTA.tst",
    "POLENTA2.tst", # slow
    #"POLENTA3.tst", # VERY slow
];
tests := List(tests, f -> Filename(dirs,f));

TestDirectory(tests, rec(exitGAP := true));
