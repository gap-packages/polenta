LoadPackage( "polenta" );
dirs := DirectoriesPackageLibrary( "polenta", "tst" );
tests := [
    "bugfix.tst",
    "polenta_finite.tst",
    "POLENTA.tst",
    #"POLENTA2.tst", # slow; requires aclibe
    #"POLENTA3.tst", # VERY slow
];
if IsPackageMarkedForLoading( "aclib" , "1.0" ) then
    Add(tests, "POLENTA2.tst"); # slow
fi;

tests := List(tests, f -> Filename(dirs,f));

TestDirectory(tests, rec(exitGAP := true));
