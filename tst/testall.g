LoadPackage( "polenta" );
dirs := DirectoriesPackageLibrary( "polenta", "tst" );

Test( Filename( dirs, "bugfix.tst" ) );
Test( Filename( dirs, "polenta_finite.tst" ) );
Test( Filename( dirs, "POLENTA.tst" ) );
Test( Filename( dirs, "POLENTA2.tst" ) ); # slow
#Test( Filename( dirs, "POLENTA3.tst" ) ); # VERY slow
