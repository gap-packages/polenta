LoadPackage( "polenta" );
dirs := DirectoriesPackageLibrary( "polenta", "tst" );
ReadTest( Filename( dirs, "POLENTA.tst" ) );
ReadTest( Filename( dirs, "polenta_finite.tst" ) );

