#############################################################################
##
#W init.g                  POLENTA package                     Bjoern Assmann
##
##
#H  @(#)$Id$
##
#Y 2003
##


DeclarePackage( "polenta", "1.0", function() return true; end );
#DeclarePackageDocumentation( "polenta", "doc" );
 
#############################################################################
#R  read .gd files
##
ReadPkg( "polenta/lib/finite.gd" );
ReadPkg( "polenta/lib/info.gd" );
ReadPkg( "polenta/lib/basic.gd" );
ReadPkg( "polenta/exam/test.gd" );

ReadPkg( "polenta/lib/cpcs.gd" );
ReadPkg( "polenta/lib/present.gd" );
ReadPkg( "polenta/lib/solvable.gd" );
ReadPkg( "polenta/lib/series.gd" );

############################################################################
#R  read other packages
##
RequirePackage( "polycyclic" );
RequirePackage( "alnuth" );
RequirePackage( "aclib" );

#############################################################################
##
#E


