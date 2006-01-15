#############################################################################
##
#W solvalble.gi           POLENTA package                     Bjoern Assmann
##
## Methods for testing if a matrix group 
## is solvable or polycyclic
##
#H  @(#)$Id$
##
#Y 2003
##

#############################################################################
##
#M IsPolycyclicMatGroup( G )
##
## G is a matrix group over the Rationals or a finite field. 
##
DeclareOperation( "IsPolycyclicMatGroup", [IsMatrixGroup] );

#############################################################################
##
#M IsTriangularizableMatGroup( G )
##
## G is a matrix group over the Rationals. 
##
DeclareOperation( "IsTriangularizableMatGroup", [ IsMatrixGroup ] ); 
     
#############################################################################
##
#E


