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
#M IsSolvableMatGroup( G )
##
## G is a matrix group over the Rationals or a finite field. 
##
DeclareOperation( "POL_IsSolvableMatGroup", [ IsMatrixGroup ] );

#############################################################################
##
#M IsPolycyclicMatGroup( G )
##
## G is a matrix group over the Rationals or a finite field. 
##
DeclareOperation( "IsPolycyclicMatGroup", [IsMatrixGroup] );

#############################################################################
##
#E


