#############################################################################
##
#W present.gd              POLENTA package                     Bjoern Assmann
##
## Methods for the calculation of
## pcp-presentations for matrix groups
##
#Y 2003
##

#############################################################################
##
#O PcpGroupByMatGroup( G )
##
DeclareOperation( "PcpGroupByMatGroup", [ IsMatrixGroup ] );

DeclareProperty( "IsIsomorphismByFinitePolycyclicMatrixGroup",
                  IsMapping);
DeclareProperty( "IsIsomorphismByPolycyclicMatrixGroup",
                  IsMapping);

#############################################################################
##
#E
