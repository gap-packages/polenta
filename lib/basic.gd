#############################################################################
##
#W basic.gd               POLENTA package                     Bjoern Assmann
##
## Methods for calculation of 
## constructive pc-sequences for a polycyclic rational matrix groups
##
#H  @(#)$Id$
##
#Y 2003
##


#############################################################################
##
#F DetermineAdmissiblePrime(gensOfG).........calculates a prime number which
##                                           does not divide one of
##                                           the entries of gensOfG or its
##                                           inverse
##
## input is a list of generators of a rational polycyclic matrix group
##
DeclareGlobalFunction( "DetermineAdmissiblePrime" );

############################################################################
##
#F POL_NormalSubgroupGeneratorsOfK_p(pcgs,gensOfRealG)
##
## pcgs is a constructive pc-Sequenz for I_p(G) 
## (image of G under the p-congruence hom.).
## This functions calculate the normal subgroupgenerators for K_p(G)
## (the kernel of the p-congruence hom.)
##
DeclareGlobalFunction( "POL_NormalSubgroupGeneratorsOfK_p" );

#############################################################################
##
#F Exp2Groupelement(list,exp)
##
DeclareGlobalFunction( "Exp2Groupelement" );
  
#############################################################################
##
#F CopyMatrixList(list)
##
DeclareGlobalFunction( "CopyMatrixList" );

#############################################################################
##
#F POL_CopyVectorList(list)
##
DeclareGlobalFunction( "POL_CopyVectorList" );

#############################################################################
##
#F POL_NormalSubgroupGeneratorsU_p( pcgs_GU, gens, gens_K_p )
##
## pcgs_GU  is a constructive pc-Sequenz for G/U,
## this functions calculate normal subgroupgenerators for U_p(G)
##
DeclareGlobalFunction( "POL_NormalSubgroupGeneratorsU_p" );

#############################################################################
##
#E






