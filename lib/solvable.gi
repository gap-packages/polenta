#############################################################################
##
#W solvalble.gi           POLENTA package                     Bjoern Assmann
##
## Methods for testing a matrix group 
## is solvable or polycyclic
##
#H  @(#)$Id$
##
#Y 2003
##

#############################################################################
##
#F POL_IsSolvableRationalMatGroup_infinite( G )
##
POL_IsSolvableRationalMatGroup_infinite := function( G )
    local  p, d, gens_p, bound_derivedLength, pcgs_I_p, gens_K_p,
           homSeries, gens_K_p_m, gens, gens_K_p_mutableCopy, pcgs,
           gensOfBlockAction, pcgs_nue_K_p, pcgs_GU, gens_U_p,  pcgs_U_p;

    # setup
    gens := GeneratorsOfGroup( G );
    d := Length(gens[1][1]);

    # determine an admissible prime
    p := DetermineAdmissiblePrime(gens);

    # calculate the gens of the group phi_p(<gens>) where phi_p is
    # natural homomorphism to GL(d,p)
    gens_p := InducedByField( gens, GF(p) );

    # determine un upperbound for the derived length of G
    bound_derivedLength := d+2;
 
    # finite part
    Info( InfoPolenta, 1,"determine a constructive polycyclic sequence",
          "    for the image under the p-congruence homomorph.");
    pcgs_I_p := CPCS_finite_word( gens_p, bound_derivedLength );
    Info( InfoPolenta, 1, "finite image has relative orders ",
                           RelativeOrdersPcgs_finite( pcgs_I_p ) );
    if pcgs_I_p = fail then return false; fi;
 
    gens_K_p := POL_NormalSubgroupGeneratorsOfK_p( pcgs_I_p, gens );
    gens_K_p := Filtered( gens_K_p, x -> not x = IdentityMat(d) );   
 
    # homogeneous series
    Info( InfoPolenta, 1, "compute the homogeneous series ");
    gens_K_p_mutableCopy := CopyMatrixList( gens_K_p );
    homSeries := POL_HomogeneousSeriesNormalGens( gens, 
                                                  gens_K_p_mutableCopy,
                                                  d );
    if homSeries = fail then 
        return false;
    else
        Info( InfoPolenta, 2, "homogeneous series has length ", 
                          Length( homSeries ) );
        return true;
    fi;    
 
end;

#############################################################################
##
#F POL_IsSolvableFiniteMatGroup( G )
##
POL_IsSolvableFiniteMatGroup := function( G )
    local gens, d, CPCS, bound_derivedLength;
    # calculate a constructive pc-sequenz
    gens := GeneratorsOfGroup( G );
    d := Length(gens[1][1]);
    # determine un upperbound for the derived length of G
    bound_derivedLength := d+2;
    CPCS := CPCS_finite_word( gens, bound_derivedLength );

    if CPCS = fail then 
        return false;
    else 
        return true;
    fi;
end;

#############################################################################
##
#M IsSolvableMatGroup( G )
##
## G is a matrix group over the Rationals or a finite field. 
##
InstallMethod( IsSolvableMatGroup, "for matrix groups", true,
               [ IsMatrixGroup ], 0, 
function( G ) 
        local test;
        test := POL_IsMatGroupOverFiniteField( G );
        if IsBool( test ) then
            TryNextMethod();
        fi;
        if IsAbelian( G ) then
            return true;
        elif test = 0 then
            return POL_IsSolvableRationalMatGroup_infinite( G ); 
        else
            return POL_IsSolvableFiniteMatGroup( G );
        fi;  
end );

#############################################################################
##
#M IsSolvableGroup( G )
##
## G is a matrix group over the Rationals or a finite field. 
##
InstallMethod( IsSolvableGroup, "for matrix groups", true,
               [ IsMatrixGroup ], 0, 
function( G ) 
        local test;
        test := POL_IsMatGroupOverFiniteField( G );
        if IsBool( test ) then
            TryNextMethod();
        fi;
        if IsAbelian( G ) then
            return true;
        elif test = 0 then
            return POL_IsSolvableRationalMatGroup_infinite( G ); 
        else
            return POL_IsSolvableFiniteMatGroup( G );
        fi;  
end );

#############################################################################
##
#F POL_IsPolycyclicRationalMatGroup( G )
##
POL_IsPolycyclicRationalMatGroup := function( G )
     local  cpcs;
     cpcs := CPCS_PRMGroup( G );
     if cpcs = fail then
         return false;
     else 
         return true;
     fi; 
end;


#############################################################################
##
#M IsPolycyclicMatGroup( G )
##
## G is a matrix group over the Rationals or a finite field. 
##
InstallMethod( IsPolycyclicMatGroup, "for matrix groups", true,
               [ IsMatrixGroup ], 0, 
function( G ) 
        local test;
        test := POL_IsMatGroupOverFiniteField( G );
        if IsBool( test ) then
            TryNextMethod();
        fi;
        if IsAbelian( G ) then
            return true;
        elif test = 0 then
            return POL_IsPolycyclicRationalMatGroup( G ); 
        else
            return POL_IsSolvableFiniteMatGroup( G ); 
        fi;  
end );

#############################################################################
##
#E


