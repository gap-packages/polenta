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
    Info( InfoPolenta, 1, "Chosen admissible prime: " , p );
    Info( InfoPolenta, 1, "  " );


    # calculate the gens of the group phi_p(<gens>) where phi_p is
    # natural homomorphism to GL(d,p)
    gens_p := InducedByField( gens, GF(p) );

    # determine un upperbound for the derived length of G
    bound_derivedLength := d+2;
 
    # finite part
    Info( InfoPolenta, 1,"Determine a constructive polycyclic sequence\n",
          "    for the image under the p-congruence homomorphism ..." );
    pcgs_I_p := CPCS_finite_word( gens_p, bound_derivedLength );
    if pcgs_I_p = fail then return false; fi;
    Info( InfoPolenta, 1, "Finite image has relative orders ",
                           RelativeOrdersPcgs_finite( pcgs_I_p ), "." );
    Info( InfoPolenta, 1, " " );


    # compute the normal the subgroup gens. for the kernel of phi_p
    Info( InfoPolenta, 1,"Compute normal subgroup generators for the kernel\n",
          "    of the p-congruence homomorphism ...");
    gens_K_p := POL_NormalSubgroupGeneratorsOfK_p( pcgs_I_p, gens );
    gens_K_p := Filtered( gens_K_p, x -> not x = IdentityMat(d) ); 
    Info( InfoPolenta, 1,"finished.");
    Info( InfoPolenta, 2,"The normal subgroup generators are" );
    Info( InfoPolenta, 2, gens_K_p );
    Info( InfoPolenta, 1, "  " );
  
 
    # homogeneous series
    Info( InfoPolenta, 1, "Compute the homogeneous series ... ");
    gens_K_p_mutableCopy := CopyMatrixList( gens_K_p );
    homSeries := POL_HomogeneousSeriesNormalGens( gens, 
                                                  gens_K_p_mutableCopy,
                                                  d );
    if homSeries = fail then 
        return false;
    else
        Info( InfoPolenta, 1,"finished.");
        Info( InfoPolenta, 1, "The homogeneous series has length ", 
                          Length( homSeries ), "." );
        Info( InfoPolenta, 2, "The homogeneous series is" );
        Info( InfoPolenta, 2, homSeries );
        Info( InfoPolenta, 1, " " );
        return true;
    fi;    
 
end;

#############################################################################
##
#F POL_IsSolvableFiniteMatGroup( G )
##
POL_IsSolvableFiniteMatGroup := function( G )
    local gens, d, CPCS, bound_derivedLength;
    # calculate a constructive pc-sequence
    gens := GeneratorsOfGroup( G );
    d := Length(gens[1][1]);
    # determine un upperbound for the derived length of G
    bound_derivedLength := d+2;

     Info( InfoPolenta, 1,"Determine a constructive polycyclic sequence\n",
           "    for the finite input group ..." );
    CPCS := CPCS_finite_word( gens, bound_derivedLength );

    if CPCS = fail then 
        return false;
    else 
        Info(InfoPolenta,1,"finished.");
        return true;
    fi;
end;

#############################################################################
##
#M IsSolvableMatGroup( G )
##
## G is a matrix group over the Rationals or a finite field. 
##
InstallMethod( POL_IsSolvableMatGroup, "for matrix groups", true,
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
#F POL_IsTriangularizableRationalMatGroup_infinite( G )
##
POL_IsTriangularizableRationalMatGroup_infinite := function( G )
  local   p, d, gens_p, bound_derivedLength, pcgs_I_p, gens_K_p,
            gens_K_p_m, gens, gens_K_p_mutableCopy, pcgs,
            gensOfBlockAction, pcgs_nue_K_p, pcgs_GU, gens_U_p, pcgs_U_p,
            radSeries, comSeries, recordSeries, isTriang;
    # setup
    gens := GeneratorsOfGroup( G );
    d := Length(gens[1][1]);

    # determine an admissible prime or take the wished one
    #if Length( arg ) = 2 then
    #   p := arg[2];
    #else
        p := DetermineAdmissiblePrime(gens);
    #fi;
    Info( InfoPolenta, 1, "Chosen admissible prime: " , p );
    Info( InfoPolenta, 1, "  " );

    # calculate the gens of the group phi_p(<gens>) where phi_p is
    # natural homomorphism to GL(d,p)
    gens_p := InducedByField( gens, GF(p) );

    # determine un upperbound for the derived length of G
    bound_derivedLength := d+2;
 
    # finite part
    Info( InfoPolenta, 1,"Determine a constructive polycyclic sequence\n",
          "    for the image under the p-congruence homomorphism ..." );
    pcgs_I_p := CPCS_finite_word( gens_p, bound_derivedLength );
    if pcgs_I_p = fail then return false; fi;
    Info(InfoPolenta,1,"finished.");
    Info( InfoPolenta, 1, "Finite image has relative orders ",
                           RelativeOrdersPcgs_finite( pcgs_I_p ), "." );
    Info( InfoPolenta, 1, " " );
 
    # compute the normal the subgroup gens. for the kernel of phi_p
    Info( InfoPolenta, 1,"Compute normal subgroup generators for the kernel\n",
          "    of the p-congruence homomorphism ...");      
    gens_K_p := POL_NormalSubgroupGeneratorsOfK_p( pcgs_I_p, gens );
    gens_K_p := Filtered( gens_K_p, x -> not x = IdentityMat(d) );
    Info( InfoPolenta, 1,"finished.");   
    Info( InfoPolenta, 2,"The normal subgroup generators are" );
    Info( InfoPolenta, 2, gens_K_p );
    Info( InfoPolenta, 1, "  " );

    # radical series
    Info( InfoPolenta, 1, "Compute the radical series ...");
    gens_K_p_mutableCopy := CopyMatrixList( gens_K_p );
    recordSeries := POL_RadicalSeriesNormalGensFullData( gens, 
                                                      gens_K_p_mutableCopy,
                                                      d );
    radSeries := recordSeries.sers;
    if radSeries=fail then return false; fi;
    Info( InfoPolenta, 1,"finished.");   
    Info( InfoPolenta, 1, "The radical series has length ", 
                          Length( radSeries ), "." );
    Info( InfoPolenta, 2, "The radical series is" );
    Info( InfoPolenta, 2, radSeries );
    Info( InfoPolenta, 1, " " );    

    # test if G is unipotent by abelian
    isTriang := POL_TestIsUnipotenByAbelianGroupByRadSeries( gens, radSeries );

    return isTriang;

end;

#############################################################################
##
#M IsTriangularizableMatGroup( G )
##
## G is a matrix group over the Rationals or a finite field. 
##
InstallMethod( IsTriangularizableMatGroup, "for matrix groups", true,
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
            return POL_IsTriangularizableRationalMatGroup_infinite( G ); 
        else
            TryNextMethod(); 
        fi;  
end );

#############################################################################
##
#E
