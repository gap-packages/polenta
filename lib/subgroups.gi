#############################################################################
##
#W subgroups.gi            POLENTA package                     Bjoern Assmann
##
## Methods for the calculation of 
## certain subgroups of matrix groups
##
#H  @(#)$Id$
##
#Y 2004
##

#############################################################################
##
#F POL_TriangNSGFI_NonAbelianPRMGroup( arg )
##
## arg[1] = G is an non-abelian  polycyclic rational matrix group
##
InstallGlobalFunction( POL_TriangNSGFI_NonAbelianPRMGroup , function( arg )
    local   p, d, gens_p,G, bound_derivedLength, pcgs_I_p, gens_K_p,
            homSeries, gens_K_p_m, gens, gens_K_p_mutableCopy, pcgs,
            gensOfBlockAction, pcgs_nue_K_p, pcgs_GU, gens_U_p, pcgs_U_p;
    # setup
    G := arg[1];
    gens := GeneratorsOfGroup( G );
    d := Length(gens[1][1]);

    # determine an admissible prime or take the wished one
    if Length( arg ) = 2 then
        p := arg[2];
    else
        p := DetermineAdmissiblePrime(gens);
    fi;
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
    if pcgs_I_p = fail then return fail; fi;
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

    # compositions series
    Info( InfoPolenta, 1, "Compute the composition series ...");
    gens_K_p_mutableCopy := CopyMatrixList( gens_K_p );
    homSeries := POL_CompositionSeriesNormalGens( gens, 
                                                  gens_K_p_mutableCopy,
                                                  d );
    if homSeries=fail then return fail; fi;
    Info( InfoPolenta, 1,"finished.");   
    Info( InfoPolenta, 1, "The composition series has length ", 
                          Length( homSeries ), "." );
    Info( InfoPolenta, 2, "The composition series is" );
    Info( InfoPolenta, 2, homSeries );
    Info( InfoPolenta, 1, " " );

    # induce K_p to the factors of the composition series
    gensOfBlockAction := POL_InducedActionToSeries(gens_K_p, homSeries);
   
    # let nue be the homomorphism which induces the action of K_p to
    # the factors of the series
    Info( InfoPolenta, 1, "Compute a constructive polycyclic sequence\n", 
     "    for the induced action of the kernel to the composition series ...");
    pcgs_nue_K_p := CPCS_AbelianSSBlocks_ClosedUnderConj( gens_K_p,
                                                       gens, homSeries );
    if pcgs_nue_K_p = fail then return fail; fi;
    Info(InfoPolenta,1,"finished.");   

    # update generators of K_p
    gens_K_p := pcgs_nue_K_p.gens_K_p;
    pcgs_nue_K_p := pcgs_nue_K_p.pcgs_nue_K_p;
    Info( InfoPolenta, 1, "This polycyclic sequence has relative orders ",
                           pcgs_nue_K_p.relOrders, "."  );
    Info( InfoPolenta, 1, " " );

    return Group( gens_K_p );

end );

#############################################################################
##
#F POL_TriangNSGFI_PRMGroup( arg )
##
## arg[1] = G is a rational polycyclic rational matrix group
##
InstallGlobalFunction( POL_TriangNSGFI_PRMGroup , function( arg ) 
    local G;
    G := arg[1];
    if IsAbelian( G ) then
        return  G;
    else
        if IsBound( arg[2] ) then
             return POL_TriangNSGFI_NonAbelianPRMGroup( arg[1], arg[2] );
        else 
             return POL_TriangNSGFI_NonAbelianPRMGroup( G );
        fi;
    fi;
end );


#############################################################################
##
#M TriangNormalSubgroupFiniteInd( G )
##
## G is a matrix group over the Rationals. 
## Returned is triangularizable normal subgroup of finite index
##
##
InstallMethod( TriangNormalSubgroupFiniteInd, "for polycyclic matrix groups", 
                true, [ IsMatrixGroup ], 0, 
function( G ) 
        local test;
        test := POL_IsMatGroupOverFiniteField( G );
        if IsBool( test ) then
            TryNextMethod();
        elif test = 0 then
            return  POL_TriangNSGFI_PRMGroup(G );
        else
            TryNextMethod();
        fi;
end) ;

InstallOtherMethod( TriangNormalSubgroupFiniteInd, 
               "for polycyclic matrix groups", true,
               [ IsMatrixGroup, IsInt], 0, 
function( G, p ) 
        local test;
        test := POL_IsMatGroupOverFiniteField( G );
        if IsBool( test ) then
            TryNextMethod();
        elif test = 0 then
            if not IsPrime(p) then
                Print( "Second argument must be a prime number.\n" );
                return fail;
            fi;    
            return POL_TriangNSGFI_PRMGroup(G ); 
         else
            TryNextMethod();
         fi;

end );
#############################################################################
##
#M TriangNormalSubgroupFiniteIndUnipo( G )
##
## G is a matrix group over the Rationals. 
## Returned is triangularizable normal subgroup K of finite index
## and an unipotent normal subgroup U of K such that K/U is abelian.
##
InstallMethod( TriangNormalSubgroupFiniteIndUnipo, 
               "for polycyclic matrix groups", 
                true, [ IsMatrixGroup ], 0, 
function( G ) 

        local test, U_p, K_p, cpcs;
        test := POL_IsMatGroupOverFiniteField( G );
        if IsBool( test ) then
            TryNextMethod();
        elif test = 0 then
            cpcs := CPCS_PRMGroup( G );
            if cpcs = fail then return fail; fi;
            if IsAbelian( G ) then
                K_p := cpcs.pcs;
                U_p := cpcs.pcgs_U_p.pcs;
                return rec( T := Group( K_p ),
                            U := Group( U_p ) );
            else
                U_p := cpcs.pcgs_U_p.pcs;
                K_p := cpcs.pcgs_GU.preImgsNue;
                K_p := Concatenation( K_p, U_p );
                return rec( T := Group( K_p ),
                            U := Group( U_p ));
             fi;
        else
            TryNextMethod();
        fi;  
end );


InstallOtherMethod( TriangNormalSubgroupFiniteIndUnipo , 
               "for polycyclic matrix groups", true,
               [ IsMatrixGroup, IsInt], 0, 
function( G,p ) 

        local test, K_p, U_p, cpcs;
        test := POL_IsMatGroupOverFiniteField( G );
        if IsBool( test ) then
            TryNextMethod();
        elif test = 0 then
            cpcs := CPCS_PRMGroup( G,p  );
            if IsAbelian( G ) then
                K_p := cpcs.pcs;
                U_p := cpcs.pcgs_U_p.pcs;
                return rec( T := Group( K_p ),
                            U := Group( U_p ));
            else
                U_p := cpcs.pcgs_U_p.pcs;
                K_p := cpcs.pcgs_GU.preImgsNue;
                K_p := Concatenation( K_p, U_p );
                return rec( T := Group( K_p ),
                            U := Group( U_p ));
                fi;
        else
            TryNextMethod();
        fi;  
end );

#############################################################################
##
#E