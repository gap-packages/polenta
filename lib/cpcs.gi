#############################################################################
##
#W cpcs.gi               POLENTA package                     Bjoern Assmann
##
## Methods for calculation of 
## constructive pc-sequences for rational matrix groups
##
#H  @(#)$Id$
##
#Y 2003
##

#############################################################################
##
#F CPCS_PRMGroup( arg )
##
## arg[1] = G is a rational polycyclic rational matrix group
##
InstallGlobalFunction( CPCS_PRMGroup , function( arg ) 
    local G;
    G := arg[1];
    if IsAbelian( G ) then
        return CPCS_AbelianPRMGroup( G );
    else
        if IsBound( arg[2] ) then
             return CPCS_NonAbelianPRMGroup( arg[1], arg[2] );
        else 
             return CPCS_NonAbelianPRMGroup( G );
        fi;
    fi;
end );

#############################################################################
##
#F CPCS_NonAbelianPRMGroup( arg )
##
## arg[1] = G is an non-abelian rational polycyclic rational matrix group
##
InstallGlobalFunction( CPCS_NonAbelianPRMGroup , function( arg )
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

    # calculate the gens of the group phi_p(<gens>) where phi_p is
    # natural homomorphism to GL(d,p)
    gens_p := InducedByField( gens, GF(p) );

    # determine un upperbound for the derived length of G
    bound_derivedLength := d+2;
 
    # finite part
    Info( InfoPolenta, 1,"determine a constructive polycyclic sequence\n",
          "    for the image under the p-congruence homomorph.");
    pcgs_I_p := CPCS_finite_word( gens_p, bound_derivedLength );
    Info( InfoPolenta, 1, "finite image has relative orders ",
                           RelativeOrdersPcgs_finite( pcgs_I_p ) );
    if pcgs_I_p = fail then return fail; fi;
 
    gens_K_p := POL_NormalSubgroupGeneratorsOfK_p( pcgs_I_p, gens );
    gens_K_p := Filtered( gens_K_p, x -> not x = IdentityMat(d) );   
 
    # compositions series
    Info( InfoPolenta, 1, "compute the composition series ");
    gens_K_p_mutableCopy := CopyMatrixList( gens_K_p );
    homSeries := POL_CompositionSeriesNormalGens( gens, 
                                                  gens_K_p_mutableCopy,
                                                  d );
    if homSeries=fail then return fail; fi;
    Info( InfoPolenta, 2, "composition series has length ", 
                          Length( homSeries ) );

    # induce K_p to the factors of the composition series
    gensOfBlockAction := POL_InducedActionToSeries(gens_K_p, homSeries);
   
    # let nue be the homomorphism which induces the action of K_p to
    # the factors of the series
    Info( InfoPolenta, 1, "compute a constructive polycyclic sequence\n", 
          "    for the induced action to the composition series");
    pcgs_nue_K_p := CPCS_AbelianSSBlocks_ClosedUnderConj( gens_K_p,
                                                       gens, homSeries );
    if pcgs_nue_K_p = fail then return fail; fi;
   
    # update generators of K_p
    gens_K_p := pcgs_nue_K_p.gens_K_p;
    pcgs_nue_K_p := pcgs_nue_K_p.pcgs_nue_K_p;
    Info( InfoPolenta, 1, "direct product of mult. subgroups of field",
                          "has relative orders ",
                           pcgs_nue_K_p.relOrders  );

    # constructive pc-sequence for G/U_p
    pcgs_GU := CPCS_FactorGU_p( gens, pcgs_I_p, gens_K_p,
                                pcgs_nue_K_p, homSeries, p );
  
    # normal subgroup generators for  U_p
    Info( InfoPolenta, 1, "calculating normal subgroup generators for the",
                          "\n    unipotent part" );
    gens_U_p := POL_NormalSubgroupGeneratorsU_p( pcgs_GU, gens, gens_K_p );

    # determine a constructive pc-sequence for the unipotent group U_p
    Info( InfoPolenta, 1 ,"determine a constructive polycyclic  sequence\n", 
                          "    for the unipotent part");
    pcgs_U_p := CPCS_Unipotent_Conjugation( gens, gens_U_p );
    if pcgs_U_p = fail then return fail; fi;

    # construct a pcs for the hole group
    pcgs := POL_MergeCPCS( pcgs_U_p, pcgs_GU);
 
    return pcgs;
end ); 

#############################################################################
##
#F CPCS_AbelianPRMGroup( G )
##
## G is an abelian rational polycyclic rational matrix group
##
InstallGlobalFunction( CPCS_AbelianPRMGroup , function( G )
    local   p, d, gens_p, bound_derivedLength, pcgs_I_p, gens_K_p,
            homSeries, gens_K_p_m, gens, gens_mutableCopy, pcgs,
            gensOfBlockAction, pcgs_nue_K_p, pcgs_GU, gens_U_p, pcgs_U_p;
    # setup
    gens := GeneratorsOfGroup( G );
    d := Length(gens[1][1]);

    # skip the the p-congruence homomorphism
    pcgs_I_p := rec( gens := [], relOrders := [], wordGens := []);
    gens_K_p := gens;
    p := 0;

    # composition series
    Info( InfoPolenta, 1, "compute the composition series ");
    gens_mutableCopy := CopyMatrixList( gens );
    homSeries := CompositionSeriesAbelianRMGroup( gens_mutableCopy, d );
    if homSeries = fail then return fail; fi; 
    Info( InfoPolenta, 2, "composition series has length ", 
                          Length( homSeries ) );

    # induce K_p to the factors of the composition series
    gensOfBlockAction := POL_InducedActionToSeries(gens_K_p, homSeries);
   
    # let nue be the homomorphism which induces the action of K_p to
    # the factors of the series
    Info( InfoPolenta, 1, "compute a constructive polycyclic sequence\n", 
          "    for the induced action to the composition series");
    pcgs_nue_K_p := CPCS_AbelianSSBlocks( gensOfBlockAction );
  
    # constructive pc-sequence for G/U_p
    pcgs_GU := CPCS_FactorGU_p( gens, pcgs_I_p, gens_K_p,
                                pcgs_nue_K_p, homSeries, p );
  
    # normal subgroup generators for  U_p
    Info( InfoPolenta, 1, "calculating normal subgroup generators for the",
                          "\n    unipotent part" );
    gens_U_p := POL_NormalSubgroupGeneratorsU_p( pcgs_GU, gens, gens_K_p );

    # determine a constructive pc-sequence for the unipotent group U_p
    Info( InfoPolenta, 1 ,"determine a constructive polycyclic  sequence\n", 
                          "    for the unipotent part");
    pcgs_U_p := CPCS_Unipotent_Conjugation( gens, gens_U_p );
    if pcgs_U_p = fail then return fail; fi;

    # construct a pcs for the hole group
    pcgs := POL_MergeCPCS( pcgs_U_p, pcgs_GU);
 
    return pcgs;
end ); 
   
#############################################################################
##
#F CPCS_FactorGU_p( gens, pcgs_I_p, gens_K_p, pcgs_nue_K_p, radicalSeries,p)
##
## calculates a constructive pcs for the G/U_p(G)
##
InstallGlobalFunction( CPCS_FactorGU_p , 
    function( gens, pcgs_I_p, gens_K_p, pcgs_nue_K_p, radicalSeries,p)
    local i,preImgsNue,preImgsI_p,pcs;

    # calculate preimages of the pcs of K_p(G)/U_p(G) which is
    # isomorph to nue(K_p(G))
    preImgsNue := POL_PreImagesPcsNueK_p_G( gens_K_p, pcgs_nue_K_p);
  
    # calculate the preimages of the pcs of G/K_p(G) which is isomorph
    # to I_p_G
    preImgsI_p := POL_PreImagesPcsI_p_G( pcgs_I_p, gens);
    # Attention in preImgsI_p we have a reversed order of the pcs so
    preImgsI_p := Reversed( preImgsI_p );
    Assert( 2,  TestPOL_PreImagesPcsI_p_G( preImgsI_p, p, pcgs_I_p ), 
            "error in the calculation of the preimages of I_p");

    # now we have the new pcs for G/U_p
    pcs := Concatenation( preImgsI_p, preImgsNue);
 
    return  rec( preImgsI_p := preImgsI_p,
                 preImgsNue := preImgsNue, 
                 pcs := pcs, 
                 p := p, 
                 radicalSeries := radicalSeries,
                 pcgs_I_p := pcgs_I_p,
                 pcgs_nue_K_p := pcgs_nue_K_p ); 
end );

#############################################################################
##
#F POL_PreImagesPcsNueK_p_G( gens_K_p, pcgs_nue_K_p )
##
InstallGlobalFunction( POL_PreImagesPcsNueK_p_G , 
                       function( gens_K_p, pcgs_nue_K_p )
    local preImages,i,l1,g,l;
 
    preImages := [];
    l1 := pcgs_nue_K_p.trsf;
    for l in l1 do
        g := gens_K_p[1]^0;
        for i in [1..Length(l)] do
            g := g*gens_K_p[i]^l[i];
        od;
        Add( preImages, g );
    od;
    return preImages;
end );
 
#############################################################################
##
#F POL_PreImagesPcsI_p_G( pcgs_I_p, gens )
##
InstallGlobalFunction( POL_PreImagesPcsI_p_G , function( pcgs_I_p, gens )
    local preImages,m,l,list,k;
 
    preImages := [];
    list := pcgs_I_p.wordGens;
    for l in list do
        k := gens[1]^0;
        for m in l do
            k := k*gens[m[1]]^m[2];
        od;
        Add(preImages,k);
    od;
    return preImages;
end );

#############################################################################
##
#F TestPOL_PreImagesPcsI_p_G( preImgsI_p, p, pcgs_I_p );
##
InstallGlobalFunction( TestPOL_PreImagesPcsI_p_G ,
                       function( preImgsI_p, p, pcgs_I_p )
    local n,i, test;
    n := Length( preImgsI_p );
    for i in [1..n] do
        test := ( preImgsI_p[i]*One(GF(p))
                =
                pcgs_I_p.gens[n-i+1]);
        if not test then
            return fail;
        fi;
    od;
    return true;
end );
 
#############################################################################
##
#F ExponentVector_CPCS_FactorGU_p(pcgs_GU,g)
##
InstallGlobalFunction( ExponentVector_CPCS_FactorGU_p , 
                       function( pcgs_GU, g )
    local h,exp_h,k,l,exp_l,exp,test;
 
    # if G is abelian skip the I_p part
    if pcgs_GU.p=0 then
        k := g;
        exp_h := [];
    else 
        # first we have to compute the part related to preImgs_I_p
        # compute the image in I_p
        h := g*One(GF(pcgs_GU.p));
        exp_h :=  ExponentvectorPcgs_finite( pcgs_GU.pcgs_I_p, h );
        if exp_h = fail then return fail; fi;
  
        # divide off to get the part of g which is in K_p
        k := POL_GetPartinK_P( g, exp_h, pcgs_GU.preImgsI_p );
  
        # assert that k is in K_p
        Assert( 2,  k*One( GF(pcgs_GU.p) ) = ( k*One(GF(pcgs_GU.p)) )^0,
            "Failure in ExponentVector_CPCS_FactorGU_p(pcgs_GU,g)\n");
 
    fi;
    # now compute the image under nue
    l:= POL_InducedActionToSeries( [k], pcgs_GU.radicalSeries );
 
    exp_l:=ExponentVector_AbelianSS( pcgs_GU.pcgs_nue_K_p, l );
    if exp_l = fail then return fail; fi; 
    # merge the exponents
    exp:=Concatenation(exp_h,exp_l);
    return exp;
end );

#############################################################################
##
#F POL_GetPartinK_P(g,exp_h,preImgsI_p)
##
InstallGlobalFunction( POL_GetPartinK_P, function( g, exp_h, preImgsI_p )
    local k,i;
    # we know g*K_p = preImgsI_p^exp_h * K_p
    k := StructuralCopy(g);
    for i in ([1..Length(exp_h)]) do
        k := ( preImgsI_p[i]^-exp_h[i] ) * k;
    od;
    return k;
end );
 
#############################################################################
##
#F RelativeOrders_CPCS_FactorGU_p( pcgs_GU )
##
InstallGlobalFunction( RelativeOrders_CPCS_FactorGU_p , function( pcgs_GU )
    local relOrders_I_p,relOrders_nue_K_p,n,relOrders;

    relOrders_I_p := RelativeOrdersPcgs_finite( pcgs_GU.pcgs_I_p );
    relOrders_nue_K_p := pcgs_GU.pcgs_nue_K_p.relOrders;
    # merge the relOrders
    relOrders := Concatenation(relOrders_I_p,relOrders_nue_K_p);
    return relOrders;
end );

#############################################################################
##
#F POL_MergeCPCS( pcgs_U_p, pcgs_GU)
##
InstallGlobalFunction( POL_MergeCPCS , function( pcgs_U_p, pcgs_GU )
    local pcs,rels1,rels2,rels3,rels;
 
    pcs := Concatenation( pcgs_GU.pcs, pcgs_U_p.pcs );
    rels1 := RelativeOrders_CPCS_FactorGU_p( pcgs_GU );
    rels2 := pcgs_U_p.rels;
    rels:=Concatenation(rels1,rels2);
    return rec( pcgs_U_p := pcgs_U_p,
                pcgs_GU := pcgs_GU, 
                pcs := pcs,
                rels := rels);
end );
 
#############################################################################
##
#F ExponentVector_CPCS_PRMGroup( matrix, pcgs )
##
## pcgs is the constructive pcs of a rational polycyclic matrix group
##
InstallGlobalFunction( ExponentVector_CPCS_PRMGroup, function(matrix,pcgs)
    local exp1,m1,m2,exp2,exp;
    # we have G = G/U * U, so matrix = m1 * m2
    exp1 := ExponentVector_CPCS_FactorGU_p( pcgs.pcgs_GU, matrix);
    if exp1 = fail then return fail; fi;
    m1 := Exp2Groupelement( pcgs.pcgs_GU.pcs, exp1 );
    m2 := (m1^-1)*matrix;
    exp2 := ExponentOfCPCS_Unipotent( m2, pcgs.pcgs_U_p );
    if exp2 = fail then return fail; fi;
    exp := Concatenation( exp1, exp2);
    Assert( 2, Exp2Groupelement( pcgs.pcs, exp) = matrix,
            "error in ExponentVector_CPCS_PRMGroup");
    return exp;
end );
 
#############################################################################
##
#E






