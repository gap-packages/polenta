#############################################################################
##
#W test.gi                 POLENTA package                     Bjoern Assmann
##
## examples for polycyclic rational matrix groups
##
#H  @(#)$Id$
##
#Y 2003
##

#############################################################################
##
#F POL_RandomGroupElement( gens )
##
InstallGlobalFunction( POL_RandomGroupElement , function( gens )
    local d,k,g,i,length,x,n;
    k:=Length(gens);
    g := gens[1]^0;
    length:=Random(5,30);
    for i in [1..length] do
        x:=Random(1,k);
        n:=Random( List( [-10..10] ) );
        g:=g*(gens[x]^n);
    od;
    return g;
end) ;

#############################################################################
##
#F POL_Test_CPCS_PRMGroup( G )
##
## G is a rational polycyclic matrix group
##
POL_Test_CPCS_PRMGroup := function( G )
    local numberOfTests, pcgs, gens, i, g, exp;
    SetAssertionLevel( 2 );
    Print( "Start testing\n" );
    numberOfTests := 10;
    pcgs := CPCS_PRMGroup( G );
    gens := GeneratorsOfGroup( G );
    for i in [1..numberOfTests] do
        Print(i);
        g := POL_RandomGroupElement( gens );
        Info( InfoPolenta, 3, "g is equal to ", g );
        exp := ExponentVector_CPCS_PRMGroup( g, pcgs);
    od;
    Print( "\n" );
end;

#############################################################################
##
#F POL_Test_CPCS_PRMGroupExams( anfang, ende )
##
## G is a rational polycyclic matrix group
##
POL_Test_CPCS_PRMGroupExams := function( anfang, ende )
     local i,G;
     SetInfoLevel( InfoPolenta, 3 );
     for i in [anfang..ende] do
         Print( "Test of group ", i, "\n" );
         G := PolExamples( i );
         POL_Test_CPCS_PRMGroup( G );
     od;
end;

#############################################################################
##
#F POL_Test_CPCS_PRMGroupRuntime( anfang, ende )
##
## G is a rational polycyclic matrix group
##
POL_Test_CPCS_PRMGroupRuntime := function( anfang, ende )
     local i,G, pcs;
     ProfileFunctions([CPCS_PRMGroup]);
     ClearProfile();
     SetInfoLevel( InfoPolenta,0 );
     SetAssertionLevel( 0 );
     for i in [anfang..ende] do
         G := PolExamples( i );
         pcs := CPCS_PRMGroup( G );
         Print( "PolExamples ", i, "\n" );
         DisplayProfile();
         ClearProfile();
         Print( "\n" );
     od;
     return 0;
end;

POL_AbelianTestGroup := function( i )
    local G,p, d, gens_p, bound_derivedLength, pcgs_I_p, gens_K_p,
          homSeries, gens_K_p_m, gens, gens_K_p_mutableCopy, pcgs,
            gensOfBlockAction;
    G := PolExamples( i );
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

    return gensOfBlockAction;
end;

     
#############################################################################
##
#E







