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
    length:=Random(2,10);
    for i in [1..length] do
        x:=Random(1,k);
        n:=Random( List( [-3..3] ) );
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
    if pcgs = fail then return 0;fi;
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
#F POL_Test_Series_PRMGroup( G )
##
## G is a rational polycyclic matrix group
##
POL_Test_Series_PRMGroup := function( G )
    local radSer, homSer, comSer;
    SetAssertionLevel( 2 );
    radSer :=RadicalSeriesSolvableMatGroup( G );
    homSer := HomogeneousSeriesAbelianMatGroup( G );
    comSer := CompositionSeriesAbelianMatGroup( G );   
    homSer := HomogeneousSeriesTriangularizableMatGroup( G );
    comSer := CompositionSeriesTriangularizableMatGroup( G );
end;

#############################################################################
##
#F POL_Test_SubgroupComp_PRMGroup( G )
##
## G is a rational polycyclic matrix group
##
POL_Test_SubgroupComp_PRMGroup := function( G )
    local T, reco;
    SetAssertionLevel( 2 );
    T := TriangNormalSubgroupFiniteInd( G );
    reco := TriangNormalSubgroupFiniteIndUnipo( G );
end;

#############################################################################
##
#F POL_Test_Properties_PRMGroup( G )
##
## G is a rational polycyclic matrix group
##
POL_Test_Properties_PRMGroup := function( G )
    local t;
    SetAssertionLevel( 2 );
    t := IsSolvableGroup( G );
    t := IsTriangularizableMatGroup(G );
    t := IsPolycyclicMatGroup( G );
end;

#############################################################################
##
#F POL_Test_Isom_PRMGroup( G )
##
## G is a rational polycyclic matrix group
##
POL_Test_Isom_PRMGroup := function( G )
    local iso, src,mats,n,numberOfTests,i, exp1,mat1,img1,mat2;
    SetAssertionLevel( 2 );

    iso := IsomorphismPcpGroup( G );
    if iso = fail then return 0;fi;
    mats := GeneratorsOfGroup( G );
    n := Length( mats );
    numberOfTests := 2;
    for i in [1..numberOfTests] do
       Print( i );
       #exp1 := List( [1..n], x-> Random( Integers ) );
       #mat1 := MappedVector( exp1, mats );
       mat1 := POL_RandomGroupElement( mats );
       img1 := ImageElm( iso, mat1 );
       mat2 := PreImage( iso, img1 );
       if not mat1 = mat2 then
           Error( "Isomorphism calculated wrong preimage\n" );
       fi;
     od;
     Print( "\n" );
end;

#############################################################################
##
#F POL_Test_AllFunctions_PRMGroup( G )
##
## G is a rational polycyclic matrix group
##
POL_Test_AllFunctions_PRMGroup := function( G )
    SetAssertionLevel( 2 );
    Info( InfoPolenta, 2, "POL_Test_Isom_PRMGroup" );
    POL_Test_Isom_PRMGroup( G );
    Info( InfoPolenta, 2, "POL_Test_Properties_PRMGroup" );
    POL_Test_Properties_PRMGroup( G );
    Info( InfoPolenta, 2, "POL_Test_SubgroupComp_PRMGroup" );
    POL_Test_SubgroupComp_PRMGroup( G );
    Info( InfoPolenta, 2, "POL_Test_Series_PRMGroup" );
    POL_Test_Series_PRMGroup( G );
end;

#############################################################################
##
#F POL_Test_AllFunctions_PolExamples( anfang, ende )
##
## G is a rational polycyclic matrix group
##
POL_Test_AllFunctions_PolExamples := function( anfang, ende )
    local i, G;
    SetAssertionLevel( 2 );
    for i in [anfang..ende] do
         Print( "Test of group ", i, "\n" );
         G := PolExamples( i );
         POL_Test_AllFunctions_PRMGroup( G );
     od;
end;

#############################################################################
##
#F POL_Test_AllFunctions_PolExamples2( anfang, ende )
##
## G is a rational polycyclic matrix group
##
POL_Test_AllFunctions_PolExamples2 := function( anfang, ende )
    local i, G;
    SetAssertionLevel( 2 );
    for i in [anfang..ende] do
         Print( "Test of group ", i, "\n" );
         G := POL_PolExamples2( i );
         POL_Test_AllFunctions_PRMGroup( G );
     od;
end;


#############################################################################
##
#F POL_Test_CPCS_PRMGroupExams( anfang, ende )
##
## G is a rational polycyclic matrix group
##
POL_Test_CPCS_PRMGroupExams := function( anfang, ende )
     local i,G;
     #SetInfoLevel( InfoPolenta, 3 );
     for i in [anfang..ende] do
         Print( "Test of PolExamples(  ", i, " )\n" );
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

     
POL_CompleteRuntime:= function( func, input )
    local rec1,rec2, user_time, user_time_child, system_time, 
          system_time_child, sum;
    rec1 := Runtimes();
    func( input );
    rec2 := Runtimes();

    user_time := rec2.user_time -rec1.user_time;
    user_time_child := rec2.user_time_children -rec1.user_time_children;
    system_time := rec2.system_time - rec1.system_time;
    system_time_child := rec2.system_time_children - rec1.system_time_children;

    sum := user_time + user_time_child + system_time + system_time_child;

    return StringTime( sum);
end;

POL_CompleteRuntime_FullInfo:= function( func, input )
    local rec1,rec2, rec3;
    rec1 := Runtimes();
    func( input );
    rec2 := Runtimes();

    rec3 := rec( 
    user_time := StringTime( rec2.user_time -rec1.user_time),
    user_time_child := StringTime( rec2.user_time_children -rec1.user_time_children ),
    system_time := StringTime( rec2.system_time - rec1.system_time ),
    system_time_child := StringTime( rec2.system_time_children - rec1.system_time_children ),
    );
    
    return rec3;
end;

#############################################################################
##
#E







