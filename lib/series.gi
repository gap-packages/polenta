#############################################################################
##
#W series.gi               POLENTA package                     Bjoern Assmann
##
## Methods for the calculation of 
## radicalseries, homogeneous series and composition series of matrix groups
##
#H  @(#)$Id$
##
#Y 2003
##

#############################################################################
##
#F POL_SplitSemisimple( base )
##
POL_SplitSemisimple := function( base )
    local  d, b, f, s, i;
    d := Length( base );
    b := PrimitiveAlgebraElement( [  ], base );
    f := Factors( b.poly );
    if Length( f ) = 1  then
        return [ rec(
                basis := IdentityMat( Length( b.elem ) ),
                poly := f ) ];
    fi;
    s := List( f, function ( x )
            return NullspaceRatMat( Value( x, b.elem ) );
        end );
    s := List( [ 1 .. Length( f ) ], function ( x )
            return rec(
                basis := s[x],
                poly := f[x] );
        end );
    return s;
end;


#############################################################################
##
#F RadicalOfAbelianRMGroup( mats, d )
##
## <mats> is an abelian rational matrix group
##
RadicalOfAbelianRMGroup := function( mats, d )
    local coms, i, j, new, base, full, nath, indm, l, algb, newv, tmpb, subb,
          f, g, h, mat;
 
    base := [];
    full := IdentityMat( d );
    # nath is the natural hom. from V to V/W
    nath := NaturalHomomorphismBySemiEchelonBases( full, base );
    # indm for induced matrices
    indm := mats; 

    # start spinning up basis and look for nilpotent elements
    i := 1;
    algb := [];
    while i <= Length( indm ) do
 
        # add next element to algebra basis
        l := Length( algb );
        newv := Flat( indm[i] );
        tmpb := SpinnUpEchelonBase(algb, [newv], indm{[1..i]},OnMatVector ); 

        # check whether we have added a non-semi-simple element
        subb := [];
        for j in [l+1..Length(tmpb)] do
            mat := MatByVector( tmpb[j], Length(indm[i]) );
            f := MinimalPolynomial( Rationals, mat );
            g := Collected( Factors( f ) );
            if ForAny( g, x -> x[2] > 1 ) then
                h := Product( List( g, x -> Value( x[1], mat ) ) );
                Append( subb, List( h, x -> ShallowCopy(x) ) );
            fi;
        od;
        #Print("found nilpotent submodule of dimension ", Length(subb),"\n");
 
        # spinn up new subspace of radical
        subb := SpinnUpEchelonBase( [], subb, indm, OnRight );
        if Length( subb ) > 0 then
            base := PreimageByNHSEB( subb, nath );
            if Length( base ) = d then
                # radical cannot be so big
                return fail;
            fi;
            nath := NaturalHomomorphismBySemiEchelonBases( full, base );
            indm := List( mats, x -> InducedActionFactorByNHSEB( x, nath ) );
            algb := [];
            i := 1;
        else
            i := i + 1;
        fi;
    od;
    return rec( radical := base, nathom := nath, algebra := algb );
end;


#############################################################################
##
#F POL_RadicalNormalGens( gens, mats, d ) 
##
## 
## mats are normal subgroup generators for the Kernel K_p in G =<gens>
##
POL_RadicalNormalGens := function( gens, mats, d )
    local coms, i, j, new, base, full, nath, indm, l, algb, 
          newv, tmpb, subb, f, g, h, mat,k,a,left,inducedk,right,
          commutes,extended,comElement,a2,y;
  
    # get commutators
    # because Q^d ( k-1 ) \subset Rad, where k in the commutator 
    # subgroup <mats>'
    coms := [];
    for i in [1..Length( mats )] do
        for j in [i+1..Length( mats )] do
            new := mats[i] * mats[j] - mats[j] * mats[i];
            Append(coms, new );
        od;
    od;

    # base is a basis for the module W, the radical
    base := SpinnUpEchelonBase( [], coms, gens, OnRight );
    if Length( base ) = d then
        # for a radical to big
        return fail;
    fi;
    full := IdentityMat( d );
    # nath is the natural hom. from V to V/W
    nath := NaturalHomomorphismBySemiEchelonBases( full, base );
    # indm for induced matrices
    indm := List( mats, x -> InducedActionFactorByNHSEB( x, nath ) );
   
    # start spinning up basis and look for nilpotent elements
    i := 1;
    algb := [];
    while i <= Length( indm ) do    
        # check if the new element commutes with all elements in algb
        # if not we get a nontrivial element of the commutator
        commutes:=true;
        for a in algb do
            a2 := MatByVector( a, Length(indm[i]) );
            left := indm[i]*a2;
            right := a2*indm[i];
            if not left=right then 
               commutes:=false;
               break;
            fi;
        od;
        # if it doesn't commute with all, spinn up new subspace of
        # the radical
        extended := false;
        if not commutes then
            subb := left-right;
            subb := SpinnUpEchelonBase( [], subb, indm, OnRight );
            if Length( subb ) > 0 then 
                base := PreimageByNHSEB( subb, nath );
                # Rad_K_p(Q^d) = Rad_G(Q^d)
                # therefore <base> must be also a G-modul 
                base := SpinnUpEchelonBase( [], base, gens, OnRight );
                if Length( base ) = d then
                    # radical cannot be so big
                    return fail;
                fi;
                nath := NaturalHomomorphismBySemiEchelonBases( full, base );
                indm := List( mats, x ->InducedActionFactorByNHSEB(x,nath ));
                algb := [];
                i := 1;
                extended:=true;
              fi;
        fi;
        if not extended then
            # add next element to algebra basis
            l := Length( algb );
            newv := Flat( indm[i] );
            tmpb := SpinnUpEchelonBase(algb, [newv], indm{[1..i]},
                                      OnMatVector );
            # close the basis under the conjugation action of G
            for k in gens do
                inducedk:=InducedActionFactorByNHSEB(k,nath);
                y:=indm[i]^inducedk;
                newv:=Flat(y);
                tmpb := SpinnUpEchelonBase( algb, [newv], indm{[1..i]}, 
                                          OnMatVector );
            od;
            # check whether we have added a non-semi-simple element
            subb := [];
            for j in [l+1..Length(tmpb)] do
                mat := MatByVector( tmpb[j], Length(indm[i]) );
                f := MinimalPolynomial( Rationals, mat );
                g := Collected( Factors( f ) );
                if ForAny( g, x -> x[2] > 1 ) then
                    h := Product( List( g, x -> Value( x[1], mat ) ) );
                    Append( subb, List( h, x -> ShallowCopy(x) ) );
                fi;
            od;
            # spinn up new subspace of radical
            subb := SpinnUpEchelonBase( [], subb, indm, OnRight );
            if Length( subb ) > 0 then 
                base := PreimageByNHSEB( subb, nath );
                # Rad_K_p(Q^d) = Rad_G(Q^d)
                # therefore <base> must be also a G-modul 
                base := SpinnUpEchelonBase( [], base, gens, OnRight );
                if Length( base ) = d then
                    # radical cannot be so big
                    return fail;
                fi;
                nath := NaturalHomomorphismBySemiEchelonBases( full, base );
                indm := List( mats,x->InducedActionFactorByNHSEB( x, nath ));
                algb := [];
                i := 1;
            else
                i := i + 1;
            fi;
        fi; 
    od;
    return rec( radical := base, nathom := nath, algebra := algb );
end;

#############################################################################
##
#F POL_HomogeneousSeriesNormalGens(gens, mats, d )
##
## mats are normal subgroup generators for the Kernel K_p in G =<gens>
## returned is homegeneous series of Q^d
##
POL_HomogeneousSeriesNormalGens := function(gens, mats, d )
    local radb, splt, nath,inducedgens, l, sers, i, sub, full, acts, rads;

    # catch the trivial case and set up
    if d = 0 then
        return []; 
    fi;
    full := IdentityMat( d );
    if Length( mats ) = 0 then 
        return [full, []]; 
    fi;
    sers := [full];

    # get the radical 
    radb := POL_RadicalNormalGens(gens, mats, d );
    if radb = fail then return fail; fi;
    splt := POL_SplitSemisimple( radb.algebra );
    nath := radb.nathom;

    # refine radical factor and initialize series
    l := Length( splt );
    for i in [2..l] do
        sub := Concatenation( List( [i..l], x -> splt[x].basis ) );
        TriangulizeMat( sub ); 
        Add( sers, PreimageByNHSEB( sub, nath ) );
    od;
    Add( sers, radb.radical );

    # induce action to radical
    nath := NaturalHomomorphismBySemiEchelonBases( full, radb.radical);
    acts := List( mats, x -> InducedActionSubspaceByNHSEB( x, nath ));
    inducedgens:=List( gens, x -> InducedActionSubspaceByNHSEB( x, nath ) );

    # use recursive call to refine radical
    rads := POL_HomogeneousSeriesNormalGens(inducedgens,acts,
                                            Length(radb.radical) );
    if rads = fail then return fail; fi;
    rads := List( rads, function(x) if x=[] then return []; else
                            return x * radb.radical; fi;end );
    Append( sers, rads{[2..Length(rads)]} );
    return sers;
end;

#############################################################################
##
#F POL_RadicalSeriesNormalGens(gens, mats, d )
##
## mats are normal subgroup generators for the Kernel K_p in G=<gens>,
## which is a rational polycyclic matrix group.
## returned is a radical series of Q^d
##
POL_RadicalSeriesNormalGens := function(gens, mats, d )
    local radb, splt, nath,inducedgens, l, sers, i, sub, full, acts, rads;

    # catch the trivial case and set up
    if d = 0 then
        return []; 
    fi;
    full := IdentityMat( d );
    if Length( mats ) = 0 then 
        return [full, []]; 
    fi;
    sers := [full];

    # get the radical 
    radb := POL_RadicalNormalGens(gens, mats, d );
    if radb = fail then return fail; fi;
    nath := radb.nathom;
    Add( sers, radb.radical );

    # induce action to radical
    nath := NaturalHomomorphismBySemiEchelonBases( full, radb.radical);
    acts := List( mats, x -> InducedActionSubspaceByNHSEB( x, nath ));
    inducedgens:=List( gens, x -> InducedActionSubspaceByNHSEB( x, nath ) );

    # use recursive call to refine radical
    rads := POL_RadicalSeriesNormalGens(inducedgens,acts,
                                            Length(radb.radical) );
    if rads = fail then return fail; fi;
    rads := List( rads, function(x) if x=[] then return []; else
                            return x * radb.radical; fi;end );
    Append( sers, rads{[2..Length(rads)]} );
    return sers;
end;

#############################################################################
##
#F RadicalSeriesAbelianRMGroup( mats, d )
##
## G is an abelian rational matrix group
##
RadicalSeriesAbelianRMGroup := function( mats, d )
    local radb, splt, nath, l, sers, i, sub, full, acts, rads;

    # catch the trivial case and set up
    if d = 0 then
        return []; 
    fi;
    full := IdentityMat( d );
    if Length( mats ) = 0 then 
        return [full, []]; 
    fi;
    sers := [full];

    # get the radical 
    radb := RadicalOfAbelianRMGroup( mats, d );
    if radb = fail then return fail; fi;
    nath := radb.nathom;
    Add( sers, radb.radical );

    # induce action to radical
    nath := NaturalHomomorphismBySemiEchelonBases( full, radb.radical);
    acts := List( mats, x -> InducedActionSubspaceByNHSEB( x, nath ));
   
    # use recursive call to refine radical
    rads := RadicalSeriesAbelianRMGroup(acts, Length(radb.radical) );
    if rads = fail then return fail; fi;
    rads := List( rads, function(x) if x=[] then return []; else
                            return x * radb.radical; fi;end );
    Append( sers, rads{[2..Length(rads)]} );
    return sers;
end;

#############################################################################
##
#F HomogeneousSeriesAbelianRMGroup( mats, d )
##
## <mats> is an abelian rational matrix group
##
HomogeneousSeriesAbelianRMGroup := function( mats, d )
    local radb, splt, nath,inducedgens, l, sers, i, sub, full, acts, rads;

    # catch the trivial case and set up
    if d = 0 then
        return []; 
    fi;
    full := IdentityMat( d );
    if Length( mats ) = 0 then 
        return [full, []]; 
    fi;
    sers := [full];

    # get the radical 
    radb := RadicalOfAbelianRMGroup( mats, d );
    if radb = fail then return fail; fi;
    splt := POL_SplitSemisimple( radb.algebra );
    nath := radb.nathom;

    # refine radical factor and initialize series
    l := Length( splt );
    for i in [2..l] do
        sub := Concatenation( List( [i..l], x -> splt[x].basis ) );
        TriangulizeMat( sub ); 
        Add( sers, PreimageByNHSEB( sub, nath ) );
    od;
    Add( sers, radb.radical );

    # induce action to radical
    nath := NaturalHomomorphismBySemiEchelonBases( full, radb.radical);
    acts := List( mats, x -> InducedActionSubspaceByNHSEB( x, nath ));
   
    # use recursive call to refine radical
    rads := HomogeneousSeriesAbelianRMGroup( acts, Length(radb.radical) );
    if rads = fail then return fail; fi;
    rads := List( rads, function(x) if x=[] then return []; else
                            return x * radb.radical; fi;end );
    Append( sers, rads{[2..Length(rads)]} );
    return sers;
end;

#############################################################################
##
#F HomogeneousSeriesAbelianMatGroup( G )
##
## <G> is an abelian rational matrix group
##
HomogeneousSeriesAbelianMatGroup := function( G )
    local mats,d;
    if not IsRationalMatrixGroup( G ) or not IsAbelian( G ) then 
        Print( "input must be an abelian rational matrix group.\n" );
    return fail;
    fi;
    mats := GeneratorsOfGroup( G );
    d := Length( mats[1] );
    return HomogeneousSeriesAbelianRMGroup( mats, d );
end;

#############################################################################
##
#F RadicalSeriesPRMGroup( G )
##
## G is a rational polycyclic matrix group
##
RadicalSeriesPRMGroup := function( G )
    local   p,d,gens_p,bound_derivedLength,pcgs_I_p,gens_K_p,
            radicalSeries,gens_K_p_m, gens, gens_K_p_mutableCopy;
 
    gens := GeneratorsOfGroup( G );
    d := Length(gens[1][1]);

    # determine an admissible prime
    p := DetermineAdmissiblePrime(gens);

    # calculate the gens of the group phi_p(<gens>) where phi_p is
    # natural homomorphism to GL(d,p)
    gens_p := InducedByField( gens, GF(p) );

    # determine un upperbound for the derived length of G
    bound_derivedLength := d+2;
 
    Info( InfoPolenta, 1,"determine a constructive polycyclic  sequence");
    Info( InfoPolenta, 1,"for the image under the p-congruence homomorph.");
    pcgs_I_p := CPCS_finite_word( gens_p, bound_derivedLength );
    Info( InfoPolenta, 1, "finite image has relative orders ",
                           RelativeOrdersPcgs_finite( pcgs_I_p ) );
 
    gens_K_p := POL_NormalSubgroupGeneratorsOfK_p( pcgs_I_p, gens );
    # Print( "gens_K_p is equal to", gens_K_p, "\n" );
 
    # step 4
    Info( InfoPolenta, 1, "compute the radical series \n");
    gens_K_p_mutableCopy := CopyMatrixList( gens_K_p );
    radicalSeries := POL_RadicalSeriesNormalGens( gens, 
                                                  gens_K_p_mutableCopy,
                                                   d );
    return radicalSeries;
end;    

#############################################################################
##
#F POL_HomogeneousSeriesPRMGroup( G )   
##
## G is a rational polycyclic matrix group,
## returned is homgeneous series of the natural K_p-module Q^d
##
POL_HomogeneousSeriesPRMGroup := function( G )
    local   p,d,gens_p,bound_derivedLength,pcgs_I_p,gens_K_p,
            homSeries,gens_K_p_m, gens, gens_K_p_mutableCopy;
 
    gens := GeneratorsOfGroup( G );
    d := Length(gens[1][1]);

    # determine an admissible prime
    p := DetermineAdmissiblePrime(gens);

    # calculate the gens of the group phi_p(<gens>) where phi_p is
    # natural homomorphism to GL(d,p)
    gens_p := InducedByField( gens, GF(p) );

    # determine un upperbound for the derived length of G
    bound_derivedLength := d+2;
 
    Info( InfoPolenta, 1,"determine a constructive polycyclic  sequence");
    Info( InfoPolenta, 1,"for the image under the p-congruence homomorph.");
    pcgs_I_p := CPCS_finite_word( gens_p, bound_derivedLength );
    Info( InfoPolenta, 1, "finite image has relative orders ",
                           RelativeOrdersPcgs_finite( pcgs_I_p ) );
 
    gens_K_p := POL_NormalSubgroupGeneratorsOfK_p( pcgs_I_p, gens );
    # Print( "gens_K_p is equal to", gens_K_p, "\n" );
 
    # step 4
    Info( InfoPolenta, 1, "compute the radical series \n");
    gens_K_p_mutableCopy := CopyMatrixList( gens_K_p );
    homSeries := POL_HomogeneousSeriesNormalGens( gens, 
                                                  gens_K_p_mutableCopy,
                                                   d );
    return homSeries;
end;    

#############################################################################
##
#F POL_InducedActionToSeries (gens_K_p, radicalSeries)
##
## returns the action of the matrices in gens_K_p induced to the
## factors of radicalSeries
##
POL_InducedActionToSeries := function(gens_K_p,radicalSeries)
    local action,blockGens,sizeOfBlock,d,homs,l,i,g,
          actionParts,hom,image_of_g,c,a,j;
 
    d:=Length(gens_K_p[1][1]);
    homs:=[];
    blockGens:=[];
    l:=Length(radicalSeries)-1;
    for i in [1..l] do
       TriangulizeMat( radicalSeries[i] );
       homs[i]:=NaturalHomomorphismBySemiEchelonBases( radicalSeries[i],
                                                       radicalSeries[i+1]);
    od;
    for hom in homs do
        actionParts:=[];
        for g in gens_K_p do
            action:=InducedActionFactorByNHSEB( g, hom );
            Add(actionParts,action);
        od;
        Add(blockGens,actionParts);
    od;
    return blockGens;
end;

#############################################################################
##
#M RadicalSeriesSolvableMatGroup( G )
##
## G is a matrix group over the Rationals
## 
##
InstallMethod( RadicalSeriesSolvableMatGroup, "for solvable matrix groups", 
               true, [ IsMatrixGroup ], 0, 
function( G ) 
        local test, mats, d;
        test := POL_IsMatGroupOverFiniteField( G );
        if IsBool( test ) then
            TryNextMethod();
        elif test = 0 then
            if IsAbelian( G ) then
                mats := GeneratorsOfGroup( G );
                d := Length( mats[1] );
                return RadicalSeriesAbelianRMGroup( mats, d );
            else 
                return  RadicalSeriesPRMGroup( G );
            fi; 
        else
            Print( "matrix groups must defined over the rationals" );
            return fail;
        fi;  
end );

#############################################################################
##
#F POL_SplitHomogeneous( base, mats )
##
## split the homogeneous module <base> into irreducibles
## 
POL_SplitHomogeneous := function( base, mats )
    local IrreducibleList,b,space_basis, space, basis;
    IrreducibleList :=[];
    # spinn up new irreducible module
    basis := SpinnUpEchelonBase( [], [base[1]], mats, OnRight ); 
    Add( IrreducibleList, basis );
    # check if basis is already big enough
    if Length( basis ) = Length( base ) then
        # <base> = <basis> but base has nicer form
        return [base];
    fi;
        
    for b in base do 
        # check if b is not already contained in one of the irreducible
        # modules in IrreducibleList
        space_basis := Concatenation( IrreducibleList );
        space := VectorSpace( Rationals, space_basis, "basis" );
        if  not b in space then 
           # spinn up new irreducible module
           basis := SpinnUpEchelonBase( [], [b], mats, OnRight ); 
           Add( IrreducibleList, basis );
        fi;
    od;
    return IrreducibleList;
end;


#############################################################################
##
#F POL_IsRationalModule( base, mats )
##
POL_IsRationalModule := function( base, mats )
    local V, b, m;
    if Length( base ) = 0 then
        return true;
    fi; 
    V := VectorSpace( Rationals, base );
    for b in base do
        for m in mats do
            if not b*m in V then
                return false;
            fi;
        od;
    od;
    return true;
end;


#############################################################################
##
#F POL_CompositionSeriesNormalGens(gens, mats, d )
##
## mats are normal subgroup generators for the Kernel K_p in G =<gens>
## returned is composition series of the K_p-module Q^d 
##
POL_CompositionSeriesNormalGens := function(gens, mats, d )
    local radb, splt, nath,inducedgens, l, sers, i,j, sub, full, acts,
          preImageSub, irreducibleList, k, rads, induced,
          irreducibles, factorMats, isomIrreds, basis, sub2;

    # catch the trivial case and set up
    if d = 0 then
        return []; 
    fi;
    full := IdentityMat( d );
    if Length( mats ) = 0 then 
        return [full, []]; 
    fi;
    sers := [full];

    # get the radical 
    radb := POL_RadicalNormalGens(gens, mats, d );
    if radb = fail then return fail; fi;
    splt := POL_SplitSemisimple( radb.algebra );
    nath := radb.nathom;
    
    # refine radical factor to irreducible compoents 
     l := Length( splt );
     irreducibles := [];
     # induce action to factor
     factorMats := List( mats, x -> InducedActionFactorByNHSEB( x, nath ));
     for i in [1..l] do
         # split i^th homogeneous component into isomorphic comp.
         basis := POL_CopyVectorList( splt[i].basis );
         Assert( 2, POL_IsRationalModule( basis, factorMats ),
                    "hom. component fails to be a module" );
         isomIrreds := POL_SplitHomogeneous( basis, factorMats );
         for j in [1..Length( isomIrreds )] do
             Assert( 2, POL_IsRationalModule( isomIrreds[j], factorMats ),
                    "irred. component fails to be a module" );
         od;
         irreducibles := Concatenation( irreducibles, isomIrreds );
     od; 

    # initialize series
    k := Length( irreducibles );
    for i in [2..k] do
        sub := Concatenation( List( [i..k], x -> irreducibles[x] ) );
        sub2 := POL_CopyVectorList( sub );
        TriangulizeMat( sub2 ); 
        Assert( 2, POL_IsRationalModule( sub2, factorMats ),
                    "sum of irred. components fails to be a module\n" );
        preImageSub := PreimageByNHSEB( sub2, nath );
        Assert( 2, POL_IsRationalModule( preImageSub, mats ),
                    "sum of irred. components fails to be a module\n" );
        Add( sers, preImageSub );
    od;
    Add( sers, radb.radical );

    # induce action to radical
    nath := NaturalHomomorphismBySemiEchelonBases( full, radb.radical);
    acts := List( mats, x -> InducedActionSubspaceByNHSEB( x, nath ));
    inducedgens:=List( gens, x -> InducedActionSubspaceByNHSEB( x, nath ) );

    # use recursive call to refine radical
    rads := POL_CompositionSeriesNormalGens(inducedgens,acts,
                                            Length(radb.radical) );
    if rads = fail then return fail; fi;
    rads := List( rads, function(x) if x=[] then return []; else
                            return x * radb.radical; fi;end );
    Append( sers, rads{[2..Length(rads)]} );
    return sers;
end;

#############################################################################
##
#F CompositionSeriesAbelianRMGroup( mats, d )
##
## <mats> is an abelian rational matrix group
## returned is composition series for the natrual <mats>-module Q^d
##
CompositionSeriesAbelianRMGroup := function( mats, d )
    local radb, splt, nath,inducedgens, l, sers, i, sub, full, acts,
          rads, preImageSub, irreducibleList, k,
          irreducibles, factorMats, isomIrreds, basis, sub2;

    # catch the trivial case and set up
    if d = 0 then
        return []; 
    fi;
    full := IdentityMat( d );
    if Length( mats ) = 0 then 
        return [full, []]; 
    fi;
    sers := [full];

    # get the radical 
    radb := RadicalOfAbelianRMGroup( mats, d );
    if radb = fail then return fail; fi;
    splt := POL_SplitSemisimple( radb.algebra );
    nath := radb.nathom;
    
    # refine radical factor to irreducible compoents 
     l := Length( splt );
     irreducibles := [];
     # induce action to factor
     factorMats := List( mats, x -> InducedActionFactorByNHSEB( x, nath ));
     for i in [1..l] do
         # split i^th homogeneous component into isomorphic comp.
         basis := POL_CopyVectorList( splt[i].basis );
         isomIrreds := POL_SplitHomogeneous( basis, factorMats );
         irreducibles := Concatenation( irreducibles, isomIrreds );
     od; 

    # initialize series
    k := Length( irreducibles );
    for i in [2..k] do
        sub := Concatenation( List( [i..k], x -> irreducibles[x] ) );
        sub2 := POL_CopyVectorList( sub );
        TriangulizeMat( sub2 ); 
        Assert( 2, POL_IsRationalModule( sub2, factorMats ),
                    "sum of irred. components fails to be a module\n" );
        preImageSub := PreimageByNHSEB( sub2, nath );
        Add( sers, preImageSub );
    od;
    Add( sers, radb.radical );

    # induce action to radical
    nath := NaturalHomomorphismBySemiEchelonBases( full, radb.radical);
    acts := List( mats, x -> InducedActionSubspaceByNHSEB( x, nath ));
   
    # use recursive call to refine radical
    rads := CompositionSeriesAbelianRMGroup( acts, Length(radb.radical) );
    if rads = fail then return fail; fi;
    rads := List( rads, function(x) if x=[] then return []; else
                            return x * radb.radical; fi;end );
    Append( sers, rads{[2..Length(rads)]} );
    return sers;
end;

#############################################################################
##
#F CompositionSeriesAbelianMatGroup( G )
##
## <G> is an abelian rational matrix group
##
CompositionSeriesAbelianMatGroup := function( G )
    local mats,d;
    if not IsRationalMatrixGroup( G ) or not IsAbelian( G ) then 
        Print( "input must be an abelian rational matrix group.\n" );
        return fail;
    fi;
    mats := GeneratorsOfGroup( G );
    d := Length( mats[1] );
    return CompositionSeriesAbelianRMGroup( mats, d );
end;

#############################################################################
##
#E
