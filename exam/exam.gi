#############################################################################
##
#W exam.gi                 POLENTA package                     Bjoern Assmann
##
## examples for polycyclic rational matrix groups
##
#H  @(#)$Id$
##
#Y 2003
##

#############################################################################
##
#F POL_KroneckerProduct
## 
## Examples build with the Kroneckerproduct.
## 
POL_KroneckerProduct := function( G, e ) 
    local newGens, gens;
    gens := GeneratorsOfGroup( G );
    if exp = 0 then 
        exp := RandomList( [1,-1] );
    fi;
    newGens := List( gens, x->KroneckerProduct(x,x^e) );
    return Group( newGens );
end;

if TestPackageAvailability( "aclib" , "1.0" )=fail then
    POL_AlmostCrystallographicGroup := false;
else 
    POL_AlmostCrystallographicGroup := function( a,b,c )
       local G, mats;
       G := AlmostCrystallographicGroup(a,b,c);
       mats := GeneratorsOfGroup( G );
       G := Group( mats );
       return G;
    end;
fi;

#############################################################################
##
#F PolExamples( n ) .............. .. Examples for polycyc rat. matrix groups
##
PolExamples := function( n )
    local i,M,P, nat, G, gens, d, l,r,s;

    # check if aclib is needed
    l := Concatenation( [9..19],[21..24]);
    if POL_AlmostCrystallographicGroup = false then 
        if n in l then
            Print( "package 'aclib' is needed for this example.\n" );
            return fail;
        fi;
    fi;

    # some unipotent groups
    if n=1 then
        return Group(
               [ [ [ 1, -4, 1, 2 ], 
                   [ 0, 1, 5, 1 ], 
                   [ 0, 0, 1, 3 ], 
                   [ 0, 0, 0, 1 ] ]
                 ,
                 [ [ 1, -1, -2, 4 ], 
                   [ 0, 1, -1, 1 ], 
                   [ 0, 0, 1, 1 ], 
                   [ 0, 0, 0, 1 ] ]
                 ,
                 [ [ 1, -4, -2, 1 ], 
                   [ 0, 1, -2, -3 ], 
                   [ 0, 0, 1, 2 ], 
                   [ 0, 0, 0, 1 ] ] 
                ]);
    fi;
    if n=2 then
        return Group(
               [ [ [ 1, 5, -3, 1, -3 ], 
                   [ 0, 1, 2, 0, 1 ], 
                   [ 0, 0, 1, 2, -2 ],
                   [ 0, 0, 0, 1, -2 ], 
                   [ 0, 0, 0, 0, 1 ] ]
                 ,
                 [ [ 1, 3, -3, -1, 1 ], 
                   [ 0, 1, 0, -2, 2 ], 
                   [ 0, 0, 1, 4, 1 ],
                   [ 0, 0, 0, 1, -1 ], 
                   [ 0, 0, 0, 0, 1 ] ] 
               ]);
    fi;
    if n=3 then
        return Group( 
               [ [ [ 73/10, -35/2, 42/5, 63/2 ], 
                   [ 27/20, -11/4, 9/5, 27/4 ],
                   [ -3/5, 1, -4/5, -9 ], 
                   [ -11/20, 7/4, -2/5, 1/4 ] ]
                 ,
                 [ [ -42/5, 423/10, 27/5, 479/10 ], 
                   [ -23/10, 227/20, 13/10, 231/20 ],
                   [ 14/5, -63/5, -4/5, -79/5 ],
                   [ -1/10, 9/20, 1/10, 37/20 ] ] 
               ]);
    fi;
    if n=4 then
        return Group(
               [ [ [ 5, 2, -8, 17, -1 ], 
                   [ -69/4, -15/4, 449/20, -163/5, 53/20 ],
                   [ -2, 4, 9/5, 63/5, 3/5 ], 
                   [ 13/4, 3/4, -121/20, 57/5, -17/20 ],
                   [ 241/4, 7/4, -1477/20, 319/5, -189/20 ] ]
                  ,
                  [ [ 19/2, 0, -3, -19/2, -1/2 ], 
                    [ -74/5, 129/20, 7/4, 159/4, 9/10 ],
                    [ 53/10, 4/5, -4, 9/2, -9/10 ], 
                    [ 37/10, -41/20, -7/4, -29/4, -3/5 ],
                    [ 137/5, -457/20, 37/4, -559/4, 3/10 ] ] 
                ]);
    fi;
    if n in [5..8] then
        return MatExamples( n-4 );
    fi;
    if n=9 then
        return POL_AlmostCrystallographicGroup( 3, 17, [1,0,-3,7] );
    fi;
    if n=10 then
        return POL_AlmostCrystallographicGroup( 3, 16, [3,4,-1,0] );
    fi;
    if n=11 then
        return POL_AlmostCrystallographicGroup( 3, 15, [-1,1,0,0] );
    fi;
    if n=12 then
        return POL_AlmostCrystallographicGroup( 3, 14, [-2,-1,2,-1] );
    fi;
    if n=13 then
        return POL_AlmostCrystallographicGroup( 4, 2, [0,-3,-2,1,-1,-2,4] );
    fi;
    if n=14 then
        return POL_AlmostCrystallographicGroup( 4, 13, [-2,0,-2,-2,-1,-1] );
    fi;
    if n=15 then
        return POL_AlmostCrystallographicGroup( 4, 37, [ 1,2,0,-3,-4] );
    fi;
    if n=16 then
        return POL_AlmostCrystallographicGroup( 4, 86,[0,-2,-1,-2,-1] );
    fi;
    if n=17 then
        return POL_AlmostCrystallographicGroup( 4, 10,[-2,-2,-1]  );
    fi;
    if n=18 then
         return POL_AlmostCrystallographicGroup( 4, 11,[-4,-2,-1] );
    fi;   
    if n=19 then
         return POL_AlmostCrystallographicGroup( 4, 12,[0,1,3,-1] );  
    fi;   
    if n=20 then
        return POL_KroneckerProduct( MatExamples(4), -1  );
    fi;
    if n in [21..24] then
        return POL_KroneckerProduct( PolExamples( n-12 ), 1 );
    fi;
    if n = 25 then 
        #Marco Costantini example
        r := [ [ 1, 0, 0, 0, 0, 0, 0, 0 ], [ 0, 0, 1, 0, 0, 0, 0, 0 ],
               [ 0, 0, 0, 1, 0, 0, 0, 0 ], [ 0, 0, 0, 0, 1, 0, 0, 0 ],
               [ 0, 0, 0, 0, 0, 1, 0, 0 ], [ 0, 0, 0, 0, 0, 0, 1, 0 ],
               [ 0, 0, 0, 0, 0, 0, 0, 1 ], [ 0, 1, 0, 0, 0, 0, 0, 0 ] ];
        s :=
             [ [ 1, 0, 0, 0, 0, 0, 0, 1 ], [ 0, E(7)^6, 0, 0, 0, 0, 0, 0 ],
               [ 0, 0, E(7), 0, 0, 0, 0, 0 ], [ 0, 0, 0, 1, 0, 0, 0, 0 ],
               [ 0, 0, 0, 0, 1, 0, 0, 0 ], [ 0, 0, 0, 0, 0, 1, 0, 0 ],
               [ 0, 0, 0, 0, 0, 0, 1, 0 ], [ 0, 0, 0, 0, 0, 0, 0, 1 ] ];
        return Group( [r,s^7] );
     fi;   
end;




#############################################################################
##
#F POL_RandomRationalTriangularGroup( dim, numberOfGens )
##
POL_RandomRationalTriangularGroup := function( dim, numberOfGens )
    local gens, i, g, j, k, G, x;
    if dim=0 then
        dim:=RandomList([1..10]);
    fi;
    if numberOfGens=0 then
        numberOfGens:=RandomList([1..5]);
    fi;
    gens:=[];
    for i in [1..numberOfGens] do
        g:=RandomInvertibleMat(dim,Rationals);
        for j in [1..dim] do
            for k in [1..j] do
                if j=k then
                   g[j][k]:=1;
                elif j>k then
                   g[j][k]:=0;
                fi;
            od;
        od;
        Add(gens,g);
    od;
    G := Group( gens );
    return G;
end; 

#############################################################################
##
#F  POL_RandomRationalUnipotentGroup( dim, numberOfGens )
##
POL_RandomRationalUnipotentGroup := function( dim, numberOfGens )
    local G, x;
    G := POL_RandomRationalTriangularGroup( dim, numberOfGens );
    x := RandomInvertibleMat( dim, Rationals );
    return G^x;
end;

#############################################################################
##
#F POL_RandomIntegralTriangularGroup( dim, numberOfGens )
##
POL_RandomIntegralTriangularGroup := function( dim, numberOfGens )
    local gens, i, g, j, k, G, x;
    if dim=0 then
        dim:=RandomList([1..10]);
    fi;
    if numberOfGens=0 then
        numberOfGens:=RandomList([1..5]);
    fi;
    gens:=[];
    for i in [1..numberOfGens] do
        g:=RandomInvertibleMat(dim,Integers);
        for j in [1..dim] do
            for k in [1..j] do
                if j=k then
                   g[j][k]:=1;
                elif j>k then
                   g[j][k]:=0;
                fi;
            od;
        od;
        Add(gens,g);
    od;
    G := Group( gens );
    return G;
end; 

#############################################################################
##
#F  POL_RandomAlmostIntegralUnipotentGroup( dim, numberOfGens )
##
POL_RandomAlmostIntegralUnipotentGroup := function( dim, numberOfGens )
    local G, x;
    G := POL_RandomIntegralTriangularGroup( dim, numberOfGens );
    x := RandomInvertibleMat( dim, Integers );
    Print(x);
    return G^x;
end;

#############################################################################
##
#F POL_RandomSubgroup( G, numberOfgens )
##
##
POL_RandomSubgroup := function( G, numberOfNewGens )
    local gens,newGens, i;
    gens := GeneratorsOfGroup( G );
    newGens := [];
    for i in [1..numberOfNewGens] do
        Add( newGens, POL_RandomGroupElement( gens ) );
    od;
    return Group( newGens );
end;

#############################################################################
##
#F POL_DirectProduct( G1, G2 )
##
##
POL_DirectProduct:= function( G1, G2 )
    local n1, n2, gens, k, i, j, matrix, x, G, gens1, gens2;
    gens1 := GeneratorsOfGroup( G1 );
    gens2 := GeneratorsOfGroup( G2 );
    n1 := Length( gens1[1] );
    n2 := Length( gens2[2] );
    gens := [];
    for k in [1..Length( gens1 )] do
        matrix := IdentityMat( n1+n2 );
        for i in [1..n1] do
            for j in [1..n1] do
                 matrix[i][j] := gens1[k][i][j]; 
            od;
        od;
        Add( gens, matrix );
    od;
    for k in [1..Length( gens2 )] do
        matrix := IdentityMat( n1+n2 );
        for i in [1..n2] do
            for j in [1..n2] do
                 matrix[i+n1][j+n1] := gens2[k][i][j]; 
            od;
        od;
        Add( gens, matrix );
    od; 
    return Group( gens ); 
end;

#############################################################################
##
#F POL_KroneckerProduct
## 
## Examples build with the Kroneckerproduct.
## 
POL_KroneckerProduct := function( G, e ) 
    local newGens, gens;
    gens := GeneratorsOfGroup( G );
    if exp = 0 then 
        exp := RandomList( [1,-1] );
    fi;
    newGens := List( gens, x->KroneckerProduct(x,x^e) );
    return Group( newGens );
end;

#############################################################################
##
#F POL_SemidirectProductVectorSpace( G )
##
POL_SemidirectProductVectorSpace := function( G )
    local gens, n, newGens, g, h, i, v;
    gens := GeneratorsOfGroup( G );
    n := Length( gens[1] );
    newGens := [];
    for g in gens do 
        h := POL_CopyVectorList( g );
        for i in [1..n] do
            Add( h[i], 0 );
        od;
        v := List( [1..n+1], x->0 );
        v[n+1] := 1;
        Add( h, v );
        Add( newGens, h );
    od;
    for i in [1..n] do
        h := IdentityMat( n+1 );
        h[i][n+1] := 1;
        Add( newGens, h );
    od;
    return Group( newGens );
end;


if TestPackageAvailability( "aclib" , "1.0" )=fail then
    POL_AlmostCrystallographicGroup := false;
else 
    POL_AlmostCrystallographicGroup := AlmostCrystallographicGroup;
fi;




#############################################################################
##
#F POL_PolExamples2( n ) .............. .. Examples for polycyc rat. matrix groups
##
POL_PolExamples2 := function( n )
    local i,M,P, nat, G, gens, d, l, l1, l2, l3,h1,h2,H;

    # check if aclib is needed
    l1 := Concatenation( [9..16],[21..28],[37..40]);
    l2 := l1 + 40;
    l3 := [82..100];
    l := Concatenation( l1, l2 ,l3);
    l1 := l+100;
    l := Concatenation( l, l1 );
    l1 := l+300;
    l := Concatenation( l, l1 ); 
    l1 := l+400;
    l := Concatenation( l, l1 ); 
    if POL_AlmostCrystallographicGroup = false then 
        if n in l then
            Print( "package 'aclib' is needed for this example.\n" );
            return fail;
        fi;
    fi;

    # some unipotent groups
    if n=1 then
        return Group(
               [ [ [ 1, -4, 1, 2 ], 
                   [ 0, 1, 5, 1 ], 
                   [ 0, 0, 1, 3 ], 
                   [ 0, 0, 0, 1 ] ]
                 ,
                 [ [ 1, -1, -2, 4 ], 
                   [ 0, 1, -1, 1 ], 
                   [ 0, 0, 1, 1 ], 
                   [ 0, 0, 0, 1 ] ]
                 ,
                 [ [ 1, -4, -2, 1 ], 
                   [ 0, 1, -2, -3 ], 
                   [ 0, 0, 1, 2 ], 
                   [ 0, 0, 0, 1 ] ] 
                ]);
    fi;
    if n=2 then
        return Group(
               [ [ [ 1, 5, -3, 1, -3 ], 
                   [ 0, 1, 2, 0, 1 ], 
                   [ 0, 0, 1, 2, -2 ],
                   [ 0, 0, 0, 1, -2 ], 
                   [ 0, 0, 0, 0, 1 ] ]
                 ,
                 [ [ 1, 3, -3, -1, 1 ], 
                   [ 0, 1, 0, -2, 2 ], 
                   [ 0, 0, 1, 4, 1 ],
                   [ 0, 0, 0, 1, -1 ], 
                   [ 0, 0, 0, 0, 1 ] ] 
               ]);
    fi;
    if n=3 then
        return Group( 
               [ [ [ 73/10, -35/2, 42/5, 63/2 ], 
                   [ 27/20, -11/4, 9/5, 27/4 ],
                   [ -3/5, 1, -4/5, -9 ], 
                   [ -11/20, 7/4, -2/5, 1/4 ] ]
                 ,
                 [ [ -42/5, 423/10, 27/5, 479/10 ], 
                   [ -23/10, 227/20, 13/10, 231/20 ],
                   [ 14/5, -63/5, -4/5, -79/5 ],
                   [ -1/10, 9/20, 1/10, 37/20 ] ] 
               ]);
    fi;
    if n=4 then
        return Group(
               [ [ [ 5, 2, -8, 17, -1 ], 
                   [ -69/4, -15/4, 449/20, -163/5, 53/20 ],
                   [ -2, 4, 9/5, 63/5, 3/5 ], 
                   [ 13/4, 3/4, -121/20, 57/5, -17/20 ],
                   [ 241/4, 7/4, -1477/20, 319/5, -189/20 ] ]
                  ,
                  [ [ 19/2, 0, -3, -19/2, -1/2 ], 
                    [ -74/5, 129/20, 7/4, 159/4, 9/10 ],
                    [ 53/10, 4/5, -4, 9/2, -9/10 ], 
                    [ 37/10, -41/20, -7/4, -29/4, -3/5 ],
                    [ 137/5, -457/20, 37/4, -559/4, 3/10 ] ] 
                ]);
    fi;
    if n in [5..8] then
        return MatExamples( n-4 );
    fi;
    if n=9 then
        return POL_AlmostCrystallographicGroup( 3, 17, [1,0,-3,7] );
    fi;
    if n=10 then
        return POL_AlmostCrystallographicGroup( 3, 16, [3,4,-1,0] );
    fi;
    if n=11 then
        return POL_AlmostCrystallographicGroup( 3, 15, [-1,1,0,0] );
    fi;
    if n=12 then
        return POL_AlmostCrystallographicGroup( 3, 14, [-2,-1,2,-1] );
    fi;
    if n=13 then
        return POL_AlmostCrystallographicGroup( 4, 2, [0,-3,-2,1,-1,-2,4] );
    fi;
    if n=14 then
        return POL_AlmostCrystallographicGroup( 4, 13, [-2,0,-2,-2,-1,-1] );
    fi;
    if n=15 then
        return POL_AlmostCrystallographicGroup( 4, 37, [ 1,2,0,-3,-4] );
    fi;
    if n=16 then
        return POL_AlmostCrystallographicGroup( 4, 86,[0,-2,-1,-2,-1] );
    fi;
    if n in [17..20] then
        return POL_KroneckerProduct( MatExamples( n-16), -1  );
    fi;
    if n in [21..28] then
        return POL_KroneckerProduct( PolExamples( n-12 ), 1 );
    fi; 
    if n=29 then
        return POL_DirectProduct( PolExamples( 1 ), PolExamples( 2 ) );
    fi;
    if n=30 then
        return POL_DirectProduct( PolExamples( 1 ), PolExamples( 4 ) );
    fi;
    if n=31 then
        return POL_DirectProduct( PolExamples( 2 ), PolExamples( 8 ) );
    fi;
    if n=32 then
        return POL_DirectProduct( PolExamples( 6 ), PolExamples( 7 ) );
    fi;
    if n=33 then
        return POL_DirectProduct( PolExamples( 1 ), PolExamples( 2 ) );
    fi;
    if n=34 then
        return POL_DirectProduct( PolExamples( 1 ), PolExamples( 4 ) );
    fi;
    if n=35 then
        return POL_DirectProduct( PolExamples( 2 ), PolExamples( 8 ) );
    fi;
    if n=36 then
        return POL_DirectProduct( PolExamples( 6 ), PolExamples( 7 ) );
    fi;
    if n=37 then
        return POL_DirectProduct( PolExamples( 1 ), PolExamples( 9 ) );
    fi; 
    if n=38 then
        return POL_DirectProduct( PolExamples( 10 ), PolExamples( 11 ) );
    fi; 
    if n=39 then
        return POL_DirectProduct( PolExamples( 17 ), PolExamples( 21 ) );
    fi; 
    if n=40 then
        return POL_DirectProduct( PolExamples( 2 ), PolExamples( 27 ) );
    fi; 
    if n in [41..81] then
        return POL_SemidirectProductVectorSpace( PolExamples( n-40 ) );
    fi;
    if n in [82..100] then
        return POL_AlmostCrystallographicGroup( 4, n-5, false );
    fi;
    if n in [101..200] then
        P := SymmetricGroup( 3 );
        M := POL_PolExamples2( n-100 );
        return WreathProductOfMatrixGroup( M, P );
    fi;
    if n in [201..300] then
        P := SmallGroup( 8, 3 );
        nat := RegularActionHomomorphism( P );
        P := Image( nat );
        M := POL_PolExamples2( n-200 );
        return WreathProductOfMatrixGroup( M, P );
    fi;
    if n in [301..600] then
        G := POL_PolExamples2( n-300 );
        gens := GeneratorsOfGroup( G );
        d := Length( gens[1] );
        M := RandomInvertibleMat( Integers, d );
        return G^M;
    fi;
    if n in [601..1200]  then
        return POL_RandomSubgroup( POL_PolExamples2( n-600), 2 );
    fi; 
    # examples which can be nonpolycyclic or nonsolvable
    if n = 1201 then
          h1 := [[2,0],[0,1]];
          h2 := [[1,1],[0,1]];
          H := Group( [h1,h2] );
          return H;
     fi;
     if n = 1202 then
         gens := [ [ [ -1/4, 5/4, -4/3 ], [ 1, 0, 1 ], [ 0, -5/3, -1 ] ],
                 [ [ 2, 3/5, 4 ], [ 2/3, 0, 1 ], [ 5/3, 0, 1/2 ] ] ];
         return Group( gens );
     fi;
     if n = 1203 then
         gens := [ [ [ 2, 4/3, 1/2, 1/2 ], [ -2, -1/2, -1/4, 1 ], 
                    [ -1/2, 0, 0, 3/2 ],
                  [ 0, 1/3, 0, 3/2 ] ],
              [ [ 2, 2, 1, 2 ], [ -2/5, 3/2, 0, -1/2 ], [ -3/2, 0, 1/3, 4 ],
                  [ -1/4, 0, -3, 0 ] ] ];
         return Group( gens );
     fi;
     if n = 1204 then
         gens := [ [ [ 3, 1, 3, -1 ], 
                   [ 2, -1, 1, 1 ], 
                  [ -1, 0, 1, 3 ], [ 1, 1, -1, 0 ] ],
              [ [ 0, -3, -1, 2 ], [ -3, -3, 0, -1 ], [ -1, -2, -1, 0 ],
                  [ 1, -1, 2, -5 ] ] ];
         return Group( gens );
     fi;

          

end;
   











