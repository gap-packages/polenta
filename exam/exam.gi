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













