gap> START_TEST("Test of POLENTA package");  
gap> G := MatExamples(2);
<matrix group with 6 generators>
gap> mats := GeneratorsOfGroup( G );;
gap> nat := IsomorphismPcpGroup( G );;
gap> H := Image( nat );;
gap> h := GeneratorsOfGroup( H );;
gap> mats2 := List( h, x -> PreImage( nat, x ) );;
gap> exp :=  [ 1, 1, 1, 1, 0, 0, 0, 0 ];;
gap> g := MappedVector( exp, mats2 );
[ [ 229793843, -345584045, -503782245, 823202280 ],
  [ 397912065, -598518263, -872506665, 1425593295 ],
  [ 141954212, -213549855, -311309508, 508615375 ],
  [ 189806315, -285510521, -416211500, 680033928 ] ]
gap> i := ImageElm( nat, g );;
gap> Exponents( i );
[ 1, 1, 1, 1, 0, 0, 0, 0 ]
gap> PreImagesRepresentative( nat, i );
[ [ 229793843, -345584045, -503782245, 823202280 ],
  [ 397912065, -598518263, -872506665, 1425593295 ],
  [ 141954212, -213549855, -311309508, 508615375 ],
  [ 189806315, -285510521, -416211500, 680033928 ] ]
gap> last = g;
true
gap> STOP_TEST( "POLENTA.tst", 100000);   




















