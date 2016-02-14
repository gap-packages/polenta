gap> m :=
> [ [ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, -1 ] ],
>  [ [ 1, 0, 0 ], [ 0, -1, 0 ], [ 0, 0, 1 ] ],
>  [ [ -1, 0, 0 ], [ 0, -1, 0 ], [ 0, 0, 1 ] ],
>  [ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, -1 ] ],
>  [ [ -1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] ];;
gap> M := Group(m);
<matrix group with 5 generators>
gap> matrix := [ [ -1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ];
[ [ -1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ]
gap> IsomorphismPcpGroup(M);
[ [ [ -1, 0, 0 ], [ 0, -1, 0 ], [ 0, 0, 1 ] ],
  [ [ 1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, -1 ] ],
  [ [ -1, 0, 0 ], [ 0, 1, 0 ], [ 0, 0, 1 ] ] ] -> [ g1, g2, g3 ]