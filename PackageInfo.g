#############################################################################
##
##  PackageInfo.g        GAP4 Package `Polenta'                Bjoern Assmann
##  

SetPackageInfo( rec(

PackageName := "Polenta",
Subtitle := "Polycyclic presentations for matrix groups",
Version := "1.2.1",
Date := "09/08/2005",

ArchiveURL := Concatenation([ 
"http://www.icm.tu-bs.de/ag_algebra/software/assmann/Polenta/Polenta-", 
~.Version]),
ArchiveFormats := ".tar.gz",


Persons := [

  rec(
      LastName      := "Assmann",
      FirstNames    := "Bjoern",
      IsAuthor      := true,
      IsMaintainer  := true,
      Email         := "BjoernAssmann@gmx.net",
      PostalAddress := Concatenation( [
            "Mathematical Institute\n",
            "University of St. Andrews\n",
            "North Haugh, St. Andrews\n Fife, KY 16 9SS, Scotland" ] ),
      Place         := "St. Andrews",
      Institution   := "University of St. Andrews"),

],

Status := "accepted",
CommunicatedBy := "Charles Wright (Eugene)",
AcceptDate := "08/2005",


README_URL := "http://www.icm.tu-bs.de/ag_algebra/software/assmann/Polenta/README",
PackageInfoURL := "http://www.icm.tu-bs.de/ag_algebra/software/assmann/Polenta/PackageInfo.g",

AbstractHTML := 
"The <span class=\"pkgname\">Polenta</span> package provides  methods to compute polycyclic presentations of matrix groups (finite or infinite). As a by-product, this package gives some functionality to compute certain module series for modules of solvable groups. For example, if G is a rational polycyclic matrix group, then we can compute the radical series of the natural Q[G]-module Q^d.",

PackageWWWHome := "http://www.icm.tu-bs.de/ag_algebra/software/assmann/Polenta",

PackageDoc := rec(          
  BookName  := "Polenta",
  ArchiveURLSubset := ["doc", "htm"],
  HTMLStart := "htm/chapters.htm",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Polycyclic presentations for matrix groups",
  Autoload  := true),

Dependencies := rec(
  GAP := ">= 4.3fix4",
  NeededOtherPackages := [[ "polycyclic", ">=1.1" ],
                          [ "alnuth" , "2.0"], ],
  SuggestedOtherPackages := [ ["aclib", "1.0"]], 
  ExternalConditions := 
[["needs KANT/KASH Computer Algebra System Version >=2.4",
"http://www.math.tu-berlin.de/~kant/" ]] 
), 

AvailabilityTest := ReturnTrue,             
BannerString := Concatenation([ 
"Loading Polenta ",
~.Version,
" ... \n" ]),     
Autoload := true,
TestFile := "tst/testall.g",
Keywords := ["polycyclic presentations", "matrix groups", "test solvability", "triangularizable subgroup", "unipotent subgroup", "radical series", "composition series of triangularizable groups" ]

));

#############################################################################
##
#E













