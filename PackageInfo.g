#############################################################################
##
##  PackageInfo.g        GAP4 Package `Polenta'                Bjoern Assmann
##  

SetPackageInfo( rec(

PackageName := "Polenta",
Subtitle := "Polycyclic presentations for matrix groups",
Version := "1.0",
Date := "02/11/2003",

ArchiveURL := "http://cayley.math.nat.tu-bs.de/software/assmann/Polenta/Polenta-1.0",
ArchiveFormats := ".tar.gz",


Persons := [

  rec(
      LastName      := "Assmann",
      FirstNames    := "Bjoern",
      IsAuthor      := true,
      IsMaintainer  := true,
      Email         := "bjoerna77@gmx.de",
      PostalAddress := Concatenation( [
            "Institut f\"ur Geometrie, Algebra und diskrete Mathematik\n",
            "TU Braunschweig\n",
            "Pockelsstr. 14\n D-38106 Braunschweig\n Germany" ] ),
      Place         := "Braunschweig",
      Institution   := "TU Braunschweig"),

],

# Status := "accepted",
Status := "deposited",

##  You must provide the next two entries if and only if the status is 
##  "accepted" because is was successfully refereed:
# format: 'name (place)'
# CommunicatedBy := "Mike Atkinson (St. Andrews)",
#CommunicatedBy := "",
# format: mm/yyyy
# AcceptDate := "08/1999",
#AcceptDate := "",

README_URL := "http://cayley.math.nat.tu-bs.de/software/assmann/Polenta/README",
PackageInfoURL := "http://cayley.math.nat.tu-bs.de/software/assmann/Polenta/PackageInfo.g",

AbstractHTML := 
"The <span class=\"pkgname\">Polenta</span> package provides  methods to compute polycyclic presentations of matrix groups (finite or infinite). As a by-product, this package gives some functionality to compute certain module series for modules of solvable groups. For example, if G is a rational polycyclic matrix group, then we can compute the radical series of the natural Q[G]-module Q^d.",

PackageWWWHome := "http://cayley.math.nat.tu-bs.de/software/assmann/Polenta",

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
                          [ "alnuth" , "1,0"], ],
  SuggestedOtherPackages := [ ["aclib", "1.0"]], 
  ExternalConditions := ["needs KANT/KASH Computer Algebra System"] ), 

AvailabilityTest := ReturnTrue,             
BannerString := "Loading Polenta 1.0 ... \n",     
Autoload := true,
TestFile := "tst/testall.g",
Keywords := ["polycyclic presentations", "matrix groups" ]

));

#############################################################################
##
#E













