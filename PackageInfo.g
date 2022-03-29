#############################################################################
##
##  PackageInfo.g        GAP4 Package `Polenta'                Bjoern Assmann
##  

SetPackageInfo( rec(

PackageName := "Polenta",
Subtitle := "Polycyclic presentations for matrix groups",
Version := "1.3.10",
Date := "29/03/2022", # dd/mm/yyyy format
License := "GPL-2.0-or-later",

Persons := [

  rec(
      LastName      := "Assmann",
      FirstNames    := "Björn",
      IsAuthor      := true,
      IsMaintainer  := false,
  ),

  rec( LastName      := "Horn",
       FirstNames    := "Max",
       IsAuthor      := false,
       IsMaintainer  := true,
       Email         := "horn@mathematik.uni-kl.de",
       WWWHome       := "https://www.quendi.de/math",
       PostalAddress := Concatenation(
                          "Fachbereich Mathematik\n",
                          "TU Kaiserslautern\n",
                          "Gottlieb-Daimler-Straße 48\n",
                          "67663 Kaiserslautern\n",
                          "Germany" ),
       Place         := "Kaiserslautern, Germany",
       Institution   := "TU Kaiserslautern"
  ),

],

Status := "accepted",
CommunicatedBy := "Charles Wright (Eugene)",
AcceptDate := "08/2005",

PackageWWWHome := "https://gap-packages.github.io/polenta/",
README_URL     := Concatenation(~.PackageWWWHome, "README.md"),
PackageInfoURL := Concatenation(~.PackageWWWHome, "PackageInfo.g"),
ArchiveURL     := Concatenation("https://github.com/gap-packages/polenta/",
                                "releases/download/v", ~.Version,
                                "/polenta-", ~.Version),
ArchiveFormats := ".tar.gz .tar.bz2",

SourceRepository := rec( 
  Type := "git", 
  URL := "https://github.com/gap-packages/polenta"
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),

AbstractHTML := 
"The <span class=\"pkgname\">Polenta</span> package provides  methods to compute polycyclic presentations of matrix groups (finite or infinite). As a by-product, this package gives some functionality to compute certain module series for modules of solvable groups. For example, if G is a rational polycyclic matrix group, then we can compute the radical series of the natural Q[G]-module Q^d.",

PackageDoc := rec(          
  BookName  := "Polenta",
  ArchiveURLSubset := [ "doc" ],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Polycyclic presentations for matrix groups",
  Autoload  := true),

Dependencies := rec(
  GAP := ">= 4.7",
  NeededOtherPackages := [[ "polycyclic", "2.10.1" ],
                          [ "alnuth" , "2.2.3"],
                          [ "radiroot", "2.4" ],
                         ],
  SuggestedOtherPackages := [ ["aclib", "1.0"]],
), 

AvailabilityTest := ReturnTrue,             
Autoload := true,
TestFile := "tst/testall.g",
Keywords := [
  "polycyclic presentations",
  "matrix groups",
  "test solvability",
  "triangularizable subgroup",
  "unipotent subgroup",
  "radical series",
  "composition series of triangularizable groups"
],

AutoDoc := rec(
    TitlePage := rec(
        Copyright := "\
            <Index>License</Index>\
            &copyright; 2003-2007 by Björn Assmann<P/>\
            The &Polenta; package is free software; \
            you can redistribute it and/or modify it under the terms of the \
            <URL Text=\"GNU General Public License\">http://www.fsf.org/licenses/gpl.html</URL> \
            as published by the Free Software Foundation; either version 2 of the License, \
            or (at your option) any later version.",
        Acknowledgements := "\
            We appreciate very much all past and future comments, suggestions and \
            contributions to this package and its documentation provided by &GAP; \
            users and developers.",
    ),
),

));

#############################################################################
##
#E
