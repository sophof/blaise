context("reading and parsing blaise datamodels")

makeblafile = function(model){
  blafile = tempfile('testbla', fileext = '.bla')
  writeLines(model, con = blafile)
  return(blafile)
}

test_that("correct datamodel can be read and reproduced", {
  model = "
DATAMODEL Test
FIELDS
  A     : STRING[9]
  B     : INTEGER[2]
  C     : REAL[9,2]
  D     : STRING[4]
  E     : DATETYPE  {testcomment}
  F     : (Male, Female)
  G     : 1..20
  H     : 1.0..99.9
{ multiline comment {with nesting}
  second line}
ENDMODEL
"
  Ncols = 8

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_true(length(model_names(bla)) == Ncols)
  expect_true(length(model_types(bla)) == Ncols)
  expect_true(length(model_widths(bla)) == Ncols)
  expect_equivalent(model_names(bla), c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'))
  expect_equivalent(model_types(bla), c('STRING',
                                           'INTEGER',
                                           'REAL',
                                           'STRING',
                                           'DATETYPE',
                                           'ENUM',
                                           'INTEGER',
                                           'REAL'))
})

test_that("Unknown datatypes throw an error", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[9]
  B     : INTEGER[2]
  C     : ONZIN1[9,2]
  D     : ONzin2[4]
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_error(read_model(blafile))
})

test_that("datatypes are detected", {
  model =
"
DATAMODEL Test
FIELDS
  A     : STRING[9]
  B     : INTEGER[2]
  C     : REAL[9,2]
  D     : STRING[4]
  E     : DATETYPE
  F     : (MALE, FEMALE)
ENDMODEL
"
  blafile = makeblafile(model)
  bla = read_model(blafile)
  types = model_types(bla)
  expect_equivalent(types[1], 'STRING')
  expect_equivalent(types[2], 'INTEGER')
  expect_equivalent(types[3], 'REAL')
  expect_equivalent(types[4], 'STRING')
  expect_equivalent(types[5], 'DATETYPE')
  expect_equivalent(types[6], 'ENUM')
})

test_that("names are detected", {
  model =
    "
  DATAMODEL Test
  FIELDS
  A     : STRING[9]
  B     : INTEGER[2]
  C     : REAL[9,2]
  D     : STRING[4]
  ENDMODEL
  "
  blafile = makeblafile(model)
  bla = read_model(blafile)
  expect_equivalent(model_names(bla), c('A', 'B', 'C', 'D'))
})

test_that("lengths are detected", {
  model =
    "
  DATAMODEL Test
  FIELDS
  A     : STRING[9]
  B     : INTEGER[2]
  C     : REAL[9,2]
  D     : STRING[4]
  ENDMODEL
"
  blafile = makeblafile(model)
  bla = read_model(blafile)
  widths = model_widths(bla)
  expect_equivalent(widths[1], 9)
  expect_equivalent(widths[2], 2)
  expect_equivalent(widths[3], 9)
  expect_equivalent(widths[4], 4)
})

test_that("floats lengths and decimals are detected", {
  model =
    "
  DATAMODEL Test
  FIELDS
  A     : STRING[9]
  B     : REAL[3]
  C     : REAL[9,2]
  D     : STRING[4]
  ENDMODEL
"
  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  widths = model_widths(bla)
  decs = model_decimals(bla)
  expect_equivalent(widths[2], 3)
  expect_equivalent(widths[3], 9)
  expect_true(is.na(decs[1]))
  expect_true(is.na(decs[2]))
  expect_equivalent(decs[3], 2)
})

test_that("ENUMS get correctly read", {
  model =
    "
  DATAMODEL Test
  FIELDS
  A     : (Male, Female)
  B     : (1,2,3,4,5,6,7,8,9,10)
  C     : INTEGER[3]
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_equivalent(model_names(bla), c('A', 'B', 'C'))
  expect_equivalent(model_types(bla), c('ENUM', 'ENUM', 'INTEGER'))
  expect_equivalent(model_widths(bla), c(1, 2, 3))
  expect_equivalent(model_labels(bla)[[1]],
               c('Male', 'Female'))
  expect_equivalent(model_labels(bla)[[2]],
               c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10'))
  expect_equivalent(model_labels(bla)[[3]],
               NA_character_)
})

test_that("alternative representations for INTEGER and REAL work", {
  model = "
DATAMODEL Test
FIELDS
  G     : 1..20
  H     : 1.0..99.9
ENDMODEL
"
  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_equivalent(model_types(bla), c('INTEGER', 'REAL'))
})

test_that("Only 8 width or empty datetypes work", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : DATETYPE
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_silent(read_model(blafile))

  model = "
  DATAMODEL Test
  FIELDS
  A     : DATETYPE[8]
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_silent(read_model(blafile))

  model = "
  DATAMODEL Test
  FIELDS
  A     : DATETYPE[10]
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_error(read_model(blafile))
})


test_that("field descriptions over multiple lines work", {
  model =
    "
  DATAMODEL Test
  FIELDS
  A     :
          STRING[9]
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_equivalent(model_names(bla), 'A')
  expect_equivalent(model_types(bla), 'STRING')
  expect_equivalent(model_widths(bla), 9)

  model =
    "
  DATAMODEL Test
  FIELDS
  A     : (Male,
           Female)
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_equivalent(model_names(bla), 'A')
  expect_equivalent(model_types(bla), 'ENUM')
  expect_equivalent(model_widths(bla), 1)
})


test_that("malformed datamodel throws an error", {
  model =
    "
  DATAMODEL Test
  FIELDS
  A     :  STRING[9]
  END
  "
  blafile = makeblafile(model)
  expect_error({bla = read_model(blafile)})

  model =
    "
  FIELDS
  A     :  STRING[9]
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_error({bla = read_model(blafile)})

  model =
    "
  DATAMODEL test
  A     :  STRING[9]
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_error({bla = read_model(blafile)})

  model =
    "
  DATAMODEL test
  FIELDS
  A     :  STRING
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_error({bla = read_model(blafile)})

  model =
    "
  DATAMODEL test
  FIELDS
  A     :
  ENDMODEL
  "
  blafile = makeblafile(model)
  expect_error({bla = read_model(blafile)})
})

test_that("Nonsense decimals don't work", {
  model =
    "
  DATAMODEL Test
  FIELDS
  A     : REAL[3,3]
  ENDMODEL
"
  blafile = makeblafile(model)
  expect_error(read_model(blafile))

  model =
    "
  DATAMODEL Test
  FIELDS
  A     : REAL[3,2]
  ENDMODEL
"
  blafile = makeblafile(model)
  expect_error(read_model(blafile))
})

test_that("lowercase variables also work", {
  model = "
DATAMODEL Test
  FIELDS
  A     : String[9]
  B     : integer[2]
  C     : real[9,2]
  D     : STRING[4]
  E     : Datetype
  F     : (Male, Female)
  G     : 1..20
  H     : 1.0..99.9
  ENDMODEL
  "
  Ncols = 8

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_true(length(model_names(bla)) == Ncols)
  expect_true(length(model_types(bla)) == Ncols)
  expect_true(length(model_widths(bla)) == Ncols)
  expect_equivalent(model_names(bla), c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'))
  expect_equivalent(model_types(bla), c('STRING',
                                           'INTEGER',
                                           'REAL',
                                           'STRING',
                                           'DATETYPE',
                                           'ENUM',
                                           'INTEGER',
                                           'REAL'))
})

test_that("lowercase DATAMODEL etc. works", {
  model = "
Datamodel Test
  Fields
  A     : String[9]
  Endmodel
  "

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_true(length(model_names(bla)) == 1)
  expect_equivalent(model_names(bla), 'A')
  expect_equivalent(model_types(bla), c('STRING'))
})

test_that("DUMMY variables are accepted", {
  model = "
  DATAMODEL Test
  FIELDS
  B     : integer[2]
  DUMMY[1]
  D     : STRING[4]
  ENDMODEL
  "
  Ncols = 3

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_true(length(model_names(bla)) == Ncols)
  expect_true(length(model_types(bla)) == Ncols)
  expect_true(length(model_widths(bla)) == Ncols)
  expect_equivalent(model_names(bla), c('B', 'DUMMY1', 'D'))
  expect_equivalent(model_types(bla), c('INTEGER',
                                           'DUMMY',
                                           'STRING'))
})

test_that("multiple DUMMY variables are accepted", {
  model = "
  DATAMODEL Test
  FIELDS
  B     : integer[2]
  DUMMY[1]
  DUMMY[1]
  D     : STRING[4]
  ENDMODEL
  "
  Ncols = 4

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_true(length(model_names(bla)) == Ncols)
  expect_true(length(model_types(bla)) == Ncols)
  expect_true(length(model_widths(bla)) == Ncols)
  expect_equivalent(model_names(bla), c('B', 'DUMMY1', 'DUMMY2', 'D'))
  expect_equivalent(model_types(bla), c('INTEGER',
                                           'DUMMY',
                                           'DUMMY',
                                           'STRING'))
})

test_that("real with spaces is read", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : STRING[9]
  C     : REAL[9, 2]
  ENDMODEL
  "
  Ncols = 2

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_true(length(model_names(bla)) == Ncols)
  expect_true(length(model_types(bla)) == Ncols)
  expect_true(length(model_widths(bla)) == Ncols)
  expect_equivalent(model_names(bla), c('A', 'C'))
  expect_equivalent(model_types(bla), c('STRING',
                                           'REAL'))
  expect_equivalent(model_decimals(bla), c(NA, 2))
})

test_that("numbered enums work", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : (Male (1), Female (2), Unknown (9))
  B     : (M(1),F(2),X(10))
  ENDMODEL
  "
  Ncols = 2

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_equivalent(model_names(bla), c('A', 'B'))
  expect_equivalent(model_types(bla), c('ENUM', 'ENUM'))
  expect_equivalent(model_widths(bla), c(1, 2))
  expect_equal(sapply(variables(bla), is.numbered_enum), c(A = T, B = T))
  expect_equivalent(model_labels(bla)[[1]], c('Male', 'Female', 'Unknown'))
  expect_equivalent(model_labels(bla)[[2]], c('M', 'F', 'X'))
  expect_equivalent(model_levels(bla)[[1]], c(1, 2, 9))
  expect_equivalent(model_levels(bla)[[2]], c(1, 2, 10))
})

test_that("numbered enums work when constructed vertically", {
  model = "
  DATAMODEL Test
  FIELDS
  A     : (Male (1),
           Female (2),
           Unknown (9))
  DUMMY[1]
  B     : (M(1),
           F(2),
           X(10))
  ENDMODEL
  "
  Ncols = 2

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_equivalent(model_names(bla), c('A', 'DUMMY1', 'B'))
  expect_equivalent(model_types(bla), c('ENUM',
                                           'DUMMY',
                                           'ENUM'))
  expect_equivalent(model_widths(bla), c(1, 1, 2))
})

test_that("Custom Types can be read", {
  model = "
  DATAMODEL Test
  TYPE
    sex = (Male (1),
           Female (2),
           Unknown (9))
    YesNo = (Yes (1),
             No (0),
             dontknow (10))
  FIELDS
    A     : sex
    B     : YesNo
    C     : STRING[1]
  ENDMODEL
  "
  Ncols = 3

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_equivalent(model_names(bla), c('A', 'B', 'C'))
  expect_equivalent(model_types(bla), c('ENUM',
                                           'ENUM',
                                           'STRING'))
  expect_equivalent(model_widths(bla), c(1, 2, 1))
})

test_that("complex datamodel uit de praktijk", {
  model = "
{ ***************************************************************************
    *
  *  OMSCHRIJVING
  *    Beschrijving van de structuur van het bestand met de gegevens van
  *    van het weegkader.
  *
  *  GESCHIEDENIS
  *    1015/05/20  1.0  MWGD      ---
  *
  ***************************************************************************
  * }

  DATAMODEL ModelWeegkader
  TYPE
  TypeGeslachtklasse = (
  Man (1),
  Vrouw( 2),
  Onbekend (9))

  TypeLeeftijdklasse = (
  Van18TotEnMet20Jaar (1),
  Van21TotEnMet25Jaar (2),
  Van26TotEnMet30Jaar (3),
  Vanaf31Jaar (4),
  Onbekend (9))

  TypeBurgerlijkeStaatklasse = (
  Ongehuwd (1),
  Gehuwd (2),
  Verweduwd (3),
  Gescheiden (4),
  Partnerschap (5),
  VerweduwdNaPartnerschap (6),
  GescheidenNaPartnerschap (7),
  VerweduwdOfGescheiden (8),
  Onbekend(99))

  TypePlaatsInHuishoudenklasse = (
  ThuiswonendKind (1),
  Alleenstaande (2),
  PartnerNietGehuwdZonderKinderen (3),
  PartnerGehuwdStelZonderKinderen (4),
  PartnerNietGehuwdMetKinderen (5),
  PartnerGehuwdStelMetKinderen (6),
  OuderEenouderHH (7),
  RefPersoonOverigHH (8),
  OverigLid (9),
  LidInstitutioneelHH (10),
  Onbekend (99))

  TypeTypeHuishoudenklasse = (
  Eenpersoonshuishouden (1),
  NietGehuwdPaarZonderKinderen (2),
  GehuwdPaarZonderKinderen (3),
  NietGehuwdPaarMetKinderen (4),
  GehuwdPaarMetKinderen (5),
  Eenouderhuishouden (6),
  OverigHH (7),
  InstitutioneelHH (8),
  Onbekend (99))

  TypeSocEcoCategorieklasse = (
  Werknemer (1),
  DGA (2),
  Zelfstandige (3),
  OverigActief (4),
  OntvangerWW (5),
  OntvangerBijstand (6),
  OntvangerOverigeUitkering (7),
  OntvangerUitkeringZiekte (8),
  OntvangerPensioen (9),
  SchoolgaandStudentMetInkomen (10),
  SchoolgaandStudentZonderInkomen (11),
  OverigZonderInkomen (12),
  WerknemerEnDGA (21),
  ZelfstandigeEnOverigActief (22),
  OntvangerUItkering (23),
  SchoolgaandStudent (24),
  OverigNietActief (31),
  Onbekend (99))

  TypeStedelijkheidklasse = (
  ZeerSterStedelijk (1),
  SterkStedelijk (2),
  MatigStedelijk (3),
  WeinigStedelijk (4),
  NietStedelijk (5),
  Onbekend (9))

  TypeLandsdeelklasse = (
  NoordNederland (1),
  OostNederland (2),
  WestNederland (3),
  ZuidNederland (4),
  Onbekend (9))

  TypePersInkomensklasse = (
  Perc00TotEnMet01 (1),
  Perc02TotEnMet10 (2),
  Perc11TotEnMet20 (3),
  Perc21TotEnMet30 (4),
  Perc31TotEnMet40 (5),
  Perc41TotEnMet50 (6),
  Perc51TotEnMet60 (7),
  Perc61TotEnMet99 (8),
  Perc00TotEnMet10 (9),
  Onbekend (99))

  TypeInschrijfduurklasse = (
  Van0000TotEnMet0050Dagen ( 1),
  Van0051TotEnMet0220Dagen ( 2),
  Van0221TotEnMet0420Dagen ( 3),
  Van0421TotEnMet0650Dagen ( 4),
  Van0651TotEnMet0900Dagen ( 5),
  Van0901TotEnMet1140Dagen ( 6),
  Van1141TotEnMet1470Dagen ( 7),
  Van1471TotEnMet1860Dagen ( 8),
  Van1861TotEnMet2280Dagen ( 9),
  Van2281TotEnMet2840Dagen (10),
  Van2841TotEnMet7200Dagen (11),
  Van7201TotEnMet9999Dagen (12),
  Van0000TotEnMet0220Dagen (13),
  Van2841TotEnMet9999Dagen (14),
  Onbekend (99))

  TypeAantalImmigratiesklasse = (
  GeenImmigraties (0),
  EenImmigratie (1),
  TweeOfMeerImmigraties (2),
  Onbekend (9))

  TypeTypeNationaliteitklasse = (
  UitsluitendNederlands (1),
  NederlandsEnEenNietNL (2),
  NederlandsEnTweeNietNL (3),
  NederlandsEnDrieNietNL (4),
  EenNietNederlands (5),
  TweeNietNederlands (6),
  DrieNietNederlands (7),
  NietVanToepassing (9),
  Nederlands (10),
  NietNederlands (11),
  Onbekend (99))

  TypeSoortVerblijfklasse = (
  Onbekend (0),
  VreemdelingenWet09 ( 1),
  VreemdelingenWet10 ( 2),
  VreemdelingenWet11 ( 3),
  VreemdelingenWet12 ( 4),
  VreemdelingenWet13 ( 5),
  VreemdelingenWet14 ( 6),
  VreemdelingenWet15 ( 7),
  VreemdelingenWet16 ( 8),
  VreemdelingenWet17 ( 9),
  VreemdelingenWet18 (10),
  VreemdelingenWet19 (11),
  VreemdelingenWet20 (12),
  VreemdelingenWet21 (13),
  VreemdelingenWet22 (14),
  VreemdelingenWet23 (15),
  VreemdelingenWet24 (16),
  VreemdelingenWet25 (17),
  VreemdelingenWet26 (18),
  VreemdelingenWet27 (19),
  VreemdelingenWet28 (20),
  VreemdelingenWet29 (21),
  VreemdelingenWet30 (22),
  VreemdelingenWet31 (23),
  VreemdelingenWet32 (24),
  VreemdelingenWet33 (25),
  VreemdelingenWet34 (26),
  VreemdelingenWet35 (27),
  VreemdelingenWet36 (28),
  VreemdelingenWet37 (29),
  VreemdelingenWet38 (30),
  VreemdelingenWet39 (31),
  VreemdelingenWet40 (32),
  VreemdelingenWet41 (33),
  VreemdelingenWet42 (34),
  VreemdelingenWet43 (35),
  VreemdelingenWet91 (36),
  VreemdelingenWet92 (37),
  VreemdelingenWet93 (38),
  VreemdelingenWet98 (39),
  VreemdelingenWet99 (40))

  TypeG3klasse = (
  NoordNederland (1),
  OostNederland (2),
  WestNederland (3),
  ZuidNederland (4),
  Amsterdam (5),
  DenHaag (6),
  Rotterdam (7),
  Onbekend (9))

  TypeJaNee = (
  Nee (0),
  Ja (1),
  Onbekend (9))


  FIELDS
  CBKSoortNr                   : STRING[1]
  RINPersoon                   : STRING[9]
  RINPersoonVolgnummer         : STRING[2]
  GBAGeboortejaar              : STRING[4]
  GBAGeboortemaand             : STRING[2]
  GBAGeboorteland              : STRING[4]
  GBAGeslacht                  : STRING[1]   { * }
  RINAdres                     : STRING[9]
  GBARegiocode2014             : STRING[8]
  GBAGeboortelandMoeder        : STRING[4]
  GBAGeboortelandVader         : STRING[4]
  AflHerkomstWetSAMEN          : STRING[1]
  AflHerkomstCBS               : STRING[1]
  AflGeneratie                 : STRING[1]
  GBANationaliteit1            : STRING[4]
  GBANationaliteit2            : STRING[4]
  GBATypeNationaliteit         : STRING[1]   { ? }
  GBABurgerlijkeStaat          : STRING[1]   { * }
  GBASoortVerblijf             : STRING[2]   { * }
  GBALandVanHerkomst           : STRING[4]
  GBALandVanBestemming         : STRING[4]
  CBKBeginGeldigheid           : STRING[8]
  CBKEindGeldigheid            : STRING[8]
  CBKMutatieCode               : STRING[4]
  DatumEersteVestiging         : STRING[8]
  DatumLaatsteVestiging        : STRING[8]
  InschrijfduurInDagen         : INTEGER[6]  { * }
  AantalImmigraties            : INTEGER[3]  { * }
  AantalEmigraties             : INTEGER[3]
  AantalOverigeAfvoer          : INTEGER[3]
  AantalAdreswijzigingen       : INTEGER[3]
  IngeschrevenOp20140819       : INTEGER[3]
  IngeschrevenOp20141001       : INTEGER[3]
  IngeschrevenOp20150331       : INTEGER[3]
  IngeschrevenOp20150508       : INTEGER[3]
  Leeftijd                     : INTEGER[3]  { * }
  DUMMY[1]

  InWeegkader                  : INTEGER[1]
  InSteekproefkader            : INTEGER[1]
  InNettoSteekproef            : INTEGER[1]

  IsBenaderd                   : STRING[1]
  HeeftJuistAdres              : STRING[1]
  HeeftGerespondeerd           : STRING[1]
  DUMMY[1]

  PlaatsInHuishouden           : STRING[2]
  { *
  *     : Onbekend;                           (N =       380)
  *   1 : Thuiswonend kind                    (N = 4.578.201)
  *   2 : Alleenstaande                       (N = 2.842.715)
  *   3 : Partner in niet-gehuwd paar zonder
  *       kinderen                            (N = 1.070.198)
  *   4 : Partner in gehuwd paar zonder
  kinderen                            (N = 3.290.518)
  *   5 : Partner in niet-gehuwd paar met
  *       kinderen                            (N =   800.292)
  *   6 : Partner in gehuwd paar met
  kinderen                            (N = 3.257.338)
  *   7 : Ouder in eenouderhuishouden;        (N =   545.464)
  *   8 : Referentiepersoon in overig HH;     (N =    41.288)
  *   9 : Overig lid van een huishouden;      (N =   215.015)
  *  10 : Lid van institutioneel HH.          (N =   232.288)
  * }

  TypeHuishouden               : STRING[1]
  { *
  *    : Onbekend;                            (N =       380)
  *  1 : Eenpersoonshuishouden;               (N = 2.842.715)
  *  2 : Niet-gehuwd paar zonder kinderen;    (N = 1.076.259)
  *  3 : Gehuwd paar zonder kinderen;         (N = 3.328.474)
  *  4 : Niet-gehuwd paar met kinderen;       (N = 1.486.262)
  *  5 : Gehuwd paar met kinderen;            (N = 6.406.246)
  *  6 : Eenouderhuishouden;                  (N = 1.380.385)
  *  7 : Overig huishouden;                   (N =   120.508)
  *  8 : Institutioneel huishouden.           (N =   232.288)
  * }

  SocEcoCategorie              : STRING[2]
  { *
  *     : Onbekend                            (N =   476.625)
  *  11 : Werknemer;                          (N = 6.046.104)
  *  12 : DGA;                                (N =   191.648)
  *  13 : Zelfstandige;                       (N =   778.251)
  *  14 : Overige actief;                     (N =   113.348)
  *  21 : Ontvanger WW-uitkering;             (N =   248.790)
  *  22 : Ontvanger bijstandsuitkering;       (N =   356.426)
  *  23 : Ontvanger uitkering sociale voorzineningen,
  *       overig                              (N =   225.826)
  *  24 : Ontvanger uitkering ziekt, AU       (N =   486.618)
  *  25 : Ontvanger pensioensuitkering        (N = 2.851.332)
  *  26 : Nog niet schoolgaand, schoolgaand, student,
  *       met inkomen                         (N = 1.006.609)
  *  31 : Nog niet schoolgaand, schoolgaand, student,
  *       zonder inkomen                      (N = 3.237.198)
  *  32 : Zonder inkomen
  * }

  PersInkomenPerc              : STRING[2]
  HeeftPersInkomen             : STRING[1]
  OntvangtWWUitkering          : STRING[1]
  OntvangtBijstanduitkering    : STRING[1]
  OntvangtAOUitkering          : STRING[1]

  GBAGemeentecode2014          : STRING[4]
  Stedelijkheid                : STRING[1]
  Landsdeel                    : STRING[2]
  DUMMY[1]

  { *
  * De indeling van de hulpvariabelen voor de weging in klassen.
  * }

  Geslachtklasse               : TypeGeslachtklasse
  Leeftijdklasse               : TypeLeeftijdklasse
  BurgerlijkeStaatklasse       : TypeBurgerlijkeStaatklasse
  PlaatsInHuishoudenklasse     : TypePlaatsInHuishoudenklasse
  TypeHuishoudenklasse         : TypeTypeHuishoudenklasse
  SocEcoCategorieklasse        : TypeSocEcoCategorieklasse
  OntvangtWWUitkeringklasse    : TypeJaNee
  Stedelijkheidklasse          : TypeStedelijkheidklasse
  Landsdeelklasse              : TypeLandsdeelklasse
  PersInkomensklasse           : TypePersInkomensklasse
  Inschrijfduurklasse          : TypeInschrijfduurklasse
  AantalImmigratiesklasse      : TypeAantalImmigratiesklasse
  TypeNationaliteitklasse      : TypeTypeNationaliteitklasse
  SoortVerblijfklasse          : TypeSoortVerblijfklasse
  G3klasse                     : TypeG3klasse
  IngeschrevenOp20150508klasse : TypeJaNee
  ENDMODEL
  "
  Ncols = 72

  blafile = makeblafile(model)
  expect_silent({bla = read_model(blafile)})
  expect_true(length(model_names(bla)) == Ncols)
  expect_true(length(model_types(bla)) == Ncols)
  expect_true(length(model_widths(bla)) == Ncols)
  expect_equivalent(model_names(bla)[69], 'TypeNationaliteitklasse')
  expect_equivalent(model_widths(bla)[69], 2L)
  expect_equivalent(model_labels(bla)[[69]], c("1","2","3","4","5","6","7","9","10","11","99"))
  expect_equivalent(model_names(bla)[50], 'OntvangtWWUitkering')
  expect_equivalent(model_widths(bla)[50], 1L)
  expect_equivalent(model_names(bla)[44], 'DUMMY2')
  expect_equivalent(model_widths(bla)[44], 1L)
  expect_equivalent(model_names(bla)[34], 'IngeschrevenOp20150331')
  expect_equivalent(model_widths(bla)[34], 3L)
})


