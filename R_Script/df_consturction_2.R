## @knitr setup --------------------------------------------------------------------------------

suppressMessages(library(sas7bdat))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(survey))
suppressMessages(library(stringr))
suppressMessages(library(haven))

################################################################################################

### Functions ----------------------------------------------------------------------------------
################################################################################################

num.na <- function(x, percent = FALSE){
  if(is.null(dim(x))){
    if(percent){
      mean(is.na(x))
    }
    else{
      sum(is.na(x))
    }
  }
  else{
    if(percent){
      apply(X = x, MARGIN = 2, FUN = function(x){mean(is.na(x))})
    }
    else{
      apply(X = x, MARGIN = 2, FUN = function(x){sum(is.na(x))})
    }
  }  
}

quick.look <- function(x, cutoff = 0){
  a <- dim(x)
  b <- lookuptable[names(x),]
  c <- num.na(x)
  e <- c/a[1]
  f <- c[c <= cutoff]
  d <- unlist(deparse(substitute(x)))
  d <- filter(data_set_description, data_set == d)
  
  list(data_set = d, dimensions = a, variables = b, missingValues = c, missingValuePercentage = e, 
       noMissingValues = names(f))
}

uniq.length <- function(x){
  length(unique(x))
}

read.file <- function(dir.path = "Data/NASS/2014/Formatted Data/", file){ #, debug = FALSE){
  #   require(sas7bdat)
  #   read.sas7bdat(str_c(dir.path, file), debug)
  require(haven)
  read_sas(data_file = str_c(dir.path, file))
}

missing.together <- function(var1, var2){
  sum((is.na(var1) & !is.na(var2)) | (!is.na(var1) & is.na(var2)))
}

still.missing <- function(var1, var2){
  ind <- is.na(var1)
  sum(is.na(var2[ind]))
}

complement.missing <- function(var1, var2){
  sum(is.na(var2[is.na(var1)]))
}

################################################################################################

### Dictionary Definition ======================================================================

describ <- c("MAXIMUM KNOWN AIS IN ACCIDENT (AIS98 FORMAT)", 
             "MAXIMUM KNOWN AIS IN THIS CRASH (AIS08 FORMAT)",
             "Accident Event Sequence Number",
             "ACCIDENT EVENT SEQUENCE (HIGHEST)",
             "ACCIDENT EVENT SEQUENCE (2ND HIGHEST)",
             "Accident Sequence No For Highest Delta V",
             "ACCIDENT TYPE",
             "ADAPTIVE (ASSISTIVE) DRIVING EQUIPMENT",
             "ADMINISTRATIVE USE",
             "OCCUPANT’S AGE",
             "NUMBER OF SERIOUSLY INJURED OCCUPANTS (AIS98 FORMAT)",
             "NUMBER OF SERIOUSLY INJURED OCCUPANTS (AIS08 FORMAT)",
             "TOTAL NUMBER OF INJURED OCCUPANTS (AIS08 FORMAT)",
             "TOTAL NUMBER OF INJURED OCCUPANTS (AIS98 FORMAT)",
             "Abbreviated Injury Scale (AIS98 FORMAT)",
             "A.I.S. SEVERITY (AIS08 FORMAT)",
             "ALCOHOL INVOLVED IN ACCIDENT",
             "ALCOHOL TEST RESULT FOR DRIVER",
             "ROADWAY ALIGNMENT",
             "MULTI-STAGE MANUFACTURED/CERT. ALT. VEH.",
             "HEADING ANGLE FOR OTHER VEHICLE",
             "HEADING ANGLE FOR THIS VEHICLE",
             "ANTILOCK BRAKES",
             "ASPECT90",
             "MAXIMUM TREATMENT IN ACCIDENT",
             "AIR BAG SYSTEM AVAILABILITY",
             "OTHER FRONTAL AIR BAG AVAILABILITY/FUNCTION",
             "POLICE REPORTED AIRBAG AVAILABILITY/FUNCTION",
             "CDC FOR AIR BAG DEPLOYMENT IMPACT",
             "WAS THERE DAMAGE TO THE AIR BAG",
             "SOURCE OF AIR BAG DAMAGE",
             "AIR BAG SYSTEM DEPLOYED",
             "OTHER AIR BAG SYSTEM DEPLOYMENT",
             "AIR BAG DEPLOYMENT ACCIDENT EVENT SEQUENCE NUMBER",
             "AIR BAG SYSTEM FAILURE",
             "WERE AIR BAG MODULE COVER FLAPS DAMAGED",
             "DID AIR BAG MODULE COVER FLAPS OPEN AT DESG TEAR PTS",
             "PRIOR MAINTENANCE/SERVICE ON AIR BAG",
             "TYPE OF AIR BAG",
             "BARRIER EQUIVALENT SPEED",
             "SHOULDER BELT UPPER ANCHORAGE ADJUSTMENT",
             "PRIMARY SOURCE OF BELT USE DETERMINATION",
             "ARTERIAL BLOOD GASES (ABG) HC03",
             "WAS THE OCCUPANT GIVEN BLOOD?",
             "BODY REGION (O.I.C. - A.I.S.)",
             "VEHICLE BODY TYPE",
             "CARBURETION",
             "VEHICLE CARGO WEIGHT",
             "CASE NUMBER - STRATUM",
             "CASE SEQUENCE NUMBER",
             "1ST MEDICALLY REPORTED CAUSE OF DEATH",
             "2ST MEDICALLY REPORTED CAUSE OF DEATH",
             "3ST MEDICALLY REPORTED CAUSE OF DEATH",
             "1ST DOMINANT CRUSH DIRECTION",
             "10TH DOMINANT CRUSH DIRECTION",
             "2ND DOMINANT CRUSH DIRECTION",
             "3RD DOMINANT CRUSH DIRECTION",
             "4TH DOMINANT CRUSH DIRECTION",
             "5TH DOMINANT CRUSH DIRECTION",
             "6TH DOMINANT CRUSH DIRECTION",
             "7TH DOMINANT CRUSH DIRECTION",
             "8TH DOMINANT CRUSH DIRECTION",
             "9TH DOMINANT CRUSH DIRECTION",
             "CHILD SAFETY SEAT HARNESS USAGE",
             "CHILD SAFETY SEAT MAKE/MODEL",
             "CHILD SAFETY SEAT ORIENTATION",
             "HOW CHILD SAFETY SEAT USED",
             "CHILD SAFETY SEAT SHIELD USAGE",
             "CHILD SAFETY SEAT TETHER USAGE",
             "TYPE OF CHILD SAFETY SEAT",
             "WAS CHILD SEAT USED?",
             "CLASS OF FIRST VEHICLE",
             "CLASS OF OTHER VEHICLE",
             "WEATHER",
             "TELESCOPING STEERING COLUMN ADJUSTMENT",
             "TILT STEERING COLUMN ADJUSTMENT",
             "STEERING COLUMN TYPE",
             "POST COLLISION CONDITION OF TREE OR POLE",
             "VEHICLE CURB WEIGHT",
             "DAYTIME RUNNING LIGHTS",
             "DAY OF WEEK OF ACCIDENT",
             "TIME TO DEATH",
             "DIRECT DAMAGE WIDTH",
             "DIRECT/INDIRECT INJURY",
             "CDCs DOCUMENTED BUT NOT CODED ON FILE?",
             "DOCUMENTATION OF TRAJECTORY DATA",
             "DIRECTION OF FORCE (HIGHEST)",
             "DIRECTION OF FORCE (2ND HIGHEST)",
             "DRUG INVOLVED",
             "POLICE REPORTED ALCOHOL PRESENCE",
             "DRIVER'S DISTRACTION/INATTENTION TO DRIVING",
             "DRIVER PRESENCE IN VEHICLE",
             "REPORTED OTHER DRUG",
             "DRIVER'S ZIP CODE",
             "LONGITUDINAL COMPONENT OF DELTA V FOR AIR BAG",
             "BASIS FOR TOTAL DELTA V (HIGHEST)",
             "CRUSH PROFILE C1 (HIGHEST)",
             "CRUSH PROFILE C2 (HIGHEST)",
             "CRUSH PROFILE C3 (HIGHEST)",
             "CRUSH PROFILE C4 (HIGHEST)",
             "CRUSH PROFILE C5 (HIGHEST)",
             "CRUSH PROFILE C6 (HIGHEST)",
             "CONFIDENCE IN RECONSTRUCTION",
             "CRUSH PROFILE D (HIGHEST)",
             "ESTIMATED HIGHEST DELTA V",
             "CRUSH PROFILE L (HIGHEST)",
             "LATERAL COMPONENT OF DELTA V",
             "LONGITUDINAL COMPONENT OF DELTA V",
             "TOTAL DELTA V",
             "EJECTION AREA",
             "EJECTION MEDIUM",
             "EJECTION",
             "ENERGY ABSORPTION",
             "ENTRAPMENT",
             "ETHNICITY",
             "NUMBER OF RECORDED EVENTS IN ACCIDENT",
             "DEFORMATION EXTENT (HIGHEST)",
             "DEFORMATION EXTENT (2ND HIGHEST)",
             "WAS THE OCCUPANT WEARING EYE-WEAR",
             "LF DAMAGE/FAILURE ASSOCIATED W",
             "LR DAMAGE/FAILURE - OPENING IN COLLISION",
             "RF DAMAGE/FAILURE - OPENING IN COLLISION",
             "RR DAMAGE/FAILURE - OPENING IN COLLISION",
             "TG DAMAGE/FAILURE - OPENING IN COLLISION",
             "FETAL MORTALITY",
             "FIRE OCCURRENCE",
             "ORIGIN OF FIRE",
             "FOUR WHEEL DRIVE",
             "FRONT OVERRIDE/UNDERRIDE THIS VEHICLE",
             "FRONT WHEEL DRIVE",
             "LOCATION OF FUEL TANK-1 FILLER CAP",
             "LOCATION OF FUEL TANK-2 FILLER CAP",
             "FUEL CODE",
             "DAMAGE TO FUEL TANK-1",
             "DAMAGE TO FUEL TANK-2",
             "LEAKAGE LOCATION OF FUEL SYSTEM-1",
             "LEAKAGE LOCATION OF FUEL SYSTEM-2",
             "EQUIPPED WITH MORE THAN TWO FUEL TANKS",
             "LOCATION OF FUEL TANK-1",
             "LOCATION OF FUEL TANK-2",
             "FUEL TANK-1 PRECRASH CONDITIONS",
             "FUEL TANK-2 PRECRASH CONDITIONS",
             "TYPE OF FUEL TANK-1",
             "TYPE OF FUEL TANK-2",
             "FUEL TYPE-1",
             "FUEL TYPE-2",
             "DEFORMATION LOCATION (HIGHEST)",
             "DEFORMATION LOCATION (2ND HIGHEST)",
             "GENERAL AREA OF DAMAGE FIRST VEHICLE",
             "GENERAL AREA OF DAMAGE OTHER VEHICLE",
             "GLASGOW COMA SCALE (GCS) SCORE",
             "BL GLAZING DAMAGE FROM IMPACT FORCES",
             "LF GLAZING DAMAGE FROM IMPACT FORCES",
             "LR GLAZING DAMAGE FROM IMPACT FORCES",
             "OTHER GLAZING DAMAGE FROM IMPACT FORCES",
             "RF GLAZING DAMAGE FROM IMPACT FORCES",
             "RR GLAZING DAMAGE FROM IMPACT FORCES",
             "ROOF GLAZING DAMAGE FROM IMPACT FORCES",
             "WS GLAZING DAMAGE FROM IMPACT FORCES",
             "BL GLAZING DAMAGE FROM OCCUPANT CONTACT",
             "LF GLAZING DAMAGE FROM OCCUPANT CONTACT",
             "LR GLAZING DAMAGE FROM OCCUPANT CONTACT",
             "OTHER GLAZING DAMAGE FROM OCC. CONTACT",
             "RF GLAZING DAMAGE FROM OCCUPANT CONTACT",
             "RR GLAZING DAMAGE FROM OCCUPANT CONTACT",
             "ROOF GLAZING DAMAGE FROM OCC. CONTACT",
             "WS GLAZING DAMAGE FROM OCCUPANT CONTACT",
             "BL WINDOW PRECRASH GLAZING STATUS",
             "LF WINDOW PRECRASH GLAZING STATUS",
             "LR WINDOW PRECRASH GLAZING STATUS",
             "OTHER WINDOW PRECRASH GLAZING STATUS",
             "RF WINDOW PRECRASH GLAZING STATUS",
             "RR WINDOW PRECRASH GLAZING STATUS",
             "ROOF WINDOW PRECRASH GLAZING STATUS",
             "WS WINDOW PRECRASH GLAZING STATUS",
             "BL TYPE OF WINDOW/WINDSHIELD GLAZING",
             "LF TYPE OF WINDOW/WINDSHIELD GLAZING",
             "LR TYPE OF WINDOW/WINDSHIELD GLAZING",
             "OTHER TYPE OF WINDOW/WINDSHIELD GLAZING",
             "RF TYPE OF WINDOW/WINDSHIELD GLAZING",
             "RR TYPE OF WINDOW/WINDSHIELD GLAZING",
             "ROOF TYPE OF WINDOW/WINDSHIELD GLAZING",
             "WS TYPE OF WINDOW/WINDSHIELD GLAZING",
             "HEAD RESTRAINT TYPE/DAMAGE BY OCCUPANT",
             "HEIGHT OF OCCUPANT",
             "HOSPITAL STAY",
             "IMPACT SPEED",
             "1ST INTRUDING COMPONENT",
             "10TH INTRUDING COMPONENT",
             "2ND INTRUDING COMPONENT",
             "3RD INTRUDING COMPONENT",
             "4TH INTRUDING COMPONENT",
             "5TH INTRUDING COMPONENT",
             "6TH INTRUDING COMPONENT",
             "7TH INTRUDING COMPONENT",
             "8TH INTRUDING COMPONENT",
             "9TH INTRUDING COMPONENT",
             "INJURY LEVEL (AIS98 FORMAT)",
             "INJURY LEVEL (AIS08 FORMAT)",
             "INJURY NUMBER",
             "NUMBER OF RECORDED INJURIES (AIS98 FORMAT)",
             "NUMBER OF RECORDED INJURIES (AIS08 FORMAT)",
             "INJURY SEVERITY (POLICE RATING)",
             "INJURY SOURCE",
             "1ST LOCATION OF INTRUSION",
             "10TH LOCATION OF INTRUSION",
             "2ND LOCATION OF INTRUSION",
             "3RD LOCATION OF INTRUSION",
             "4TH LOCATION OF INTRUSION",
             "5TH LOCATION OF INTRUSION",
             "6TH LOCATION OF INTRUSION",
             "7TH LOCATION OF INTRUSION",
             "8TH LOCATION OF INTRUSION",
             "9TH LOCATION OF INTRUSION",
             "1ST MAGNITUDE OF INTRUSION",
             "10TH MAGNITUDE OF INTRUSION",
             "2ND MAGNITUDE OF INTRUSION",
             "3RD MAGNITUDE OF INTRUSION",
             "4TH MAGNITUDE OF INTRUSION",
             "5TH MAGNITUDE OF INTRUSION",
             "6TH MAGNITUDE OF INTRUSION",
             "7TH MAGNITUDE OF INTRUSION",
             "8TH MAGNITUDE OF INTRUSION",
             "9TH MAGNITUDE OF INTRUSION",
             "TYPE OF VEHICLE INSPECTION",
             "INTERRUPTED ROLLOVER",
             "INTEGRATED RESTRAINTS",
             "OCCUPANT AREA INTRUSION NO.",
             "INJURY SEVERITY SCORE (AIS98 FORMAT)",
             "INJURY SEVERITY SCORE (AIS08 FORMAT)",
             "NUMBER OF LANES",
             "CHILD RESTRAINT LATCH ANCHOR HOOK DESIGN",
             "CHILD RESTRAINT LATCH ANCHOR HOOK USE",
             "LESION (O.I.C. - A.I.S.)",
             "LIGHT CONDITIONS",
             "MAXIMUM KNOWN OCCUPANT AIS (AIS98 FORMAT)",
             "MAXIMUM KNOWN OCCUPANT AIS (AIS08 FORMAT)",
             "VEHICLE MAKE",
             "MAKMOD",
             "MANUAL BELT SYSTEM AVAILABILITY",
             "MANNER OF COLLISION",
             "ATTEMPTED AVOIDANCE MANEUVER",
             "MANUAL BELT FAILURE MODE DURING ACCIDENT",
             "MANUAL BELT SYSTEM USE",
             "MOTORCYCLE ENGINE DISPLACEMENT",
             "TYPE MEDICAL FACILITY INITIAL TREATMENT",
             "MEDIUM STATUS (PRIOR TO IMPACT)",
             "VEHICLE MODEL",
             "VEHICLE MODEL YEAR",
             "MONTH OF ACCIDENT",
             "OTHER VEHICLE NUMBER OR OBJECT CONTACTED",
             "OBJECT CONTACTED (HIGHEST)",
             "OBJECT CONTACTED (2ND HIGHEST)",
             "NUMBER OF OCCUPANT FORMS SUBMITTED",
             "OCCUPANT MOBILITY",
             "OCCUPANT NUMBER",
             "OCCUPANTS RACE",
             "OCCUPANTS ETHNICITY",
             "NUMBER OF OCCUPANTS THIS VEHICLE",
             "ODOMETER READING",
             "LF DOOR, TAILGATE OR HATCH OPENING",
             "LR DOOR, TAILGATE OR HATCH OPENING",
             "RF DOOR, TAILGATE OR HATCH OPENING",
             "RR DOOR, TAILGATE OR HATCH OPENING",
             "TG DOOR, TAILGATE OR HATCH OPENING",
             "ORIGINAL AVERAGE TRACK WIDTH",
             "BODY TYPE OF THE OTHER VEHICLE",
             "WEIGHT OF THE OTHER VEHICLE",
             "POLICE REPORTED RESTRAINT USE",
             "PASSENGER COMPARTMENT INTEGRITY",
             "CLOCK DIRECTION FOR PDOF IN DEGREES (HIGHEST CDC)",
             "CLOCK DIRECTION FOR PDOF IN DEGREES (2ND HIGHEST CDC)",
             "POSGUIDE",
             "SEAT BELT POSITIONING DEVICE PRESENCE",
             "POST CRASH INTEGRITY LOSS",
             "OCCUPANT'S POSTURE",
             "SEAT BELT POSITIONING DEVICE USE",
             "INITIAL CRITICAL (PRECRASH) EVENT",
             "PRE-IMPACT LOCATION",
             "PRE-IMPACT STABILITY",
             "PRE-EVENT MOVEMENT PRIOR REC CRIT EVENT",
             "HAD VEHICLE BEEN IN PREVIOUS ACCIDENTS",
             "ROADWAY PROFILE",
             "PRE ROLLOVER MANEUVER",
             "PRIMARY SAMPLING UNIT NUMBER",
             "PRIMARY SAMPLING UNIT STRATIFICATION",
             "RACE",
             "RATIO INFLATION FACTOR",
             "LOCATION STEERING RIM/SPOKE DEFORMATION",
             "BODY REGION (AIS08 FORMAT)",
             "BODY REGION (AIS98 FORMAT)",
             "RELATION TO JUNCTION",
             "RESTRAINT TYPE",
             "STEERING RIM/SPOKE DEFORMATION",
             "OCCUPANT'S ROLE",
             "DIRECTION OF INITIAL ROLL",
             "LOCATION OF ROLLOVER",
             "ROLLOVER INITIATION TYPE",
             "ESTIMATED DISTANCE OF ROLLOVER",
             "ROLLOVER INITIATION OBJECT CONTACTED",
             "ROLLOVER",
             "ROLLOVER PROTECTION",
             "ROOF",
             "OPTIONAL ROOF 1",
             "OPTIONAL ROOF 2",
             "REAR OVERRIDE/UNDERRIDE THIS VEHICLE",
             "CRUSH PROFILE C1 (2ND HIGHEST)",
             "CRUSH PROFILE C2 (2ND HIGHEST)",
             "CRUSH PROFILE C3 (2ND HIGHEST)",
             "CRUSH PROFILE C4 (2ND HIGHEST)",
             "CRUSH PROFILE C5 (2ND HIGHEST)",
             "CRUSH PROFILE C6 (2ND HIGHEST)",
             "CRUSH PROFILE D (2ND HIGHEST)",
             "CRUSH PROFILE L (2ND HIGHEST)",
             "SEAT PERFORMANCE (THIS POSITION)",
             "OCCUPANT'S SEAT POSITION",
             "SEAT TRACK ADJUSTED POSITION PRIOR TO IMPACT",
             "SEAT TYPE (THIS OCCUPANT POSITION)",
             "VIN SERIES TRUCK",
             "OCCUPANT'S SEX",
             "SPECIFIC LONGITUDINAL LOCATION (HIGHEST)",
             "SPECIFIC LONGITUDINAL LOC. (2ND HIGHEST)",
             "INJURY SOURCE CONFIDENCE LEVEL",
             "SOURCE OF INJURY DATA",
             "OTHER DRUG: SPECIMEN TEST RESULTS",
             "SPEED LIMIT",
             "SEAT BACK INCLINE PRIOR AND POST IMPACT",
             "SEAT ORIENTATION (THIS OCCUPANT POS.)",
             "CASE STRATUM",
             "SPECIFIC ANATOMIC STRUCTURE (AIS08 FORMAT)",
             "TYPE OF ANATOMIC STRUCTURE (AIS08 FORMAT)",
             "SPECIFIC ANATOMIC STRUCTURE (AIS98 FORMAT)",
             "TYPE OF ANATOMIC STRUCTURE (AIS98 FORMAT)",
             "ROADWAY SURFACE CONDITION",
             "ROADWAY SURFACE TYPE",
             "SPECIFIC VERTICAL LOCATION (HIGHEST)",
             "SPECIFIC VERTICAL LOCATION (2ND HIGHEST)",
             "SYSTEM/ORGAN (O.I.C. - A.I.S.)",
             "TYPE OF DAMAGE DISTRIBUTION (HIGHEST)",
             "TYPE OF DAMAGE DISTRIBUTION (2ND HIGHEST)",
             "TIME OF ACCIDENT",
             "TOWED TRAILING UNIT",
             "POLICE REPORTED VEHICLE DISPOSITION",
             "TRAFFIC CONTROL DEVICE",
             "TRAFFICWAY FLOW",
             "TRANSPORT STATUS",
             "POLICE REPORTED TRAVEL SPEED",
             "TRAFFIC CONTROL DEVICE FUNCTIONING",
             "TREATMENT - MORTALITY",
             "LOC. ON VEH. WHERE INIT TRIP FORCE APPL",
             "UNDEFORMED END WIDTH",
             "MAXIMUM KNOWN AIS IN THIS VEHICLE (AIS98 FORMAT)",
             "MAXIMUM KNOWN AIS IN THIS VEHICLE (AIS08 FORMAT)",
             "NUMBER GENERAL VEHICLE FORMS SUBMITTED",
             "VEHICLE NUMBER",
             "VEHICLE NUMBER",
             "TYPE OF VEHICLE",
             "VEHICLE SPECIAL USE",
             "VIN VEHICLE WEIGHT",
             "VERSION NUMBER",
             "VEHICLE IDENTIFICATION NUMBER",
             "VIN MODEL CARS AND TRUCKS",
             "VIN BODY TYPE",
             "NUMBER SERIOUSLY INJURED IN THIS VEHICLE (AIS98 FORMAT)",
             "NUMBER SERIOUSLY INJURED IN THIS VEHICLE (AIS08 FORMAT)",
             "NUMBER INJURED IN THIS VEHICLE (AIS08 FORMAT)",
             "NUMBER INJURED IN THIS VEHICLE (AIS98 FORMAT)",
             "VIN LENGTH",
             "VIN MAKE",
             "VIN MODEL YEAR",
             "MAXIMUM TREATMENT IN THIS VEHICLE",
             "OCCUPANT’S WEIGHT",
             "TRUCK WEIGHT CODE",
             "ORIGINAL WHEELBASE",
             "NUMBER WHEELS/NUMBER OF DRIVE WHEELS",
             "WORKING DAYS LOST",
             "YEAR OF ACCIDENT"
)

dataSetNames <- c("accident.dat", "event.dat", "gv.dat", "makmod.dat", "oa.dat", "oi.dat", "ve.dat", 
                  "vi.dat")
data_set_description <- data.frame(data_set = dataSetNames,
                                   description = c("Accident Records", "Accident Event Records", 
                                                   "General Vehicle", "Make and Model", 
                                                   "Occupent Assessment Records", 
                                                   "Occupent Injury Records", "Vehicle Exterior", 
                                                   "Vehicle Interior"))

vars <- c("AAIS", "AAIS08", "ACCSEQ", "ACCSEQ1", "ACCSEQ2", "ACCSEQDV", "ACCTYPE", "ADAPTEQ", 
          "ADMINSS", "AGE", "AINJSER", "AINJSER8", "AINJURD8", "AINJURED", "AIS", "AIS08", "ALCINV",
          "ALCTEST", "ALIGNMNT", "ALTVEH", "ANGOTHER", "ANGTHIS", "ANTILOCK", "ASPECT90", "ATREAT",
          "BAGAVAIL", "BAGAVOTH", "BAGAVRPT", "BAGCDC", "BAGDAMAG", "BAGDAMSO", "BAGDEPLY", 
          "BAGDEPOT", "BAGEVENT", "BAGFAIL", "BAGFLDAM", "BAGFLOPN", "BAGMAINT", "BAGTYPE", 
          "BAREQSP", "BELTANCH", "BELTSOU", "BICARB", "BLOOD", "BODYREG", "BODYTYPE", "CARBUR", 
          "CARGOWGT", "CASEID", "CASENO", "CAUSE1", "CAUSE2", "CAUSE3", "CDRIR1", "CDRIR10", 
          "CDRIR2", "CDRIR3", "CDRIR4", "CDRIR5", "CDRIR6", "CDRIR7", "CDRIR8", "CDRIR9", "CHHARNES",
          "CHMAKE", "CHORIENT", "CHOWUSED", "CHSHIELD", "CHTETHER", "CHTYPE", "CHUSED", "CLASS1", 
          "CLASS2", "CLIMATE", "COLMTELE", "COLMTILT", "COLUMTYP", "CONDTREE", "CURBWGT", "DAYRUNLT",
          "DAYWEEK", "DEATH", "DIRDAMW", "DIRINJ", "DOCCDC", "DOCTRAJ", "DOF1", "DOF2", "DRGINV", 
          "DRINKING", "DRIVDIST", "DRPRES", "DRUGS", "DRZIP", "DVBAG", "DVBASIS", "DVC1", "DVC2", 
          "DVC3", "DVC4", "DVC5", "DVC6", "DVCONFID", "DVD", "DVEST", "DVL", "DVLAT", "DVLONG", 
          "DVTOTAL", "EJCTAREA", "EJCTMED", "EJECTION", "ENERGY", "ENTRAP", "ETHNICIT", "EVENTS", 
          "EXTENT1", "EXTENT2", "EYEWEAR", "FAILLF", "FAILLR", "FAILRF", "FAILRR", "FAILTG", 
          "FETALDOA", "FIRE", "FIREORIG", "FOURWHDR", "FOVERIDE", "FRTWHLDR", "FUELCAP1", "FUELCAP2",
          "FUELCODE", "FUELDAM1", "FUELDAM2", "FUELEAK1", "FUELEAK2", "FUELGT2", "FUELLOC1",
          "FUELLOC2", "FUELPRE1", "FUELPRE2", "FUELTNK1", "FUELTNK2", "FUELTYP1", "FUELTYP2", "GAD1",
          "GAD2", "GADEV1", "GADEV2", "GLASGOW", "GLIMPBL", "GLIMPLF", "GLIMPLR", "GLIMPOTH", 
          "GLIMPRF", "GLIMPRR", "GLIMPRUF", "GLIMPWS", "GLOCCBL", "GLOCCLF", "GLOCCLR", "GLOCCOTH", 
          "GLOCCRF", "GLOCCRR", "GLOCCRUF", "GLOCCWS", "GLPREBL", "GLPRELF", "GLPRELR", "GLPREOTH", 
          "GLPRERF", "GLPRERR", "GLPRERUF", "GLPREWS", "GLTYPBL", "GLTYPLF", "GLTYPLR", "GLTYPOTH", 
          "GLTYPRF", "GLTYPRR", "GLTYPRUF", "GLTYPWS", "HEADREST", "HEIGHT", "HOSPSTAY", "IMPACTSP",
          "INCOMP1", "INCOMP10", "INCOMP2", "INCOMP3", "INCOMP4", "INCOMP5", "INCOMP6", "INCOMP7", 
          "INCOMP8", "INCOMP9", "INJLEVEL", "INJLVL08", "INJNO", "INJNUM", "INJNUM08", "INJSEV", 
          "INJSOU", "INLOC1", "INLOC10", "INLOC2", "INLOC3", "INLOC4", "INLOC5", "INLOC6", "INLOC7",
          "INLOC8", "INLOC9", "INMAG1", "INMAG10", "INMAG2", "INMAG3", "INMAG4", "INMAG5", "INMAG6",
          "INMAG7", "INMAG8", "INMAG9", "INSPTYPE", "INTEROLL", "INTGREST", "INTRUNO", "ISS", 
          "ISS08", "LANES", "LATCHDES", "LATCHUSE", "LESION", "LGTCOND", "MAIS", "MAIS08", "MAKE",
          "MAKMOD", "MANAVAIL", "MANCOLL", "MANEUVER", "MANFAIL", "MANUSE", "MCYCLDS", "MEDFACIL",
          "MEDSTA", "MODEL", "MODELYR", "MONTH", "OBJCONT", "OBJCONT1", "OBJCONT2", "OCCFORMS", 
          "OCCMOBIL", "OCCNO", "OCCRACE", "OCETHNIC", "OCUPANTS", "ODOMETER", "OPENLF", "OPENLR", 
          "OPENRF", "OPENRR", "OPENTG", "ORIGAVTW", "otbdytyp", "otvehwgt", "PARUSE", "PASINTEG", 
          "PDOF1", "PDOF2", "POSGUIDE", "POSPRES", "POSTINT", "POSTURE", "POSUSE", "PREEVENT", 
          "PREILOC", "PREISTAB", "PREMOVE", "PREVACC", "PROFILE", "PROLLMAN", "PSU", "PSUSTRAT",
          "RACE", "RATWGT", "RDEFLOC", "REGION08", "REGION90", "RELINTER", "RESTYPE", "RIMDEF", 
          "ROLE", "ROLINDIR", "ROLINLOC", "ROLINTYP", "ROLLDIST", "ROLLOBJ", "ROLLOVER", "ROLLPROT",
          "ROOF1", "ROOF2", "ROOF3", "ROVERIDE", "SDVC1", "SDVC2", "SDVC3", "SDVC4", "SDVC5",
          "SDVC6", "SDVD", "SDVL", "SEATPERF", "SEATPOS", "SEATRACK", "SEATTYPE", "SERTR", "SEX",
          "SHL1", "SHL2", "SOUCON", "SOUDAT", "SPECOTH", "SPLIMIT", "STBACINC", "STORIENT", 
          "STRATIF", "STRSPC08", "STRTYP08", "STRUSPEC", "STRUTYPE", "SURCOND", "SURTYPE", "SVL1",
          "SVL2", "SYSORG", "TDD1", "TDD2", "TIME", "TOWHITCH", "TOWPAR", "TRAFCONT", "TRAFFLOW", 
          "TRANSTAT", "TRAVELSP", "TRCTLFCT", "TREATMNT", "TRIPLOC", "UNDENDW", "VAIS", "VAIS08", 
          "VEHFORMS", "VEHNO", "VEHNUM", "VEHTYPE", "VEHUSE", "VEHWGT", "VERSION", "VIN", "VINAMOD", 
          "VINBT", "VINJSER", "VINJSER8", "VINJURD8", "VINJURED", "VINLNGTH", "VINMAKE", "VINMODYR", 
          "VTREAT", "WEIGHT", "WGTCDTR", "WHEELBAS", "WHLDRWHL", "WORKDAYS", "YEAR")

lookuptable <- data.frame(variable = vars, description = describ)
row.names(lookuptable) <- vars

lookup <- function(data) {
  lookuptable[names(data),]
}

#############################################################################################

### Read in Data -------------------------------------------------------------------------------
################################################################################################
## @knitr read_data

setwd("~/Documents/School/Fall_16/Accidents/")

yrs <- 2009:2015
yrs_cutoff <- yrs - 10

file.names <- c("accident", "event", "gv", "oa", "oi", "ve", "vi")
file.type <- str_c(file.names, ".sas7bdat")

data <- list()

for(i in 1:length(yrs)){
  new.list <- lapply(file.type, function(x){ 
    read_sas(data_file = str_c("Data/NASS/", yrs[i],"/Formatted Data/", x))
  })
  names(new.list) <- file.names
  
  data[[str_c("year_",yrs[i])]] <- new.list
}

################################################################################################
# 
# temp1 <- read_sas(data_file = str_c("Data/NASS/20", "09", "/Formatted Data/oa.sas7bdat"))
# temp2 <- read_sas(data_file = str_c("Data/NASS/20", "09", "/Formatted Data/gv.sas7bdat"))
# 
# temp2<- mutate(temp2, ID = str_c(CASENO, "-", PSU),
#                VID = str_c(VEHNO, "-", ID))
# temp1<- mutate(temp1, ID = str_c(CASENO, "-", PSU),
#                VID = str_c(VEHNO, "-", ID),
#                OID = str_c(OCCNO, "-", VEHNO, "-", ID))
# temp3 <- merge(temp1, select(temp2, MODELYR, OCCFORMS, VID, ID), all = TRUE)
# xtabs(~ temp3$MODELYR + temp3$ISS)
# num.na(temp1$ISS)
# 
# xtabs(~ (temp3$MODELYR > yr_cutoff) + is.na(temp3$MAIS08))

################################################################################################

### Define Identification variable -----------------------------------------------------------
##############################################################################################

data <- lapply(data, function(x){
  x$accident <- mutate(x$accident, ID = str_c(CASENO, "-", PSU),
                       YID = str_c(YEAR, "-", CASENO, "-", PSU))
  x$event <- mutate(x$event, ID = str_c(CASENO, "-", PSU))
  x$gv <- mutate(x$gv, ID = str_c(CASENO, "-", PSU),
                 VID = str_c(VEHNO, "-", ID))
  x$oa <- mutate(x$oa, ID = str_c(CASENO, "-", PSU),
                 VID = str_c(VEHNO, "-", ID),
                 OID = str_c(OCCNO, "-", VEHNO, "-", ID))
  x$oi <- mutate(x$oi, ID = str_c(CASENO, "-", PSU),
                 VID = str_c(VEHNO, "-", ID),
                 OID = str_c(OCCNO, "-", VEHNO, "-", ID))
  x$ve <- mutate(x$ve, ID = str_c(CASENO, "-", PSU),
                 VID = str_c(VEHNO, "-", ID))
  x$vi <- mutate(x$vi, ID = str_c(CASENO, "-", PSU),
                 VID = str_c(VEHNO, "-", ID))
  x
})

#############################################################################################

### Join the Data ------------------------------------------------------------------------------
################################################################################################

# By Year
data.by.year <- lapply(data, function(x){
  right_join(x$accident, x$event) %>% left_join(x$gv) %>% left_join(x$oa) %>% left_join(x$ve) %>% 
    left_join(x$vi)
})

# Join years together
entire.data <- Reduce(function(...) full_join(...), data.by.year)

# Interesting observation
table(entire.data$YEAR)

#############################################################################################

### Variables of interest ------------------------------------------------------------------
############################################################################################

## Note CLASS2 == 0 should indicate a nonvehicle (or VEHTYPE)
## You'd expect that in the case of two car collision two events would be created. That is
## only true about half the time. Other times only one event is created.

### Variables of interest
# GV: CURBWGT, otvehwgt (for checking), VEHWGT, VINLNGTH, WGTCDTR
# GV: DVCONFID, DVEST, DVTOTAL, DVLONG, DVLAT, ENERGY
# GV: BODYTYPE, otbdytyp, MAKE, VINMAKE, MODEL, VINAMOD, MODELYR, VINMODYR, VINBT
# OTVEHWGT, WGTCDTR, VEHWGT, CARGOWGT, CURBWGT, WEIGHT
# DVBASIS, DVEST, DVLAT, DVLONG, DVTOTAL, ENERGY, TRAVELSP, IMPACTSP
# ANGOTHER, ANGTHIS, DOF
# AAIS08, AINJSER8, AINJURED

# OA: AGE, ROLE

# OI: AIS ## DIRINJ, INJLVL08

# Helpful variables to condition on:
## ACCIDENT: MANCOLL, EVENTS, VEHFORMS
## EVENT: ACCSEQ, CLASS2, OBJCONT, VEHNUM, "MAXACCSEQ", "MAXVEHNUM
## GV: DRPRES, OCCFORMS, OCUPANTS, VEHNO, VEHTYPE, VEHUSE
## VE: DOF1/2, PDOF1/2
## VI:--
## OA: INJNUM08
## OI: AIS, INJLEVEL, INJNO

############################################################################################

### Filter Cases ----------------------------------------------------------------------------
#############################################################################################

### Compute maximum num of vehicle involved and maximum number of ACCSEQ
event.by.id <- group_by(entire.data, YID)
max.veh <- summarise(event.by.id,
                     MAXVEHNUM = max(VEHNUM))
max.accseq <- summarise(event.by.id,
                        MAXACCSEQ = max(ACCSEQ))
obj.cont.grt.2 <- summarise(event.by.id,
                            obj.cont.grt.2 = (max(VEHNO) > 2 | max(OBJCONT) > 2))

entire.data <- full_join(entire.data, max.veh, by = "YID")
entire.data <- full_join(entire.data, max.accseq, by = "YID")
entire.data <- full_join(entire.data, obj.cont.grt.2, by = "YID")

### Compute number of drivers oa's
oa.driver <- filter(entire.data, ROLE == 1)
oa.by.id <- group_by(oa.driver, YID)
max.driver.oa <- summarise(oa.by.id,
                           MAXDOA = n())
entire.data <- full_join(entire.data, max.driver.oa, by = "YID")

### Modify OID to Include the Year
entire.data <- mutate(entire.data, YOID = str_c(YEAR, "-", OID),
                      YVID = str_c(YEAR, "-", VID),
                      NEWCAR = (YEAR - MODELYR) < 10)

### Select events where CLASS2 == 0 (event that involves a non-vehicle object)
non.veh.cases <- filter(entire.data, CLASS2 == 0)$YID

### Select events where no more than two vehicles are involved
only.two.veh.cases <- filter(entire.data, !obj.cont.grt.2)$YID

# MANCOLL == 2  ## MANCOLL != 0 for two vehicle accidents
# Filter 'data set' by CASENO and PSU

### Select "Head On" or "REAR-END" Collision
# MANCOLL == 2  ## MANCOLL != 0 for two vehicle accidents
# Filter 'data set' by CASENO and PSU
mancoll.cases <- filter(entire.data, MANCOLL == 2 | MANCOLL == 1)$YID

cases <- setdiff(intersect(mancoll.cases, only.two.veh.cases), non.veh.cases)

### Select simple cases
multi.event.cases <- filter(entire.data, MAXACCSEQ > 1)$YID

cases <- setdiff(cases, multi.event.cases)

# Exclude cases where we don't know if driver is present or if they're missing (e.g. parked car)
unknow.drpres <- filter(entire.data, is.na(DRPRES) | DRPRES == 0)$YID

cases <- setdiff(cases, unknow.drpres)

# Exclude cases if we don't have both drivers oa's.
# both.driver.assessments <- filter(entire.data, MAXDOA == 2)$YID
# cases <- intersect(cases, both.driver.assessments)

# Pick out relevent cases from 'data set'
data.of.interest <- filter(entire.data, YID %in% cases)

# Keep only driver occupents
data.of.interest <- filter(data.of.interest, ROLE == 1)

# Keep only one record per driver
# duplicate <- c(FALSE, data.of.interest[1:(dim(data.of.interest)[1]-1),"YOID"] == 
#                  data.of.interest[2:dim(data.of.interest)[1],"YOID"])

### Fill in missing otvehwgt and otbdytyp columns ---------------------------------------------------
### Note this will not generalize to accidents with more (or less) than two vehicles
gv.by.id <- group_by(entire.data, YID)
max.vehno <- summarise(gv.by.id,
                       MAXVEHNO = max(VEHNO))

entire.data <- full_join(entire.data, max.vehno, by = "YID")

lkup <- select(entire.data, YVID, CURBWGT, BODYTYPE, MAXVEHNO)

lkup <- mutate(lkup, oYVID = str_c(str_sub(YVID, end = 5), as.numeric(str_sub(YVID, 6,6)) %% 2 + 1, 
               str_sub(YVID, start = 7)),
               YVID = oYVID,
               otvehwgt2 = CURBWGT,
               otbdytyp2 = BODYTYPE) %>% 
        filter(MAXVEHNO == 2) %>% 
        select(-oYVID, -CURBWGT, -BODYTYPE) %>%
        distinct()

# Re-filter the data so it includes MAXVEHNO variable
# Pick out relevent cases from 'data set'
# data.of.interest <- filter(entire.data, YID %in% cases)

# Keep only driver occupents
# data.of.interest <- filter(data.of.interest, ROLE == 1)

# df <- data.of.interest
data.of.interest <- left_join(data.of.interest, lkup, by = c("YVID"))


### Subset the data ---------------------------------------------------------------------------------

df <- select(data.of.interest, ID, CASEID, CASENO, PSU, RATWGT, STRATIF, VID, OID, YID, YVID, YOID, 
             BLOOD, DEATH, HEIGHT, HOSPSTAY, INSPTYPE, VEHNO, MAXVEHNO,
             INJNUM08, ISS08, INJSEV, MAIS08, OCCMOBIL, PREVACC, TREATMNT, WEIGHT, WORKDAYS, AGE, 
             MANCOLL, DAYWEEK, BICARB, CAUSE1, TOWPAR, OCCFORMS, NEWCAR,
             EVENTS, MONTH, PSUSTRAT, TIME, YEAR, ACCSEQ, CLASS2, CLASS1, GADEV1, GADEV2, OBJCONT, 
             VEHNUM, MAXVEHNUM, MAXACCSEQ, ACCTYPE, ANGTHIS, ANGOTHER, DOF1, DOF2, PDOF1, PDOF2, 
             CLIMATE, BODYTYPE, CARGOWGT, ABELTUSE, MANUSE, BAREQSP,
             CURBWGT, WHEELBAS, DVEST, DOCTRAJ, DRPRES, DVBASIS, DVCONFID, DVLAT, DVLONG, DVTOTAL, ENERGY, 
             ETHNICIT, IMPACTSP, LGTCOND, MAKE, MODEL, MODELYR, OCCFORMS, OCUPANTS, SURCOND, SURTYPE, 
             SEX, RACE, EYEWEAR, SPLIMIT, TRAVELSP, VEHNO, VINLNGTH, VAIS08, VINMODYR, VEHWGT, VINMAKE,
             VINAMOD, VINBT, VEHTYPE, WGTCDTR, otvehwgt2, otbdytyp2, VINJSER8, VINJURD8, VTREAT)

# Data Check
# entire.data %>% filter(MAXVEHNO == 2) %>% left_join(lkup, by = c("YVID")) %>% 
# distinct(YVID, .keep_all = TRUE) %>% select(CURBWGT, BODYTYPE, otvehwgt, otvehwgt2, otbdytyp,
# otbdytyp2) %>% num.na

num.na(df)
num.na(df)/dim(df)[1]

# Remove old cars from the data set
df.new <- filter(df, NEWCAR)


### Response Variable -------------------------------------------------------------------------------

dim(df.new)
num.na(df.new$TREATMNT)

df.new <- mutate(df.new,
                 response = ifelse(is.na(TREATMNT), "mild_injury", 
                                   ifelse(TREATMNT == 1, "serious_injury", 
                                          ifelse(TREATMNT %in% c(0, 4, 5, 6, 8), "mild_injury", 
                                                 ifelse(is.na(HOSPSTAY), TREATMNT,
                                                        ifelse(HOSPSTAY == 1, "mild_injury", 
                                                               "serious_injury"))))),
                 response = ifelse(response != "3", response,
                                   ifelse(INJSEV %in% c(3, 4), "serious_injury",
                                          response))
                 )
df.new <- filter(df.new, response != "3")
df.new <- mutate(df.new,
                 response = factor(response))


dim(filter(df, NEWCAR, is.na(TREATMNT), !is.na(DEATH)))[1]
dim(filter(df, NEWCAR, is.na(TREATMNT), !is.na(OCCMOBIL)))[1]
dim(filter(df, NEWCAR, is.na(TREATMNT), !is.na(HOSPSTAY)))[1]
dim(filter(df, NEWCAR, is.na(TREATMNT), is.na(INJSEV)))[1]
table(filter(df, NEWCAR, is.na(TREATMNT), !is.na(MAIS08))$MAIS08)
dim(filter(df, NEWCAR, is.na(TREATMNT), !is.na(CAUSE1)))[1]
table(filter(df, NEWCAR, is.na(TREATMNT), !is.na(CAUSE1))$CAUSE1)
dim(filter(df, NEWCAR, is.na(TREATMNT), !is.na(WORKDAYS)))[1]
dim(filter(df, NEWCAR, is.na(TREATMNT), !is.na(BICARB)))[1]

#############################################################################################

### Build dataset -----------------------------------------------------------------------------------

dat <- df.new

dat <- dat %>% select(response, ID, CASEID, CASENO, PSU, RATWGT, STRATIF, VID, OID, YID, YVID, YOID, 
                      MONTH, PSUSTRAT, TIME, YEAR, DAYWEEK, MANCOLL, ACCTYPE,
                      CURBWGT, otvehwgt2, VEHWGT, WHEELBAS, OCUPANTS,
                      DVEST, DVCONFID, DVLAT, DVLONG, DVTOTAL, DVBASIS, ENERGY, IMPACTSP, TRAVELSP, 
                      SPLIMIT,
                      AGE, SEX, WEIGHT, HEIGHT,
                      ANGTHIS, ANGOTHER,
                      MAKE, VINMAKE, MODEL, VINAMOD, MODELYR, VINMODYR, BODYTYPE, VINBT, 
                      VEHTYPE, CLASS1, CLASS2, otbdytyp2, INSPTYPE, PREVACC, VEHNO,
                      SURCOND, SURTYPE, LGTCOND, CLIMATE,
                      GADEV1, GADEV2,
                      ABELTUSE, MANUSE)

dat <- select(dat, -VEHNO, -VID, -OID, -YID, -ID)

### Fill in NA where approperiate ------------------------------------------------------------------

dat <- mutate(dat,
              MAKE = ifelse(MAKE == 98, NA, MAKE),
              VINMODYR = ifelse(VINMODYR == 9999, NA, VINMODYR),
              VEHWGT = ifelse(VEHWGT == 9999, NA, VEHWGT),
              VINBT = ifelse(VINBT == "" | VINBT == "99", NA, VINBT),
              VINMAKE = ifelse(VINMAKE == 9999 | VINMAKE == "", NA, VINMAKE),
              VEHTYPE = ifelse(VEHTYPE == "9" | VEHTYPE == "U", NA, VEHTYPE),
              GADEV1 = ifelse(GADEV1 == "9", NA, GADEV1),
              GADEV2 = ifelse(GADEV2 == "9", NA, GADEV2)
              )

# Remove data point where VINMODYR == 1982 and MODELYR == 2012, because of the discrepancy
dat <- filter(dat, !(VINMODYR == 1982 & MODELYR == 2012))

# Remove IMPACTSP - Not informative
dat <- select(dat, -IMPACTSP)

# Check missingness in VINMAKE and compare to MAKE
mk_of_interest <- dat %>% filter(is.na(VINMAKE)) %>% select(MAKE) %>% unique %>% unlist
dat %>% filter(MAKE %in% mk_of_interest) %>% with(table(MAKE, VINMAKE))
temp_lookup <- (dat %>% filter(MAKE %in% mk_of_interest) %>% with(table(MAKE, VINMAKE)) > 2) %>% apply(MARGIN = 1, function(x){ names(x)[x] })

dat <- mutate(dat,
              VINMODYR = ifelse(is.na(VINMODYR), MODELYR, VINMODYR),
              VINMAKE = ifelse(!is.na(VINMAKE), VINMAKE,
                               ifelse(MAKE == 7, "DODG",
                               ifelse(MAKE == 12, "FORD",
                               ifelse(MAKE == 13, "LINC",
                               ifelse(MAKE == 20, "CHEV",
                               ifelse(MAKE == 23, "GMC",
                               ifelse(MAKE == 30, "VOLK",
                               ifelse(MAKE == 32, "AUDI",
                               ifelse(MAKE == 34, "BMW",
                               ifelse(MAKE == 35, "NISS",
                               ifelse(MAKE == 37, "HOND",
                               ifelse(MAKE == 41, "MAZD",
                               ifelse(MAKE == 42, "MERZ",
                               ifelse(MAKE == 49, "TOYT",
                               ifelse(MAKE == 52, "MITS",
                               ifelse(MAKE == 54, "ACUR",
                               ifelse(MAKE == 55, "HYUN",
                               ifelse(MAKE == 58, "INFI",
                               ifelse(MAKE == 69, "MNNI",
                                      VINMAKE)))))))))))))))))))
)

# Construct SEATBELT variable
dat <- mutate(dat,
              SEATBELT = ifelse(is.na(ABELTUSE) & is.na(MANUSE), NA,
                                ifelse(MANUSE %in% c(2:5, 8, 12:15, 18) | ABELTUSE == 1, "in-use",
                                       ifelse(MANUSE %in% c(0, 1) | ABELTUSE %in% c(0, 2), "not-used", NA)))
)
dat <- select(dat, -ABELTUSE, -MANUSE)

dat <- mutate(dat,
              response = factor(response),
              MANCOLL = factor(MANCOLL),
              ACCTYPE = factor(ACCTYPE),
              SEX = factor(SEX),
              MAKE = factor(MAKE),
              VINMAKE = factor(VINMAKE),
              MODEL = factor(MODEL),
              VINAMOD = factor(VINAMOD),
              BODYTYPE = factor(BODYTYPE),
              VINBT = factor(VINBT),
              VEHTYPE = factor(VEHTYPE),
              CLASS1 = factor(CLASS1),
              CLASS2 = factor(CLASS2),
              otbdytyp2 = factor(otbdytyp2),
              INSPTYPE = factor(INSPTYPE),
              PREVACC = factor(PREVACC),
              SURCOND = factor(SURCOND),
              SURTYPE = factor(SURTYPE),
              LGTCOND = factor(LGTCOND),
              CLIMATE = factor(CLIMATE),
              GADEV1 = factor(GADEV1),
              GADEV2 = factor(GADEV2),
              SEATBELT = factor(SEATBELT),
              MONTH = factor(MONTH),
              DAYWEEK = factor(DAYWEEK))

### Impute missingness -------------------------------------------------------------------------
## @knitr impute

library(rpart)
library(caret)
library(missForest)
library(mice)
library(VIM)

temp <- select(dat, -response, -CASEID, -CASENO, -PSU, -RATWGT, -STRATIF, 
               -YOID, -MONTH, -PSUSTRAT, -YEAR, -DAYWEEK, -MANCOLL, -ACCTYPE, -OCUPANTS, -DVCONFID, 
               -DVBASIS, -MAKE, -VINMAKE, -MODEL, -VINAMOD, -MODELYR, -VINMODYR, -BODYTYPE, -CLASS1, 
               -CLASS2, -otbdytyp2, -INSPTYPE)

md.pattern(temp)

mice_plot <- aggr(temp, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(temp), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

### Impute SEX ---------------------------------------------------------------------------------

dat <- mutate(dat,
              DVEST1 = ifelse(DVEST %in% c(1:5), DVEST, NA),
              DVEST2 = ifelse(DVEST %in% c(6:8), DVEST, NA)
)

datImp <- dat

# Subset dataframe

temp <- select(entire.data, SEX, MAKE, VINMAKE, MODEL, VINAMOD, MODELYR, VINMODYR, BODYTYPE, VINBT,
               CURBWGT, VEHWGT, WHEELBAS, WEIGHT, HEIGHT, AGE, 
               DVBASIS, DVCONFID, DVEST, DVTOTAL, DVLAT, DVLONG, ENERGY, TRAVELSP, SPLIMIT, ACCTYPE, SURCOND,
               SURTYPE, LGTCOND, CLIMATE, MANCOLL,
               MONTH, TIME, YEAR,
               ABELTUSE, MANUSE,
               ROLE, YVID) %>% filter(ROLE == 1) %>% select(-ROLE)
emptyRow <- apply(temp, MARGIN = 1, function(x){ all(is.na(x)) })
temp <- temp[!emptyRow, ]

temp <- mutate(temp,
               MAKE = ifelse(MAKE == 98, NA, MAKE),
               VINMODYR = ifelse(VINMODYR == 9999, NA, VINMODYR),
               VINBT = ifelse(VINBT == "" | VINBT == "99", NA, VINBT),
               VINMAKE = ifelse(VINMAKE == 9999 | VINMAKE == "", NA, VINMAKE),
               DVEST1 = ifelse(DVEST %in% c(1:5), DVEST, NA),
               DVEST2 = ifelse(DVEST %in% c(6:8), DVEST, NA)
               )

temp <- mutate(temp,
               SEX = factor(SEX),
               MAKE = factor(MAKE),
               VINMAKE = factor(VINMAKE),
               MODEL = factor(MODEL),
               VINAMOD = factor(VINAMOD),
               BODYTYPE = factor(BODYTYPE),
               DVEST = factor(DVEST),
               DVBASIS = factor(DVBASIS),
               DVCONFID = factor(DVCONFID),
               SURCOND = factor(SURCOND),
               SURTYPE = factor(SURTYPE),
               LGTCOND = factor(LGTCOND),
               ACCTYPE = factor(ACCTYPE),
               MANCOLL = factor(MANCOLL),
               CLIMATE = factor(CLIMATE),
               MONTH = factor(MONTH),
               SEATBELT = ifelse(is.na(ABELTUSE) & is.na(MANUSE), NA,
                                 ifelse(MANUSE %in% c(2:5, 8, 12:15, 18) | ABELTUSE == 1, "in-use",
                                        ifelse(MANUSE %in% c(0, 1) | ABELTUSE %in% c(0, 2), "not-used", NA)))
               )

# Using missForest
# convert  to data.frame to avoid errors in missForest
# missForest(as.data.frame(temp))

# Using caret
temp1 <- filter(temp, !is.na(SEX))

set.seed(42)
sexImputeModel <- train(
  SEX ~ MAKE + MODEL + VINMODYR + BODYTYPE + CURBWGT + WEIGHT + HEIGHT,
  data = temp1, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  na.action = na.rpart
)

datImp$SEX[is.na(datImp$SEX)] <- predict(sexImputeModel, newdata = dat[is.na(dat$SEX), ], 
                                         na.action = na.rpart)

# Rpart model
# gender.model <- rpart(factor(SEX) ~ factor(MAKE) + factor(MODEL) + factor(MODELYR) + factor(BODYTYPE) + WEIGHT 
#                       + HEIGHT + AGE, 
#                        data=filter(entire.data, !is.na(SEX), ROLE == 1) , method="class")
# gender.pred <- predict(gender.model, data.of.interest)
# sqrt(mean((gender.pred - data.of.interest$SEX)^2, na.rm = TRUE)) # training error
# 
# temp1 <- filter(entire.data, !is.na(SEX))
# temp2 <- entire.data %>% filter(MAKE %in% temp1$MAKE, VINMAKE %in% temp1$VINMAKE, MODEL %in% 
#                                   temp1$MODEL, VINAMOD %in% temp1$VINAMOD, MODELYR %in% temp1$VINMODYR,
#                                 BODYTYPE %in% temp1$BODYTYPE, VINBT %in% temp1$VINBT)
# gender.pred2 <- predict(gender.model, temp2)
# sqrt(mean((gender.pred2 - temp2$SEX)^2, na.rm = TRUE)) # training error


### Impute AGE -------------------------------------------------------------------------------------
temp2 <- filter(temp, !is.na(AGE))

set.seed(42)
ageImputeModel <- train(
  AGE ~ MAKE + MODEL + VINMODYR + BODYTYPE + CURBWGT + SEX,
  data = temp2, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  na.action = na.rpart
)

datImp$AGE[is.na(datImp$AGE)] <- predict(ageImputeModel, newdata = dat[is.na(dat$AGE), ], 
                                         na.action = na.rpart)

### Impute HEIGHT ----------------------------------------------------------------------------------

temp3 <- filter(temp, !is.na(HEIGHT))

set.seed(42)
heightImputeModel <- train(
  HEIGHT ~ MAKE + MODEL + VINMODYR + BODYTYPE + CURBWGT + SEX,
  data = temp3, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  na.action = na.rpart
)

datImp$HEIGHT[is.na(datImp$HEIGHT)] <- predict(heightImputeModel, newdata = dat[is.na(dat$HEIGHT), ], 
                                         na.action = na.rpart)

### Impute WEIGHT ----------------------------------------------------------------------------------

temp4 <- filter(temp, !is.na(WEIGHT))

set.seed(42)
weightImputeModel <- train(
  WEIGHT ~ MAKE + MODEL + VINMODYR + BODYTYPE + CURBWGT + SEX + WHEELBAS,
  data = temp4, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  na.action = na.rpart
)

datImp$WEIGHT[is.na(datImp$WEIGHT)] <- predict(weightImputeModel, newdata = dat[is.na(dat$WEIGHT), ], 
                                         na.action = na.rpart)

### Impute DVEST1, DVEST2, DVTOTAL -----------------------------------------------------------------

temp5 <- filter(temp, !is.na(DVEST1))
temp6 <- filter(temp, !is.na(DVEST2))
temp7 <- filter(temp, !is.na(DVTOTAL))

set.seed(42)
dvest1ImputeModel <- train(
  ordered(DVEST1) ~ MAKE + MODEL + VINMODYR + BODYTYPE + CURBWGT + SEX + AGE + DVTOTAL + DVEST2 + TRAVELSP + SPLIMIT 
  + ENERGY + ACCTYPE + SURCOND + SURTYPE + LGTCOND + CLIMATE + MONTH + TIME + YEAR,
  data = temp5, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  na.action = na.rpart
)

datImp$DVEST1[is.na(datImp$DVEST1)] <- predict(dvest1ImputeModel, newdata = dat[is.na(dat$DVEST1), ], 
                                         na.action = na.rpart)

set.seed(42)
dvest2ImputeModel <- train(
  ordered(DVEST2) ~ MAKE + MODEL + VINMODYR + BODYTYPE + CURBWGT + SEX + AGE + DVTOTAL + DVEST1 + TRAVELSP + SPLIMIT 
  + ENERGY + ACCTYPE + SURCOND + SURTYPE + LGTCOND + CLIMATE + MONTH + TIME + YEAR,
  data = temp6, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  na.action = na.rpart
)

datImp$DVEST2[is.na(datImp$DVEST2)] <- predict(dvest2ImputeModel, newdata = dat[is.na(dat$DVEST2), ], 
                                         na.action = na.rpart)

set.seed(42)
dvtotalImputeModel <- train(
  DVTOTAL ~ MAKE + MODEL + VINMODYR + BODYTYPE + CURBWGT + SEX + AGE + DVEST1 + DVEST2 + TRAVELSP + SPLIMIT 
  + ENERGY + ACCTYPE + SURCOND + SURTYPE + LGTCOND + CLIMATE + MONTH + TIME + YEAR,
  data = temp7, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  na.action = na.rpart
)

datImp$DVTOTAL[is.na(datImp$DVTOTAL)] <- predict(dvtotalImputeModel, newdata = dat[is.na(dat$DVTOTAL), ], 
                                         na.action = na.rpart)

dvpred <- data.frame(dvest1pred = predict(dvest1ImputeModel, newdata = temp, na.action = na.rpart),
                     dvest2pred = predict(dvest2ImputeModel, newdata = temp, na.action = na.rpart),
                     dvtotalpred = predict(dvtotalImputeModel, newdata = temp, na.action = na.rpart))

cor(apply(dvpred, 2, as.numeric))
summary(transmute(temp, factor(DVEST1), factor(DVEST2), DVTOTAL))
summary(select(temp, DVEST1, DVEST2, DVTOTAL))

dvest1ImputeModel
dvest2ImputeModel
dvtotalImputeModel

# categorize dvtotal
dvtotalf <- factor(
 ifelse(dvpred$dvtotalpred < 10, 1, 
        ifelse(dvpred$dvtotalpred < 25, 2, 
               ifelse(dvpred$dvtotalpred < 40, 3,
                      ifelse(dvpred$dvtotalpred < 55, 4, 5)))))

dat %>% filter(!is.na(DVEST1)) %>% predict(dvtotalImputeModel, newdata = ., na.action = na.rpart)
dat %>% filter(!is.na(DVEST1)) %>% select(DVEST1) %>% t

### Impute CURBWGT ----------------------------------------------------------------------------

temp8 <- filter(temp, !is.na(CURBWGT))

set.seed(42)
curbwgtImputeModel <- train(
  CURBWGT ~ MAKE + MODEL + VINMODYR + BODYTYPE + VEHWGT,
  data = temp8, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  na.action = na.rpart
)

datImp$CURBWGT[is.na(datImp$CURBWGT)] <- predict(curbwgtImputeModel, newdata = dat[is.na(dat$CURBWGT), ], 
                                         na.action = na.rpart)

temp10 <- mutate(entire.data, 
                 MAKE = factor(MAKE),
                 MODEL = factor(MODEL),
                 BODYTYPE = factor(BODYTYPE)) %>% 
  filter(!(MAKE %in% c(50, 71:73, 76, 78, 79, 84:87, 98)),
         !(MODEL %in% c(705, 706, 709, 799, 880:884, 890, 898, 899, 981, 982, 988, 989)))
temp10$CURBWGTpred <- predict(curbwgtImputeModel, newdata = temp10, na.action = na.rpart)


### Fill-in otvehwgt2 --------------------------------------------------------------------------

lkup2 <- select(temp10, YVID, CURBWGTpred)

lkup2 <- mutate(lkup2, oYVID = str_c(str_sub(YVID, end = 5), as.numeric(str_sub(YVID, 6,6)) %% 2 + 1, 
                                   str_sub(YVID, start = 7)),
               YVID = oYVID,
               otvehwgt2pred = CURBWGTpred) %>%
  select(-oYVID, -CURBWGTpred) %>%
  distinct()

datImp[is.na(datImp$otvehwgt2), 'otvehwgt2'] <- datImp[is.na(datImp$otvehwgt2), c('YVID')] %>% 
  left_join(lkup2, by = c('YVID')) %>% select(otvehwgt2pred)


# curbwgt.model <- rpart(CURBWGT ~ factor(MAKE) + VINMAKE + factor(MODEL) + VINAMOD + factor(MODELYR) + 
#                          factor(VINMODYR) + factor(BODYTYPE) + VINBT + VEHWGT, 
#                        data=filter(entire.data, !is.na(CURBWGT)) , method="anova")
# curbwgt.pred <- predict(curbwgt.model, df.new)
# sqrt(mean((curbwgt.pred - df.new$CURBWGT)^2, na.rm = TRUE)) # training error
# 
# temp1 <- filter(entire.data, !is.na(CURBWGT))
# temp2 <- entire.data %>% filter(MAKE %in% temp1$MAKE, VINMAKE %in% temp1$VINMAKE, MODEL %in% 
#                                   temp1$MODEL, VINAMOD %in% temp1$VINAMOD, MODELYR %in% temp1$VINMODYR, BODYTYPE %in% 
#                                   temp1$BODYTYPE, VINBT %in% temp1$VINBT)
# curbwgt.pred2 <- predict(curbwgt.model, temp2)
# sqrt(mean((curbwgt.pred2 - temp2$CURBWGT)^2, na.rm = TRUE)) # training error


#############################################################################################

### Impute Seatbelt ----------------------------------------------------------------------------
#############################################################################################

temp9 <- filter(temp, !is.na(SEATBELT))

set.seed(42)
seatbeltImputeModel <- train(
  factor(SEATBELT) ~ MAKE + MODEL + VINMODYR + BODYTYPE + CURBWGT + SEX + AGE +
    TRAVELSP + SPLIMIT + SURCOND + SURTYPE + LGTCOND + CLIMATE + MONTH + TIME + YEAR,
  data = temp9, 
  method = "rpart",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE),
  na.action = na.rpart
)

datImp$SEATBELT[is.na(datImp$SEATBELT)] <- predict(seatbeltImputeModel, newdata = dat[is.na(dat$SEATBELT), ], 
                                               na.action = na.rpart)

## Check party package for handling missing values
## Check randomForestSRC also
### Save data frame ----------------------------------------------------------------------------
## @knitr write_data

dat <- select(dat, -YVID)
datImp <- select(datImp, -YVID)

write.csv(dat, file = "Data/MyData.csv", row.names = FALSE, na = "")


### Clean Up Factor Levels ---------------------------------------------------------------------

datImp <- mutate(datImp,
                 VEHTYPE = factor(ifelse(BODYTYPE == "1", "Convertible",
                                  ifelse(BODYTYPE %in% c(2:10, 17), "Sedan",
                                  ifelse(BODYTYPE %in% c(14:16, 19), "SUV",
                                  ifelse(BODYTYPE %in% c(20:29), "Van",
                                  ifelse(BODYTYPE %in% c(30:40), "Pickup", NA)))))),
                 oVEHTYPE = factor(ifelse(otbdytyp2 == "1", "Convertible",
                                         ifelse(otbdytyp2 %in% c(2:10, 17), "Sedan",
                                                ifelse(otbdytyp2 %in% c(14:16, 19), "SUV",
                                                       ifelse(otbdytyp2 %in% c(20:29), "Van",
                                                              ifelse(otbdytyp2 %in% c(30:40), "Pickup", NA)))))),
                 SEXF = ifelse(SEX %in% c(3:6), "2", SEX),
                 SEXF = factor(SEXF, labels = c("Male", "Female")))

dat <- mutate(dat,
                 VEHTYPE = factor(ifelse(BODYTYPE == "1", "Convertible",
                                         ifelse(BODYTYPE %in% c(2:10, 17), "Sedan",
                                                ifelse(BODYTYPE %in% c(14:16, 19), "SUV",
                                                       ifelse(BODYTYPE %in% c(20:29), "Van",
                                                              ifelse(BODYTYPE %in% c(30:40), "Pickup", NA)))))),
                 oVEHTYPE = factor(ifelse(otbdytyp2 == "1", "Convertible",
                                          ifelse(otbdytyp2 %in% c(2:10, 17), "Sedan",
                                                 ifelse(otbdytyp2 %in% c(14:16, 19), "SUV",
                                                        ifelse(otbdytyp2 %in% c(20:29), "Van",
                                                               ifelse(otbdytyp2 %in% c(30:40), "Pickup", NA)))))),
                 SEXF = ifelse(SEX %in% c(3:6), "2", SEX),
                 SEXF = factor(SEXF, labels = c("Male", "Female")))

dat <- mutate(dat,
              AGEF = factor(ifelse(AGE >= 60, "60 OR OLDER", "YOUNGER THAN 60")))

datImp <- mutate(datImp,
              AGEF = factor(ifelse(AGE >= 60, "60 OR OLDER", "YOUNGER THAN 60")))

# Check for seatbelt failure (none found)
dat %>% left_join(data.of.interest %>% select(MANFAIL, ABLTFAIL, YOID), by = "YOID") %>% select(MANFAIL, ABLTFAIL) %>% table

## Save Imputed Data --------------------------------------------------------------------------

write.csv(datImp, file = "Data/Imputed_data.csv", row.names = FALSE, na = "")

######### Scratch work ------------------------------------------------------------------------

xtabs(~ df$MODELYR + is.na(df$MAIS08))
xtabs(~ df$NEWCAR + is.na(df$MAIS08))
xtabs(~ df$NEWCAR + is.na(df$TREATMNT))
xtabs(~ df$NEWCAR + is.na(df$INJSEV))
xtabs(~ df$NEWCAR + (is.na(df$INJSEV) & is.na(df$TREATMNT)))






























