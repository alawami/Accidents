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

### Read in Data -------------------------------------------------------------------------------
################################################################################################

setwd("~/Documents/School/Fall_16/Accidents/")

accident.dat <- read.file(file = "accident.sas7bdat")
event.dat <- read.file(file = "event.sas7bdat")
gv.dat <- read.file(file = "gv.sas7bdat")
makmod.dat <- read.file(file = "makmod.sas7bdat")
oa.dat <- read.file(file = "oa.sas7bdat")
oi.dat <- read.file(file = "oi.sas7bdat")
ve.dat <- read.file(file = "ve.sas7bdat")
vi.dat <- read.file(file = "vi.sas7bdat")
  
# temp <- read.sas7bdat("Data/NASS/2014/Expanded SAS/airbag.sas7bdat")
# glimpse(temp)

################################################################################################

### Define Variables ---------------------------------------------------------------------------

################################################################################################

### Define Dictionary and Unique Case ID -------------------------------------------------------

### Dictionary Definition ======================================================================
  # str_c("names(", unlist(str_split("accident.dat, event.dat, gv.dat, makmod.dat, oa.dat, oi.dat, 
  #                                  ve.dat, vi.dat", pattern = ", ")), ")", collapse = ", ")
  # sort(unique(c(names(accident.dat), names(event.dat), names(gv.dat), names(makmod.dat), names(oa.dat),
  #               names(oi.dat), names(ve.dat), names(vi.dat))))
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
  
  data_set_description <- data.frame(data_set = c("accident.dat", "event.dat", "gv.dat", "makmod.dat", 
                                                  "oa.dat", "oi.dat", "ve.dat", "vi.dat") ,
                                     description = c("Accident Records", "Accident Event Records", 
                                                     "General Vehicle", "Make and Model", 
                                                     "Occupent Assessment Records", 
                                                     "Occupent Injury Records", "Vehicle Exterior", 
                                                     "Vehicle Interior"))
  vars <- sort(unique(c(names(accident.dat), names(event.dat), names(gv.dat), names(makmod.dat),
                        names(oa.dat), names(oi.dat), names(ve.dat), names(vi.dat))))
  # writeLines(vars, sep = ",\n                 ")
  
  lookuptable <- data.frame(variable = vars, description = describ)
  row.names(lookuptable) <- vars
################################################################################################

dataSetNames <- c("accident.dat", "event.dat", "gv.dat", "oa.dat", "oi.dat", "ve.dat", "vi.dat")

# Example
lookuptable[names(accident.dat),]

lookup <- function(data) {
  lookuptable[names(data),]
}

# Define Identification variable
accident.dat<- mutate(accident.dat, ID = str_c(CASENO, "-", PSU))
event.dat<- mutate(event.dat, ID = str_c(CASENO, "-", PSU))
gv.dat<- mutate(gv.dat, ID = str_c(CASENO, "-", PSU),
                VID = str_c(VEHNO, "-", ID))
oa.dat<- mutate(oa.dat, ID = str_c(CASENO, "-", PSU),
                VID = str_c(VEHNO, "-", ID),
                OID = str_c(OCCNO, "-", VEHNO, "-", ID))
oi.dat<- mutate(oi.dat, ID = str_c(CASENO, "-", PSU),
                VID = str_c(VEHNO, "-", ID),
                OID = str_c(OCCNO, "-", VEHNO, "-", ID))
ve.dat<- mutate(ve.dat, ID = str_c(CASENO, "-", PSU),
                VID = str_c(VEHNO, "-", ID))
vi.dat<- mutate(vi.dat, ID = str_c(CASENO, "-", PSU),
                VID = str_c(VEHNO, "-", ID))

# Template to distribute code on all sets
# sapply(X = dataSetNames, FUN = function(x){
#   message(str_c(x, "<- mutate(", x, ", ID = str_c(CASENO, \"-\", PSU))"))})


#############################################################################################

### Is case id in all data sets? -----------------------------------------------------------------
################################################################################################

dataSetList <- list(accident = names(accident.dat), event = names(event.dat), gv = names(gv.dat),
                    oi = names(oi.dat), oa = names(oa.dat), vi = names(vi.dat), ve = names(ve.dat))
var_to_check <- "YEAR"
lapply(X = dataSetList, FUN = function(x){`%in%`(var_to_check,x)})

#############################################################################################

### Accident Records --------------------------------------------------------------------------
#############################################################################################

accident.dat <- mutate(accident.dat,
                       AAIS, # MAXIMUM KNOWN AIS IN ACCIDENT # Abbreviated Injury Scale (AIS)
                       AAIS08,
                       AINJSER, # NUMBER OF SERIOUSLY INJURED OCCUPANTS
                       AINJSER8,
                       AINJURED, # TOTAL NUMBER OF INJURED OCCUPANTS 
                       ALCINV = as.factor(ALCINV), # ALCOHOL INVOLVED IN ACCIDENT
                       ATREAT = as.factor(ATREAT), # MAXIMUM TREATMENT IN ACCIDENT
                       CASEID, # CASE NUMBER - STRATUM
                       CASENO = as.factor(CASENO), # CASE SEQUENCE NUMBER
                       DAYWEEK = as.factor(DAYWEEK), # DAY OF WEEK OF ACCIDENT
                       DRGINV = as.factor(DRGINV), # DRUG INVOLVED
                       EVENTS, # NUMBER OF RECORDED EVENTS IN ACCIDENT
                       MANCOLL = as.factor(MANCOLL), # MANNER OF COLLISION
                       MONTH = as.factor(MONTH), # MONTH OF ACCIDENT
                       PSU = as.factor(PSU), # PRIMARY SAMPLING UNIT NUMBER
                       PSUSTRAT = as.factor(PSUSTRAT), # PRIMARY SAMPLING UNIT STRATIFICATION
                       RATWGT, # RATIO INFLATION FACTOR
                       STRATIF, # CASE STRATUM
                       TIME, # TIME OF ACCIDENT
                       VEHFORMS, # NUMBER GENERAL VEHICLE FORMS SUBMITTED 
                       YEAR = as.factor(YEAR), # YEAR OF ACCIDENT
                       VERSION, # VERSION NUMBER
                       ADMINSS # ADMINISTRATIVE USE
)

# EDA
table(accident.dat$CASENO)
n_distinct(accident.dat$CASENO)
n_distinct(accident.dat$CASEID)
filter(accident.dat, CASENO == "1")

unique(accident.dat$PSUSTRAT)
n_distinct(accident.dat$PSUSTRAT)

unique(accident.dat$STRATIF)
n_distinct(accident.dat$STRATIF)

unique(accident.dat$PSU)
n_distinct(accident.dat$PSU)

lookuptable[names(accident.dat),]

#############################################################################################

### Survey Design ---------------------------------------------------------------------------
#############################################################################################
## @knitr survey

des <- svydesign(ids = ~PSU, strata = ~PSUSTRAT, weights = accident.dat$RATWGT, data = accident.dat)

svymean(~AAIS, design = des, na.rm = TRUE)
svytotal(~ALCINV, design = des)
svymean(~ALCINV, design = des)
svyby(~STRATIF, by=~ALCINV, design = des, FUN = svymean)
# confint(svymean(...))

# mod <- svyglm(formula, design = des)

svyboxplot(AAIS~STRATIF, des, main="")#, ylim=c(,))
table(accident.dat$STRATIF)
svyplot(AAIS~STRATIF, des, main="SRS")
# Check out binning and transprency options

prop <- svymean(~ALCINV, design = des)
barplot(prop)
prop <- svyby(~STRATIF, by=~ALCINV, design = des, FUN = svymean)
barplot(prop, legend.text=TRUE)

#############################################################################################

### Accident Event Record ---------------------------------------------------------------------
#############################################################################################
## @knitr all

lookuptable[names(event.dat),]

# EDA
quick.look(event.dat)

#############################################################################################

### General Vehicle -------------------------------------------------------------------------
#############################################################################################

lookuptable[names(gv.dat),]

# paste(names(gv.dat), collapse = ",\n                 ")
# message(*insert output of last function*)

# EDA
quick.look(gv.dat)

#############################################################################################

### Make and model ----------------------------------------------------------------------------
#############################################################################################


#############################################################################################

### Occupent Assessment Record ----------------------------------------------------------------
#############################################################################################

quick.look(oa.dat)

#############################################################################################

### Occupent Injury Record --------------------------------------------------------------------
#############################################################################################

quick.look(oi.dat)

#############################################################################################

### Vehicle Exterior --------------------------------------------------------------------------
#############################################################################################

quick.look(ve.dat)

#############################################################################################

### Vehicle Interior --------------------------------------------------------------------------
#############################################################################################

quick.look(vi.dat)

#############################################################################################

### Impute DVEST ----------------------------------------------------------------------------
#############################################################################################

library(rpart)

deltaV.model <- rpart(DVEST ~ ., data=gv.dat[!is.na(gv.dat$DVEST),] , method="anova")
# deltaV.pred <- predict(deltaV.model, gv.dat)
# sqrt(mean((deltaV.pred - gv.dat$DVEST)^2, na.rm = TRUE)) # training error


#############################################################################################

### Filter Cases ----------------------------------------------------------------------------
#############################################################################################

# MANCOLL == 2  ## MANCOLL != 0 for two vehicle accidents
# Filter 'data set' by CASENO and PSU
cases <- filter(accident.dat, MANCOLL == 2)$ID
# Pick out relevent data from 'data set'
temp <- filter(event.dat, ID %in% cases)


# Test Merge
merge(head(filter(accident.dat, MANCOLL == 2)), head(temp), all = TRUE)
# full_join(tbl_df(head(filter(accident.dat, MANCOLL == 2))), tbl_df(head(temp)))

############################################################################################

### Variables of interest ------------------------------------------------------------------
############################################################################################

# OTVEHWGT, WGTCDTR, VEHWGT, CARGOWGT, CURBWGT, WEIGHT

head(select(gv.dat, otvehwgt, WGTCDTR, VEHWGT, CARGOWGT, CURBWGT))
num.na(select(gv.dat, otvehwgt, WGTCDTR, VEHWGT, CARGOWGT, CURBWGT))

# DVBASIS, DVEST, DVLAT, DVLONG, DVTOTAL, ENERGY, TRAVELSP, IMPACTSP

head(select(gv.dat, DVBASIS, DVEST, DVCONFID, DVLAT, DVLONG, DVTOTAL, ENERGY, TRAVELSP, IMPACTSP))
num.na(select(gv.dat, DVBASIS, DVEST, DVCONFID, DVLAT, DVLONG, DVTOTAL, ENERGY, TRAVELSP, IMPACTSP))

# If DVEST is NA, then DVTOTAL is also NA.
sum(!is.na(gv.dat$DVTOTAL[is.na(gv.dat$DVEST)]))
xtabs( ~ (gv.dat$DVEST > 0) + is.na(gv.dat$DVTOTAL) + is.na(gv.dat$DVEST))
sum(gv.dat$DVEST == 0, na.rm = TRUE)
xtabs( ~ (gv.dat$DVEST) + is.na(gv.dat$DVTOTAL))

# if DVTOTAL is NA, DVCONFID is 0
with(gv.dat, xtabs(~ DVCONFID + is.na(gv.dat$DVTOTAL)))
# Also, if DVCONFID is NA, then both DVEST & DVTOTAL are NA
with(gv.dat[is.na(gv.dat$DVCONFID),], sum(!is.na(DVTOTAL) | !is.na(DVEST)))

# IMPACTSP is NA iff DVTOTAL is NA
with(gv.dat, sum(is.na(DVTOTAL) != is.na(IMPACTSP)))
table(gv.dat$IMPACTSP)
table(gv.dat$TRAVELSP)
hist(gv.dat$TRAVELSP[gv.dat$TRAVELSP != 777])

# ANGOTHER, ANGTHIS, DOF

head(select(gv.dat, ANGOTHER, ANGTHIS))
num.na(select(gv.dat, ANGOTHER, ANGTHIS))

# AAIS08, AINJSER8, AINJURED

head(select(accident.dat, AAIS08, AINJSER8, AINJURED, ATREAT))
num.na(select(accident.dat, AAIS08, AINJSER8, AINJURED, ATREAT))


############################################################################################

### Construct Data Frame -------------------------------------------------------------------
############################################################################################

## Note CLASS2 == 0 should indicate a nonvehicle (or VEHTYPE)
## You'd expect that in the case of two car collision two events would be created. That is
## only true about half the time. Other times only one event is created.

### Variables of interest
# GV: CURBWGT, otvehwgt (for checking), VEHWGT, VINLNGTH, WGTCDTR
# GV: DVCONFID, DVEST, DVTOTAL, DVLONG, DVLAT, ENERGY
# GV: BODYTYPE, otbdytyp, MAKE, VINMAKE, MODEL, VINAMOD, MODELYR, VINMODYR, VINBT

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

### Compute maximum num of vehicle involved and maximum number of ACCSEQ
event.by.id <- group_by(event.dat, ID)
max.veh <- summarise(event.by.id,
                     MAXVEHNUM = max(VEHNUM))
max.accseq <- summarise(event.by.id,
                        MAXACCSEQ = max(ACCSEQ))
obj.cont.grt.2 <- summarise(event.by.id,
                            obj.cont.grt.2 = (max(VEHNUM) > 2 | max(OBJCONT) > 2))

event.dat <- full_join(event.dat, max.veh, by = "ID")
event.dat <- full_join(event.dat, max.accseq, by = "ID")
event.dat <- full_join(event.dat, obj.cont.grt.2, by = "ID")

### Compute number of drivers oa's
oa.driver <- filter(oa.dat, ROLE == 1)
oa.by.id <- group_by(oa.driver, ID)
max.driver.oa <- summarise(oa.by.id,
                           MAXDOA = n())
oa.dat <- full_join(oa.dat, max.driver.oa, by = "ID")

### Compute maximum sever injury
oi.by.oid <- group_by(oi.dat, OID)
max.ais <- summarise(oi.by.oid,
                     MAXAIS = max(AIS, na.rm = TRUE))
oi.dat <- full_join(oi.dat, max.ais, by = "OID")

### Select events where CLASS2 == 0 (event that involves a non-vehicle object)
non.veh.cases <- filter(event.dat, CLASS2 == 0)$ID

### Select events where no more than two vehicles are involved
only.two.veh.cases <- filter(event.dat, !obj.cont.grt.2)$ID

### Select "Head On" or "REAR-END" Collision
# MANCOLL == 2  ## MANCOLL != 0 for two vehicle accidents
# Filter 'data set' by CASENO and PSU
mancoll.cases <- filter(accident.dat, MANCOLL == 2 | MANCOLL == 1)$ID

cases <- setdiff(intersect(mancoll.cases, only.two.veh.cases), non.veh.cases)


### Select simple cases
multi.event.cases <- filter(event.dat, MAXACCSEQ > 1)$ID

cases <- setdiff(cases, multi.event.cases)

event.dat <- select(event.dat, -obj.cont.grt.2)

# Exclude cases where we don't know if driver is present or if there missing (e.g. parked car)
unknow.drpres <- filter(gv.dat, is.na(DRPRES) | DRPRES == 0)$ID

cases <- setdiff(cases, unknow.drpres)

# Exclude cases if we don't have both drivers oa's.
both.driver.assessments <- filter(oa.dat, MAXDOA == 2)$ID
cases <- intersect(cases, both.driver.assessments)

# Pick out relevent data from 'data set'
temp.acc <- filter(accident.dat, ID %in% cases)
temp.event <- filter(event.dat, ID %in% cases)
temp.gv <- filter(gv.dat, ID %in% cases)

temp.ve <- filter(ve.dat, ID %in% cases)
temp.vi <- filter(vi.dat, ID %in% cases)
temp.oa <- filter(oa.dat, ID %in% cases, ROLE == 1)
# Ignore oi
temp.oi <- filter(oi.dat, OID %in% temp.oa$OID)

# Keep only one record per driver
duplicate <- c(FALSE, temp.oi[1:(dim(temp.oi)[1]-1),"OID"] == temp.oi[2:dim(temp.oi)[1],"OID"])
temp.oi <- filter(temp.oi, duplicate)

# Ignore merging oi and only use oa
# temp.om <- right_join(filter(max.ais, OID %in% temp.oa$OID), select(temp.oa, AGE, VID, OID, ID))
# temp.om <- select(temp.oa, AGE, VID, OID, ID, BLOOD, DEATH, HEIGHT, HOSPSTAY, INJNUM08, ISS08, INJSEV, 
#                   MAIS08, OCCMOBIL, PREVACC, TREATMNT, WEIGHT, WORKDAYS, BICARB, CAUSE1)
temp.om <- temp.oa

# Merge
# Examples:
# merge(head(filter(accident.dat, MANCOLL == 2)), head(temp), all = TRUE)
# full_join(tbl_df(head(filter(accident.dat, MANCOLL == 2))), tbl_df(head(temp)))

# Error
# m.df <- merge(temp.om, temp.acc, all = TRUE)
# m.df <- merge(m.df, temp.event, all = TRUE)
# e.df <- merge(m.df, temp.gv, all = TRUE) # Error

m.df <- merge(temp.om, temp.gv, all = TRUE)
m.df <- merge(m.df, temp.event, all = TRUE)
m.df <- merge(m.df, temp.acc, all = TRUE)


df <- select(m.df, ID, CASEID, CASENO, PSU, RATWGT, STRATIF, VID, OID, BLOOD, DEATH, HEIGHT, HOSPSTAY,
             INJNUM08, ISS08, INJSEV, MAIS08, OCCMOBIL, PREVACC, TREATMNT, WEIGHT, WORKDAYS, AGE, 
             MANCOLL, DAYWEEK, BICARB, CAUSE1, TOWPAR, OCCFORMS,
             EVENTS, MONTH, PSUSTRAT, TIME, YEAR, ACCSEQ, CLASS2, CLASS1, GADEV1, GADEV2, OBJCONT, 
             VEHNUM, MAXVEHNUM, MAXACCSEQ, ACCTYPE, ANGTHIS, ANGOTHER, CLIMATE, BODYTYPE, CARGOWGT, 
             CURBWGT, DVEST, DOCTRAJ, DRPRES, DVBASIS, DVCONFID, DVLAT, DVLONG, DVTOTAL, ENERGY, 
             ETHNICIT, IMPACTSP, LGTCOND, MAKE, MODEL, MODELYR, OCCFORMS, OCUPANTS, SURCOND, SURTYPE, 
             RACE, SPLIMIT, TRAVELSP, VEHNO, VINLNGTH, VAIS08, VINMODYR, VEHWGT, VINMAKE, VINAMOD, 
             VINBT, VEHTYPE, WGTCDTR, otvehwgt, otbdytyp, VINJSER8, VINJURD8, VTREAT)

num.na(df)
num.na(df)/dim(df)[1]

# GV: CURBWGT, otvehwgt (for checking), VEHWGT, VINLNGTH, WGTCDTR
# GV: DVCONFID, DVEST, DVTOTAL, DVLONG, DVLAT, ENERGY
# GV: BODYTYPE, otbdytyp, MAKE, VINMAKE, MODEL, VINAMOD, MODELYR, VINMODYR, VINBT

# OA: AGE, ROLE

# OI: AIS ## DIRINJ, INJLVL08

### Check some data consistency ---------------------------------------------------------------------
#############################################################################################

# Unimplemented idea
# crash.by.id <- group_by(df.dat, ID)
# consistent.crash <- summarise(crash.by.id,
#                               consistent.weight = CURBWGT)

temp <- (select(df, CURBWGT, otvehwgt, VEHWGT, WGTCDTR, ANGTHIS, ANGOTHER, ID))
# Check that a cases come in pairs
sum((temp$ID[2*(1:141)] != temp$ID[2*(1:141)-1]))
# Check weight information
sum(((temp$CURBWGT[2*(1:141)] != temp$otvehwgt[2*(1:141)-1]) | 
       (temp$otvehwgt[2*(1:141)] != temp$CURBWGT[2*(1:141)-1])), na.rm = TRUE)
# Check angle information
sum(((temp$ANGTHIS[2*(1:141)] != temp$ANGOTHER[2*(1:141)-1]) | 
       (temp$ANGOTHER[2*(1:141)] != temp$ANGTHIS[2*(1:141)-1])), na.rm = TRUE)

#############################################################################################

######### Scratch work ------------------------------------------------------------------------------
temp.event$CLASS2
temp.event[49,]
filter(accident.dat, ID == "35-13")
filter(gv.dat, ID == "35-13")
filter(event.dat, ID == "35-13")

filter(accident.dat, ID == "53-9")
filter(event.dat, ID == "53-9")
select(filter(gv.dat, ID == "53-9"), ACCSEQDV, ACCTYPE, ANGTHIS, ANGOTHER, BODYTYPE, CURBWGT, MAKE, 
       MODEL, MODELYR, RATWGT, VEHNO, VINMAKE, VINAMOD, VEHWGT, VINBT, VINMODYR, WGTCDTR)
with(gv.dat, num.na(cbind(ACCSEQDV, ACCTYPE, ANGTHIS, ANGOTHER, BODYTYPE, CURBWGT, MAKE, 
                          MODEL, MODELYR, RATWGT, VEHNO, VINMAKE, VINAMOD, VEHWGT, VINBT, 
                          VINMODYR, WGTCDTR)))

cas <- "102-79"
filter(accident.dat, ID == cas)
filter(event.dat, ID == cas)
filter(gv.dat, ID == cas)
select(filter(gv.dat, ID == cas), ACCSEQDV, ACCTYPE, ANGTHIS, ANGOTHER, BODYTYPE, CURBWGT, DRPRES, MAKE, 
       MODEL, MODELYR, RATWGT, VEHNO, VINMAKE, VINAMOD, VEHWGT, VINBT, VINMODYR, WGTCDTR)
with(temp.gv, num.na(cbind(ACCSEQDV, ACCTYPE, ANGTHIS, ANGOTHER, BODYTYPE, CURBWGT, MAKE, 
                           MODEL, MODELYR, RATWGT, VEHNO, VINMAKE, VINAMOD, VEHWGT, VINBT, 
                           VINMODYR, WGTCDTR)))
filter(ve.dat, ID == cas)
filter(vi.dat, ID == cas)
filter(oa.dat, ID == cas)
filter(oi.dat, ID == cas)
