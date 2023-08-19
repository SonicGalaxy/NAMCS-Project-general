library(haven)
import_nam_data <- function(year) {
  file_path <- paste0("C:/Users/aleong/Desktop/NAMCS/Replication/nam", year, ".dta")
  data <- read_dta(file_path)
  return(data)
}

nam93 <- import_nam_data("93")
nam94 <- import_nam_data("94")
nam95 <- import_nam_data("95")
nam96 <- import_nam_data("96")
nam97 <- import_nam_data("97")
nam98 <- import_nam_data("98")
nam99 <- import_nam_data("99")
nam00 <- import_nam_data("00")
nam01 <- import_nam_data("01")
nam02 <- import_nam_data("02")
nam03 <- import_nam_data("03")
nam04 <- import_nam_data("04")
nam05 <- import_nam_data("05")
nam06 <- import_nam_data("06")
nam07 <- import_nam_data("07")
nam08 <- import_nam_data("08")
nam09 <- import_nam_data("09")
nam10 <- import_nam_data("10")
nam11 <- import_nam_data("11")
nam12 <- import_nam_data("12")
nam13 <- import_nam_data("13")
nam14 <- import_nam_data("14")
nam15 <- import_nam_data("15")
nam16 <- import_nam_data("16")

#under18
data_frame_names <- c("nam93", "nam94", "nam95", "nam96", "nam97", "nam98", "nam99", "nam00", "nam01", "nam02", "nam03", "nam04", "nam05", "nam06",
                      "nam07", "nam08", "nam09", "nam10", "nam11", "nam12", "nam13", "nam14",
                      "nam15", "nam16")
for (df_name in data_frame_names) {
  df <- get(df_name)  # Get the data frame by name
  colnames(df) <- toupper(colnames(df))  # Convert column names to uppercase
  assign(df_name, df, envir = .GlobalEnv)  # Update the data frame in the environment
}

for (df_name in data_frame_names) {
  df <- get(df_name)  # Get the data frame by name
  df <- df[df$AGE <18, ] # Subset the data frame to those only with age under 18
  ############################ df <- df[df$AGE >= 1 & df$AGE <= 17, ]
  assign(df_name, df, envir = .GlobalEnv)  # Update the data frame in the environment
}

"Also, prior to 2006, (i.e. before CHCs were sampled as a separate stratum), physicians who were sampled from the master files of the American Medical Association 
or American Osteopathic Association but who saw patients in CHCs were not excluded nor were their CHC visits excluded from survey eligibility.
For that reason, in all previous years, it is possible to have at least some (albeit generally a very small percentage of) CHC visits in the data. 
This is not the case beginning in 2012 when there are no CHC visits on the file."

"To compare 2012 data with previous years of NAMCS data, it is necessary to restrict one's 
analysis to non-CHC visits prior to 2012. This can be done by using the RETYPOFF variable 
(Type of office setting for this visit) and excluding category 3 for community health centers"

#remove CHCs from 2006 to 2011 data sets [RETYPOFF == 3]
library(summarytools)
dataset_names_RETYPOFF <- list("nam06",
                               "nam07", "nam08", "nam09", "nam10", "nam11")

for (name in dataset_names_RETYPOFF) {
  assign(name, get(name)[get(name)$RETYPOFF != 3, ])
}

for (name in dataset_names_RETYPOFF) {
  cat("Frequency table for", name, ":\n")
  print(table(get(name)$RETYPOFF))
  cat("\n")
} # check frequency tables

data_frames <- list(nam93, nam94, nam95, nam96, nam97, nam98, nam99, nam00, nam01, nam02, nam03, nam04, nam05, nam06, nam07, nam08, nam09,
                    nam10, nam11, nam12, nam13, nam14, nam15, nam16)
variable <- "SPECR" #check with SPECR (all), SPECCAT (nam05 onwards only), CPSUM & CSTRATM (nam02 onwards only), PATWT (all)
for (i in seq_along(data_frames)) {
  if (exists(variable, where = data_frames[[i]])) {
    print(paste("Variable", variable, "is present in data frame", i))
  } else {
    print(paste("Variable", variable, "is NOT present in data frame", i))
  }
}
#SPECCAT not present for data frames before nam05, CPSUM & CSTRATM not present for data frames before nam02
#2012: SPECR_14 instead of SPECR

### create CPSUM, CSTRATM variables for data frames before nam02
data_frame_names_selected <- c("nam93", "nam94", "nam95", "nam96", "nam97", "nam98", "nam99", "nam00", "nam01")
for (df_name in data_frame_names_selected) {
  df <- get(df_name)
  # Assign CSTRATM and CPSUM columns
  df$CSTRATM <- df$STRATM
  df$CPSUM <- df$PSUM
  # Apply ifelse condition to modify CSTRATM
  df$CSTRATM <- ifelse(df$CPSUM %in% c(1, 2, 3, 4),
                       (df$STRATM * 100000) + (1000 * (df$YEAR %% 100)) +
                         (df$SUBFILE * 100) + df$PROSTRAT,
                       df$STRATM * 100000)
  # Update the modified data frame in the environment
  assign(df_name, df, envir = .GlobalEnv)
}

#singleton elimination (https://www.cdc.gov/nchs/data/ahcd/ultimatecluster.pdf)
subset_condition_nam96 <- nam96$SUBFILE == 1 & nam96$PROSTRAT == 14 & nam96$YEAR == 1996 & nam96$STRATM == 202
nam96$CSTRATM <- ifelse(subset_condition_nam96,
                        (203 * 100000) + (1000 * (nam96$YEAR %% 100)) +
                          (nam96$SUBFILE * 100) + nam96$PROSTRAT,
                        nam96$STRATM)

subset_condition_nam99 <- nam99$SUBFILE == 1 & nam99$PROSTRAT == 12 & nam99$YEAR == 1999 & nam99$STRATM == 202
nam99$CSTRATM <- ifelse(subset_condition_nam99,
                        (203 * 100000) + (1000 * (nam99$YEAR %% 100)) +
                          (nam99$SUBFILE * 100) + nam99$PROSTRAT,
                        nam99$STRATM)

subset_condition_nam00 <- nam00$SUBFILE == 1 & nam00$PROSTRAT == 1 & nam00$YEAR == 2000 & nam00$STRATM == 203
nam00$CSTRATM <- ifelse(subset_condition_nam00,
                        (202 * 100000) + (1000 * (nam00$YEAR %% 100)) +
                          (nam00$SUBFILE * 100) + nam00$PROSTRAT,
                        nam00$STRATM)

#check that CPSUM, CSTRATM present in all data frames
data_frames <- list(nam93, nam94, nam95, nam96, nam97, nam98, nam99, nam00, nam01, nam02, nam03, nam04, nam05, nam06, nam07, nam08, nam09,
                    nam10, nam11, nam12, nam13, nam14, nam15, nam16)
variable <- "CSTRATM" #check CPSUM & CSTRATM (all present)
for (i in seq_along(data_frames)) {
  if (exists(variable, where = data_frames[[i]])) {
    print(paste("Variable", variable, "is present in data frame", i))
  } else {
    print(paste("Variable", variable, "is NOT present in data frame", i))
  }
}

#creating SPECCAT from SPEC, according to 2005 codebook: https://ftp.cdc.gov/pub/Health_Statistics/NCHS/Dataset_Documentation/NAMCS/doc05.pdf

#function to create SPECCAT for each dataset
#new function with including omitted categories PH (3), CDS (2) and PS (2)
speccat_replicate_new <- function(dataset_name) {
  dataset <- get(dataset_name)
  list_PRIMCARE <- c("ADL", "AMI", "FP", "FPG", "FSM", "GP", "GYN","IM", "IMG",
                     "MPD","MFM", "OBG","OBS","PD","PSM")
  dataset$PRIMCARE <- ifelse(dataset$SPEC %in% list_PRIMCARE, 1, 0)
  list_SURCARE <-c("AS","OAR","CRS","CS","CFS","OCC","CCS", "DS","ESN","FPS","OFA","GS",
                   "GO","HS","HSP","HSO","HSS","HNS","OMO","NS","OPH","OMF","OTO","NO",
                   "ORS","OSS","OTR","PCS","OP","PO","PDO","NSP","PDS","UP","PS","PSH",
                   "OSM","SO","TS","TTS","TRS","U","VS","CDS","PS")
  dataset$SURCARE <- ifelse(dataset$SPEC %in% list_SURCARE, 1, 0)
  
  list_MEDCARE <- c("A","ADM","ADP","AI","ALI","AM","CBG","CCG","CCM","CCP","CD","CG",
                    "CMG","CHN","CHP","CN","D","DBP","DDL","DIA","EM","END","EP",
                    "ESM","ETX","GE","GPM","HEM","HEP","HO","IC","ICE","ID","IG",
                    "ILI","ISM","LM","MDM","MG","NDN","N","NEP","NPM","NRN","NTR",
                    "NUP","PHM","OM","OMM","ON","P","PA","PCC","PDA","PDC","PDE",
                    "PDI","PDP","PDT","PE","PEM","PFP","PG","PHP","PHO","PLI","PLM","PM",
                    "PMD","PMM","PN","PPR","PRM","PRO","PTX","PUD","PYA","PYG","REN",
                    "RHU","SCI","SM","UCM","UM","VM","OS","US","PH")
  dataset$MEDCARE <- ifelse(dataset$SPEC %in% list_MEDCARE, 1, 0)
  dataset$SPECCAT <- case_when(
    dataset$PRIMCARE == 1 ~ 1,
    dataset$SURCARE == 1 ~ 2,
    dataset$MEDCARE == 1 ~ 3,
    TRUE ~ NA_integer_
  )
  # Return the modified dataset
  return(dataset)
}

library(dplyr)

#only for data sets prior to 2005
nam93_repl <- speccat_replicate_new("nam93")
nam94_repl <- speccat_replicate_new("nam94")
nam95_repl <- speccat_replicate_new("nam95")
nam96_repl <- speccat_replicate_new("nam96")
nam97_repl <- speccat_replicate_new("nam97")
nam98_repl <- speccat_replicate_new("nam98")
nam99_repl <- speccat_replicate_new("nam99")
nam00_repl <- speccat_replicate_new("nam00")
nam01_repl <- speccat_replicate_new("nam01")
nam02_repl <- speccat_replicate_new("nam02")
nam03_repl <- speccat_replicate_new("nam03")
nam04_repl <- speccat_replicate_new("nam04")

#checking for NAs in SPECCAT
library(summarytools)
print_speccat_frequency <- function(data) {
  freq_table <- freq(data$SPECCAT)
  cat("Frequency table for SPECCAT:\n")
  print(freq_table)
}

print_speccat_frequency(nam93_repl) #no NAs
print_speccat_frequency(nam94_repl) #no! NAs
print_speccat_frequency(nam95_repl) #no! NAs
print_speccat_frequency(nam96_repl) #no! NAs
print_speccat_frequency(nam97_repl) #no! NAs
print_speccat_frequency(nam98_repl) #no NAs
print_speccat_frequency(nam99_repl) #no NAs
print_speccat_frequency(nam00_repl) #no NAs
print_speccat_frequency(nam01_repl) #no NAs
print_speccat_frequency(nam02_repl) #no NAs
print_speccat_frequency(nam03_repl) #no NAs
print_speccat_frequency(nam04_repl) #no NAs

#if excluding visits with unspecified specialites
############################ nam93_filtered <- nam93_repl[nam93_repl$SPECR != 15, ]

### write a function [create_specialty_variables()] to create PEDGEN, NONPEDGEN, PEDSPEC, NONPEDSPEC for any data set
create_specialty_variables <- function(data) {
  data <- data[data$SPECCAT != 2, ]
  ############################ data_filtered <- data_repl[data$SPECR != 15, ]
  # PEDGEN: if SPECR == 4 (Pediatrics) and SPECCAT == 1 (Primary care specialty)
  data$PEDGEN <- ifelse(data$SPECCAT == 1 & data$SPECR == 4, 1, 0)
  # NONPEDGEN: if SPECR != 4 (Pediatrics) and SPECCAT == 1 (Primary care specialty)
  data$NONPEDGEN <- ifelse(data$SPECCAT == 1 & data$SPECR != 4, 1, 0)
  # PEDSPEC: if SPECR == 4 (Pediatrics) and SPECCAT == 3 (Medical care specialty)
  data$PEDSPEC <- ifelse(data$SPECCAT == 3 & data$SPECR == 4, 1, 0)
  # NONPEDSPEC: if SPECR != 4 (Pediatrics) and SPECCAT == 3 (Medical care specialty)
  data$NONPEDSPEC <- ifelse(data$SPECCAT == 3 & data$SPECR != 4, 1, 0)
  
  data$PEDGEN <- as.factor(data$PEDGEN)
  data$NONPEDGEN <- as.factor(data$NONPEDGEN)
  data$PEDSPEC <- as.factor(data$PEDSPEC)
  data$NONPEDSPEC <- as.factor(data$NONPEDSPEC)
  
  return(data)
}

#nam12 does not have SPECR but has SPECR_14 and SPECR_17 instead. SPECR_14 uses the methodology
#that other survey year use. 
create_specialty_variables_nam12 <- function(data) {
  data <- data[data$SPECCAT != 2, ]
  ############################ data_filtered <- data_repl[data$SPECR != 15, ]
  # PEDGEN nam12: if SPECR == 4 (Pediatrics) and SPECCAT == 1 (Primary care specialty)
  data$PEDGEN <- ifelse(data$SPECCAT == 1 & data$SPECR_14 == 4, 1, 0)
  # NONPEDGEN nam12: if SPECR != 4 (Pediatrics) and SPECCAT == 1 (Primary care specialty)
  data$NONPEDGEN <- ifelse(data$SPECCAT == 1 & data$SPECR_14 != 4, 1, 0)
  # PEDSPEC nam12: if SPECR == 4 (Pediatrics) and SPECCAT == 3 (Medical care specialty)
  data$PEDSPEC <- ifelse(data$SPECCAT == 3 & data$SPECR_14 == 4, 1, 0)
  # NONPEDSPEC nam12: if SPECR != 4 (Pediatrics) and SPECCAT == 3 (Medical care specialty)
  data$NONPEDSPEC <- ifelse(data$SPECCAT == 3 & data$SPECR_14 != 4, 1, 0)
  
  data$PEDGEN <- as.factor(data$PEDGEN)
  data$NONPEDGEN <- as.factor(data$NONPEDGEN)
  data$PEDSPEC <- as.factor(data$PEDSPEC)
  data$NONPEDSPEC <- as.factor(data$NONPEDSPEC)
  
  return(data)
}


#namxx_filtered_gen: under18 only, speccat variables generated from spec, speccat obs == 2 (medical removed),
# specialty variables created
nam93_filtered_gen <- as.data.frame(create_specialty_variables(nam93_repl))
nam94_filtered_gen <- as.data.frame(create_specialty_variables(nam94_repl))
nam95_filtered_gen <- as.data.frame(create_specialty_variables(nam95_repl))
nam96_filtered_gen <- as.data.frame(create_specialty_variables(nam96_repl))
nam97_filtered_gen <- as.data.frame(create_specialty_variables(nam97_repl))
nam98_filtered_gen <- as.data.frame(create_specialty_variables(nam98_repl))
nam99_filtered_gen <- as.data.frame(create_specialty_variables(nam99_repl))
nam00_filtered_gen <- as.data.frame(create_specialty_variables(nam00_repl))
nam01_filtered_gen <- as.data.frame(create_specialty_variables(nam01_repl))
nam02_filtered_gen <- as.data.frame(create_specialty_variables(nam02_repl))
nam03_filtered_gen <- as.data.frame(create_specialty_variables(nam03_repl))
nam04_filtered_gen <- as.data.frame(create_specialty_variables(nam04_repl))
nam05_filtered_gen <- as.data.frame(create_specialty_variables(nam05)) #nam05 onwards has SPECCAT?
nam06_filtered_gen <- as.data.frame(create_specialty_variables(nam06))
nam07_filtered_gen <- as.data.frame(create_specialty_variables(nam07))
nam08_filtered_gen <- as.data.frame(create_specialty_variables(nam08))
nam09_filtered_gen <- as.data.frame(create_specialty_variables(nam09))
nam10_filtered_gen <- as.data.frame(create_specialty_variables(nam10))
nam11_filtered_gen <- as.data.frame(create_specialty_variables(nam11))
nam13_filtered_gen <- as.data.frame(create_specialty_variables(nam13))
nam14_filtered_gen <- as.data.frame(create_specialty_variables(nam14))
nam15_filtered_gen <- as.data.frame(create_specialty_variables(nam15))
nam16_filtered_gen <- as.data.frame(create_specialty_variables(nam16))

nam12_filtered_gen <- as.data.frame(create_specialty_variables_nam12(nam12))

###filtering by PAYTYPER

#1993 - 1994 inclusive
#Expected sources of payment
#12: HMO/Other prepaid
#13: Medicare
#14: Medicaid
#15: Other govt
#16: Private/Commercial Insurance
#17: Patient paid
#18: No charge
#19: Other

###1993-1994: #14: Medicaid, #12 and #16: Private insurance, #17: Self-pay
###1995-1996: (Part A) #4: Self-pay (Part B) #18: Medicaid, #15 and #16: Private insurance. 

#1995 - 1996 inclusive
#Part A: Type of Payment
#1 = Preferred Provider Option                                                                                 
#2 = Insured, Fee-For-Service                                                                                  
#3 = HMO/Other Prepaid                                                                                         
#4 = Self-Pay                                                                                                  
#5 = No Charge                                                                                                 
#6 = Other                                                                                                     
#7 = Type of payment unspecified, but source of insurance was checked in Part B                                                                                                 
#0 = No answer (both Part A and Part B were blank)  

#Part B: Expected sources of insurance
#15 = Blue Cross / Blue Shield
#16 = Other private insurance
#17 = Medicare
#18 = Medicaid
#19 = Worker's Compensation
#20 = Other Insurance
#21 = Unknown

#1997-2000 inclusive
#1 = Private insurance
#2 = Medicare
#3 = Medicaid/CHIP (CHIP 2001 onwards, inclusive) 
#4 = Worker's Compensation
#5 = Self-pay 
#6 = No charge (Charity 2001 onwards, inclusive)
#7 = Other
#8 = Unknown
#9 = Blank

#2001-2007 inclusive
#0 = Blank
#1 = Private insurance
#2 = Medicare
#3 = Medicaid/CHIP (CHIP 2001 onwards, inclusive) 
#4 = Worker's Compensation
#5 = Self-pay 
#6 = No charge / Charity  (Charity 2001 onwards, inclusive)
#7 = Other
#8 = Unknown

#2008-2016 inclusive
#-9 = Blank
#-8 = Unknown
#1 = Private insurance
#2 = Medicare
#3 = Medicaid/CHIP (CHIP 2001 onwards, inclusive) 
#4 = Worker's Compensation
#5 = Self-pay 
#6 = No charge / Charity  (Charity 2001 onwards, inclusive)
#7 = Other

######coding before 1997 inconsistent, exclude pre-1997

#checking if PAYTYPER variable exists in all data frames using a function
PAYTYPE_check_list<-list(nam93_filtered_gen, nam94_filtered_gen, nam95_filtered_gen, nam96_filtered_gen, nam97_filtered_gen,
                         nam98_filtered_gen, nam99_filtered_gen, nam00_filtered_gen, nam01_filtered_gen, nam02_filtered_gen,
                         nam03_filtered_gen, nam04_filtered_gen, nam05_filtered_gen, nam06_filtered_gen, nam07_filtered_gen,
                         nam08_filtered_gen, nam09_filtered_gen, nam10_filtered_gen, nam11_filtered_gen, nam12_filtered_gen,
                         nam13_filtered_gen, nam14_filtered_gen, nam15_filtered_gen, nam16_filtered_gen)

check_variable_existence <- function(data_list, variable_name) {
  missing_frames <- character(0)  # Initialize an empty character vector to store missing data frame names
  for (i in seq_along(data_list)) {
    df <- data_list[[i]]
    if (!(variable_name %in% names(df))) {
      missing_frames <- c(missing_frames, paste0("df", i))
    }
  }
  if (length(missing_frames) > 0) {
    cat("Variable is missing in the following data frames:", paste(missing_frames, collapse = ", "))
    return(FALSE)
  }
  return(TRUE)
}

check_variable_existence (PAYTYPE_check_list, "PAYTYPE") #missing PAYTYPE variable from df 1-4, (nam93 to nam96) AND df16-df24 (nam08 to nam16)
check_variable_existence (PAYTYPE_check_list, "PAYTYPER") #missing PAYTYPE variable from df 1-15 (nam93 to nam07)

#For 2008: "PLEASE NOTE the change in hierarchy for 2008 relative to previous years. 
#Dual-eligible Medicare and Medicaid recipients had previously been grouped under Medicaid; 
#this was changed to Medicare. Researchers can also create their own hierarchy as desired."

###to harmonize classification, for 2008 onwards, if an observation has PAYMCAID == 1 and 
# PAYMCARE == 1, classify PAYTYPER == 3

#check value of PAYTYPER for dual-eligible Medicare and Medicaid recipients:

PAYTYPER_check_list<-list(nam08_filtered_gen, nam09_filtered_gen, nam10_filtered_gen, nam11_filtered_gen, nam12_filtered_gen,
                          nam13_filtered_gen, nam14_filtered_gen, nam15_filtered_gen, nam16_filtered_gen)

print_PAYTYPER_table <- function(data_list) {
  for (i in 1:length(data_list)) {
    df <- data_list[[i]]
    
    if ("PAYMCAID" %in% names(df) && "PAYMCARE" %in% names(df) && "PAYTYPER" %in% names(df)) {
      filtered_df <- subset(df, PAYMCAID == 1 & PAYMCARE == 1, select = PAYTYPER)
      
      if (nrow(filtered_df) > 0) {
        print(paste("Data Frame", i))
        print(filtered_df)
      } else {
        print(paste("Data Frame", i))
        print("No observations found.")
      }
    } else {
      print(paste("Data Frame", i))
      print("Required variables not found in the data frame.")
    }
    
    print("-------------------------------------------------")
  }
}

print_PAYTYPER_table(PAYTYPER_check_list) 
#all dual Medicare and Medicaid recipients indeed are classified as Medicare (PAYTYPER==2)

#reclassify all dual recipients under Medicaid (PAYTYPER==3), for consistency with prior years
PAYTYPER_check_list<-list(nam08_filtered_gen, nam09_filtered_gen, nam10_filtered_gen, nam11_filtered_gen, nam12_filtered_gen,
                          nam13_filtered_gen, nam14_filtered_gen, nam15_filtered_gen, nam16_filtered_gen)

nam16_filtered_gen$PAYTYPER[nam16_filtered_gen$PAYMCARE == 1 & nam16_filtered_gen$PAYMCAID == 1] <- 3
nam15_filtered_gen$PAYTYPER[nam15_filtered_gen$PAYMCARE == 1 & nam15_filtered_gen$PAYMCAID == 1] <- 3
nam14_filtered_gen$PAYTYPER[nam14_filtered_gen$PAYMCARE == 1 & nam14_filtered_gen$PAYMCAID == 1] <- 3
nam13_filtered_gen$PAYTYPER[nam13_filtered_gen$PAYMCARE == 1 & nam13_filtered_gen$PAYMCAID == 1] <- 3
nam12_filtered_gen$PAYTYPER[nam12_filtered_gen$PAYMCARE == 1 & nam12_filtered_gen$PAYMCAID == 1] <- 3
nam11_filtered_gen$PAYTYPER[nam11_filtered_gen$PAYMCARE == 1 & nam11_filtered_gen$PAYMCAID == 1] <- 3
nam10_filtered_gen$PAYTYPER[nam10_filtered_gen$PAYMCARE == 1 & nam10_filtered_gen$PAYMCAID == 1] <- 3
nam09_filtered_gen$PAYTYPER[nam09_filtered_gen$PAYMCARE == 1 & nam09_filtered_gen$PAYMCAID == 1] <- 3
nam08_filtered_gen$PAYTYPER[nam08_filtered_gen$PAYMCARE == 1 & nam08_filtered_gen$PAYMCAID == 1] <- 3

PAYTYPER_check_list<-list(nam08_filtered_gen, nam09_filtered_gen, nam10_filtered_gen, nam11_filtered_gen, nam12_filtered_gen,
                          nam13_filtered_gen, nam14_filtered_gen, nam15_filtered_gen, nam16_filtered_gen)

print_PAYTYPER_table(PAYTYPER_check_list) #all cases where Medicare == 1 and Medicaid == 1 are
#now classified under Medicaid, as per pre-2008

#freq(nam99_filtered_gen$PAYTYPE) run to check if renaming variables is done correctly

#rename PAYTYPE variables from 1997 to 2007 as PAYTYPER
PATYPER_list_rename<-list(nam97_filtered_gen, nam98_filtered_gen, nam99_filtered_gen, nam00_filtered_gen, nam01_filtered_gen,
                          nam02_filtered_gen, nam03_filtered_gen, nam04_filtered_gen,
                          nam05_filtered_gen, nam06_filtered_gen, nam07_filtered_gen)

replace_variable_names_list <- lapply(PATYPER_list_rename, function(df) {
  names(df) <- gsub("PAYTYPE", "PAYTYPER", names(df))
  return(df)
})

nam97_filtered_gen <- replace_variable_names_list[[1]]
nam98_filtered_gen <- replace_variable_names_list[[2]]
nam99_filtered_gen <- replace_variable_names_list[[3]]
nam00_filtered_gen <- replace_variable_names_list[[4]]
nam01_filtered_gen <- replace_variable_names_list[[5]]
nam02_filtered_gen <- replace_variable_names_list[[6]]
nam03_filtered_gen <- replace_variable_names_list[[7]]
nam04_filtered_gen <- replace_variable_names_list[[8]]
nam05_filtered_gen <- replace_variable_names_list[[9]]
nam06_filtered_gen <- replace_variable_names_list[[10]]
nam07_filtered_gen <- replace_variable_names_list[[11]]

#freq(nam99_filtered_gen$PAYTYPER) run to check if renaming variables is done correctly

#1997-2000 inclusive
#1 = Private insurance
#2 = Medicare
#3 = Medicaid/CHIP (CHIP 2001 onwards, inclusive) 
#4 = Worker's Compensation
#5 = Self-pay 
#6 = No charge (Charity 2001 onwards, inclusive)
#7 = Other
#8 = Unknown
#9 = Blank

#2001-2007 inclusive
#0 = Blank
#1 = Private insurance
#2 = Medicare
#3 = Medicaid/CHIP (CHIP 2001 onwards, inclusive) 
#4 = Worker's Compensation
#5 = Self-pay 
#6 = No charge / Charity  (Charity 2001 onwards, inclusive)
#7 = Other
#8 = Unknown

#2008-2016 inclusive
#-9 = Blank
#-8 = Unknown
#1 = Private insurance
#2 = Medicare
#3 = Medicaid/CHIP (CHIP 2001 onwards, inclusive) 
#4 = Worker's Compensation
#5 = Self-pay 
#6 = No charge / Charity  (Charity 2001 onwards, inclusive)
#7 = Other

#1993 - 1994 inclusive
#Expected sources of payment
#12: HMO/Other prepaid
#13: Medicare
#14: Medicaid
#15: Other govt
#16: Private/Commercial Insurance
#17: Patient paid
#18: No charge
#19: Other
#20: Unknown

#1995 - 1996 inclusive
#Part A: Type of Payment
#1 = Preferred Provider Option                                                                                 
#2 = Insured, Fee-For-Service                                                                                  
#3 = HMO/Other Prepaid                                                                                         
#4 = Self-Pay                                                                                                  
#5 = No Charge                                                                                                 
#6 = Other                                                                                                     
#7 = Type of payment unspecified, but source of insurance was checked in Part B                                                                                                 
#0 = No answer (both Part A and Part B were blank)  

#Part B: Expected sources of insurance
#15 = Blue Cross / Blue Shield
#16 = Other private insurance
#17 = Medicare
#18 = Medicaid
#19 = Worker's Compensation
#20 = Other Insurance
#21 = Unknown

### summary statistics data frame for PAYTYPER
frequencies_df_PAYTYPER <- data.frame(variable = character(),
                                      value = character(),
                                      frequency = numeric(),
                                      percent = numeric(),
                                      dataframename = character(),
                                      stringsAsFactors = FALSE)

###!!!cannot use 93 to 16 as coding is inconsistent before 1997
PAYTYPE_check_list97to16<-list(nam97_filtered_gen,
                               nam98_filtered_gen, nam99_filtered_gen, nam00_filtered_gen, nam01_filtered_gen, nam02_filtered_gen,
                               nam03_filtered_gen, nam04_filtered_gen, nam05_filtered_gen, nam06_filtered_gen, nam07_filtered_gen,
                               nam08_filtered_gen, nam09_filtered_gen, nam10_filtered_gen, nam11_filtered_gen, nam12_filtered_gen,
                               nam13_filtered_gen, nam14_filtered_gen, nam15_filtered_gen, nam16_filtered_gen)

#summary statistics for 1997 onwards 
for (i in seq_along(PAYTYPE_check_list97to16)) {
  year <- 1996 + i
  freq_table <- table(PAYTYPE_check_list97to16[[i]]$PAYTYPER)
  
  variable <- "PAYTYPER"
  value <- names(freq_table)
  frequency <- as.numeric(freq_table)
  
  percent <- frequency / sum(frequency) * 100
  
  df <- data.frame(variable = variable,
                   value = value,
                   frequency = frequency,
                   percent = percent,
                   dataframename = paste0("nam", year),
                   stringsAsFactors = FALSE)
  frequencies_df_PAYTYPER <- rbind(frequencies_df_PAYTYPER, df)
}


frequencies_df_PAYTYPER_1 <- frequencies_df_PAYTYPER[frequencies_df_PAYTYPER$value == 1, ]
frequencies_df_PAYTYPER_3 <- frequencies_df_PAYTYPER[frequencies_df_PAYTYPER$value == 3, ]
frequencies_df_PAYTYPER_5 <- frequencies_df_PAYTYPER[frequencies_df_PAYTYPER$value == 5, ]
descr(frequencies_df_PAYTYPER_1)
descr(frequencies_df_PAYTYPER_3)
descr(frequencies_df_PAYTYPER_5)

#no PAYTYPER summary stats for 1993 to 1996 as coding does not ask for "PRIMARY" source
#of payment, could be double-counted

target_vars_doublecount_93 <- c("MEDICAID", "SELFPAY", "HMO", "PRIVINS")
column_numbers_doublecount_93 <- which(colnames(nam93_filtered_gen) %in% target_vars_doublecount_93)
print(column_numbers_doublecount_93)
#8 10 12 13

doublecount_93 <- sum(rowSums(nam93_filtered_gen[, c(8, 10, 12, 13)] == 1) > 1)
print(doublecount_93) #number of observations with more than one payment method in "expected sources of payment"
# for nam93
#268/4637 5.8% of observations have >1 payment method

column_numbers_doublecount_94 <- which(colnames(nam94_filtered_gen) %in% target_vars_doublecount_93)
print(column_numbers_doublecount_94)
#8 10 12 13
doublecount_94 <- sum(rowSums(nam94_filtered_gen[, c(8, 10, 12, 13)] == 1) > 1)
print(doublecount_94) #number of observations with more than one payment method in "expected sources of payment"
# for nam94 
#141/4420 3.1% of observations have >1 payment method

###########################
###########################
#### PAYTYPER function ####
for (year in 1997:2016) {
  # Create the data frame name
  df_name <- paste0("nam", substr(year, 3, 4), "_filtered_gen")
  # Filter the data frame based on the MSA condition
  #filtered_data <- get(df_name)[get(df_name)$PAYTYPER == 1, ] #1: Private Insurance
  filtered_data <- get(df_name)[get(df_name)$PAYTYPER == 3, ] #3: Medicaid/CHIP (CHIP 2001 onwards)
  #filtered_data <- get(df_name)[get(df_name)$PAYTYPER == 5, ] #5: Self-pay
  assign(df_name, filtered_data, envir = .GlobalEnv)
}


PAYTYPE_check_list97to16<-list(nam97_filtered_gen,
                               nam98_filtered_gen, nam99_filtered_gen, nam00_filtered_gen, nam01_filtered_gen, nam02_filtered_gen,
                               nam03_filtered_gen, nam04_filtered_gen, nam05_filtered_gen, nam06_filtered_gen, nam07_filtered_gen,
                               nam08_filtered_gen, nam09_filtered_gen, nam10_filtered_gen, nam11_filtered_gen, nam12_filtered_gen,
                               nam13_filtered_gen, nam14_filtered_gen, nam15_filtered_gen, nam16_filtered_gen)

#Checking that all data frames only have PAYTYPER=filtered value
frequencies <- vector("list", length(PAYTYPE_check_list97to16))
for (i in seq_along(PAYTYPE_check_list97to16)) {
  frequencies[[i]] <- freq(PAYTYPE_check_list97to16[[i]]$PAYTYPER)
}
for (i in seq_along(PAYTYPE_check_list97to16)) {
  year <- 1996 + i
  cat("Frequencies for nam", year, ":\n")
  print(frequencies[[i]])
  cat("\n")
}

PAYTYPE_check_list97to16<-list(nam97_filtered_gen,
                               nam98_filtered_gen, nam99_filtered_gen, nam00_filtered_gen, nam01_filtered_gen, nam02_filtered_gen,
                               nam03_filtered_gen, nam04_filtered_gen, nam05_filtered_gen, nam06_filtered_gen, nam07_filtered_gen,
                               nam08_filtered_gen, nam09_filtered_gen, nam10_filtered_gen, nam11_filtered_gen, nam12_filtered_gen,
                               nam13_filtered_gen, nam14_filtered_gen, nam15_filtered_gen, nam16_filtered_gen)

frequencies <- vector("list", length(PAYTYPE_check_list97to16))
for (i in seq_along(PAYTYPE_check_list97to16)) {
  frequencies[[i]] <- freq(PAYTYPE_check_list97to16[[i]]$PAYTYPER)
}
for (i in seq_along(PAYTYPE_check_list97to16)) {
  year <- 1996 + i
  cat("Frequencies for nam", year, ":\n")
  print(frequencies[[i]])
  cat("\n")
}


############################################################################

###creating function [create_survey_design()] to apply survey weights to all data sets
create_survey_design <- function(data) {
  design <- svydesign(id = ~CPSUM,
                      strata  = ~CSTRATM,
                      weights = ~PATWT,
                      nest    = TRUE,
                      data    = data)
  return(design)
}

#applying function to all variables to generate survey object for each namxx_filtered_gen
#namxx_survey: survey object of namxx_filtered_gen
library(survey)
nam97_survey <- create_survey_design(nam97_filtered_gen)
nam98_survey <- create_survey_design(nam98_filtered_gen)
nam99_survey <- create_survey_design(nam99_filtered_gen)
nam00_survey <- create_survey_design(nam00_filtered_gen)
nam01_survey <- create_survey_design(nam01_filtered_gen)
nam02_survey <- create_survey_design(nam02_filtered_gen)
nam03_survey <- create_survey_design(nam03_filtered_gen)
nam04_survey <- create_survey_design(nam04_filtered_gen)
nam05_survey <- create_survey_design(nam05_filtered_gen)
nam06_survey <- create_survey_design(nam06_filtered_gen)
nam07_survey <- create_survey_design(nam07_filtered_gen)
nam08_survey <- create_survey_design(nam08_filtered_gen)
nam09_survey <- create_survey_design(nam09_filtered_gen)
nam10_survey <- create_survey_design(nam10_filtered_gen)
nam11_survey <- create_survey_design(nam11_filtered_gen)
nam12_survey <- create_survey_design(nam12_filtered_gen)
nam13_survey <- create_survey_design(nam13_filtered_gen)
nam14_survey <- create_survey_design(nam14_filtered_gen)
nam15_survey <- create_survey_design(nam15_filtered_gen)
nam16_survey <- create_survey_design(nam16_filtered_gen)

#############################################################
calculate_svymeans <- function(data, variables) {
  means <- list()
  for (var in variables) {
    mean <- svymean(as.formula(paste0("~", var)), data, na.rm = TRUE)
    conf_interval <- confint(mean, level = 0.95)
    means[[var]] <- list(mean = mean, conf_interval = conf_interval)
  }
  return(means)
}

svymeans_variables <- c("PEDGEN", "NONPEDGEN", "PEDSPEC", "NONPEDSPEC")
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
#applying function to calculate survey means for each year 1997 - 2016
svymeans_nam97 <- calculate_svymeans(nam97_survey, svymeans_variables)
svymeans_nam98 <- calculate_svymeans(nam98_survey, svymeans_variables)
svymeans_nam99 <- calculate_svymeans(nam99_survey, svymeans_variables)
svymeans_nam00 <- calculate_svymeans(nam00_survey, svymeans_variables)
svymeans_nam01 <- calculate_svymeans(nam01_survey, svymeans_variables)
svymeans_nam02 <- calculate_svymeans(nam02_survey, svymeans_variables)
svymeans_nam03 <- calculate_svymeans(nam03_survey, svymeans_variables)
svymeans_nam04 <- calculate_svymeans(nam04_survey, svymeans_variables)
svymeans_nam05 <- calculate_svymeans(nam05_survey, svymeans_variables)
svymeans_nam06 <- calculate_svymeans(nam06_survey, svymeans_variables)
svymeans_nam07 <- calculate_svymeans(nam07_survey, svymeans_variables)
svymeans_nam08 <- calculate_svymeans(nam08_survey, svymeans_variables)
svymeans_nam09 <- calculate_svymeans(nam09_survey, svymeans_variables)
svymeans_nam10 <- calculate_svymeans(nam10_survey, svymeans_variables)
svymeans_nam11 <- calculate_svymeans(nam11_survey, svymeans_variables)
svymeans_nam12 <- calculate_svymeans(nam12_survey, svymeans_variables)
svymeans_nam13 <- calculate_svymeans(nam13_survey, svymeans_variables)
svymeans_nam14 <- calculate_svymeans(nam14_survey, svymeans_variables)
svymeans_nam15 <- calculate_svymeans(nam15_survey, svymeans_variables)
svymeans_nam16 <- calculate_svymeans(nam16_survey, svymeans_variables)

svymeans_df_byvar <- function(svymeans_obj, prefix) {
  variable_names <- names(svymeans_obj)
  for (variable in variable_names) {
    variable_means <- svymeans_obj[[variable]]
    variable_df <- as.data.frame(variable_means)
    transposed_df <- as.data.frame(t(variable_df))
    rownames(transposed_df) <- c("mean", "SE", "lowerCI", "upperCI")
    output_variable_name <- paste(prefix, variable, sep = "_")
    assign(output_variable_name, transposed_df, envir = .GlobalEnv)
  }
}

svymeans_df_byvar(svymeans_nam97, "svymeans_nam97")
svymeans_df_byvar(svymeans_nam98, "svymeans_nam98")
svymeans_df_byvar(svymeans_nam99, "svymeans_nam99")
svymeans_df_byvar(svymeans_nam00, "svymeans_nam00")
svymeans_df_byvar(svymeans_nam01, "svymeans_nam01")
svymeans_df_byvar(svymeans_nam02, "svymeans_nam02")
svymeans_df_byvar(svymeans_nam03, "svymeans_nam03")
svymeans_df_byvar(svymeans_nam04, "svymeans_nam04")
svymeans_df_byvar(svymeans_nam05, "svymeans_nam05")
svymeans_df_byvar(svymeans_nam06, "svymeans_nam06")
svymeans_df_byvar(svymeans_nam07, "svymeans_nam07")
svymeans_df_byvar(svymeans_nam08, "svymeans_nam08")
svymeans_df_byvar(svymeans_nam09, "svymeans_nam09")
svymeans_df_byvar(svymeans_nam10, "svymeans_nam10")
svymeans_df_byvar(svymeans_nam11, "svymeans_nam11")
svymeans_df_byvar(svymeans_nam12, "svymeans_nam12")
svymeans_df_byvar(svymeans_nam13, "svymeans_nam13")
svymeans_df_byvar(svymeans_nam14, "svymeans_nam14")
svymeans_df_byvar(svymeans_nam15, "svymeans_nam15")
svymeans_df_byvar(svymeans_nam16, "svymeans_nam16")

library(dplyr)

###1. PEDGEN: create and apply a function to create a data frame for PEDGEN, with means and SE from 1997 to 2016
combined_svymeans_years <- c("97", "98", "99", "00", "01", "02", "03", "04",
                             "05","06","07","08","09","10","11","12","13","14","15","16")

combined9716_df_svymeans_PEDGEN <- data.frame()
for (year in combined_svymeans_years) {
  df_name_svy1 <- paste0("svymeans_nam", year, "_PEDGEN")
  current_df_svy1 <- get(df_name_svy1)
  
  # Extract only the columns for PEDGEN0 and PEDGEN1
  selected_cols_svy1 <- c("PEDGEN0", "PEDGEN1")
  current_df_svy1 <- current_df_svy1[, selected_cols_svy1]
  
  combined9716_df_svymeans_PEDGEN <- rbind(combined9716_df_svymeans_PEDGEN, current_df_svy1)
}

###write a function to rename observation names for PEDGEN according to years
replace_row_names_PEDGEN <- function(data_frame, replacements) {
  for (original_row in names(replacements)) {
    row.names(data_frame)[row.names(data_frame) == original_row] <- replacements[original_row]
  }
  data_frame
}

replacements <- c("mean" = "mean1997", "SE" = "SE1997", "mean1" = "mean1998", "SE1" = "SE1998", 
                  "mean2" = "mean1999", "SE2" = "SE1999", "mean3" = "mean2000", "SE3" = "SE2000",
                  "mean4" = "mean2001", "SE4" = "SE2001", "mean5" = "mean2002", "SE5" = "SE2002",
                  "mean6" = "mean2003", "SE6" = "SE2003", "mean7" = "mean2004", "SE7" = "SE2004",
                  "mean8" = "mean2005", "SE8" = "SE2005", "mean9" = "mean2006", "SE9" = "SE2006",
                  "mean10" = "mean2007", "SE10" = "SE2007", "mean11" = "mean2008", "SE11" = "SE2008",
                  "mean12" = "mean2009", "SE12" = "SE2009", "mean13" = "mean2010", "SE13" = "SE2010",
                  "mean14" = "mean2011", "SE14" = "SE2011", "mean15" = "mean2012", "SE15" = "SE2012",
                  "mean16" = "mean2013", "SE16" = "SE2013", "mean17" = "mean2014", "SE17" = "SE2014",
                  "mean18" = "mean2015", "SE18" = "SE2015",
                  "mean19" = "mean2016", "SE19" = "SE2016", "upperCI" = "upperCI1997", "lowerCI" = "lowerCI1997",
                  "upperCI1" = "upperCI1998", "lowerCI1" ="lowerCI1998", "upperCI2" = "upperCI1999",
                  "lowerCI2" = "lowerCI1999", "upperCI3" = "upperCI2000","lowerCI3" = "lowerCI2000",
                  "upperCI4" = "upperCI2001", "lowerCI4" ="lowerCI2001", "upperCI5" = "upperCI2002",
                  "lowerCI5" = "lowerCI2002", "upperCI6" = "upperCI2003", "lowerCI6" = "lowerCI2003",
                  "upperCI7" = "upperCI2004", "lowerCI7"= "lowerCI2004", "upperCI8" = "upperCI2005",
                  "lowerCI8" = "lowerCI2005", "upperCI9" = "upperCI2006", "lowerCI9"= "lowerCI2006",
                  "upperCI10" = "upperCI2007", "lowerCI10" = "lowerCI2007", "upperCI11" = "upperCI2008",
                  "lowerCI11"="lowerCI2008","upperCI12" = "upperCI2009", "lowerCI12" = "lowerCI2009",
                  "upperCI13" = "upperCI2010", "lowerCI13"="lowerCI2010","upperCI14"="upperCI2011",
                  "lowerCI14"="lowerCI2011","upperCI15"="upperCI2012", "lowerCI15"="lowerCI2012",
                  "upperCI16"="upperCI2013","lowerCI16"="lowerCI2013","upperCI17"="upperCI2014",
                  "lowerCI17"="lowerCI2014","upperCI18"="upperCI2015","lowerCI18"="lowerCI2015",
                  "upperCI19"="upperCI2016","lowerCI19"="lowerCI2016")

combined9716_df_svymeans_PEDGEN <- replace_row_names_PEDGEN(combined9716_df_svymeans_PEDGEN, replacements)

###2. NONPEDGEN: create and apply a function to create a data frame for NONPEDGEN, with means and SE from 1997 to 2016
combined9716_df_svymeans_NONPEDGEN <- data.frame()
for (year in combined_svymeans_years) {
  df_name_svy1 <- paste0("svymeans_nam", year, "_NONPEDGEN") #!
  current_df_svy1 <- get(df_name_svy1)
  
  # Extract only the columns for NONPEDGEN0 and NONPEDGEN1
  selected_cols_svy1 <- c("NONPEDGEN0", "NONPEDGEN1") #!
  current_df_svy1 <- current_df_svy1[, selected_cols_svy1]
  
  combined9716_df_svymeans_NONPEDGEN <- rbind(combined9716_df_svymeans_NONPEDGEN, current_df_svy1)
}

###write a function to rename observation names for NONPEDGEN according to years
replace_row_names_NONPEDGEN <- function(data_frame, replacements) {
  for (original_row in names(replacements)) {
    row.names(data_frame)[row.names(data_frame) == original_row] <- replacements[original_row]
  }
  data_frame
}

combined9716_df_svymeans_NONPEDGEN <- replace_row_names_NONPEDGEN(combined9716_df_svymeans_NONPEDGEN, replacements)

###3. PEDSPEC: create and apply a function to create a data frame for PEDSPEC, with means and SE from 1997 to 2004
combined9716_df_svymeans_PEDSPEC <- data.frame()
for (year in combined_svymeans_years) {
  df_name_svy1 <- paste0("svymeans_nam", year, "_PEDSPEC") #!
  current_df_svy1 <- get(df_name_svy1)
  
  # Extract only the columns for PEDSPEC0 and PEDSPEC1
  selected_cols_svy1 <- c("PEDSPEC0", "PEDSPEC1") #!
  current_df_svy1 <- current_df_svy1[, selected_cols_svy1]
  
  combined9716_df_svymeans_PEDSPEC <- rbind(combined9716_df_svymeans_PEDSPEC, current_df_svy1)
}

###write a function to rename observation names for PEDSPEC according to years
replace_row_names_PEDSPEC <- function(data_frame, replacements) {
  for (original_row in names(replacements)) {
    row.names(data_frame)[row.names(data_frame) == original_row] <- replacements[original_row]
  }
  data_frame
}

combined9716_df_svymeans_PEDSPEC <- replace_row_names_PEDSPEC(combined9716_df_svymeans_PEDSPEC, replacements)

###4. NONPEDSPEC: create and apply a function to create a data frame for NONPEDSPEC, with means and SE from 1997 to 2016
combined9716_df_svymeans_NONPEDSPEC <- data.frame() #!
for (year in combined_svymeans_years) {
  df_name_svy1 <- paste0("svymeans_nam", year, "_NONPEDSPEC") #!
  current_df_svy1 <- get(df_name_svy1)
  
  # Extract only the columns for NONPEDSPEC0 and NONPEDSPEC1
  selected_cols_svy1 <- c("NONPEDSPEC0", "NONPEDSPEC1") #!
  current_df_svy1 <- current_df_svy1[, selected_cols_svy1]
  
  combined9716_df_svymeans_NONPEDSPEC <- rbind(combined9716_df_svymeans_NONPEDSPEC, current_df_svy1)
}

###write a function to rename observation names for NONPEDSPEC according to years
replace_row_names_NONPEDSPEC <- function(data_frame, replacements) {
  for (original_row in names(replacements)) {
    row.names(data_frame)[row.names(data_frame) == original_row] <- replacements[original_row]
  }
  data_frame
}

combined9716_df_svymeans_NONPEDSPEC <- replace_row_names_NONPEDSPEC(combined9716_df_svymeans_NONPEDSPEC, replacements)

####

# Filter rows based on the specific strings and create separate data frames
combined9716_df_MEAN_PEDGEN <- combined9716_df_svymeans_PEDGEN[grepl("mean", rownames(combined9716_df_svymeans_PEDGEN)), ]
combined9716_df_SE_PEDGEN <- combined9716_df_svymeans_PEDGEN[grepl("SE", rownames(combined9716_df_svymeans_PEDGEN)), ]
combined9716_df_lowerCI_PEDGEN <- combined9716_df_svymeans_PEDGEN[grepl("lowerCI", rownames(combined9716_df_svymeans_PEDGEN)), ]
combined9716_df_upperCI_PEDGEN <- combined9716_df_svymeans_PEDGEN[grepl("upperCI", rownames(combined9716_df_svymeans_PEDGEN)), ]

combined9716_df_MEAN_NONPEDGEN <- combined9716_df_svymeans_NONPEDGEN[grepl("mean", rownames(combined9716_df_svymeans_NONPEDGEN)), ]
combined9716_df_SE_NONPEDGEN <- combined9716_df_svymeans_NONPEDGEN[grepl("SE", rownames(combined9716_df_svymeans_NONPEDGEN)), ]
combined9716_df_lowerCI_NONPEDGEN <- combined9716_df_svymeans_NONPEDGEN[grepl("lowerCI", rownames(combined9716_df_svymeans_NONPEDGEN)), ]
combined9716_df_upperCI_NONPEDGEN <- combined9716_df_svymeans_NONPEDGEN[grepl("upperCI", rownames(combined9716_df_svymeans_NONPEDGEN)), ]

combined9716_df_MEAN_PEDSPEC <- combined9716_df_svymeans_PEDSPEC[grepl("mean", rownames(combined9716_df_svymeans_PEDSPEC)), ]
combined9716_df_SE_PEDSPEC <- combined9716_df_svymeans_PEDSPEC[grepl("SE", rownames(combined9716_df_svymeans_PEDSPEC)), ]
combined9716_df_lowerCI_PEDSPEC <- combined9716_df_svymeans_PEDSPEC[grepl("lowerCI", rownames(combined9716_df_svymeans_PEDSPEC)), ]
combined9716_df_upperCI_PEDSPEC <- combined9716_df_svymeans_PEDSPEC[grepl("upperCI", rownames(combined9716_df_svymeans_PEDSPEC)), ]

combined9716_df_MEAN_NONPEDSPEC <- combined9716_df_svymeans_NONPEDSPEC[grepl("mean", rownames(combined9716_df_svymeans_NONPEDSPEC)), ]
combined9716_df_SE_NONPEDSPEC <- combined9716_df_svymeans_NONPEDSPEC[grepl("SE", rownames(combined9716_df_svymeans_NONPEDSPEC)), ]
combined9716_df_lowerCI_NONPEDSPEC <- combined9716_df_svymeans_NONPEDSPEC[grepl("lowerCI", rownames(combined9716_df_svymeans_NONPEDSPEC)), ]
combined9716_df_upperCI_NONPEDSPEC <- combined9716_df_svymeans_NONPEDSPEC[grepl("upperCI", rownames(combined9716_df_svymeans_NONPEDSPEC)), ]

###merging for combined 93-16 mean, SE, upper CI, lower CI data frames
merged_9716_mean <- do.call(cbind, list(combined9716_df_MEAN_PEDGEN, combined9716_df_MEAN_NONPEDGEN, 
                                        combined9716_df_MEAN_PEDSPEC,combined9716_df_MEAN_NONPEDSPEC))

merged_9716_SE <- do.call(cbind, list(combined9716_df_SE_PEDGEN, combined9716_df_SE_NONPEDGEN, 
                                      combined9716_df_SE_PEDSPEC,combined9716_df_SE_NONPEDSPEC))

merged_9716_lowerCI <- do.call(cbind, list(combined9716_df_lowerCI_PEDGEN, combined9716_df_lowerCI_NONPEDGEN, 
                                           combined9716_df_lowerCI_PEDSPEC,combined9716_df_lowerCI_NONPEDSPEC))

merged_9716_upperCI <- do.call(cbind, list(combined9716_df_upperCI_PEDGEN, combined9716_df_upperCI_NONPEDGEN, 
                                           combined9716_df_upperCI_PEDSPEC,combined9716_df_upperCI_NONPEDSPEC))

###keep only VAR01(1,3,5,7 variables) variable for PEDGEN, NONPEDGEN, PEDSPEC, and NONPEDSPEC in combined data frame
keepNthColumns <- function(data, columns) {
  return(data[, columns, drop = FALSE])
}

#mean
merged_9716_mean_01<-keepNthColumns(merged_9716_mean, c(2,4,6,8))
# add year variable, multiply by 100
merged_9716_mean_01$year <- 1997 + seq_len(nrow(merged_9716_mean_01)) - 1

merged_9716_mean_01$PEDGEN1 <- merged_9716_mean_01$PEDGEN1 * 100
merged_9716_mean_01$NONPEDGEN1 <- merged_9716_mean_01$NONPEDGEN1 * 100
merged_9716_mean_01$PEDSPEC1 <- merged_9716_mean_01$PEDSPEC1 * 100
merged_9716_mean_01$NONPEDSPEC1 <- merged_9716_mean_01$NONPEDSPEC1 * 100

#SE
merged_9716_SE_01<-keepNthColumns(merged_9716_SE, c(2,4,6,8)) 

merged_9716_SE_01$year <- 1997 + seq_len(nrow(merged_9716_SE_01)) - 1

merged_9716_SE_01$PEDGEN1 <- merged_9716_SE_01$PEDGEN1 * 100
merged_9716_SE_01$NONPEDGEN1 <- merged_9716_SE_01$NONPEDGEN1 * 100
merged_9716_SE_01$PEDSPEC1 <- merged_9716_SE_01$PEDSPEC1 * 100
merged_9716_SE_01$NONPEDSPEC1 <- merged_9716_SE_01$NONPEDSPEC1 * 100

#lower CI
merged_9716_lowerCI_01<-keepNthColumns(merged_9716_lowerCI, c(2,4,6,8))

merged_9716_lowerCI_01$year <- 1997 + seq_len(nrow(merged_9716_lowerCI_01)) - 1

merged_9716_lowerCI_01$PEDGEN1 <- merged_9716_lowerCI_01$PEDGEN1 * 100
merged_9716_lowerCI_01$NONPEDGEN1 <- merged_9716_lowerCI_01$NONPEDGEN1 * 100
merged_9716_lowerCI_01$PEDSPEC1 <- merged_9716_lowerCI_01$PEDSPEC1 * 100
merged_9716_lowerCI_01$NONPEDSPEC1 <- merged_9716_lowerCI_01$NONPEDSPEC1 * 100

#upper CI
merged_9716_upperCI_01<-keepNthColumns(merged_9716_upperCI, c(2,4,6,8))

merged_9716_upperCI_01$year <- 1997 + seq_len(nrow(merged_9716_upperCI_01)) - 1

merged_9716_upperCI_01$PEDGEN1 <- merged_9716_upperCI_01$PEDGEN1 * 100
merged_9716_upperCI_01$NONPEDGEN1 <- merged_9716_upperCI_01$NONPEDGEN1 * 100
merged_9716_upperCI_01$PEDSPEC1 <- merged_9716_upperCI_01$PEDSPEC1 * 100
merged_9716_upperCI_01$NONPEDSPEC1 <- merged_9716_upperCI_01$NONPEDSPEC1 * 100

library(reshape2) 
merged_9716_mean_02 <- merged_9716_mean_01[merged_9716_mean_01$year != 2014, ]  #exclude 2014 as row does not add up to 100
merged_9716_mean_long_02 <- melt(merged_9716_mean_02, id.vars = "year")  
merged_9716_mean_long_02 <- merged_9716_mean_long_02 %>%
  rename(mean = value)

merged_9716_upperCI_02 <- merged_9716_upperCI_01[merged_9716_upperCI_01$year != 2014, ]  #exclude 2014 as row does not add up to 100
merged_9716_upperCI_long_02 <- melt(merged_9716_upperCI_02, id.vars = "year") 
merged_9716_upperCI_long_02 <- merged_9716_upperCI_long_02 %>%
  rename(upperCI = value)

merged_9716_lowerCI_02 <- merged_9716_lowerCI_01[merged_9716_lowerCI_01$year != 2014, ]  #exclude 2014 as row does not add up to 100
merged_9716_lowerCI_long_02 <- melt(merged_9716_lowerCI_02, id.vars = "year") 
merged_9716_lowerCI_long_02 <- merged_9716_lowerCI_long_02 %>%
  rename(lowerCI = value)

mean_CI_df <- merge(merged_9716_upperCI_long_02, merged_9716_lowerCI_long_02)
mean_CI_df <- merge(merged_9716_mean_long_02, mean_CI_df)
#library(readr)
#write_csv(mean_CI_df, "C:/Users/aleong/Desktop/NAMCS/Replication/mean_CI_df.csv")

mean_CI_df$lowerCI_positive <- ifelse(mean_CI_df$lowerCI< 0, 0, mean_CI_df$lowerCI)
mean_CI_df$variable <- factor(mean_CI_df$variable , levels = c("PEDGEN1", "NONPEDGEN1", "PEDSPEC1", "NONPEDSPEC1"))

row_sums_02 <- as.data.frame(rowSums(merged_9716_mean_02[, 1:4]))
print(row_sums_02) #check that all rows add up to 100

library(ggplot2) 

ggplot(mean_CI_df, aes(x = year, y = mean, col = variable, shape = variable, fill=variable)) + #remove fill if grey
  geom_point(size = 1.5) +
  geom_smooth(method = "loess", se=FALSE, size=0.25) + #local regression 
  scale_x_continuous(breaks = seq(min(mean_CI_df$year), max(mean_CI_df$year), by = 2)) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Year", y = "Proportion of visits to children <18y, \n Medicaid & CHIP only(%)", col = "Variable", shape = "Variable") +
  theme(legend.title = element_blank(), legend.position = "top",
        panel.background = element_rect(fill = "white"),
        panel.grid.major.y = element_line(color = "grey", linetype = 3),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "black", fill = NA),
        axis.line = element_line(color = "black"),
        axis.ticks = element_line(color = "black")) +
  scale_color_manual(values = c("blue", "red2", "purple1", "darkgreen"), labels = c("Pediatric \nGeneralists", "Non-Pediatric \nGeneralists", "Pediatric \nSpecialists", "Non-Pediatric \nSpecialists")) +
  scale_shape_manual(values = c(18, 15, 16, 17), labels = c("Pediatric \nGeneralists", "Non-Pediatric \nGeneralists", "Pediatric \nSpecialists", "Non-Pediatric \nSpecialists")) +
  scale_fill_manual(values = c("blue", "red2", "purple1", "darkgreen"), labels = NULL, guide = "none") +
  geom_ribbon(aes(ymin = lowerCI_positive, ymax = upperCI), alpha = 0.06, color = NA) 

#dotted line on graph is where Freed's paper ends (2006)

#########################################################

#renaming variables in SE data frame
library(dplyr)
merged_9716_SE_01_edit <- merged_9716_SE_01 %>%
  rename(PEDGEN1_SE = PEDGEN1,
         NONPEDGEN1_SE = NONPEDGEN1,
         PEDSPEC1_SE = PEDSPEC1,
         NONPEDSPEC1_SE = NONPEDSPEC1)

#renaming variables in lower CI data frame
merged_9716_lowerCI_01_edit <- merged_9716_lowerCI_01 %>%
  rename(PEDGEN1_lowerCI = PEDGEN1,
         NONPEDGEN1_lowerCI = NONPEDGEN1,
         PEDSPEC1_lowerCI = PEDSPEC1,
         NONPEDSPEC1_lowerCI = NONPEDSPEC1)

#renaming variables in upper CI data frame
merged_9716_upperCI_01_edit <- merged_9716_upperCI_01 %>%
  rename(PEDGEN1_upperCI = PEDGEN1,
         NONPEDGEN1_upperCI = NONPEDGEN1,
         PEDSPEC1_upperCI = PEDSPEC1,
         NONPEDSPEC1_upperCI = NONPEDSPEC1)

#combine mean and SE data frames
merged_9716_ALL <- cbind(merged_9716_mean_01, merged_9716_SE_01_edit[match(merged_9716_mean_01$year, merged_9716_SE_01_edit$year), ])
#append lower CI
merged_9716_ALL <- cbind(merged_9716_ALL, merged_9716_lowerCI_01_edit[match(merged_9716_ALL$year, merged_9716_lowerCI_01_edit$year), ])
#append upper CI
merged_9716_ALL <- cbind(merged_9716_ALL, merged_9716_upperCI_01_edit[match(merged_9716_ALL$year, merged_9716_upperCI_01_edit$year), ])

#subset by pedtype
PEDGEN1_9716_ci <- merged_9716_ALL[, c(1, 5, 6, 11, 16)] #PEDGEN1
PEDGEN1_9716_ci <- PEDGEN1_9716_ci %>%
  mutate (percent_SE = PEDGEN1_SE / PEDGEN1 * 100)

NONPEDGEN1_9716_ci <- merged_9716_ALL[, c(2, 5, 7, 12, 17)] #NONPEDGEN1
NONPEDGEN1_9716_ci <- NONPEDGEN1_9716_ci %>%
  mutate (percent_SE = NONPEDGEN1_SE / NONPEDGEN1 * 100)

PEDSPEC1_9716_ci <- merged_9716_ALL[, c(3, 5, 8, 13, 18)] #PEDSPEC1
PEDSPEC1_9716_ci <- PEDSPEC1_9716_ci %>%
  mutate (percent_SE = PEDSPEC1_SE / PEDSPEC1 * 100)

NONPEDSPEC1_9716_ci <- merged_9716_ALL[, c(4, 5, 9, 14, 19)] #NONPEDSPEC1
NONPEDSPEC1_9716_ci <- NONPEDSPEC1_9716_ci %>%
  mutate (percent_SE = NONPEDSPEC1_SE / NONPEDSPEC1 * 100)

#end