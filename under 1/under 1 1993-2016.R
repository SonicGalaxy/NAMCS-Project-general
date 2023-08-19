#reference paper: https://pubmed.ncbi.nlm.nih.gov/20227714/

library(haven)
#import NAMCS stata files from 1993 to 2016 
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
  df <- get(df_name) 
  colnames(df) <- toupper(colnames(df))  # Convert column names to uppercase
  assign(df_name, df, envir = .GlobalEnv)  
}

for (df_name in data_frame_names) {
  df <- get(df_name) 
  #df <- df[df$AGE <18, ] #only include observations where patient age <18
  df <- df[df$AGE <1, ] #only include observations where patient age <1
  assign(df_name, df, envir = .GlobalEnv) 
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
  return(dataset)
}

library(dplyr)

#create SPECCAT for each dataset only for data sets prior to 2005
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
#nam93_filtered <- nam93_repl[nam93_repl$SPECR != 15, ]

### write a function [create_specialty_variables()] to create PEDGEN, NONPEDGEN, PEDSPEC, NONPEDSPEC for any data set
create_specialty_variables <- function(data) {
  data <- data[data$SPECCAT != 2, ] #excluding surgical specialties
  #data_filtered <- data_repl[data$SPECR != 15, ]
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
#that other survey year use. create a unique create_specialty_variables() function for nam12
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


#applying function on all data sets to generate specialty variables from 1993 to 2016 (PEDGEN, NONPEDGEN,
# PEDSPEC, NONPEDSPEC)

#namxx_filtered_gen: under18 only, speccat variables generated from spec, speccat obs == 2 (medical removed),
#specialty variables created
nam93_filtered_gen <- create_specialty_variables(nam93_repl) 
nam94_filtered_gen <- create_specialty_variables(nam94_repl)
nam95_filtered_gen <- create_specialty_variables(nam95_repl)
nam96_filtered_gen <- create_specialty_variables(nam96_repl)
nam97_filtered_gen <- create_specialty_variables(nam97_repl)
nam98_filtered_gen <- create_specialty_variables(nam98_repl)
nam99_filtered_gen <- create_specialty_variables(nam99_repl)
nam00_filtered_gen <- create_specialty_variables(nam00_repl)
nam01_filtered_gen <- create_specialty_variables(nam01_repl)
nam02_filtered_gen <- create_specialty_variables(nam02_repl)
nam03_filtered_gen <- create_specialty_variables(nam03_repl)
nam04_filtered_gen <- create_specialty_variables(nam04_repl)
nam05_filtered_gen <- create_specialty_variables(nam05) #nam05 onwards has SPECCAT
nam06_filtered_gen <- create_specialty_variables(nam06)
nam07_filtered_gen <- create_specialty_variables(nam07)
nam08_filtered_gen <- create_specialty_variables(nam08)
nam09_filtered_gen <- create_specialty_variables(nam09)
nam10_filtered_gen <- create_specialty_variables(nam10)
nam11_filtered_gen <- create_specialty_variables(nam11)
nam13_filtered_gen <- create_specialty_variables(nam13)
nam14_filtered_gen <- create_specialty_variables(nam14)
nam15_filtered_gen <- create_specialty_variables(nam15)
nam16_filtered_gen <- create_specialty_variables(nam16)

nam12_filtered_gen <- create_specialty_variables_nam12(nam12)

#nam14 is a problem, to be excluded later
freq(nam14$SPECCAT) #coding issue, codes do not correspond with codebook
freq(nam14$SPECR) #coding issue, codes do not correspond with codebook

##summary statistics for all tables
nam_list_9316 <- list (nam93_filtered_gen, nam94_filtered_gen, nam95_filtered_gen,
                       nam96_filtered_gen, nam97_filtered_gen, nam98_filtered_gen,
                       nam99_filtered_gen, nam00_filtered_gen, nam01_filtered_gen,
                       nam02_filtered_gen, nam03_filtered_gen, nam04_filtered_gen,
                       nam05_filtered_gen, nam06_filtered_gen, nam07_filtered_gen,
                       nam08_filtered_gen, nam09_filtered_gen, nam10_filtered_gen,
                       nam11_filtered_gen, nam12_filtered_gen, nam13_filtered_gen,
                       nam14_filtered_gen, nam15_filtered_gen, nam16_filtered_gen
)

#PEDGEN summary statistics
library(tidyr)
freq_PEDGEN_df_nam_9316 <- data.frame(variable = character(),
                                      value = character(),
                                      frequency = numeric(),
                                      percent = numeric(),
                                      dataframename = character(),
                                      stringsAsFactors = FALSE)
for (i in seq_along(nam_list_9316)) {
  year <- 1992 + i
  freq_table <- table(nam_list_9316[[i]]$PEDGEN)
  variable <- "PEDGEN"
  value <- names(freq_table)
  frequency <- as.numeric(freq_table)
  percent <- frequency / sum(frequency) * 100
  df_PEDGEN <- data.frame(variable = variable,
                          value = value,
                          frequency = frequency,
                          percent = percent,
                          dataframename = paste0("nam", year),
                          stringsAsFactors = FALSE)
  freq_PEDGEN_df_nam_9316<- rbind(freq_PEDGEN_df_nam_9316, df_PEDGEN)
}

freq_PEDGEN_df_nam_9316_total <- freq_PEDGEN_df_nam_9316 %>%
  group_by(dataframename) %>%
  summarise(totalobs_PEDGEN = sum(frequency))

freq_PEDGEN_df_nam_9316_wide <- freq_PEDGEN_df_nam_9316 %>%
  pivot_wider(
    id_cols = dataframename,
    names_from = value,
    values_from = c(frequency, percent)
  ) %>%
  rename(freq_0 = frequency_0,
         freq_1 = frequency_1,
         percent_0 = percent_0,
         percent_1 = percent_1)
freq_PEDGEN_total <- inner_join(freq_PEDGEN_df_nam_9316_total, freq_PEDGEN_df_nam_9316_wide, by = "dataframename")
descr(freq_PEDGEN_total) # totalobs PEDGEN = total observations for age condition in each df
max(freq_PEDGEN_total$totalobs_PEDGEN)
max_observation <- max(freq_PEDGEN_total$totalobs_PEDGEN)
max_row <- freq_PEDGEN_total[freq_PEDGEN_total$totalobs_PEDGEN == max_observation, ]
max_year <- max_row$dataframename
print(max_year)

min(freq_PEDGEN_total$totalobs_PEDGEN)
min_observation <- min(freq_PEDGEN_total$totalobs_PEDGEN)
min_row <- freq_PEDGEN_total[freq_PEDGEN_total$totalobs_PEDGEN == min_observation, ]
min_year <- min_row$dataframename
print(min_year)

#NONPEDGEN summary statistics
freq_NONPEDGEN_df_nam_9316 <- data.frame(variable = character(),
                                         value = character(),
                                         frequency = numeric(),
                                         percent = numeric(),
                                         dataframename = character(),
                                         stringsAsFactors = FALSE)

for (i in seq_along(nam_list_9316)) {
  year <- 1992 + i
  freq_table <- table(nam_list_9316[[i]]$NONPEDGEN)
  variable <- "NONPEDGEN"
  value <- names(freq_table)
  frequency <- as.numeric(freq_table)
  percent <- frequency / sum(frequency) * 100
  df_NONPEDGEN <- data.frame(variable = variable,
                             value = value,
                             frequency = frequency,
                             percent = percent,
                             dataframename = paste0("nam", year),
                             stringsAsFactors = FALSE)
  freq_NONPEDGEN_df_nam_9316<- rbind(freq_NONPEDGEN_df_nam_9316, df_NONPEDGEN)
}

freq_NONPEDGEN_df_nam_9316_total <- freq_NONPEDGEN_df_nam_9316 %>%
  group_by(dataframename) %>%
  summarise(totalobs_NONPEDGEN = sum(frequency))

freq_NONPEDGEN_df_nam_9316_wide <- freq_NONPEDGEN_df_nam_9316 %>%
  pivot_wider(
    id_cols = dataframename,
    names_from = value,
    values_from = c(frequency, percent)
  ) %>%
  rename(freq_0 = frequency_0,
         freq_1 = frequency_1,
         percent_0 = percent_0,
         percent_1 = percent_1)
freq_NONPEDGEN_total <- inner_join(freq_NONPEDGEN_df_nam_9316_total, freq_NONPEDGEN_df_nam_9316_wide, by = "dataframename")
descr(freq_NONPEDGEN_total)

#PEDSPEC summary statistics
freq_PEDSPEC_df_nam_9316 <- data.frame(variable = character(),
                                       value = character(),
                                       frequency = numeric(),
                                       percent = numeric(),
                                       dataframename = character(),
                                       stringsAsFactors = FALSE)
for (i in seq_along(nam_list_9316)) {
  year <- 1992 + i
  freq_table <- table(nam_list_9316[[i]]$PEDSPEC)
  variable <- "PEDSPEC"
  value <- names(freq_table)
  frequency <- as.numeric(freq_table)
  percent <- frequency / sum(frequency) * 100
  df_PEDSPEC <- data.frame(variable = variable,
                           value = value,
                           frequency = frequency,
                           percent = percent,
                           dataframename = paste0("nam", year),
                           stringsAsFactors = FALSE)
  freq_PEDSPEC_df_nam_9316<- rbind(freq_PEDSPEC_df_nam_9316, df_PEDSPEC)
}

freq_PEDSPEC_df_nam_9316_total <- freq_PEDSPEC_df_nam_9316 %>%
  group_by(dataframename) %>%
  summarise(totalobs_PEDSPEC = sum(frequency))

freq_PEDSPEC_df_nam_9316_wide <- freq_PEDSPEC_df_nam_9316 %>%
  pivot_wider(
    id_cols = dataframename,
    names_from = value,
    values_from = c(frequency, percent)
  ) %>%
  rename(freq_0 = frequency_0,
         freq_1 = frequency_1,
         percent_0 = percent_0,
         percent_1 = percent_1)
freq_PEDSPEC_total <- inner_join(freq_PEDSPEC_df_nam_9316_total, freq_PEDSPEC_df_nam_9316_wide, by = "dataframename")
descr(freq_PEDSPEC_total)

#NONPEDSPEC summary statistics
freq_NONPEDSPEC_df_nam_9316 <- data.frame(variable = character(),
                                          value = character(),
                                          frequency = numeric(),
                                          percent = numeric(),
                                          dataframename = character(),
                                          stringsAsFactors = FALSE)
for (i in seq_along(nam_list_9316)) {
  year <- 1992 + i
  freq_table <- table(nam_list_9316[[i]]$NONPEDSPEC)
  variable <- "NONPEDSPEC"
  value <- names(freq_table)
  frequency <- as.numeric(freq_table)
  percent <- frequency / sum(frequency) * 100
  df_NONPEDSPEC <- data.frame(variable = variable,
                              value = value,
                              frequency = frequency,
                              percent = percent,
                              dataframename = paste0("nam", year),
                              stringsAsFactors = FALSE)
  freq_NONPEDSPEC_df_nam_9316<- rbind(freq_NONPEDSPEC_df_nam_9316, df_NONPEDSPEC)
}

freq_NONPEDSPEC_df_nam_9316_total <- freq_NONPEDSPEC_df_nam_9316 %>%
  group_by(dataframename) %>%
  summarise(totalobs_NONPEDSPEC = sum(frequency))

freq_NONPEDSPEC_df_nam_9316_wide <- freq_NONPEDSPEC_df_nam_9316 %>%
  pivot_wider(
    id_cols = dataframename,
    names_from = value,
    values_from = c(frequency, percent)
  ) %>%
  rename(freq_0 = frequency_0,
         freq_1 = frequency_1,
         percent_0 = percent_0,
         percent_1 = percent_1)
freq_NONPEDSPEC_total <- inner_join(freq_NONPEDSPEC_df_nam_9316_total, freq_NONPEDSPEC_df_nam_9316_wide, by = "dataframename")
descr(freq_NONPEDSPEC_total)

#for 0 observations in nam93 PEDSPEC if use AGE<1
nam93_filtered_gen$PEDSPEC <- factor(nam93_filtered_gen$PEDSPEC, levels = c(0, 1))

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
nam93_survey <- create_survey_design(nam93_filtered_gen)
nam94_survey <- create_survey_design(nam94_filtered_gen)
nam95_survey <- create_survey_design(nam95_filtered_gen)
nam96_survey <- create_survey_design(nam96_filtered_gen)
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

####################################################
#caclulate svymeans_ for each survey object, returning mean and 95% confidence interval
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
#applying function to calculate survey means for each year 1993 - 2016
svymeans_nam93 <- calculate_svymeans(nam93_survey, svymeans_variables)
svymeans_nam94 <- calculate_svymeans(nam94_survey, svymeans_variables)
svymeans_nam95 <- calculate_svymeans(nam95_survey, svymeans_variables)
svymeans_nam96 <- calculate_svymeans(nam96_survey, svymeans_variables)
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
    # Get the survey means for the current variable
    variable_means <- svymeans_obj[[variable]]
    # Convert survey means to a data frame
    variable_df <- as.data.frame(variable_means)
    # Transpose the data frame
    transposed_df <- as.data.frame(t(variable_df))
    # Set the desired row names
    rownames(transposed_df) <- c("mean", "SE", "lowerCI", "upperCI")
    # Create the output variable name
    output_variable_name <- paste(prefix, variable, sep = "_")
    # Assign the data frame to a separate variable with the desired name
    assign(output_variable_name, transposed_df, envir = .GlobalEnv)
  }
}

svymeans_df_byvar(svymeans_nam93, "svymeans_nam93")
svymeans_df_byvar(svymeans_nam94, "svymeans_nam94")
svymeans_df_byvar(svymeans_nam95, "svymeans_nam95")
svymeans_df_byvar(svymeans_nam96, "svymeans_nam96")
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

###1. PEDGEN: create and apply a function to create a data frame for PEDGEN, with means, SE, lowerCI and upperCI from 1993 to 2016
combined_svymeans_years <- c("93", "94", "95", "96", "97", "98", "99", "00", "01", "02", "03", "04",
                             "05","06","07","08","09","10","11","12","13","14","15","16")

combined9316_df_svymeans_PEDGEN <- data.frame()
for (year in combined_svymeans_years) {
  df_name_svy1 <- paste0("svymeans_nam", year, "_PEDGEN")
  current_df_svy1 <- get(df_name_svy1)
  selected_cols_svy1 <- c("PEDGEN0", "PEDGEN1")
  current_df_svy1 <- current_df_svy1[, selected_cols_svy1]
  combined9316_df_svymeans_PEDGEN <- rbind(combined9316_df_svymeans_PEDGEN, current_df_svy1)
}

###write a function to rename observation names for PEDGEN according to years
replace_row_names_PEDGEN <- function(data_frame, replacements) {
  for (original_row in names(replacements)) {
    row.names(data_frame)[row.names(data_frame) == original_row] <- replacements[original_row]
  }
  data_frame
}

replacements <- c("mean" = "mean1993", "SE" = "SE1993", "mean1" = "mean1994", "SE1" = "SE1994", 
                  "mean2" = "mean1995", "SE2" = "SE1995", "mean3" = "mean1996", "SE3" = "SE1996",
                  "mean4" = "mean1997", "SE4" = "SE1997", "mean5" = "mean1998", "SE5" = "SE1998",
                  "mean6" = "mean1999", "SE6" = "SE1999", "mean7" = "mean2000", "SE7" = "SE2000",
                  "mean8" = "mean2001", "SE8" = "SE2001", "mean9" = "mean2002", "SE9" = "SE2002",
                  "mean10" = "mean2003", "SE10" = "SE2003", "mean11" = "mean2004", "SE11" = "SE2004",
                  "mean12" = "mean2005", "SE12" = "SE2005", "mean13" = "mean2006", "SE13" = "SE2006",
                  "mean14" = "mean2007", "SE14" = "SE2007", "mean15" = "mean2008", "SE15" = "SE2008",
                  "mean16" = "mean2009", "SE16" = "SE2009", "mean17" = "mean2010", "SE17" = "SE2010",
                  "mean18" = "mean2011", "SE18" = "SE2011",
                  "mean19" = "mean2012", "SE19" = "SE2012", "mean20" = "mean2013", "SE20" = "SE2013",
                  "mean21" = "mean2014", "SE21" = "SE2014", "mean22" = "mean2015", "SE22" = "SE2015",
                  "mean23" = "mean2016", "SE23" = "SE2016", "upperCI" = "upperCI1993", "lowerCI" = "lowerCI1993",
                  "upperCI1" = "upperCI1994", "lowerCI1" ="lowerCI1994", "upperCI2" = "upperCI1995",
                  "lowerCI2" = "lowerCI1995", "upperCI3" = "upperCI1996","lowerCI3" = "lowerCI1996",
                  "upperCI4" = "upperCI1997", "lowerCI4" ="lowerCI1997", "upperCI5" = "upperCI1998",
                  "lowerCI5" = "lowerCI1998", "upperCI6" = "upperCI1999", "lowerCI6" = "lowerCI1999",
                  "upperCI7" = "upperCI2000", "lowerCI7"= "lowerCI2000", "upperCI8" = "upperCI2001",
                  "lowerCI8" = "lowerCI2001", "upperCI9" = "upperCI2002", "lowerCI9"= "lowerCI2002",
                  "upperCI10" = "upperCI2003", "lowerCI10" = "lowerCI2003", "upperCI11" = "upperCI2004",
                  "lowerCI11"="lowerCI2004","upperCI12" = "upperCI2005", "lowerCI12" = "lowerCI2005",
                  "upperCI13" = "upperCI2006", "lowerCI13"="lowerCI2006","upperCI14"="upperCI2007",
                  "lowerCI14"="lowerCI2007","upperCI15"="upperCI2008", "lowerCI15"="lowerCI2008",
                  "upperCI16"="upperCI2009","lowerCI16"="lowerCI2009","upperCI17"="upperCI2010",
                  "lowerCI17"="lowerCI2010","upperCI18"="upperCI2011","lowerCI18"="lowerCI2011",
                  "upperCI19"="upperCI2012","lowerCI19"="lowerCI2012","upperCI20"="upperCI2013",
                  "lowerCI20"="lowerCI2013","upperCI21"="upperCI2014","lowerCI21"="lowerCI2014",
                  "upperCI22"="upperCI2015","lowerCI22"="lowerCI2015","upperCI23"="upperCI2016",
                  "lowerCI23"="lowerCI2016")

combined9316_df_svymeans_PEDGEN <- replace_row_names_PEDGEN(combined9316_df_svymeans_PEDGEN, replacements)

###2. NONPEDGEN: create and apply a function to create a data frame for NONPEDGEN, with means, SE, lowerCI and upperCI  from 1993 to 2016
combined9316_df_svymeans_NONPEDGEN <- data.frame()
for (year in combined_svymeans_years) {
  df_name_svy1 <- paste0("svymeans_nam", year, "_NONPEDGEN") #!
  current_df_svy1 <- get(df_name_svy1)
  selected_cols_svy1 <- c("NONPEDGEN0", "NONPEDGEN1") #!
  current_df_svy1 <- current_df_svy1[, selected_cols_svy1]
  combined9316_df_svymeans_NONPEDGEN <- rbind(combined9316_df_svymeans_NONPEDGEN, current_df_svy1)
}

###write a function to rename observation names for NONPEDGEN according to years
replace_row_names_NONPEDGEN <- function(data_frame, replacements) {
  for (original_row in names(replacements)) {
    row.names(data_frame)[row.names(data_frame) == original_row] <- replacements[original_row]
  }
  data_frame
}

combined9316_df_svymeans_NONPEDGEN <- replace_row_names_NONPEDGEN(combined9316_df_svymeans_NONPEDGEN, replacements)

###3. PEDSPEC: create and apply a function to create a data frame for PEDSPEC, with means, SE, lowerCI and upperCI from 1993 to 2016
combined9316_df_svymeans_PEDSPEC <- data.frame()
for (year in combined_svymeans_years) {
  df_name_svy1 <- paste0("svymeans_nam", year, "_PEDSPEC") #!
  current_df_svy1 <- get(df_name_svy1)
  selected_cols_svy1 <- c("PEDSPEC0", "PEDSPEC1") #!
  current_df_svy1 <- current_df_svy1[, selected_cols_svy1]
  combined9316_df_svymeans_PEDSPEC <- rbind(combined9316_df_svymeans_PEDSPEC, current_df_svy1)
}

###write a function to rename observation names for PEDSPEC according to years
replace_row_names_PEDSPEC <- function(data_frame, replacements) {
  for (original_row in names(replacements)) {
    row.names(data_frame)[row.names(data_frame) == original_row] <- replacements[original_row]
  }
  data_frame
}

combined9316_df_svymeans_PEDSPEC <- replace_row_names_PEDSPEC(combined9316_df_svymeans_PEDSPEC, replacements)

###4. NONPEDSPEC: create and apply a function to create a data frame for NONPEDSPEC, with means, SE, lowerCI and upperCI from 1993 to 2016
combined9316_df_svymeans_NONPEDSPEC <- data.frame() #!
for (year in combined_svymeans_years) {
  df_name_svy1 <- paste0("svymeans_nam", year, "_NONPEDSPEC") #!
  current_df_svy1 <- get(df_name_svy1)
  selected_cols_svy1 <- c("NONPEDSPEC0", "NONPEDSPEC1") #!
  current_df_svy1 <- current_df_svy1[, selected_cols_svy1]
  combined9316_df_svymeans_NONPEDSPEC <- rbind(combined9316_df_svymeans_NONPEDSPEC, current_df_svy1)
}

###write a function to rename observation names for NONPEDSPEC according to years
replace_row_names_NONPEDSPEC <- function(data_frame, replacements) {
  for (original_row in names(replacements)) {
    row.names(data_frame)[row.names(data_frame) == original_row] <- replacements[original_row]
  }
  data_frame
}

combined9316_df_svymeans_NONPEDSPEC <- replace_row_names_NONPEDSPEC(combined9316_df_svymeans_NONPEDSPEC, replacements)

####

# Filter rows based on the specific strings and create separate data frames for mean, SE, lowerCI and upperCI
combined9316_df_MEAN_PEDGEN <- combined9316_df_svymeans_PEDGEN[grepl("mean", rownames(combined9316_df_svymeans_PEDGEN)), ]
combined9316_df_SE_PEDGEN <- combined9316_df_svymeans_PEDGEN[grepl("SE", rownames(combined9316_df_svymeans_PEDGEN)), ]
combined9316_df_lowerCI_PEDGEN <- combined9316_df_svymeans_PEDGEN[grepl("lowerCI", rownames(combined9316_df_svymeans_PEDGEN)), ]
combined9316_df_upperCI_PEDGEN <- combined9316_df_svymeans_PEDGEN[grepl("upperCI", rownames(combined9316_df_svymeans_PEDGEN)), ]

combined9316_df_MEAN_NONPEDGEN <- combined9316_df_svymeans_NONPEDGEN[grepl("mean", rownames(combined9316_df_svymeans_NONPEDGEN)), ]
combined9316_df_SE_NONPEDGEN <- combined9316_df_svymeans_NONPEDGEN[grepl("SE", rownames(combined9316_df_svymeans_NONPEDGEN)), ]
combined9316_df_lowerCI_NONPEDGEN <- combined9316_df_svymeans_NONPEDGEN[grepl("lowerCI", rownames(combined9316_df_svymeans_NONPEDGEN)), ]
combined9316_df_upperCI_NONPEDGEN <- combined9316_df_svymeans_NONPEDGEN[grepl("upperCI", rownames(combined9316_df_svymeans_NONPEDGEN)), ]

combined9316_df_MEAN_PEDSPEC <- combined9316_df_svymeans_PEDSPEC[grepl("mean", rownames(combined9316_df_svymeans_PEDSPEC)), ]
combined9316_df_SE_PEDSPEC <- combined9316_df_svymeans_PEDSPEC[grepl("SE", rownames(combined9316_df_svymeans_PEDSPEC)), ]
combined9316_df_lowerCI_PEDSPEC <- combined9316_df_svymeans_PEDSPEC[grepl("lowerCI", rownames(combined9316_df_svymeans_PEDSPEC)), ]
combined9316_df_upperCI_PEDSPEC <- combined9316_df_svymeans_PEDSPEC[grepl("upperCI", rownames(combined9316_df_svymeans_PEDSPEC)), ]

combined9316_df_MEAN_NONPEDSPEC <- combined9316_df_svymeans_NONPEDSPEC[grepl("mean", rownames(combined9316_df_svymeans_NONPEDSPEC)), ]
combined9316_df_SE_NONPEDSPEC <- combined9316_df_svymeans_NONPEDSPEC[grepl("SE", rownames(combined9316_df_svymeans_NONPEDSPEC)), ]
combined9316_df_lowerCI_NONPEDSPEC <- combined9316_df_svymeans_NONPEDSPEC[grepl("lowerCI", rownames(combined9316_df_svymeans_NONPEDSPEC)), ]
combined9316_df_upperCI_NONPEDSPEC <- combined9316_df_svymeans_NONPEDSPEC[grepl("upperCI", rownames(combined9316_df_svymeans_NONPEDSPEC)), ]

###merging to create combined 93-16 mean, SE, upper CI, lower CI data frames
merged_9316_mean <- do.call(cbind, list(combined9316_df_MEAN_PEDGEN, combined9316_df_MEAN_NONPEDGEN, 
                                        combined9316_df_MEAN_PEDSPEC,combined9316_df_MEAN_NONPEDSPEC))

merged_9316_SE <- do.call(cbind, list(combined9316_df_SE_PEDGEN, combined9316_df_SE_NONPEDGEN, 
                                      combined9316_df_SE_PEDSPEC,combined9316_df_SE_NONPEDSPEC))

merged_9316_lowerCI <- do.call(cbind, list(combined9316_df_lowerCI_PEDGEN, combined9316_df_lowerCI_NONPEDGEN, 
                                           combined9316_df_lowerCI_PEDSPEC,combined9316_df_lowerCI_NONPEDSPEC))

merged_9316_upperCI <- do.call(cbind, list(combined9316_df_upperCI_PEDGEN, combined9316_df_upperCI_NONPEDGEN, 
                                           combined9316_df_upperCI_PEDSPEC,combined9316_df_upperCI_NONPEDSPEC))

###keep only VAR01(1,3,5,7 variables) variable for PEDGEN, NONPEDGEN, PEDSPEC, and NONPEDSPEC in combined data frame
keepNthColumns <- function(data, columns) {
  return(data[, columns, drop = FALSE])
}

#mean data frame for PEDGEN, NONPEDGEN, PEDSPEC and NONPEDSPEC

merged_9316_mean_01<-keepNthColumns(merged_9316_mean, c(2,4,6,8))
merged_9316_mean_01$year <- 1993 + seq_len(nrow(merged_9316_mean_01)) - 1

merged_9316_mean_01$PEDGEN1 <- merged_9316_mean_01$PEDGEN1 * 100
merged_9316_mean_01$NONPEDGEN1 <- merged_9316_mean_01$NONPEDGEN1 * 100
merged_9316_mean_01$PEDSPEC1 <- merged_9316_mean_01$PEDSPEC1 * 100
merged_9316_mean_01$NONPEDSPEC1 <- merged_9316_mean_01$NONPEDSPEC1 * 100

#SE
merged_9316_SE_01<-keepNthColumns(merged_9316_SE, c(2,4,6,8)) 
merged_9316_SE_01$year <- 1993 + seq_len(nrow(merged_9316_SE_01)) - 1

merged_9316_SE_01$PEDGEN1 <- merged_9316_SE_01$PEDGEN1 * 100
merged_9316_SE_01$NONPEDGEN1 <- merged_9316_SE_01$NONPEDGEN1 * 100
merged_9316_SE_01$PEDSPEC1 <- merged_9316_SE_01$PEDSPEC1 * 100
merged_9316_SE_01$NONPEDSPEC1 <- merged_9316_SE_01$NONPEDSPEC1 * 100

#lower CI
merged_9316_lowerCI_01<-keepNthColumns(merged_9316_lowerCI, c(2,4,6,8))
merged_9316_lowerCI_01$year <- 1993 + seq_len(nrow(merged_9316_lowerCI_01)) - 1

merged_9316_lowerCI_01$PEDGEN1 <- merged_9316_lowerCI_01$PEDGEN1 * 100
merged_9316_lowerCI_01$NONPEDGEN1 <- merged_9316_lowerCI_01$NONPEDGEN1 * 100
merged_9316_lowerCI_01$PEDSPEC1 <- merged_9316_lowerCI_01$PEDSPEC1 * 100
merged_9316_lowerCI_01$NONPEDSPEC1 <- merged_9316_lowerCI_01$NONPEDSPEC1 * 100

#upper CI
merged_9316_upperCI_01<-keepNthColumns(merged_9316_upperCI, c(2,4,6,8))
merged_9316_upperCI_01$year <- 1993 + seq_len(nrow(merged_9316_upperCI_01)) - 1

merged_9316_upperCI_01$PEDGEN1 <- merged_9316_upperCI_01$PEDGEN1 * 100
merged_9316_upperCI_01$NONPEDGEN1 <- merged_9316_upperCI_01$NONPEDGEN1 * 100
merged_9316_upperCI_01$PEDSPEC1 <- merged_9316_upperCI_01$PEDSPEC1 * 100
merged_9316_upperCI_01$NONPEDSPEC1 <- merged_9316_upperCI_01$NONPEDSPEC1 * 100

library(reshape2) 
merged_9316_mean_02 <- merged_9316_mean_01[merged_9316_mean_01$year != 2014, ]  #exclude 2014 as row does not add up to 100
merged_9316_mean_long_02 <- melt(merged_9316_mean_02, id.vars = "year")  
merged_9316_mean_long_02 <- merged_9316_mean_long_02 %>%
  rename(mean = value)

merged_9316_upperCI_02 <- merged_9316_upperCI_01[merged_9316_upperCI_01$year != 2014, ]  #exclude 2014 as row does not add up to 100
merged_9316_upperCI_long_02 <- melt(merged_9316_upperCI_02, id.vars = "year") 
merged_9316_upperCI_long_02 <- merged_9316_upperCI_long_02 %>%
  rename(upperCI = value)

merged_9316_lowerCI_02 <- merged_9316_lowerCI_01[merged_9316_lowerCI_01$year != 2014, ]  #exclude 2014 as row does not add up to 100
merged_9316_lowerCI_long_02 <- melt(merged_9316_lowerCI_02, id.vars = "year") 
merged_9316_lowerCI_long_02 <- merged_9316_lowerCI_long_02 %>%
  rename(lowerCI = value)

mean_CI_df <- merge(merged_9316_upperCI_long_02, merged_9316_lowerCI_long_02)
mean_CI_df <- merge(merged_9316_mean_long_02, mean_CI_df)
#library(readr)
#write_csv(mean_CI_df, "C:/Users/aleong/Desktop/NAMCS/Replication/means_under1_1993-2016.csv)

mean_CI_df$lowerCI_positive <- ifelse(mean_CI_df$lowerCI< 0, 0, mean_CI_df$lowerCI)
mean_CI_df$variable <- factor(mean_CI_df$variable , levels = c("PEDGEN1", "NONPEDGEN1", "PEDSPEC1", "NONPEDSPEC1"))


row_sums_02 <- as.data.frame(rowSums(merged_9316_mean_02[, 1:4]))
print(row_sums_02) #check that all rows add up to 100

library(ggplot2) 
ggplot(mean_CI_df, aes(x = year, y = mean, col = variable, shape = variable, fill=variable)) + #remove fill if grey
  geom_point(size = 1.5) +
  geom_smooth(method = "loess", se=FALSE, size=0.25) + #local regression 
  #geom_vline(xintercept = 2006, linetype = "dashed", color = "black", alpha = 0.5) + #remove if no line
  scale_x_continuous(breaks = seq(min(mean_CI_df$year), max(mean_CI_df$year), by = 2)) +
  ### CHECK THIS LINE ABOVE (should still be the same)
  scale_y_continuous(limits = c(0, 100)) +
  labs(x = "Year", y = "Proportion of visit records \nfor infants <1y (%)", col = "Variable", shape = "Variable") +
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
#annotate("text", x = 2006, y = 90, label = "Freed (2010)", vjust = 1, hjust = -0.09, size = 3)

#dotted line on graph is where Freed's paper ends (2006)

#renaming variables in SE data frame
library(dplyr)
merged_9316_SE_01_edit <- merged_9316_SE_01 %>%
  rename(PEDGEN1_SE = PEDGEN1,
         NONPEDGEN1_SE = NONPEDGEN1,
         PEDSPEC1_SE = PEDSPEC1,
         NONPEDSPEC1_SE = NONPEDSPEC1)

#renaming variables in lower CI data frame
merged_9316_lowerCI_01_edit <- merged_9316_lowerCI_01 %>%
  rename(PEDGEN1_lowerCI = PEDGEN1,
         NONPEDGEN1_lowerCI = NONPEDGEN1,
         PEDSPEC1_lowerCI = PEDSPEC1,
         NONPEDSPEC1_lowerCI = NONPEDSPEC1)

#renaming variables in upper CI data frame
merged_9316_upperCI_01_edit <- merged_9316_upperCI_01 %>%
  rename(PEDGEN1_upperCI = PEDGEN1,
         NONPEDGEN1_upperCI = NONPEDGEN1,
         PEDSPEC1_upperCI = PEDSPEC1,
         NONPEDSPEC1_upperCI = NONPEDSPEC1)

#combine mean and SE data frames
merged_9316_ALL <- cbind(merged_9316_mean_01, merged_9316_SE_01_edit[match(merged_9316_mean_01$year, merged_9316_SE_01_edit$year), ])
#append lower CI
merged_9316_ALL <- cbind(merged_9316_ALL, merged_9316_lowerCI_01_edit[match(merged_9316_ALL$year, merged_9316_lowerCI_01_edit$year), ])
#append upper CI
merged_9316_ALL <- cbind(merged_9316_ALL, merged_9316_upperCI_01_edit[match(merged_9316_ALL$year, merged_9316_upperCI_01_edit$year), ])
#remove 2014
merged_9316_ALL<-merged_9316_ALL[merged_9316_ALL$year != 2014,]

#subset by pedtype
PEDGEN1_9316_ci <- merged_9316_ALL[, c(1, 5, 6, 11, 16)] #PEDGEN1
PEDGEN1_9316_ci <- PEDGEN1_9316_ci %>%
  mutate (percent_SE = PEDGEN1_SE / PEDGEN1 * 100)
#results comparison (u1)
#2006: mine = 83% (95% CI, 77% - 89%)  paper = 84% (95% CI, 77% - 90%)

NONPEDGEN1_9316_ci <- merged_9316_ALL[, c(2, 5, 7, 12, 17)] #NONPEDGEN1
NONPEDGEN1_9316_ci <- NONPEDGEN1_9316_ci %>%
  mutate (percent_SE = NONPEDGEN1_SE / NONPEDGEN1 * 100)
#results comparison (u1)
#2006: mine = 16% (95% CI, 10% - 22%)  paper = 15% (95% CI, 9% - 22%) 

PEDSPEC1_9316_ci <- merged_9316_ALL[, c(3, 5, 8, 13, 18)] #PEDSPEC1
PEDSPEC1_9316_ci <- PEDSPEC1_9316_ci %>%
  mutate (percent_SE = PEDSPEC1_SE / PEDSPEC1 * 100)

NONPEDSPEC1_9316_ci <- merged_9316_ALL[, c(4, 5, 9, 14, 19)] #NONPEDSPEC1
NONPEDSPEC1_9316_ci <- NONPEDSPEC1_9316_ci %>%
  mutate (percent_SE = NONPEDSPEC1_SE / NONPEDSPEC1 * 100)

#test for trend
#install.packages("Kendall")
library(Kendall)
result_mk_PEDGEN1 <- Kendall(PEDGEN1_9316_ci$PEDGEN1, PEDGEN1_9316_ci$year)
print(result_mk_PEDGEN1) 
#tau = 0.542, 2-sided pvalue = 0.00032842 (u18)
#tau = 0.562, 2-sided pvalue = 0.00019515 (u1)

result_mk_NONPEDGEN1 <- Kendall(NONPEDGEN1_9316_ci$NONPEDGEN1, NONPEDGEN1_9316_ci$year)
print(result_mk_NONPEDGEN1)
#tau = -0.794, 2-sided pvalue =1.2772e-07 (u18)
#tau = -0.652, 2-sided pvalue = 1.4822e-05 (u1)