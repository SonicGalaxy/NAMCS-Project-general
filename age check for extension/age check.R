#reference paper: https://pubmed.ncbi.nlm.nih.gov/20227714/

#import NAMCS stata files from 2012 to 2016 
library(haven)
import_nam_data <- function(year) {
  file_path <- paste0("C:/Users/aleong/Desktop/NAMCS/Replication/nam", year, ".dta")
  data <- read_dta(file_path)
  return(data)
}

nam12 <- import_nam_data("12")
nam13 <- import_nam_data("13")
nam14 <- import_nam_data("14")
nam15 <- import_nam_data("15")
nam16 <- import_nam_data("16")

#under18
data_frame_names <- c("nam12", "nam13", "nam14",
                      "nam15", "nam16")
for (df_name in data_frame_names) {
  df <- get(df_name) 
  colnames(df) <- toupper(colnames(df))  # Convert column names to uppercase
  assign(df_name, df, envir = .GlobalEnv)  
}

for (df_name in data_frame_names) {
  df <- get(df_name) 
  #df <- df[df$AGE <18, ] #dont filter by age!!!
  assign(df_name, df, envir = .GlobalEnv) 
}

data_frames <- list(nam12, nam13, nam14, nam15, nam16)
variable <- "SPECR" #check with SPECR (all), SPECCAT (nam05 onwards only), CPSUM & CSTRATM (nam02 onwards only), PATWT (all)
for (i in seq_along(data_frames)) {
  if (exists(variable, where = data_frames[[i]])) {
    print(paste("Variable", variable, "is present in data frame", i))
  } else {
    print(paste("Variable", variable, "is NOT present in data frame", i))
  }
}

#2012: SPECR not present, SPECR_14 instead of SPECR

### write a function [create_specialty_variables()] to create PEDGEN, NONPEDGEN, PEDSPEC, NONPEDSPEC for any data set
create_specialty_variables <- function(data) {
  data <- data[data$SPECCAT != 2, ] #excluding surgical specialties
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
nam12_filtered_gen <- create_specialty_variables_nam12(nam12)
nam13_filtered_gen <- create_specialty_variables(nam13)
nam14_filtered_gen <- create_specialty_variables(nam14)
nam15_filtered_gen <- create_specialty_variables(nam15)
nam16_filtered_gen <- create_specialty_variables(nam16)

nam12_filtered_gen_agecheck  <- nam12_filtered_gen[, c("PEDGEN", "PEDSPEC", "NONPEDGEN", "NONPEDSPEC", "AGE", "PHYCODE")]
nam13_filtered_gen_agecheck  <- nam13_filtered_gen[, c("PEDGEN", "PEDSPEC", "NONPEDGEN", "NONPEDSPEC", "AGE", "PHYCODE")]
nam14_filtered_gen_agecheck  <- nam14_filtered_gen[, c("PEDGEN", "PEDSPEC", "NONPEDGEN", "NONPEDSPEC", "AGE", "PHYCODE")]
nam15_filtered_gen_agecheck  <- nam15_filtered_gen[, c("PEDGEN", "PEDSPEC", "NONPEDGEN", "NONPEDSPEC", "AGE", "PHYCODE")]
nam16_filtered_gen_agecheck  <- nam16_filtered_gen[, c("PEDGEN", "PEDSPEC", "NONPEDGEN", "NONPEDSPEC", "AGE", "PHYCODE")]

library(tidyverse)

#add constant string to PHYCODE, to prevent identical PHYCODES when combining data across years
nam12_filtered_gen_agecheck$PHYCODE <- paste0(nam12_filtered_gen_agecheck$PHYCODE, "2012")
nam13_filtered_gen_agecheck$PHYCODE <- paste0(nam13_filtered_gen_agecheck$PHYCODE, "2013")
nam14_filtered_gen_agecheck$PHYCODE <- paste0(nam14_filtered_gen_agecheck$PHYCODE, "2014")
nam15_filtered_gen_agecheck$PHYCODE <- paste0(nam15_filtered_gen_agecheck$PHYCODE, "2015")
nam16_filtered_gen_agecheck$PHYCODE <- paste0(nam16_filtered_gen_agecheck$PHYCODE, "2016")

list_bind <- list(nam12_filtered_gen_agecheck, nam13_filtered_gen_agecheck, nam14_filtered_gen_agecheck,
                  nam15_filtered_gen_agecheck, nam16_filtered_gen_agecheck)
library(tidyverse)
agecheck <- bind_rows(list_bind)

counts <- sapply(list_bind, count)
counts <- unlist(counts)
sum(counts)

#PEDGEN
agecheck$U18[agecheck$AGE <18] <- 1
agecheck$U18[agecheck$AGE >=18] <- 0
sum(is.na(agecheck$U18))

length(unique(agecheck$PHYCODE))

counts_ped<- agecheck%>% group_by(PHYCODE) %>% summarize(U18_count = sum(U18 == 1))
table_ped <- as.data.frame (table(agecheck$PHYCODE))
names(table_ped)[names(table_ped)== "Var1"] <- "PHYCODE"
names(table_ped)[names(table_ped)== "Freq"] <- "PATIENTCOUNT"
ped_counts <- merge(table_ped, counts_ped, by ="PHYCODE")
ped_vector <- agecheck [, c("PHYCODE", "PEDGEN", "PEDSPEC", "NONPEDGEN", "NONPEDSPEC")]
ped_vector$PHYCODE <- as.factor(ped_vector$PHYCODE)

grouped_pedgen_counts <- ped_vector %>%
  group_by(PHYCODE) %>%
  summarize (unique_y_count = n_distinct (PEDGEN))

same_unique_obs_pedgen <- all(grouped_pedgen_counts$unique_y_count 
                              == grouped_pedgen_counts$unique_y_count [1]) 
# TRUE
# All PHYCODE have a unique categorization for PEDGEN across time

## repeat for PEDSPEC
grouped_pedspec_counts <- ped_vector %>%
  group_by(PHYCODE) %>%
  summarize (unique_y2_count = n_distinct (PEDSPEC))

same_unique_obs_pedspec <- all(grouped_pedspec_counts$unique_y2_count 
                               == grouped_pedspec_counts$unique_y2_count [1])

# TRUE
# All PHYCODE have a unique categorization for PEDSPEC across time

ped_vector_filtered_unique <- distinct(ped_vector, PHYCODE, .keep_all=TRUE)

####

ped_joined <- right_join(ped_counts, ped_vector_filtered_unique, by = "PHYCODE")
ped_joined$prop <- (ped_joined$U18_count / ped_joined$PATIENTCOUNT) * 100

ped_joined_PEDGEN1 <- ped_joined[ped_joined$PEDGEN == 1,] # only PEDGEN == 1
ggplot(ped_joined_PEDGEN1, aes(x=prop)) +
  geom_histogram(binwidth = 1, fill ="red") + 
  ggtitle("Proportion of U18 patients for PEDGEN == 1, 2012-2016") +
  xlab("Percentage of U18 patients (%)") +
  theme(plot.title = element_text(size=10),
        axis.title.x = element_text (size=8),
        axis.title.y = element_text (size=8))

ped_joined_PEDSPEC1 <- ped_joined[ped_joined$PEDSPEC == 1,] # only PEDSPEC == 1
ggplot(ped_joined_PEDSPEC1, aes(x=prop)) +
  geom_histogram(binwidth = 1, fill ="red") + 
  ggtitle("Proportion of U18 patients for PEDSPEC == 1, 2012-2016") +
  xlab("Percentage of U18 patients") +
  theme(plot.title = element_text(size=10),
        axis.title.x = element_text (size=8),
        axis.title.y = element_text (size=8))

#ped_joined_PEDGEN0 <- ped_joined[ped_joined$PEDGEN == 0 & ped_joined$PEDSPEC == 0,] #non PEDGEN but also non PEDSPEC
#ggplot(ped_joined_PEDGEN0, aes(x=prop)) +
  #geom_histogram(binwidth = 1, fill ="red") + 
  #ggtitle("Percentage of U18 patients for PEDGEN == 0 \nand PEDSPEC == 0, 2012-2016") +
  #xlab ("Percentage of U18 patients (%)") +
  #theme(plot.title = element_text(size=10),
        #axis.title.x = element_text (size=8),
        #axis.title.y = element_text (size=8))

ped_joined_NONPEDGEN1 <- ped_joined[ped_joined$NONPEDGEN == 1,] #NONPEDGEN == 1
ggplot(ped_joined_NONPEDGEN1, aes(x=prop)) +
  geom_histogram(binwidth = 1, fill ="red") + 
  ggtitle("Percentage of U18 patients for NONPEDGEN == 1, 2012-2016") +
  xlab ("Percentage of U18 patients (%)") +
  theme(plot.title = element_text(size=10),
        axis.title.x = element_text (size=8),
        axis.title.y = element_text (size=8))

ped_joined_NONPEDSPEC1 <- ped_joined[ped_joined$NONPEDSPEC == 1,] #NONPEDSPEC == 1
ggplot(ped_joined_NONPEDSPEC1, aes(x=prop)) +
  geom_histogram(binwidth = 1, fill ="red") + 
  ggtitle("Percentage of U18 patients for NONPEDSPEC == 1, 2012-2016") +
  xlab ("Percentage of U18 patients (%)") +
  theme(plot.title = element_text(size=10),
        axis.title.x = element_text (size=8),
        axis.title.y = element_text (size=8))

### using U18>=75 as a predictor of PED, U18 < 75 as predictor of NONPED
prop_PEDGEN_pred <- (nrow(ped_joined_PEDGEN1[ped_joined_PEDGEN1$prop >= 75, ]) / nrow(ped_joined_PEDGEN1))
print(prop_PEDGEN_pred) #94.35% of PEDGENs have U18 >= 75

prop_PEDSPEC_pred <- (nrow(ped_joined_PEDSPEC1[ped_joined_PEDSPEC1$prop >= 75, ]) / nrow(ped_joined_PEDSPEC1))
print(prop_PEDSPEC_pred) #88.31% of PEDSPECS have U18 >= 75

prop_NONPEDGEN_pred <- (nrow(ped_joined_NONPEDGEN1[ped_joined_NONPEDGEN1$prop < 75, ]) / nrow(ped_joined_NONPEDGEN1))
print(prop_NONPEDGEN_pred) #99.58% of NONPEDGEN1 have U18 < 75

prop_NONPEDSPEC_pred <- (nrow(ped_joined_NONPEDSPEC1[ped_joined_NONPEDSPEC1$prop < 75, ]) / nrow(ped_joined_NONPEDSPEC1))
print(prop_NONPEDSPEC_pred ) #97.33% of NONPEDSPEC1 have U18 < 75
