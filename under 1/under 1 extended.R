# extending the time series for u18s
# csv files from original replication

library(haven)
library(tidyverse)
#import NAMCS stata files from 2018, 2019
import_nam_data <- function(year) {
  file_path <- paste0("C:/Users/aleong/Desktop/NAMCS/Replication/nam", year, ".dta")
  data <- read_dta(file_path)
  return(data)
}

nam18 <- import_nam_data("18")
nam19 <- import_nam_data("19")

# nam18 and nam19 have no SPECR but have SPECCAT
# PEDGEN: SPECCAT == 1 and prop u18 > 75
# PEDSPEC: SPECCAT == 3 and prop u18 < 75
# NONPEDGEN: SPECCAT == 1 and prop u18 < 75
# NONPEDSPEC: SPECCAT == 3 and prop u18 > 75

data_frame_names <- c("nam18", "nam19")
for (df_name in data_frame_names) {
  df <- get(df_name) 
  colnames(df) <- toupper(colnames(df))  # Convert column names to uppercase
  assign(df_name, df, envir = .GlobalEnv)  
}

nam18_agecheck  <- nam18[, c("AGE", "PHYCODE")]
nam19_agecheck  <- nam19[, c("AGE", "PHYCODE")]

#create PED variable for nam18
library(summarytools)
nam18_agecheck$U18[nam18_agecheck$AGE <18] <- 1
nam18_agecheck$U18[nam18_agecheck$AGE >=18] <- 0
sum(is.na(nam18_agecheck$U18))

nam18_counts_ped<- nam18_agecheck %>% group_by(PHYCODE) %>% summarize(U18_count = sum(U18 == 1))

nam18_table <- as.data.frame (table(nam18_agecheck$PHYCODE))
names(nam18_table)[names(nam18_table)=="Var1"] <-"PHYCODE"
names(nam18_table)[names(nam18_table)=="Freq"] <-"PATIENTCOUNT"
nam18_ped_counts <- merge(nam18_table, nam18_counts_ped, by ="PHYCODE")
nam18_ped_counts$prop <- (nam18_ped_counts$U18_count/nam18_ped_counts$PATIENTCOUNT)*100
nam18_ped_counts$PED <- ifelse(nam18_ped_counts$prop >= 75, 1, 0)
nam18_ped_counts  <- nam18_ped_counts [, c(1, 5)]
freq(nam18_ped_counts$PED) #496
length(unique(nam18$PHYCODE))
nam18_ped <- merge(nam18, nam18_ped_counts)

#create PED variable for nam19

nam19_agecheck$U18[nam19_agecheck$AGE <18] <- 1
nam19_agecheck$U18[nam19_agecheck$AGE >=18] <- 0
sum(is.na(nam19_agecheck$U18))

nam19_counts_ped<- nam19_agecheck %>% group_by(PHYCODE) %>% summarize(U18_count = sum(U18 == 1))

nam19_table <- as.data.frame (table(nam19_agecheck$PHYCODE))
names(nam19_table)[names(nam19_table)=="Var1"] <-"PHYCODE"
names(nam19_table)[names(nam19_table)=="Freq"] <-"PATIENTCOUNT"
nam19_ped_counts <- merge(nam19_table, nam19_counts_ped, by ="PHYCODE")
nam19_ped_counts$prop <- (nam19_ped_counts$U18_count/nam19_ped_counts$PATIENTCOUNT)*100
nam19_ped_counts$PED <- ifelse(nam19_ped_counts$prop >= 75, 1, 0)
nam19_ped_counts  <- nam19_ped_counts [, c(1, 5)]
freq(nam19_ped_counts$PED) #398
length(unique(nam19$PHYCODE))
nam19_ped <- merge(nam19, nam19_ped_counts)

#repeat analysis

#filter for u18
data_frame_names <- c("nam18_ped", "nam19_ped")
for (df_name in data_frame_names) {
  df <- get(df_name) 
  #df <- df[df$AGE <18, ] #only include observations where patient age <18
  df <- df[df$AGE <1, ] #only include observations where patient age <1
  assign(df_name, df, envir = .GlobalEnv) 
}

create_specialty_variables <- function(data) {
  data <- data[data$SPECCAT != 2, ] #excluding surgical specialties
  # PEDGEN: if PED == 1 (Pediatrics) and SPECCAT == 1 (Primary care specialty)
  data$PEDGEN <- ifelse(data$SPECCAT == 1 & data$PED == 1, 1, 0)
  # NONPEDGEN: if PED == 0 (Pediatrics) and SPECCAT == 1 (Primary care specialty)
  data$NONPEDGEN <- ifelse(data$SPECCAT == 1 & data$PED == 0, 1, 0)
  # PEDSPEC: if PED == 1 (Pediatrics) and SPECCAT == 3 (Medical care specialty)
  data$PEDSPEC <- ifelse(data$SPECCAT == 3 & data$PED == 1, 1, 0)
  # NONPEDSPEC: if PED == 0 (Pediatrics) and SPECCAT == 3 (Medical care specialty)
  data$NONPEDSPEC <- ifelse(data$SPECCAT == 3 & data$PED == 0, 1, 0)
  # Convert the variables to factors
  data$PEDGEN <- as.factor(data$PEDGEN)
  data$NONPEDGEN <- as.factor(data$NONPEDGEN)
  data$PEDSPEC <- as.factor(data$PEDSPEC)
  data$NONPEDSPEC <- as.factor(data$NONPEDSPEC)
  # Return the modified data
  return(data)
}

nam18_ped <- create_specialty_variables(nam18_ped) 
nam19_ped <- create_specialty_variables(nam19_ped)

### may want to edit this code to reflect original replication rscript
freq(nam18_ped$PEDGEN)
freq(nam18_ped$PEDSPEC) #0!
nam18_ped$PEDSPEC <- factor(nam18_ped$PEDSPEC, levels = c(0, 1))

freq(nam18_ped$NONPEDGEN)
freq(nam18_ped$NONPEDSPEC)

freq(nam19_ped$PEDGEN)
freq(nam19_ped$PEDSPEC)
freq(nam19_ped$NONPEDGEN)
freq(nam19_ped$NONPEDSPEC)

###creating function [create_survey_design()] to apply survey weights to all data sets
library(survey)
create_survey_design <- function(data) {
  design <- svydesign(id = ~CPSUM,
                      strata  = ~CSTRATM,
                      weights = ~PATWT,
                      nest    = TRUE,
                      data    = data)
  return(design)
}

nam18_survey <- create_survey_design(nam18_ped)
nam19_survey <- create_survey_design(nam19_ped)

#calculate svymeans_ for each survey object, returning mean and 95% confidence interval
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

svymeans_nam18 <- calculate_svymeans(nam18_survey, svymeans_variables)
svymeans_nam19 <- calculate_svymeans(nam19_survey, svymeans_variables)

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

svymeans_df_byvar(svymeans_nam18, "svymeans_nam18")
svymeans_df_byvar(svymeans_nam19, "svymeans_nam19")

combined_svymeans_years <- c("18", "19")

replace_row_names <- function(data_frame, replacements) {
  for (original_row in names(replacements)) {
    row.names(data_frame)[row.names(data_frame) == original_row] <- replacements[original_row]
  }
  data_frame
}

replacements <- c("mean" = "mean2018", "SE" = "SE2018", "upperCI" = "upperCI2018", "lowerCI" ="lowerCI2018",
                  "mean1" = "mean2019", "SE1" = "SE2019", "upperCI1" = "upperCI2019", "lowerCI1" ="lowerCI2019")

combined1819_svymeans_PEDGEN <- data.frame()
for (year in combined_svymeans_years) {
  df_name_svy1 <- paste0("svymeans_nam", year, "_PEDGEN")
  current_df_svy1 <- get(df_name_svy1)
  selected_cols_svy1 <- c("PEDGEN0", "PEDGEN1")
  current_df_svy1 <- current_df_svy1[, selected_cols_svy1]
  combined1819_svymeans_PEDGEN  <- rbind(combined1819_svymeans_PEDGEN, current_df_svy1)
}

combined1819_svymeans_PEDGEN<-replace_row_names(combined1819_svymeans_PEDGEN, replacements)

combined1819_svymeans_NONPEDGEN <- data.frame()
for (year in combined_svymeans_years) {
  df_name_svy1 <- paste0("svymeans_nam", year, "_NONPEDGEN") #!
  current_df_svy1 <- get(df_name_svy1)
  selected_cols_svy1 <- c("NONPEDGEN0", "NONPEDGEN1") #!
  current_df_svy1 <- current_df_svy1[, selected_cols_svy1]
  combined1819_svymeans_NONPEDGEN <- rbind(combined1819_svymeans_NONPEDGEN, current_df_svy1)
}
combined1819_svymeans_NONPEDGEN<-replace_row_names(combined1819_svymeans_NONPEDGEN, replacements)

combined1819_svymeans_PEDSPEC <- data.frame()
for (year in combined_svymeans_years) {
  df_name_svy1 <- paste0("svymeans_nam", year, "_PEDSPEC") #!
  current_df_svy1 <- get(df_name_svy1)
  selected_cols_svy1 <- c("PEDSPEC0", "PEDSPEC1") #!
  current_df_svy1 <- current_df_svy1[, selected_cols_svy1]
  combined1819_svymeans_PEDSPEC <- rbind(combined1819_svymeans_PEDSPEC, current_df_svy1)
}

combined1819_svymeans_PEDSPEC<-replace_row_names(combined1819_svymeans_PEDSPEC, replacements)

combined1819_svymeans_NONPEDSPEC <- data.frame() #!
for (year in combined_svymeans_years) {
  df_name_svy1 <- paste0("svymeans_nam", year, "_NONPEDSPEC") #!
  current_df_svy1 <- get(df_name_svy1)
  selected_cols_svy1 <- c("NONPEDSPEC0", "NONPEDSPEC1") #!
  current_df_svy1 <- current_df_svy1[, selected_cols_svy1]
  combined1819_svymeans_NONPEDSPEC <- rbind(combined1819_svymeans_NONPEDSPEC, current_df_svy1)
}

combined1819_svymeans_NONPEDSPEC<-replace_row_names(combined1819_svymeans_NONPEDSPEC, replacements)

num_rows <- 8
year_values <- rep(2018:(2018 + (num_rows - 1) %/% 4), each = 4) #num_rows=8
combined1819_svymeans_PEDGEN$year <- year_values
combined1819_svymeans_NONPEDGEN$year <- year_values
combined1819_svymeans_PEDSPEC$year <- year_values
combined1819_svymeans_NONPEDSPEC$year <- year_values

combined1819_MEAN_PEDGEN <- combined1819_svymeans_PEDGEN[grepl("mean", rownames(combined1819_svymeans_PEDGEN)), ]
combined1819_SE_PEDGEN <- combined1819_svymeans_PEDGEN[grepl("SE", rownames(combined1819_svymeans_PEDGEN)), ]
combined1819_lowerCI_PEDGEN <- combined1819_svymeans_PEDGEN[grepl("lowerCI", rownames(combined1819_svymeans_PEDGEN)), ]
combined1819_upperCI_PEDGEN <- combined1819_svymeans_PEDGEN[grepl("upperCI", rownames(combined1819_svymeans_PEDGEN)), ]

combined1819_MEAN_NONPEDGEN <- combined1819_svymeans_NONPEDGEN[grepl("mean", rownames(combined1819_svymeans_NONPEDGEN)), ]
combined1819_SE_NONPEDGEN <- combined1819_svymeans_NONPEDGEN[grepl("SE", rownames(combined1819_svymeans_NONPEDGEN)), ]
combined1819_lowerCI_NONPEDGEN <- combined1819_svymeans_NONPEDGEN[grepl("lowerCI", rownames(combined1819_svymeans_NONPEDGEN)), ]
combined1819_upperCI_NONPEDGEN <- combined1819_svymeans_NONPEDGEN[grepl("upperCI", rownames(combined1819_svymeans_NONPEDGEN)), ]

combined1819_MEAN_PEDSPEC <- combined1819_svymeans_PEDSPEC[grepl("mean", rownames(combined1819_svymeans_PEDSPEC)), ]
combined1819_SE_PEDSPEC <- combined1819_svymeans_PEDSPEC[grepl("SE", rownames(combined1819_svymeans_PEDSPEC)), ]
combined1819_lowerCI_PEDSPEC <- combined1819_svymeans_PEDSPEC[grepl("lowerCI", rownames(combined1819_svymeans_PEDSPEC)), ]
combined1819_upperCI_PEDSPEC <- combined1819_svymeans_PEDSPEC[grepl("upperCI", rownames(combined1819_svymeans_PEDSPEC)), ]

combined1819_MEAN_NONPEDSPEC <- combined1819_svymeans_NONPEDSPEC[grepl("mean", rownames(combined1819_svymeans_NONPEDSPEC)), ]
combined1819_SE_NONPEDSPEC <- combined1819_svymeans_NONPEDSPEC[grepl("SE", rownames(combined1819_svymeans_NONPEDSPEC)), ]
combined1819_lowerCI_NONPEDSPEC <- combined1819_svymeans_NONPEDSPEC[grepl("lowerCI", rownames(combined1819_svymeans_NONPEDSPEC)), ]
combined1819_upperCI_NONPEDSPEC <- combined1819_svymeans_NONPEDSPEC[grepl("upperCI", rownames(combined1819_svymeans_NONPEDSPEC)), ]

merged_1819_mean <- do.call(cbind, list(combined1819_MEAN_PEDGEN, combined1819_MEAN_NONPEDGEN, 
                                        combined1819_MEAN_PEDSPEC,combined1819_MEAN_NONPEDSPEC))

merged_1819_SE <- do.call(cbind, list(combined1819_SE_PEDGEN , combined1819_SE_NONPEDGEN, 
                                      combined1819_SE_PEDSPEC, combined1819_SE_NONPEDSPEC ))

merged_1819_lowerCI <- do.call(cbind, list(combined1819_lowerCI_PEDGEN, combined1819_lowerCI_NONPEDGEN, 
                                           combined1819_lowerCI_PEDSPEC,combined1819_lowerCI_NONPEDSPEC))

merged_1819_upperCI <- do.call(cbind, list(combined1819_upperCI_PEDGEN, combined1819_upperCI_NONPEDGEN, 
                                           combined1819_upperCI_PEDSPEC, combined1819_upperCI_NONPEDSPEC))

keepNthColumns <- function(data, columns) {
  return(data[, columns, drop = FALSE])
}

merged_1819_mean_01<-keepNthColumns(merged_1819_mean, c(2,5,8,11,12))
merged_1819_mean_01$PEDGEN1 <- merged_1819_mean_01$PEDGEN1 * 100
merged_1819_mean_01$NONPEDGEN1 <- merged_1819_mean_01$NONPEDGEN1 * 100
merged_1819_mean_01$PEDSPEC1 <- merged_1819_mean_01$PEDSPEC1 * 100
merged_1819_mean_01$NONPEDSPEC1 <- merged_1819_mean_01$NONPEDSPEC1 * 100
row_sums <- as.data.frame(rowSums(merged_1819_mean_01[, 1:4]))
print(row_sums) #100

merged_1819_SE_01<-keepNthColumns(merged_1819_SE, c(2,5,8,11,12))
merged_1819_SE_01$PEDGEN1 <- merged_1819_SE_01$PEDGEN1 * 100
merged_1819_SE_01$NONPEDGEN1 <- merged_1819_SE_01$NONPEDGEN1 * 100
merged_1819_SE_01$PEDSPEC1 <- merged_1819_SE_01$PEDSPEC1 * 100
merged_1819_SE_01$NONPEDSPEC1 <- merged_1819_SE_01$NONPEDSPEC1 * 100

merged_1819_lowerCI_01<-keepNthColumns(merged_1819_lowerCI, c(2,5,8,11,12))
merged_1819_lowerCI_01$PEDGEN1 <- merged_1819_lowerCI_01$PEDGEN1 * 100
merged_1819_lowerCI_01$NONPEDGEN1 <- merged_1819_lowerCI_01$NONPEDGEN1 * 100
merged_1819_lowerCI_01$PEDSPEC1 <- merged_1819_lowerCI_01$PEDSPEC1 * 100
merged_1819_lowerCI_01$NONPEDSPEC1 <- merged_1819_lowerCI_01$NONPEDSPEC1 * 100

merged_1819_upperCI_01<-keepNthColumns(merged_1819_upperCI, c(2,5,8,11,12))
merged_1819_upperCI_01$PEDGEN1 <- merged_1819_upperCI_01$PEDGEN1 * 100
merged_1819_upperCI_01$NONPEDGEN1 <- merged_1819_upperCI_01$NONPEDGEN1 * 100
merged_1819_upperCI_01$PEDSPEC1 <- merged_1819_upperCI_01$PEDSPEC1 * 100
merged_1819_upperCI_01$NONPEDSPEC1 <- merged_1819_upperCI_01$NONPEDSPEC1 * 100

merged_1819_mean_long_02 <- melt(merged_1819_mean_01, id.vars = "year")  
merged_1819_mean_long_02 <- merged_1819_mean_long_02 %>%
  rename(mean = value)

merged_1819_upperCI_long_02 <- melt(merged_1819_upperCI_01, id.vars = "year") 
merged_1819_upperCI_long_02 <- merged_1819_upperCI_long_02 %>%
  rename(upperCI = value)

merged_1819_lowerCI_long_02 <- melt(merged_1819_lowerCI_01, id.vars = "year") 
merged_1819_lowerCI_long_02 <- merged_1819_lowerCI_long_02 %>%
  rename(lowerCI = value)

mean_CI_df_1819 <- merge(merged_1819_upperCI_long_02, merged_1819_lowerCI_long_02)
mean_CI_df_1819 <- merge(merged_1819_mean_long_02, mean_CI_df_1819)

mean_CI_df_1819$lowerCI_positive <- ifelse(mean_CI_df_1819$lowerCI< 0, 0, mean_CI_df_1819$lowerCI)

library(readr)
mean_CI_df <- read_csv("means_under1_1993-2016.csv")
mean_CI_df_combined <- rbind(mean_CI_df, mean_CI_df_1819)
mean_CI_df_combined$variable <- factor(mean_CI_df_combined$variable , levels = c("PEDGEN1", "NONPEDGEN1", "PEDSPEC1", "NONPEDSPEC1"))

library(ggplot2) 
ggplot(mean_CI_df_combined, aes(x = year, y = mean, col = variable, shape = variable, fill=variable)) + #remove fill if grey
  geom_point(size = 1.5) +
  geom_smooth(method = "loess", se=FALSE, size=0.25) + #local regression 
  geom_vline(xintercept = 2017, linetype = "dashed", color = "black", alpha = 0.5) + #remove if no line
  scale_x_continuous(breaks = seq(min(mean_CI_df_combined$year), max(mean_CI_df_combined$year), by = 2)) +
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

pedgen_series <- mean_CI_df_combined %>% 
  filter (variable == "PEDGEN1")

nonpedgen_series <- mean_CI_df_combined %>% 
  filter (variable == "NONPEDGEN1")

pedspec_series <- mean_CI_df_combined %>% 
  filter (variable == "PEDSPEC1")

nonpedspec_series <- mean_CI_df_combined %>% 
  filter (variable == "NONPEDSPEC1")

#test for trend
####
install.packages("Kendall")
library(Kendall)
result_mk_extend_PEDGEN1 <- Kendall(pedgen_series$mean, pedgen_series$year)
print(result_mk_extend_PEDGEN1) 
#tau = 0.524, 2-sided pvalue = 0.0002681 (u1)

result_mk_extend_NONPEDGEN1 <- Kendall(nonpedgen_series$mean, nonpedgen_series$year)
print(result_mk_extend_NONPEDGEN1) 
#tau = -0.68, 2-sided pvalue = 2.126e-06 (u1)
