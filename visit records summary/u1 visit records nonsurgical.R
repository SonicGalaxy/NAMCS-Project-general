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
nam18 <- import_nam_data("18")
nam19 <- import_nam_data("19")

#under18
data_frame_names <- c("nam93", "nam94", "nam95", "nam96", "nam97", "nam98", "nam99", "nam00", "nam01", "nam02", "nam03", "nam04", "nam05", "nam06",
                      "nam07", "nam08", "nam09", "nam10", "nam11", "nam12", "nam13", "nam14",
                      "nam15", "nam16", "nam18", "nam19")
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
#create SPECCAT for each dataset only for data sets prior to 2005
nam93 <- speccat_replicate_new("nam93")
nam94 <- speccat_replicate_new("nam94")
nam95 <- speccat_replicate_new("nam95")
nam96 <- speccat_replicate_new("nam96")
nam97 <- speccat_replicate_new("nam97")
nam98 <- speccat_replicate_new("nam98")
nam99 <- speccat_replicate_new("nam99")
nam00 <- speccat_replicate_new("nam00")
nam01 <- speccat_replicate_new("nam01")
nam02 <- speccat_replicate_new("nam02")
nam03 <- speccat_replicate_new("nam03")
nam04 <- speccat_replicate_new("nam04")

data_frame_names <- c("nam93", "nam94", "nam95", "nam96", "nam97", "nam98", "nam99", "nam00", "nam01", "nam02", "nam03", "nam04", "nam05", "nam06",
                      "nam07", "nam08", "nam09", "nam10", "nam11", "nam12", "nam13", "nam14",
                      "nam15", "nam16", "nam18", "nam19")

for (df_name in data_frame_names) {
  assign(df_name, get(df_name)[get(df_name)$SPECCAT != 2, ])
}

data_frame_names <- c("nam93", "nam94", "nam95", "nam96", "nam97", "nam98", "nam99", "nam00", "nam01", "nam02", "nam03", "nam04", "nam05", "nam06",
                      "nam07", "nam08", "nam09", "nam10", "nam11", "nam12", "nam13", "nam14",
                      "nam15", "nam16", "nam18", "nam19")

obs_counts <- sapply(data_frame_names, function(name) nrow(get(name)))
obs_counts_df <- data.frame(
  name = data_frame_names,
  Observations_U1_nonsurgical = obs_counts
)
row.names(obs_counts_df) <- NULL

obs_counts_df$year <- substr(obs_counts_df$name,4,5)
for (i in 1:7) {
  obs_counts_df$year[i] <- paste0("19", obs_counts_df$year[i])
}

for (i in 8:26) {
  obs_counts_df$year[i] <- paste0("20", obs_counts_df$year[i])
}
obs_counts_df$year <- as.numeric(obs_counts_df$year)

ggplot(data=obs_counts_df, aes(x=year, y=Observations_U1_nonsurgical))+
  geom_bar(stat="identity", colour="black", fill ="skyblue1") +
  labs (title = "Number of non-surgical visit records over time, \npatients under age 1, NAMCS 1993-2019", x = "Survey year", y = "Number of records") +
  scale_x_continuous(breaks = seq(min(obs_counts_df$year), max(obs_counts_df$year), by = 2)) +
  scale_y_continuous(limits = c(0, 13000)) +
  geom_text(aes(label = Observations_U1_nonsurgical), vjust = -0.5, size=3)
