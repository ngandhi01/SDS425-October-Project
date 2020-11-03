#------------------------------------------------------------------------------#
# File: clean.R
# Description: Cleans and combines data from the World Bank Databank and the
#              World Bank and the World Bank Climate Change Knowledge Portal.
# Outputs: the following .csv files
#           - data_long.csv: contains combined Databank and Climate Change
#                            Knowledge Portal data in long format
#           - varnames_clean.csv: contains each time series code in
#                                 data_long.csv as well as their definitions
#           - data_wide_subset.csv: contains selected countries and variables of
#                                   interest in wide format
# Note: to minimize the number of .csv files created and rounding errors from
#       saving files to .csv and reading them in again, all data cleaning is
#       performed in a single script.
#------------------------------------------------------------------------------#
### Section 1: World Bank Databank cleaning
# Function: clean_wb
# Description: modifies data downloaded from the World Bank databank by
#              reshaping it into long format and encoding missing values.
#              Saves a csv called 'clean_worldbank.csv' of the cleaned data and
#              'varnames.csv' of the time series codes and their descriptions.
# Input(s): data.file - string of the path to the folder containing data
#                       downloaded from the World Bank
# Output(s): varnames - a data frame containing the time series codes in the
#                       data and their long definitions
#            data - a data frame containing each time series of each country in
#                   long format

# Clear workspace
rm(list = ls())

# Read in the data
clean_wb <- function(data.file) {
  x <- read.csv(data.file, stringsAsFactors = FALSE)
  
  # Make the data long
  xx <- reshape(x, 
                varying = names(x)[5:ncol(x)], 
                timevar = "year", 
                v.names = "value", 
                direction = "long", 
                times = names(x)[5:ncol(x)])
  
  # Clean up year column
  xx$year <- gsub(pattern = "\\..*", replacement = "", x = xx$year)
  xx$year <- as.numeric(gsub(pattern = "X", replacement = "", x = xx$year))
  
  # Delete the row names
  rownames(xx) <- NULL
  
  # Delete few rows of junk at the end
  xx <- xx[xx$Country.Name != "", ]
  
  # Save the time series variables and their descriptions
  nms <- unique(xx[, 1:2])
  
  # Delete the indicator column
  xx[, 1] <- NULL
  
  # Delete id column
  xx$id <- NULL
  
  # Recode Missing Values (missing values are encoded as ".." as.numeric 
  # introduces NA by coercion)
  xx$value <- as.numeric(xx$value)
  
  return(list(varnames = nms, data = xx))
}

# We downloaded data from the Databank on two separate occasions, and we will
# be cleaning and combining both

# First, clean both downloaded datasets
wb1 <- clean_wb(data.file = paste0(getwd(), "/raw/Data_Extract_Full/data.csv"))
wb2 <- clean_wb(data.file = paste0(getwd(), "/raw/Popular Indicators/data.csv"))

# Save the time series definition (varnames) data
vnames <- rbind(wb1$varnames, wb2$varnames)
write.csv(vnames, paste0(getwd(), "/final/varnames_clean.csv"))

# Combine the two datasets
# Also, we will consider data from 1991
wb_combn <- rbind(wb1$data[wb1$data$year >= 1991, ], 
                  wb2$data[wb2$data$year >= 1991, ])

# Remove duplicates entries (do not use last column for finding duplicates due 
# to rounding errors)
wb_combn <- wb_combn[which(!duplicated(wb_combn[, 1:4])), ]

### Section 2: World Bank Climate Change Knowledge Portal cleaning
# Read in the data
wb_temp <- read.csv(paste0(getwd(), "/raw/country_temp.csv"), 
                    stringsAsFactors = FALSE)
wb_temp$Temperature....Celsius. <- as.numeric(wb_temp$Temperature....Celsius.)
wb_temp$Year <- as.numeric(wb_temp$Year)

# Remove whitespace at the beginning of each country name
wb_temp$Country <- gsub(pattern = "^\\s", replacement = "", x = wb_temp$Country)
wb_temp$ISO3 <- gsub(pattern = "^\\s", replacement = "", x = wb_temp$ISO3)

# This data is on the month level. To get year level temperature data, we 
# averaged the temperatures over 12 months
wb_temp <- aggregate(x = wb_temp$Temperature....Celsius., 
                     by = list(Year = wb_temp$Year, 
                               Country = wb_temp$Country, 
                               ISO3 = wb_temp$ISO3), 
                     FUN = mean)
wb_temp$Series.Code <- "TEMP.YEARLY"

# To get a world level yearly temperature, we averaged the yearly temperature
# over all countries
wld.temp <- aggregate(x = wb_temp$x,
                      by = list(Year=wb_temp$Year),
                      FUN = mean)
wld.temp$Series.Code <- "WLD.AVG.TEMP.YEARLY"
wld.temp$Country <- "World"
wld.temp$ISO3 <- "WLD"

wb_temp <- rbind(wb_temp, wld.temp)
names(wb_temp)[names(wb_temp) == "x"] <- "value"

# Rename columns so that they match the names of the Databank data frame
names(wb_temp)[names(wb_temp) == "Year"] <- "year"
names(wb_temp)[names(wb_temp) == "Country"] <- "Country.Name"
names(wb_temp)[names(wb_temp) == "ISO3"] <- "Country.Code"

### Section 3: Combine Databank and Climate Change Knowledge Portal data
# Take data from 1991 to 2017
data_all_long <- rbind(wb_combn, wb_temp)
data_all_long <- data_all_long[data_all_long$year >= 1991, ]
data_all_long <- data_all_long[data_all_long$year <= 2017, ]

# Save combined dataset
write.csv(data_all_long, paste0(getwd(), "/final/data_long.csv"))

### Section 4: get an aggregated dataset in wide format
# Function: datawide
# Description: takes long format data from the World Bank and converts it to
#              wide format
# Input(s): x - a long format data frame
# Output(s): x - a wide format data frame
datawide <- function(x){
  # Take out the country name/code columns before calling reshape()
  x <- x[, !(names(x) %in% c("Country.Name", "Country.Code"))]
  
  # reshape data into wide
  x <- reshape(x,
               idvar = "year",
               timevar = "Series.Code",
               direction = "wide")
  
  # Remove variables with too many missing values
  x <- x[,!colSums(is.na(x)) > 3]
  x <- x[complete.cases(x),]
  return(x)
}

# Countries of interest
countries <- c("Brazil", 
               "China", 
               "Germany", 
               "India", 
               "United Kingdom", 
               "United States", 
               "World")

# Variables we selected to narrow down
vars <- c("year",
          "TEMP.YEARLY",
          "WLD.AVG.TEMP.YEARLY",
          "SP.DYN.LE00.FE.IN", 
          "SP.DYN.LE00.MA.IN",
          "SL.UEM.TOTL.ZS",
          "EN.POP.DNST",
          "SP.POP.GROW",
          "SP.URB.TOTL.IN.ZS",
          "AG.SRF.TOTL.K2",
          "AG.PRD.LVSK.XD",
          "AG.LND.AGRI.ZS",
          "EN.ATM.CO2E.KT",
          "EG.USE.COMM.CL.ZS",
          "EG.USE.PCAP.KG.OE",
          "EG.FEC.RNEW.ZS")

# Combine all of the country level dataframe together
x <- c()
for (i in 1:length(countries)) {
  # Subset each country
  temp <- data_all_long[data_all_long$Country.Name == countries[i], ]
  
  # Format into wide format
  temp <- datawide(temp)
  
  # Clean up the variable names
  names(temp) <- gsub("value.", "", names(temp))
  
  # Only select variables of interest
  temp <- temp[, names(temp) %in% vars]
  
  # Add a string specifying the country
  temp$Country.Name <- countries[i]
  
  # Rename the variable name for the world level
  if(i == 7) names(temp)[names(temp) == "WLD.AVG.TEMP.YEARLY"] <- "TEMP.YEARLY" 
  
  # Add a variable looking at the difference between the temperature in year t 
  # and the temperature in 1991. This is a better metric for climate change 
  # because it tracks the change in temperature and subtracts out the baseline 
  # temperature
  temp$temp.diff <- temp$TEMP.YEARLY - temp$TEMP.YEARLY[1]
  
  # Concatenate data frame onto the big data frame
  x <- rbind(x, temp)
}

# Save aggregated data
write.csv(x, paste0(getwd(), "/final/", "data_wide_subset.csv"))
