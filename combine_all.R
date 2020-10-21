# file: combine_all.R
# Description: Combines the cleaned world bank data (clean_wb_add.csv) and the
#              cleaned global temperature data(temp_combn.csv) and saves it as
#              data_all.csv

# World bank data
wb <- read.csv(paste0(getwd(), "/saveTo/clean_wb_add.csv"), stringsAsFactors = FALSE)

# Delete row names
wb$X.1 <- NULL
wb$X <- NULL

# Temperature data
temp <- read.csv(paste0(getwd(), "/saveTo/temp_combn.csv"), stringsAsFactors = FALSE)

# Delete row names
temp$X <- NULL

# Rename columns
names(temp)[names(temp) == "Year"] <- "year"
names(temp)[names(temp) == "Country"] <- "Country.Name"
names(temp)[names(temp) == "ISO3"] <- "Country.Code"

# Determine row cutoffs
wb.temp <- rbind(wb, temp)
wb.temp <- wb.temp[wb.temp$year >= 1991 | wb.temp$year <= 2019, ]

# Save
write.csv(wb.temp, paste0(getwd(), "/final/data_all.csv"))