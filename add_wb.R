# file: add_wb.R
# Description: adds additional downloaded timeseries from the World Bank
#              databank to the 'cleaned_wb.csv' dataset and their variables
#              names to 'varnames.csv'

# Run the "clean_wb.R" script to load the function into the workspace
source("clean_wb.R")

# # Run the scripts on the two datasets
# clean_wb(data.file = paste0(getwd(),"/raw/Data_Extract_Full/data.csv"), 
#          out.filenames = c(paste0(getwd(), "/saveTo/varnames.csv"), paste0(getwd(), "/saveTo/cleaned_wb.csv"))
# 
# clean_wb(data.file = paste0(getwd(),"/raw/Popular Indicators/data.csv"), 
#          out.filenames = c(paste0(getwd(), "/saveTo/varnames2.csv"), paste0(getwd(), "/saveTo/cleaned_wb2.csv"))

# Load these datasets back in
x <- read.csv(paste0(getwd(), "/saveTo/cleaned_wb.csv"), stringsAsFactors = FALSE)
x2 <- read.csv(paste0(getwd(), "/saveTo/cleaned_wb2.csv"), stringsAsFactors = FALSE)

xx <- rbind(x, x2)
write.csv(xx, paste0(getwd(), "/saveTo/clean_wb_add.csv"))

# Load the variable names back in
v <- read.csv(paste0(getwd(), "/saveTo/varnames.csv"), stringsAsFactors = FALSE)
v2 <- read.csv(paste0(getwd(), "/saveTo/varnames2.csv"), stringsAsFactors = FALSE)

vv <- rbind(v, v2)
write.csv(vv, paste0(getwd(), "/final/varnames_clean.csv"))