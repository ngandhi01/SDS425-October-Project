# File: clean_wb.R (contains function clean_wb)
# Description: modifies data downloaded from the World Bank databank by
#              reshaping it into long format and encoding missing values.
#              Saves a csv called 'clean_worldbank.csv' of the cleaned data and
#              'varnames.csv' of the time series codes and their descriptions.

# Read in the data
clean_wb <- function(data.file, out.filenames) {
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
  xx <- xx[xx$Country.Name != "",]
  
  # Save the time series variables and their descriptions
  nms <- unique(xx[, 1:2])
  
  # Save as .csv
  write.csv(nms, out.filenames[1])
  
  # Delete the indicator column
  xx[, 1] <- NULL
  
  # Delete id column
  xx$id <- NULL
  
  # Recode Missing Values
  xx$value <- as.numeric(xx$value)
  
  # Save dataset
  write.csv(xx, paste0(getwd(), "/saveTo/", out.filenames[2]))
}