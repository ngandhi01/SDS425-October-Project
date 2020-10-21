# file: clean_temp.R
# Description: cleans and combines the temperature data

# Clean world data
world <- read.csv(paste0(getwd(), "/raw/data_global_temp.csv"), stringsAsFactors = FALSE)
world <- as.data.frame(world[-(1:4), ])
names(world) <- c("Year", "value")

world$Series.Code <- "WLD.TEMP.ANOMALY"
world$Country <- "World"
world$ISO3 <- "WLD"

world$Year <- as.numeric(world$Year)
world$value <- as.numeric(world$value)

# Clean country data
country <- read.csv(paste0(getwd(), "/raw/country_temp.csv", stringsAsFactors = FALSE))
country$Temperature....Celsius. <- as.numeric(country$Temperature....Celsius.)
country$Year <- as.numeric(country$Year)

# Average the temperatures over 12 months (report average yearly temp)
country <- aggregate(x = country$Temperature....Celsius., 
                     by = list(Year=country$Year, 
                               Country=country$Country, 
                               ISO3=country$ISO3), 
                     FUN = mean)
country$Series.Code <- "TEMP.YEARLY"

# Average the temperatures over all countries (report world temp)
wld.temp <- aggregate(x = country$x,
                      by = list(Year=country$Year),
                      FUN = mean)
wld.temp$Series.Code <- "WLD.AVG.TEMP.YEARLY"
wld.temp$Country <- "World"
wld.temp$ISO3 <- "WLD"

country <- rbind(country, wld.temp)
names(country)[names(country) == "x"] <- "value"

# Combine country and world data
temp_combn <- rbind(country, world)

# Save
write.csv(temp_combn, paste0(getwd(), "/saveTo/temp_combn.csv"))