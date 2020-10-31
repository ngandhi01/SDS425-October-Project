# File: modeling.R
# Description: this file examines the cleaned data in more detail to determine
# which variables should be transformed, etc.

library(e1071)

### Section 1: Read in the data
# Countries of interest
countries <- c("Brazil", "China", "Germany", "India", "UK", "US", "world")

# Variables we selected to narrow down
vars <- c("year",
          "TEMP.YEARLY",
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

### Section 2: aggregate country level data together
# Combine all of the country level dataframe together
x <- c()
for (i in 1:(length(countries)-1)) {
  temp <- read.csv(paste0(getwd(), 
                          "/cleaning/final/", 
                          countries[i], 
                          "_data_wide.csv"), 
                   stringsAsFactors = FALSE)
  names(temp) <- gsub("value.", "", names(temp))
  temp <- temp[, names(temp) %in% vars]
  temp$Country.Name <- countries[i]
  x <- rbind(x, temp)
}

### Section 3: Data transformation
# Plot histograms of each variables and transformed variables
# Also plot skewness of data
# Transforms used:
#   - No transform: x
#   - log: ln(x)
#   - square root: sqrt(x)
#   - squared: x^2
#   - z-standardized: (x-mean(x)) / sd(x)
#   - max-min: (x - min(x)) / (max(x) - min(x))
pdf("world_plots.pdf", width = 9, height = 6)
for(j in 3:ncol(x)-1) {
  par(mfrow = c(2, 3))
  nm <- names(x)[j]
  
  temp <- x[, j] 
  hist(temp, 
       main = paste0(nm, "\n skew=", as.character(skewness(temp))), 
       freq = FALSE)
  lines(density(temp))
  
  temp <- log(x[, j][x[, j] > 0]) 
  hist(temp, 
       main = paste0("ln(", nm, ") \n skew=", as.character(skewness(temp))), 
       freq = FALSE)
  lines(density(temp))
  
  temp <- sqrt(x[, j][x[, j] > 0]) 
  hist(temp, 
       main = paste0("sqrt(", nm, ") \n skew=", as.character(skewness(temp))), 
       freq = FALSE)
  lines(density(temp))
  
  temp <- x[, j]^2 
  hist(temp, 
       main = paste0(nm, "^2 \n skew=", as.character(skewness(temp))), 
       freq = FALSE)
  lines(density(temp))
  
  temp <- scale(x[, j]) 
  hist(temp, 
       main = paste0("z-scale", nm, "\n skew=", as.character(skewness(temp))), 
       freq = FALSE)
  lines(density(temp))
  
  temp <- (x[, j] - min(x[, j]))/(max(x[, j] - min(x[, j]))) 
  hist(temp, 
       main = paste0("max-min", nm, "\n skew=", as.character(skewness(temp))), 
       freq = FALSE)
  lines(density(temp))
}

# Look at the difference between the temperature in year t and the temperature
# in 1991. This is a better metric for climate change because it tracks the 
# change in temperature and subtracts out the baseline temp
y <- c()
for(i in 1:(length(countries)-1)) {
  temp <- x$TEMP.YEARLY[x$Country.Name == countries[i]]
  y <- c(y, temp - temp[1])
}
x$temp.diff <- y

# Repeat the same set of plots for this variable
par(mfrow = c(2, 3))

temp <- y
hist(temp, 
     main = paste0("Temp diff \n skew=", as.character(skewness(temp))), 
     freq = FALSE)
lines(density(temp))

temp <- log(y[y > 0]) 
hist(temp, 
     main = paste0("log(Temp diff) \n skew=", as.character(skewness(temp))), 
freq = FALSE)
lines(density(temp))

temp <- sqrt(y[y > 0]) 
hist(temp, 
     main = paste0("sqrt( Temp diff) \n skew=", as.character(skewness(temp))), 
     freq = FALSE)
lines(density(temp))

temp <- y^2 
hist(temp, main = paste0("Temp diff^2, \n skew=", as.character(skewness(temp))), 
     freq = FALSE)
lines(density(temp))

temp <- scale(y) 
hist(temp, 
     main = paste0("z-scale Temp diff \n skew=", as.character(skewness(temp))), 
     freq = FALSE)
lines(density(temp))

temp <- (y - min(y))/(max(y) - min(y)) 
hist(temp, 
     main = paste0("max-min Temp diff \n skew=", as.character(skewness(temp))), 
     freq = FALSE)
lines(density(temp))
dev.off()

# Save the country level aggregated data
write.csv(x, paste0(getwd(), "/cleaning/final/", "country_aggregated.csv"))

# Based on the plots, try transforming the following variables
x$SP.DYN.LE00.FE.IN_sq <- x$SP.DYN.LE00.FE.IN^2
x$SP.DYN.LE00.MA.IN_sq <- x$SP.DYN.LE00.MA.IN^2
x$SL.UEM.TOTL.ZS_sqrt <- sqrt(x$SL.UEM.TOTL.ZS)
x$EN.POP.DNST_sqrt <- sqrt(x$EN.POP.DNST)
x$SP.URB.TOTL.IN.ZS_sq <- x$SP.URB.TOTL.IN.ZS^2
x$AG.PRD.LVSK.XD_sq <- x$AG.PRD.LVSK.XD^2
x$EN.ATM.CO2E.KT_log <- log(x$EN.ATM.CO2E.KT)
x$EG.USE.COMM.CL.ZS_sq <- x$EG.USE.COMM.CL.ZS^2
x$EG.USE.PCAP.KG.OE_log <- log(x$EG.USE.PCAP.KG.OE)
x$EG.FEC.RNEW.ZS_sq <- sqrt(x$EG.FEC.RNEW.ZS)

### Section 4: Correlated variables
# There are too many variables to do a proper pairs plot
# Instead, iterate and put each plot in a pdf
transformed <- c("SP.DYN.LE00.FE.IN",
                 "SP.DYN.LE00.MA.IN",
                 "SL.UEM.TOTL.ZS",
                 "EN.POP.DNST",
                 "SP.URB.TOTL.IN.ZS",
                 "AG.PRD.LVSK.XD",
                 "EN.ATM.CO2E.KT",
                 "EG.USE.COMM.CL.ZS",
                 "EG.USE.PCAP.KG.OE",
                 "EG.FEC.RNEW.ZS")
# Subset the data to only contain the transformed variables
xx <- x[, !(names(x) %in% transformed)]
xx$Country.Name <- as.factor(xx$Country.Name)

varpairs <- combn(names(xx), 2)
pdf("pairsplot", width = 5, height = 5)
for(i in 1:ncol(varpairs)) {
  plot(xx[, varpairs[1, i]], xx[, varpairs[2, i]], 
       xlab = varpairs[1, i], ylab = varpairs[2, i], 
       main = paste0(varpairs[2, i], " vs. ", varpairs[1, i]))  
}
dev.off()