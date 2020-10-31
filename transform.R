library(e1071)

countries <- c("Brazil", "China", "Germany", "India", "UK", "US", "world")
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

for (i in 1:length(countries)) {
  x <- read.csv(paste0(getwd(), 
                       "/cleaning/final/", 
                       countries[i], 
                       "_data_wide.csv"), stringsAsFactors = FALSE)
  names(x) <- gsub("value.", "", names(x))
  x <- x[, names(x) %in% vars]
  
  pdf(paste0(countries[i], "_hists.pdf"))
  for(j in 2:ncol(x)) {
    par(mfrow = c(2, 2))
    hist(x[, j], main = names(x)[j]) 
    plot(x$year, x[, j],
         xlab = "year",
         ylab = names(x)[j],
         main = paste("Time Series of", names(x)[j]))
    plot(x[, j], x$WLD.TEMP.ANOMALY, 
         xlab = names(x)[j],
         ylab = "Temp fluctutations",
         main = paste("Temp anomaly vs.", names(x)[j]))
    plot(x[, j], x$WLD.AVG.TEMP.YEARLY, 
         xlab = names(x)[j],
         ylab = "Avg surface temp",
         main = paste("Avg Temp vs.", names(x)[j]))
  }
  dev.off()
}


x <- c()
for (i in 1:(length(countries)-1)) {
  temp <- read.csv(paste0(getwd(), "/cleaning/final/", countries[i], "_data_wide.csv"), stringsAsFactors = FALSE)
  names(temp) <- gsub("value.", "", names(temp))
  temp <- temp[, names(temp) %in% vars]
  temp$Country.Name <- countries[i]
  x <- rbind(x, temp)

}

pdf("world_plots.pdf", width = 9, height = 6)
for(j in 3:ncol(x)-1) {
  par(mfrow = c(2, 3))
  hist(x[, j], main = paste0(names(x)[j], "\n skewness = ", as.character(skewness(x[, j]))), freq = FALSE)
  lines(density(x[, j]))
  
  hist(log(x[, j][x[, j] > 0]), main = paste0("log(", names(x)[j], ") \n skewness = ", as.character(skewness(log(x[, j][x[, j] > 0])))), freq = FALSE)
  lines(density(log(x[, j][x[, j] > 0])))
  
  hist(sqrt(x[, j][x[, j] > 0]), main = paste0("sqrt(", names(x)[j], ") \n skewness = ", as.character(skewness(sqrt(x[, j][x[, j] > 0])))), freq = FALSE)
  lines(density(sqrt(x[, j][x[, j] > 0])))
  
  hist(x[, j]^2, main = paste0(names(x)[j], "^2, \n skewness = ", as.character(skewness(x[, j]^2))), freq = FALSE)
  lines(density(x[, j]^2))
  
  hist(scale(x[, j]), main = paste0("z-scale", names(x)[j], "\n skewness = ", as.character(skewness(scale(x[, j])))), freq = FALSE)
  lines(density(scale(x[, j])))
  
  hist((x[, j] - min(x[, j]))/(max(x[, j] - min(x[, j]))), main = paste0("max-min", names(x)[j], "\n skewness = ", as.character(skewness((x[, j] - min(x[, j]))/(max(x[, j] - min(x[, j])))))), freq = FALSE)
  lines(density((x[, j] - min(x[, j]))/(max(x[, j]) - min(x[, j]))))
}

y <- c()
for(i in 1:(length(countries)-1)) {
  temp <- x$TEMP.YEARLY[x$Country.Name == countries[i]]
  y <- c(y, temp - temp[1])
}

par(mfrow = c(2, 3))
hist(y, main = paste0("Temp diff \n skewness = ", as.character(skewness(y))), freq = FALSE)
lines(density(y))

hist(log(y[y > 0]), main = paste0("log(Temp diff) \n skewness = ", as.character(skewness(log(y[y > 0])))), freq = FALSE)
lines(density(log(y[y > 0])))

hist(sqrt(y[y > 0]), main = paste0("sqrt( Temp diff) \n skewness = ", as.character(skewness(sqrt(y[y > 0])))), freq = FALSE)
lines(density(sqrt(y[y > 0])))

hist(y^2, main = paste0("Temp diff^2, \n skewness = ", as.character(skewness(y^2))), freq = FALSE)
lines(density(y^2))

hist(scale(y), main = paste0("z-scale Temp diff \n skewness = ", as.character(skewness(scale(y)))), freq = FALSE)
lines(density(scale(y)))

hist((y - min(y))/(max(y) - min(y)), main = paste0("max-min Temp diff \n skewness = ", as.character(skewness((y - min(y))/(max(y) - min(y))))), freq = FALSE)
lines(density((y - min(y))/(max(y) - min(y))))
dev.off()
