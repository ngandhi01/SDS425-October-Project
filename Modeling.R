library(MASS)
library(GGally)
library(tidyverse)
library(caret)
library(leaps)
library(plm)
library(corrplot)

# Read in the data
# wb_data_long <- read.csv("data_all.csv", stringsAsFactors = FALSE)
wb_data_long <- read.csv("data_long.csv", stringsAsFactors = FALSE)

# Remove duplicate rows 
wb_data_long <- wb_data_long[which(!duplicated(wb_data_long[, 1:4])), ]

# Sanity Check
nrow(wb_data_long)
head(wb_data_long)
tail(wb_data_long)

# Remove all the NA rows
wb_data_long <- wb_data_long[!is.na(wb_data_long$value),]

# Subset to only include information up until 2017 
wb_data_long <- wb_data_long[!wb_data_long$year > 2017,]

# Extract only the world information
world_data <- wb_data_long[wb_data_long$Country.Name == "World",]
head(world_data)

# Remove the country name and country code as this is irrelevant information in 
# this subsetted data 
world_data <- world_data[,-c(1,3,4)]

# Make the information in wide format so it is easier to model 
world_data_wide <- reshape(world_data, 
                        idvar = "year", 
                        timevar = "Series.Code", 
                        direction = "wide")

# Remove indicators with excessive missing information
world_data_wide <- world_data_wide[,!colSums(is.na(world_data_wide)) > 3]

# Complete cases so we can later run linear regression
world_data_wide <- world_data_wide[complete.cases(world_data_wide),]

# Build the above into a function so it is easy to repeat for other countries
datawide <- function(x){
  x <- x[,-c(1,3,4)]
  x <- reshape(x,
               idvar = "year",
               timevar = "Series.Code",
               direction = "wide")
  x <- x[,!colSums(is.na(x)) > 3]
  x <- x[complete.cases(x),]
  return(x)
}

# summary(lm(world_data_wide$value.WLD.AVG.TEMP.YEARLY ~., 
#            data = world_data_wide))
# 
# summary(lm(world_data_wide$value.WLD.AVG.TEMP.YEARLY ~ value.SP.RUR.TOTL.ZS, data = world_data_wide))

nrow(world_data_wide)
ncol(world_data_wide)

corrplot(cor(world_data_wide), method= "number")

# Modeling for the World Data 
# full.world.model <- regsubsets(world_data_wide$value.WLD.AVG.TEMP.YEARLY ~.,
#                                data = world_data_wide,
#                                nvmax = 5,
#                                method = "seqrep")

# # List of the predictors that we have selected after qualitative processing
# world_data_wide$value.WLD.AVG.TEMP.YEARLY
# world_data_wide$value.SP.DYN.LE00.FE.IN
# world_data_wide$value.SP.DYN.LE00.MA.IN
# world_data_wide$value.SL.UEM.TOTL.ZS
# world_data_wide$value.EN.POP.DNST
# world_data_wide$value.SP.POP.GROW
# world_data_wide$value.SP.URB.TOTL.IN.ZS
# world_data_wide$value.AG.SRF.TOTL.K2
# world_data_wide$value.AG.PRD.LVSK.XD
# world_data_wide$value.AG.LND.AGRI.ZS
# world_data_wide$value.EN.ATM.CO2E.KT
# world_data_wide$value.EG.USE.COMM.CL.ZS
# world_data_wide$value.EG.USE.PCAP.KG.OE
# world_data_wide$value.EG.FEC.RNEW.ZS

full.world.model1 <- lm(value.WLD.AVG.TEMP.YEARLY ~ 
     value.SP.DYN.LE00.FE.IN + value.SP.DYN.LE00.MA.IN + value.SL.UEM.TOTL.ZS + 
     value.EN.POP.DNST + value.SP.POP.GROW + value.SP.URB.TOTL.IN.ZS + 
     value.AG.SRF.TOTL.K2 + value.AG.PRD.LVSK.XD + value.AG.LND.AGRI.ZS +
     value.EN.ATM.CO2E.KT + value.EG.USE.COMM.CL.ZS + value.EG.USE.PCAP.KG.OE + 
     value.EG.FEC.RNEW.ZS, data = world_data_wide)

# Forward stepwise regression
step.world.model1 <- stepAIC(full.world.model1, direction = "forward")
summary(step.world.model1)

# Backward stepwise regression
step.world.model1 <- stepAIC(full.world.model1, direction = "backward")
summary(step.world.model1)

# Both
step.world.model1 <- stepAIC(full.world.model1, direction = "both")
summary(step.world.model1)

# # Significant predictors
# world_data_wide$value.SP.URB.TOTL.IN.ZS
# world_data_wide$value.AG.LND.AGRI.ZS
# world_data_wide$value.EN.ATM.CO2E.KT

# Old (Before 10/28) - For Reference only, will be deleted in final submission
# full.world.model <- lm(value.WLD.AVG.TEMP.YEARLY ~ value.IS.AIR.GOOD.MT.K1 + 
#                          value.IS.AIR.PSGR + value.SP.DYN.LE00.FE.IN + 
#                          value.SP.DYN.LE00.MA.IN + value.SM.POP.REFG + 
#                          value.SL.TLF.CACT.FE.ZS + value.SL.TLF.CACT.MA.ZS + 
#                          value.SL.UEM.TOTL.ZS + value.EN.POP.DNST + 
#                          value.SP.POP.GROW + value.AG.LND.FRST.ZS + 
#                          value.SP.URB.TOTL.IN.ZS + value.AG.SRF.TOTL.K2 + 
#                          value.AG.PRD.CROP.XD + value.AG.PRD.FOOD.XD + 
#                          value.AG.PRD.LVSK.XD + value.SP.RUR.TOTL + 
#                          value.SP.URB.TOTL + value.AG.LND.AGRI.ZS + 
#                          value.AG.LND.ARBL.ZS + value.AG.LND.ARBL.HA.PC + 
#                          value.EN.ATM.CO2E.KT, data = world_data_wide)
# 
# summary(full.world.model)
# 
# world_data_wide$value.IS.AIR.GOOD.MT.K1
# 
# # combine several levels into one 
# 
# # check value.EN.ATM.CO2E.PC as this was creating problems 
# 
# summary(full.world.model)
# 
# # Forward stepwise regression
# step.world.model <- stepAIC(full.world.model, direction = "forward")
# summary(step.world.model)
# 
# # Backward stepwise regression
# step.world.model <- stepAIC(full.world.model, direction = "backward")
# summary(step.world.model)
# 
# # Both
# step.world.model <- stepAIC(full.world.model, direction = "both")
# summary(step.world.model)


# Now that we've found these significant predictors, let's see their predictions
# at a country level 
# Extract only information about the US and repeat the above 
US_data <- wb_data_long[wb_data_long$Country.Name == "United States",]
US_data_wide <- datawide(US_data)

# full.US.model <- lm(value.TEMP.YEARLY ~ value.SP.URB.TOTL.IN.ZS + 
#                       value.AG.LND.AGRI.ZS + value.EN.ATM.CO2E.KT, 
#                     data = US_data_wide)

full.US.model <- lm(value.TEMP.YEARLY ~ value.SP.DYN.LE00.FE.IN + 
                      value.SP.DYN.LE00.MA.IN + value.SL.UEM.TOTL.ZS + 
                      value.EN.POP.DNST + value.SP.POP.GROW + 
                      value.SP.URB.TOTL.IN.ZS + value.AG.SRF.TOTL.K2 + 
                      value.AG.PRD.LVSK.XD + value.AG.LND.AGRI.ZS +
                      value.EN.ATM.CO2E.KT + value.EG.USE.COMM.CL.ZS + 
                      value.EG.USE.PCAP.KG.OE + value.EG.FEC.RNEW.ZS, 
                    data = US_data_wide)
summary(stepAIC(full.US.model, direction = 'both'))

# Extract only information about China and repeat the above
China_data <- wb_data_long[wb_data_long$Country.Name == "China",]
China_data_wide <- datawide(China_data)

full.China.model <- lm(value.TEMP.YEARLY ~ value.SP.DYN.LE00.FE.IN + 
                         value.SP.DYN.LE00.MA.IN + value.SL.UEM.TOTL.ZS + 
                         value.EN.POP.DNST + value.SP.POP.GROW + 
                         value.SP.URB.TOTL.IN.ZS + value.AG.SRF.TOTL.K2 + 
                         value.AG.PRD.LVSK.XD + value.AG.LND.AGRI.ZS +
                         value.EN.ATM.CO2E.KT + value.EG.USE.COMM.CL.ZS + 
                         value.EG.USE.PCAP.KG.OE + value.EG.FEC.RNEW.ZS,
                       data = China_data_wide)
summary(stepAIC(full.China.model, direction = 'both'))

# Extract only information about Brazil and repeat the above 
Brazil_data <- wb_data_long[wb_data_long$Country.Name == "Brazil",]
Brazil_data_wide <- datawide(Brazil_data)

full.Brazil.model <- lm(value.TEMP.YEARLY ~ value.SP.DYN.LE00.FE.IN + 
                          value.SP.DYN.LE00.MA.IN + value.SL.UEM.TOTL.ZS + 
                          value.EN.POP.DNST + value.SP.POP.GROW + 
                          value.SP.URB.TOTL.IN.ZS + value.AG.SRF.TOTL.K2 + 
                          value.AG.PRD.LVSK.XD + value.AG.LND.AGRI.ZS +
                          value.EN.ATM.CO2E.KT + value.EG.USE.COMM.CL.ZS + 
                          value.EG.USE.PCAP.KG.OE + value.EG.FEC.RNEW.ZS,
                        data = Brazil_data_wide)
summary(stepAIC(full.Brazil.model, direction = 'both'))

# Extract only information about India and repeat the above
India_data <- wb_data_long[wb_data_long$Country.Name == "India",]
India_data_wide <- datawide(India_data)

full.India.model <- lm(value.TEMP.YEARLY ~ value.SP.DYN.LE00.FE.IN + 
                         value.SP.DYN.LE00.MA.IN + value.SL.UEM.TOTL.ZS + 
                         value.EN.POP.DNST + value.SP.POP.GROW + 
                         value.SP.URB.TOTL.IN.ZS + value.AG.SRF.TOTL.K2 + 
                         value.AG.PRD.LVSK.XD + value.AG.LND.AGRI.ZS +
                         value.EN.ATM.CO2E.KT + value.EG.USE.COMM.CL.ZS + 
                         value.EG.USE.PCAP.KG.OE + value.EG.FEC.RNEW.ZS,
                       data = India_data_wide)
summary(stepAIC(full.India.model, direction = 'both'))

# Extract only information about Germany and repeat the above
Germany_data <- wb_data_long[wb_data_long$Country.Name == "Germany",]
Germany_data_wide <- datawide(Germany_data)

full.Germany.model <- lm(value.TEMP.YEARLY ~ value.SP.DYN.LE00.FE.IN + 
                           value.SP.DYN.LE00.MA.IN + value.SL.UEM.TOTL.ZS + 
                           value.EN.POP.DNST + value.SP.POP.GROW + 
                           value.SP.URB.TOTL.IN.ZS + value.AG.SRF.TOTL.K2 + 
                           value.AG.PRD.LVSK.XD + value.AG.LND.AGRI.ZS +
                           value.EN.ATM.CO2E.KT + value.EG.USE.COMM.CL.ZS + 
                           value.EG.USE.PCAP.KG.OE + value.EG.FEC.RNEW.ZS,
                         data = Germany_data_wide)
summary(stepAIC(full.Germany.model, direction = 'both'))

# Extract only information about the UK and repeat the above
UK_data <- wb_data_long[wb_data_long$Country.Name == "United Kingdom",]
UK_data_wide <- datawide(UK_data)

full.UK.model <- lm(value.TEMP.YEARLY ~ value.SP.DYN.LE00.FE.IN + 
                      value.SP.DYN.LE00.MA.IN + value.SL.UEM.TOTL.ZS + 
                      value.EN.POP.DNST + value.SP.POP.GROW + 
                      value.SP.URB.TOTL.IN.ZS + value.AG.SRF.TOTL.K2 + 
                      value.AG.PRD.LVSK.XD + value.AG.LND.AGRI.ZS +
                      value.EN.ATM.CO2E.KT + value.EG.USE.COMM.CL.ZS + 
                      value.EG.USE.PCAP.KG.OE + value.EG.FEC.RNEW.ZS,
                    data = UK_data_wide)
summary(stepAIC(full.UK.model, direction = 'both'))
