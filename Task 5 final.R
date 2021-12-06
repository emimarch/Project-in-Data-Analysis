
#############
## READ ME ##
#############

# This files reports all the code used for tables and graphs in the report. 
# It is divided into sections and the section follow the flow of the report. 
# The last section "OTHERS" includes all the analysis and additional models that did not get
# included in the report because not relevant. Before running the OTHER sections all other previous 
# sections should be run (because of variables that are created before the section). 
# In case the graphs do not show, it helps to dev.off() as the par() function might 
# affect how the graphs are graphed. 

# Please install the following packages 
# corrplot
# ggplot2
# ggpubr
# vioplot
# stats
# graphics
# stargazer  (for latex output plot)

# Possibly others that I had already downloaded. 

#################################
## STEP 1: Importing data sets ##
#################################


setwd('C:/Users/Emilia Marchese/OneDrive/Project in Data Anlysis')
electionstat <- read.csv(file = 'ek2019.csv', stringsAsFactors = FALSE)
incomestat <- read.csv(file = 'tulot2017.csv', stringsAsFactors = FALSE)

# Merging datasets by municipality (column name Alue)
# Missing observations were dropped in the merging. So total number of observations is 295

mergedf <- merge(electionstat, incomestat, by = "Alue")
str(mergedf)

#Getting parties name and colors
parties <- colnames(electionstat)[3:ncol(electionstat)-1]
print(parties)
colors <- rainbow(length(parties))

#############################
## STEP 2: Party selection ##
#############################

# Find total votes by party

df1 <-  subset(mergedf, select = c(SDP:Muut))
colsum <- colSums(df1)
print(colsum)

#   SDP     PS    KOK   KESK   VIHR    VAS    RKP     KD    SIN    PIR    STL     KP     FP 
# 4706.4 5748.9 3411.8 7841.7 1675.0 2146.5 1371.2 1357.1  326.2   71.4  130.3   89.7   22.6 
# LIB    SKP    EOP     IP    SKE    KTP   Muut 
# 17.1   31.9   14.3   21.9   18.2   10.2  484.5


# Select only the ones with sum > 1000, so 8 parties 
eightMostVotedIDs<- order(colsum,decreasing=TRUE)[1:8] 
eightMostVotedNames <- names(df1[eightMostVotedIDs])

print(eightMostVotedNames)
# [1] "KESK" "PS"   "SDP"  "KOK"  "VAS"  "VIHR" "RKP"  "KD" 

########################################
## STEP 3:  Exploratory Data Analysis ##
########################################

# Summary statistics about eightMostVotedNames

summary_parties <- lapply(mergedf[,eightMostVotedNames], summary)

# Summary statistics about income and taxation variables chosen 

summary_inc <- lapply(mergedf[, c("Tulot", "Ansiotulot", "P‰‰omatulot", "Verot", "Valtionvero", "Kunnallisvero", "Mediaanitulot")], summary)


# Box plots and violin plots for variables
library(vioplot)
boxplot(mergedf$Tulot,mergedf$Ansiotulot, mergedf$P‰‰omatulot, mergedf$Verot, mergedf$Kunnallisvero, mergedf$Valtionvero, names = c("Tulot", "Ansiotulot", "Paaomatulot", "Verot", "Kunnallisvero", "Valtionvero"), main = "Boxplots for income variables")$out
vioplot(mergedf$Tulot,mergedf$Ansiotulot, mergedf$P‰‰omatulot, mergedf$Verot, mergedf$Kunnallisvero, mergedf$Valtionvero, names = c("Tulot", "Ansiotulot", "Paaomatulot", "Verot", "Kunnallisvero", "Valtionvero"), main = "Violin plots for income variables")

# Finding extreme outlier in plots

max <- max(mergedf$Tulot)
# 63150

maxindex <- which(mergedf$Tulot == max, arr.ind = TRUE)
# 81

print(mergedf$Alue[81])
# "Kauniainen"

# Checking that Kauniainen is the maximum in all

print(mergedf$Tulot[mergedf$Alue == "Kauniainen"])
print(which(mergedf$Ansiotulot == max(mergedf$Ansiotulot), arr.ind = TRUE)) #81
print(which(mergedf$P‰‰omatulot == max(mergedf$P‰‰omatulot), arr.ind = TRUE)) #81
print(which(mergedf$Verot == max(mergedf$Verot), arr.ind = TRUE)) #81
print(which(mergedf$Kunnallisvero == max(mergedf$Kunnallisvero), arr.ind = TRUE)) #81
print(which(mergedf$Valtionvero == max(mergedf$Valtionvero), arr.ind = TRUE))#81


# Creating estimates for tax-rates

mergedf$VerotRate <- (mergedf$Verot*100/mergedf$Tulot)
mergedf$KunnallisveroRate <- (mergedf$Kunnallisvero*100/mergedf$Tulot)
mergedf$ValtionveroRate <- (mergedf$Valtionvero*100/mergedf$Tulot)

#Checking that Kauniainen is the maximum in ValtionveroRate and VerotRate

print(which(mergedf$Verot == max(mergedf$Verot), arr.ind = TRUE)) #81
print(which(mergedf$KunnallisveroRate == max(mergedf$KunnallisveroRate), arr.ind = TRUE)) #25
print(which(mergedf$ValtionveroRate == max(mergedf$ValtionveroRate), arr.ind = TRUE))#81


# Creating box plots and violin plots for taxation-rates

library(vioplot)
boxplot(mergedf$VerotRate,mergedf$KunnallisveroRate, mergedf$ValtionveroRate, names = c("Verot rate", "Kunnallisverot rate", "Valtionverot rate"), main = "Boxplots for tax rates estiamates")$out
vioplot(mergedf$VerotRate,mergedf$KunnallisveroRate, mergedf$ValtionveroRate, names = c("Verot rate", "Kunnallisverot rate", "Valtionverot rate"), main = "Violinplots for tax rates estiamates")

# Printing municipality rate for Kauniainen

print(mergedf$KunnallisveroRate[mergedf$Alue == "Kauniainen"])
# 11.81314

# Creating new dataframe without Kauniainen. Note, this new data set will be used from now on

mergedfn <- subset(mergedf, mergedf$Alue != "Kauniainen")

# Correlation matrix

library(stargazer)
correlation.matrix.parties <- cor(mergedfn[,c("KESK","PS","SDP","KOK","VAS","VIHR","RKP","KD","Tulot", "Ansiotulot", "P‰‰omatulot", "VerotRate", "ValtionveroRate","KunnallisveroRate")])
stargazer(correlation.matrix.parties, title = "Party Support and Income Correlation Matrix")
          
# Correlation plot

library(corrplot)
corrplot(correlation.matrix.parties, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

###################################
## STEP 3:  Simple linear models ##
###################################


# This function is used to return the significance level in asterisk form to add to the scatterplots for the beta parameter
return_significance <- function(x){
  if(x<0.0001){return("***")}
  if(x<0.001){return("**")}
  if(x<0.01){return("*")}
  if(x<0.05){return(".")}
  else{return("")}
}


###############
#### TULOT ####
###############

# Scatter plots for simple model with Tulot 

par(mfrow = c(2,4), oma = c(0, 0, 2, 0))
colors <- rainbow(8)

for (i in eightMostVotedIDs){
  print(i)
  fit <- lm(mergedfn[,i+1]~I(mergedfn$Tulot/100)) #we add 1 because the index in the dataframe mergedf are shifted by 1
  m <- max(mergedfn[,i+1])
  xm <- max(I(mergedfn$Tulot/100))
  plot(I(mergedfn$Tulot/100),mergedfn[,i+1], col = colors[i], xlab = "Tulot (100:10000)", ylab = paste(parties[i], "Support", sep = " "),main = parties[i], ylim = c(0,100), lty = i, pch = 20)
  abline(fit)
  rsquared <- formatC(summary(fit)$r.squared, format = "e", digits = 2)
  slope <- formatC(summary(fit)$coefficients[2], format = "e", digits = 2)
  significance <- return_significance(summary(fit)$coefficients[2,4])
  text(x = xm-15, y = 90,label= paste("r2= ", rsquared), adj = 1)
  text(x = xm-15, y = 80, label = paste("slope = ",slope, significance), adj = 1)
}
mtext("Party Support and Average Income", outer = TRUE, cex = 1.5)

# Regression table outputs for Latex

fitKESK <-lm(mergedfn$KESK ~I(mergedfn$Tulot/100))
fitPS <-lm(mergedfn$PS ~I(mergedfn$Tulot/100))
fitSDP <-lm(mergedfn$SDP ~I(mergedfn$Tulot/100))
fitKOK <-lm(mergedfn$KOK ~I(mergedfn$Tulot/100))
fitVAS <-lm(mergedfn$VAS ~I(mergedfn$Tulot/100))
fitVIHR <-lm(mergedfn$VIHR ~I(mergedfn$Tulot/100))
fitRKP <-lm(mergedfn$RKP ~I(mergedfn$Tulot/100))
fitKD <-lm(mergedfn$KD ~I(mergedfn$Tulot/100))

library(stargazer)
stargazer(fitKESK, fitPS, fitSDP, fitKOK, fitVAS, fitVIHR, fitRKP, fitKD, align = TRUE, title = "Univariate regression with Tulot for party supports")

####################
#### ANSIOTULOT ####
####################

# Scatterplots for Ansiotulot

par(mfrow = c(2,4), oma = c(0, 0, 2, 0))

colors <- rainbow(8)
for (i in eightMostVotedIDs){
  print(i)
  fit <- lm(mergedfn[,i+1]~I(mergedfn$Ansiotulot/100)) #we add 1 because the index in the dataframe mergedf are shifted by 1
  m <- max(mergedfn[,i+1])
  plot(I(mergedfn$Ansiotulot/100),mergedfn[,i+1], col = colors[i], xlab = "Ansiotulot (100:10000)", ylab = paste(parties[i], "Support", sep = " "),main = parties[i], ylim = c(0,100), lty = i, pch = 20)
  abline(fit)
  xm <- max(I(mergedfn$Ansiotulot/100))
  rsquared <- formatC(summary(fit)$r.squared, format = "e", digits = 2)
  slope <- formatC(summary(fit)$coefficients[2], format = "e", digits = 2)
  significance <- return_significance(summary(fit)$coefficients[2,4])
  text(x = xm-15, y = 90,label= paste("r2 = ", rsquared), adj = 1)
  text(x = xm-15, y = 80, label = paste("slope = ",slope, significance), adj = 1)
  
}
mtext("Party Support and Earned Income", outer = TRUE, cex = 1.5)

# Regression table earned income for Latex

fitKESK <-lm(mergedfn$KESK ~I(mergedfn$Ansiotulot/100))
fitPS <-lm(mergedfn$PS ~I(mergedfn$Ansiotulot/100))
fitSDP <-lm(mergedfn$SDP ~I(mergedfn$Ansiotulot/100))
fitKOK <-lm(mergedfn$KOK ~I(mergedfn$Ansiotulot/100))
fitVAS <-lm(mergedfn$VAS ~I(mergedfn$Ansiotulot/100))
fitVIHR <-lm(mergedfn$VIHR ~I(mergedfn$Ansiotulot/100))
fitRKP <-lm(mergedfn$RKP ~I(mergedfn$Ansiotulot/100))
fitKD <-lm(mergedfn$KD ~I(mergedfn$Ansiotulot/100))

library(stargazer)
stargazer(fitKESK, fitPS, fitSDP, fitKOK, fitVAS, fitVIHR, fitRKP, fitKD, align = TRUE, title = "Univariate regression with Ansiotulot for party supports")

#####################
#### P‰‰OMATULOT ####
#####################

par(mfrow = c(2,4), oma = c(0, 0, 2, 0))

colors <- rainbow(8)
for (i in eightMostVotedIDs){
  print(i)
  fit <- lm(mergedfn[,i+1]~I(mergedfn$P‰‰omatulot/100)) #we add 1 because the index in the dataframe mergedf are shifted by 1
  m <- max(mergedfn[,i+1])
  plot(I(mergedfn$P‰‰omatulot/100),mergedfn[,i+1], col = colors[i], xlab = "P‰‰omatulot (100:10000)", ylab = paste(parties[i], "Support", sep = " "),main = parties[i], ylim = c(0,100), lty = i, pch = 20)
  abline(fit)
  print(summary(fit))
  xm <- max(I(mergedfn$P‰‰omatulot/100))
  rsquared <- formatC(summary(fit)$r.squared, format = "e", digits = 2)
  slope <- formatC(summary(fit)$coefficients[2], format = "e", digits = 2)
  significance <- return_significance(summary(fit)$coefficients[2,4])
  text(x = xm-5, y = 90,label= paste("r2 = ", rsquared), adj = 1)
  text(x = xm-5, y = 80, label = paste("slope = ",slope, significance), adj = 1)
  
}
mtext("Party Support and Investment Income", outer = TRUE, cex = 1.5)

#Regression table fot investment income

fitKESK <-lm(mergedfn$KESK ~I(mergedfn$P‰‰omatulot/100))
fitPS <-lm(mergedfn$PS ~I(mergedfn$P‰‰omatulot/100))
fitSDP <-lm(mergedfn$SDP ~I(mergedfn$P‰‰omatulot/100))
fitKOK <-lm(mergedfn$KOK ~I(mergedfn$P‰‰omatulot/100))
fitVAS <-lm(mergedfn$VAS ~I(mergedfn$P‰‰omatulot/100))
fitVIHR <-lm(mergedfn$VIHR ~I(mergedfn$P‰‰omatulot/100))
fitRKP <-lm(mergedfn$RKP ~I(mergedfn$P‰‰omatulot/100))
fitKD <-lm(mergedfn$KD ~I(mergedfn$P‰‰omatulot/100))


library(stargazer)
stargazer(fitKESK, fitPS, fitSDP, fitKOK, fitVAS, fitVIHR, fitRKP, fitKD, align = TRUE, title = "Univariate regression with P‰‰omatulot for party supports")

###################
#### VEROTRATE ####
###################

par(mfrow = c(2,4), oma = c(0, 0, 2, 0))

colors <- rainbow(8)
for (i in eightMostVotedIDs){
  print(i)
  fit <- lm(mergedfn[,i+1]~mergedfn$VerotRate) 
  m <- max(mergedfn[,i+1])
  plot(mergedfn$VerotRate,mergedfn[,i+1], col = colors[i], xlab = "VerotRate",ylab = paste(parties[i], "Support", sep = " "),main = parties[i], ylim = c(0,100), lty = i, pch = 20)
  abline(fit)
  xm <- max(mergedfn$VerotRate)
  rsquared <- formatC(summary(fit)$r.squared, format = "e", digits = 2)
  slope <- formatC(summary(fit)$coefficients[2], format = "e", digits = 2)
  significance <- return_significance(summary(fit)$coefficients[2,4])
  text(x = xm-0.2, y = 90,label= paste("r2 = ", rsquared), adj = 1)
  text(x = xm-0.2, y = 80, label = paste("slope = ",slope, significance), adj = 1)
  
}
mtext("Party Support and VerotRate", outer = TRUE, cex = 1.5)

fitKESK <-lm(mergedfn$KESK ~mergedfn$VerotRate)
fitPS <-lm(mergedfn$PS ~mergedfn$VerotRate)
fitSDP <-lm(mergedfn$SDP ~mergedfn$VerotRate)
fitKOK <-lm(mergedfn$KOK ~mergedfn$VerotRate)
fitVAS <-lm(mergedfn$VAS ~mergedfn$VerotRate)
fitVIHR <-lm(mergedfn$VIHR ~mergedfn$VerotRate)
fitRKP <-lm(mergedfn$RKP ~mergedfn$VerotRate)
fitKD <-lm(mergedfn$KD ~mergedfn$VerotRate)

library(stargazer)
stargazer(fitKESK, fitPS, fitSDP, fitKOK, fitVAS, fitVIHR, fitRKP, fitKD, align = TRUE, title = "Univariate regression with VerotRate for party supports")

##########################
#### VALTIONVEROTRATE ####
##########################



par(mfrow = c(2,4), oma = c(0, 0, 2, 0))

colors <- rainbow(8)
for (i in eightMostVotedIDs){
  print(i)
  fit <- lm(mergedfn[,i+1]~mergedfn$ValtionveroRate)
  m <- max(mergedfn[,i+1])
  plot(mergedfn$ValtionveroRate,mergedfn[,i+1], col = colors[i], xlab = "ValtionveroRate",ylab = paste(parties[i], "Support", sep = " "),main = parties[i], ylim = c(0,100), lty = i, pch = 20)
  abline(fit)
  xm <- max(mergedfn$ValtionveroRate)
  rsquared <- formatC(summary(fit)$r.squared, format = "e", digits = 2)
  slope <- formatC(summary(fit)$coefficients[2], format = "e", digits = 2)
  significance <- return_significance(summary(fit)$coefficients[2,4])
  text(x = xm, y = 90,label= paste("r2 = ", rsquared), adj = 1)
  text(x = xm, y = 80, label = paste("slope = ",slope, significance), adj = 1)
  
}
mtext("Party Support and ValtionveroRate", outer = TRUE, cex = 1.5)

fitKESK <-lm(mergedfn$KESK ~mergedfn$ValtionveroRate)
fitPS <-lm(mergedfn$PS ~mergedfn$ValtionveroRate)
fitSDP <-lm(mergedfn$SDP ~mergedfn$ValtionveroRate)
fitKOK <-lm(mergedfn$KOK ~mergedfn$ValtionveroRate)
fitVAS <-lm(mergedfn$VAS ~mergedfn$ValtionveroRate)
fitVIHR <-lm(mergedfn$VIHR ~mergedfn$ValtionveroRate)
fitRKP <-lm(mergedfn$RKP ~mergedfn$ValtionveroRate)
fitKD <-lm(mergedfn$KD ~mergedfn$ValtionveroRate)


library(stargazer)
stargazer(fitKESK, fitPS, fitSDP, fitKOK, fitVAS, fitVIHR, fitRKP, fitKD, align = TRUE, title = "Univariate regression with ValtionverotRate for party supports")


############################
#### KUNNALLISVEROTRATE ####
############################

par(mfrow = c(2,4), oma = c(0, 0, 2, 0))

colors <- rainbow(8)
for (i in eightMostVotedIDs){
  print(i)
  fit <- lm(mergedfn[,i+1]~mergedfn$KunnallisveroRate)
  m <- max(mergedfn[,i+1])
  plot(mergedfn$KunnallisveroRate,mergedfn[,i+1], col = colors[i], xlab = "KunnallisveroRate",ylab = paste(parties[i], "Support", sep = " "),main = parties[i], ylim = c(0,100), lty = i, pch = 20)
  abline(fit)
  xm <- max(mergedfn$KunnallisveroRate)
  rsquared <- formatC(summary(fit)$r.squared, format = "e", digits = 2)
  slope <- formatC(summary(fit)$coefficients[2], format = "e", digits = 2)
  significance <- return_significance(summary(fit)$coefficients[2,4])
  text(x = xm, y = 90,label= paste("r2 = ", rsquared), adj = 1)
  text(x = xm, y = 80, label = paste("slope = ",slope, significance), adj = 1)
  
}
mtext("Party Support and KunnallisveroRate", outer = TRUE, cex = 1.5)

fitKESK <-lm(mergedfn$KESK ~mergedfn$KunnallisveroRate)
fitPS <-lm(mergedfn$PS ~mergedfn$KunnallisveroRate)
fitSDP <-lm(mergedfn$SDP ~mergedfn$KunnallisveroRate)
fitKOK <-lm(mergedfn$KOK ~mergedfn$KunnallisveroRate)
fitVAS <-lm(mergedfn$VAS ~mergedfn$KunnallisveroRate)
fitVIHR <-lm(mergedfn$VIHR ~mergedfn$KunnallisveroRate)
fitRKP <-lm(mergedfn$RKP ~mergedfn$KunnallisveroRate)
fitKD <-lm(mergedfn$KD ~mergedfn$KunnallisveroRate)

library(stargazer)
stargazer(fitKESK, fitPS, fitSDP, fitKOK, fitVAS, fitVIHR, fitRKP, fitKD, align = TRUE, title = "Univariate regression with KunnallisveroRate for party supports")


#####################################
## STEP 4:  Multiple linear models ##
#####################################


# Tulot-Verot: Significant for KESK, SDP

for (i in eightMostVotedIDs){
  fit <- lm(mergedfn[,i + 1] ~ I(mergedfn$Tulot/100) + mergedfn$VerotRate)
  print(parties[i])
  print(summary(fit))
}

fitKESK <- lm(mergedfn$KESK ~ I(mergedfn$Tulot/100) + mergedfn$VerotRate)
fitPS <- lm(mergedfn$PS ~ I(mergedfn$Tulot/100) + mergedfn$VerotRate)
fitVAS<- lm(mergedfn$VAS ~ I(mergedfn$Tulot/100) + mergedfn$VerotRate)
fitVIHR <- lm(mergedfn$VIHR ~ I(mergedfn$Tulot/100) + mergedfn$VerotRate)
fitSDP <- lm(mergedfn$SDP ~ I(mergedfn$Tulot/100) + mergedfn$VerotRate)
fitKOK <- lm(mergedfn$KOK ~ I(mergedfn$Tulot/100) + mergedfn$VerotRate)
fitRKP <- lm(mergedfn$RKP ~ I(mergedfn$Tulot/100) + mergedfn$VerotRate)
fitKD <- lm(mergedfn$KD ~ I(mergedfn$Tulot/100) + mergedfn$VerotRate)
library(stargazer)
stargazer(fitKESK,fitPS, fitSDP, fitKOK, fitVAS, fitVIHR, fitRKP, fitKD, title = "Multivariate regression for party supports with Tulot and VerotRate", align = TRUE)

# KunnallisveroRate and Paaomatulot

fitKESK <- lm(mergedfn$KESK ~ I(mergedfn$P‰‰omatulot/100) + mergedfn$KunnallisveroRate)
fitPS <- lm(mergedfn$PS ~ I(mergedfn$P‰‰omatulot/100) + mergedfn$KunnallisveroRate)
fitVAS<- lm(mergedfn$VAS ~ I(mergedfn$P‰‰omatulot/100) + mergedfn$KunnallisveroRate)
fitVIHR <- lm(mergedfn$VIHR ~ I(mergedfn$P‰‰omatulot/100) + mergedfn$KunnallisveroRate)
fitSDP <- lm(mergedfn$SDP ~ I(mergedfn$P‰‰omatulot/100) + mergedfn$KunnallisveroRate)
fitKOK <- lm(mergedfn$KOK ~ I(mergedfn$P‰‰omatulot/100) + mergedfn$KunnallisveroRate)
fitRKP <- lm(mergedfn$RKP ~ I(mergedfn$P‰‰omatulot/100) + mergedfn$KunnallisveroRate)
fitKD <- lm(mergedfn$KD ~ I(mergedfn$P‰‰omatulot/100) + mergedfn$KunnallisveroRate)
library(stargazer)
stargazer(fitKESK,fitPS, fitSDP, fitKOK, fitVAS, fitVIHR, fitRKP, fitKD, title = "Multivariate regression for party supports with P‰‰omatulot and KunnallisveroRate", align = TRUE)



# Ansiotulot and Paaomatulot: significant for KESK, VIHR, SDP, KOK,RKP
colors <- rainbow(8)
for (i in eightMostVotedIDs){
  fit <- lm(mergedfn[,i + 1] ~ I(mergedfn$Ansiotulot/100) + I(mergedfn$P‰‰omatulot/100))
  print(parties[i])
  print(summary(fit))
}


fitKESK <- lm(mergedfn$KESK ~ I(mergedfn$Ansiotulot/100) + I(mergedfn$P‰‰omatulot/100))
fitPS <- lm(mergedfn$PS ~ I(mergedfn$Ansiotulot/100) + I(mergedfn$P‰‰omatulot/100))
fitVAS<- lm(mergedfn$VAS ~ I(mergedfn$Ansiotulot/100) + I(mergedfn$P‰‰omatulot/100))
fitVIHR <- lm(mergedfn$VIHR ~ I(mergedfn$Ansiotulot/100) + I(mergedfn$P‰‰omatulot/100))
fitSDP <- lm(mergedfn$SDP ~ I(mergedfn$Ansiotulot/100) + I(mergedfn$P‰‰omatulot/100))
fitKOK <- lm(mergedfn$KOK ~ I(mergedfn$Ansiotulot/100) + I(mergedfn$P‰‰omatulot/100))
fitRKP <- lm(mergedfn$RKP ~ I(mergedfn$Ansiotulot/100) + I(mergedfn$P‰‰omatulot/100))
fitKD <- lm(mergedfn$KD ~ I(mergedfn$Ansiotulot/100) + I(mergedfn$P‰‰omatulot/100))
library(stargazer)
stargazer(fitKESK,fitPS, fitSDP, fitKOK, fitVAS, fitVIHR, fitRKP, fitKD, title = "Multivariate regression for party supports", align = TRUE)


###### Multiple linear regression for KOK that apprears in Apprendix 7.1 ######

fit <- lm(mergedfn$KOK ~ I(mergedfn$Ansiotulot/100) + I(mergedfn$P‰‰omatulot/100) + mergedfn$ValtionveroRate + mergedfn$KunnallisveroRate)


#################################
## STEP 5:  Interaction models ##
#################################

# Diving Paaomatulot into Brackets
invincstat <- summary(mergedfn$P‰‰omatulot)
mergedfn$P‰‰omatulotBraket[mergedfn$P‰‰omatulot <= 1614] <- "Low"
mergedfn$P‰‰omatulotBraket[mergedfn$P‰‰omatulot > 1614 & mergedfn$P‰‰omatulot <= 1877 ] <- "Medium-Low"
mergedfn$P‰‰omatulotBraket[mergedfn$P‰‰omatulot > 1877 & mergedfn$P‰‰omatulot <= 2268] <- "Medium-High"
mergedfn$P‰‰omatulotBraket[mergedfn$P‰‰omatulot > 2268] <- "High"

# Dividing Ansiotulot into Brackets
eincstat <- summary(mergedfn$Ansiotulot)
mergedfn$AnsiotulotBraket[mergedfn$Ansiotulot <= 21870] <- "Low"
mergedfn$AnsiotulotBraket[mergedfn$Ansiotulot > 21870 & mergedfn$Ansiotulot <= 24096 ] <- "Medium-Low"
mergedfn$AnsiotulotBraket[mergedfn$Ansiotulot > 24096 & mergedfn$Ansiotulot <= 26149] <- "Medium-High"
mergedfn$AnsiotulotBraket[mergedfn$Ansiotulot > 26149] <- "High"

# Diving Tulot into Brackets

incstat <- summary(mergedfn$Tulot)
mergedfn$TulotBraket[mergedfn$Tulot <= 23987] <- "Low"
mergedfn$TulotBraket[mergedfn$Tulot > 23987 & mergedfn$Tulot <= 26133 ] <- "Medium-Low"
mergedfn$TulotBraket[mergedfn$Tulot > 26133 & mergedfn$Tulot <= 27968] <- "Medium-High"
mergedfn$TulotBraket[mergedfn$Tulot > 27968] <- "High"


# Dividing VerotRate into Brackets
verostat <- summary(mergedfn$VerotRate)
mergedfn$VerotBraket[mergedfn$VerotRate <= 18.46] <- "Low"
mergedfn$VerotBraket[mergedfn$VerotRate > 18.46 & mergedfn$VerotRate <= 19.46 ] <- "Medium-Low"
mergedfn$VerotBraket[mergedfn$VerotRate > 19.46 & mergedfn$VerotRate <= 20.72] <- "Medium-High"
mergedfn$VerotBraket[mergedfn$VerotRate > 20.72] <- "High"


#################
# KESK AND VIHR #
#################

# Plot for KESK interaction 
plKESK <- ggplot(mergedfn, 
                 aes(x = P‰‰omatulot/100, y = KESK, color = AnsiotulotBraket)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "P‰‰omatulot (100:10000)", y = "KESK Support", 
       color = "Ansiotulot Bracket", title = "Interaction model for P‰‰omatulot and Ansiotulot Braket for KESK")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# Plot for VIHR ineraction 
plVIHR <- ggplot(mergedfn, 
                 aes(x = P‰‰omatulot/100, y = VIHR, color = AnsiotulotBraket)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "P√§√§omatulot (100:10000)", y = "VIHR Support", 
       color = "Ansiotulot Bracket", title = "Interaction model for P‰‰omatulot and Ansiotulot Braket for VIHR")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# Tables for Latex

KESKinteraction <- lm(KESK ~ I(P‰‰omatulot/100)*AnsiotulotBraket, data = mergedfn)
VIHRinteraction <- lm(VIHR ~ I(P‰‰omatulot/100)*AnsiotulotBraket, data = mergedfn)
stargazer(KESKinteraction, VIHRinteraction, title = "Interaction among P‰‰omatulot and Ansiotulot Braket for KESK and VIHR", single.row=TRUE)


# Plot for KESK and VIHR used in report
library(ggpubr)
ggarrange(plKESK, plVIHR + rremove("x.text"), 
          #labels = c("KESK", "VIHR"),
          ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

#################
# KESK AND VIHR #
#################

# Interaction plot for SDP

ggplot(mergedfn, 
       aes(x = Tulot/100, y = SDP, color = VerotBraket)) +
  geom_point(alpha = 0.25) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Tulot (100:10000)", y = "SDP Support", 
       color = "VerotBraket", title = "Interaction model for VerotBraket and Tulot for SDP")+
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5))

# SDP interaction table used in Latex 

SDPint <- lm(SDP ~ I(Tulot/100)*VerotBraket, data = mergedfn)
stargazer(SDPint, title = "Interaction among Tulot and VerotBraket for SDP", single.row=TRUE)


##################################
## STEP 6:  Vaalipiiri Analysis ##
##################################


# RKP plot
library(ggplot2)
RKPplot<- ggplot(mergedf, aes(Tulot, RKP)) + geom_point(aes(color = Vaalipiiri)) + theme_minimal() + theme(legend.position = "top")

# Plot for each 

library(ggpubr)
KESKplot <- ggplot(mergedfn, aes(Tulot, KESK)) + geom_point(aes(color = Vaalipiiri)) + theme_minimal() #+ theme(legend.position = "top")
PSplot <- ggplot(mergedfn, aes(Tulot, PS)) + geom_point(aes(color = Vaalipiiri)) + theme_minimal() #+ theme(legend.position = "top")
SDPplot<- ggplot(mergedfn, aes(Tulot, SDP)) + geom_point(aes(color = Vaalipiiri)) + theme_minimal() #+ theme(legend.position = "top")
KOKplot <- ggplot(mergedfn, aes(Tulot, KOK)) + geom_point(aes(color = Vaalipiiri)) + theme_minimal() #+ theme(legend.position = "top")
VASplot <- ggplot(mergedfn, aes(Tulot, VAS)) + geom_point(aes(color = Vaalipiiri)) + theme_minimal() #+ theme(legend.position = "top")
VIHRplot <- ggplot(mergedfn, aes(Tulot, VIHR)) + geom_point(aes(color = Vaalipiiri)) + theme_minimal() #+ theme(legend.position = "top")
RKPplot<- ggplot(mergedfn, aes(Tulot, RKP)) + geom_point(aes(color = Vaalipiiri)) + theme_minimal() #+ theme(legend.position = "top")
KDplot <- ggplot(mergedfn, aes(Tulot, KD)) + geom_point(aes(color = Vaalipiiri)) + theme_minimal() #+ theme(legend.position = "top")

ggarrange(KESKplot,PSplot, SDPplot, KOKplot, VASplot, VIHRplot, RKPplot, KDplot + rremove("x.text"), 
          #labels = c("KESK", "PS", "SDP", "KOK", "VAS", "VIHR", "RKP", "KD"),
          ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")




###############
#### OTHER ####
###############


# Multiple linear regression with Ansiotulot, Paaomatulot and VerotRate

for (i in eightMostVotedIDs){
  fit <- lm(mergedfn[,i + 1] ~ I(mergedfn$Ansiotulot/100) + I(mergedfn$P‰‰omatulot/100) + mergedfn$VerotRate)
  print(parties[i])
  print(summary(fit))
}



# Multiple  linear regressions Ansiotulot, Paaomatulo, State Taxes, Municipal taxes

for (i in eightMostVotedIDs){
  fit <- lm(mergedfn[,i + 1] ~ I(mergedfn$Ansiotulot/100) + I(mergedfn$P‰‰omatulot/100) + mergedfn$ValtionveroRate + mergedfn$KunnallisveroRate)
  print(parties[i])
  print(summary(fit))
}

# Scatterplots for Tulot and Party support with included the outier observation for Kauniainen

par(mfrow = c(2,4), oma = c(0, 0, 2, 0))
colors <- rainbow(8)
for (i in eightMostVotedIDs){
  print(i)
  fit <- lm(mergedf[,i+1]~I(mergedf$Tulot/100))
  m <- max(mergedf[,i+1])
  xm <- max(I(mergedf$Tulot/100))
  plot(I(mergedf$Tulot/100),mergedf[,i+1], col = colors[i], xlab = "Tulot (100:10000)", ylab = paste(parties[i], "Support", sep = " "),main = parties[i], ylim = c(0,100), lty = i, pch = 20)
  abline(fit)
  #print(summary(fit))
  rsquared <- formatC(summary(fit)$r.squared, format = "e", digits = 2)
  slope <- formatC(summary(fit)$coefficients[2], format = "e", digits = 2)
  significance <- return_significance(summary(fit)$coefficients[2,4])
  text(x = xm-15, y = 90,label= paste("r2= ", rsquared), adj = 1)
  text(x = xm-15, y = 80, label = paste("slope = ",slope, significance), adj = 1)
}
mtext("Party Support and Average Income", outer = TRUE, cex = 1.5)




# Scatter plots for Mediaanitulot and Party support with outlier Kauniainen

par(mfrow = c(2,4), oma = c(0, 0, 2, 0))
colors <- rainbow(8)

for (i in eightMostVotedIDs){
  print(i)
  fit <- lm(mergedf[,i+1]~mergedf$Mediaanitulot) 
  m <- max(mergedf[,i+1])
  xm <- max(mergedf$Mediaanitulot)
  plot(mergedf$Mediaanitulot,mergedf[,i+1], col = colors[i], xlab = "Mediaanitulot", ylab = paste(parties[i], "Support", sep = " "),main = parties[i], ylim = c(0,100), lty = i, pch = 20)
  abline(fit)
  #print(summary(fit))
  rsquared <- formatC(summary(fit)$r.squared, format = "e", digits = 2)
  slope <- formatC(summary(fit)$coefficients[2], format = "e", digits = 2)
  significance <- return_significance(summary(fit)$coefficients[2,4])
  text(x = xm-150, y = 90,label= paste("r2 = ", rsquared), adj = 1)
  text(x = xm-150, y = 80, label = paste("slope = ",slope, significance), adj = 1)
}
mtext("Scatter plots of support and median income", outer = TRUE, cex = 1.5)


# Interaction models with different brackets

vverostat <- summary(mergedfn$Valtionvero)

mergedfn$ValtionveroBraket[mergedfn$Valtionvero <= 930] <- "Low"
mergedfn$ValtionveroBraket[mergedfn$Valtionvero > 930 & mergedfn$Valtionvero <= 1124 ] <- "Medium-Low"
mergedfn$ValtionveroBraket[mergedfn$Valtionvero > 1124 & mergedfn$Valtionvero <= 1351] <- "Medium-High"
mergedfn$ValtionveroBraket[mergedfn$Valtionvero > 1351] <- "High"


library(ggpubr)
ggscatter(
  mergedfn, x = "Ansiotulot", y = "KESK", 
  color = "P‰‰omatulotBraket"
) +
  stat_smooth(
    aes(color = P‰‰omatulotBraket), method = "lm",
    se = FALSE, fullrange = TRUE
  ) +
  stat_cor(
    aes(color = P‰‰omatulotBraket), 
    label.x.npc = 0.5, 
    label.y.npc = 0.3, hjust = 0
  )

library(ggpubr)
ggscatter(
  mergedfn, x = "Ansiotulot", y = "KESK", 
  color = "VerotBraket"
) +
  stat_smooth(
    aes(color = VerotBraket), method = "lm",
    se = FALSE, fullrange = TRUE
  ) +
  stat_cor(
    aes(color = VerotBraket), 
    label.x.npc = 0.5, 
    label.y.npc = 0.3, hjust = 0
  )

library(ggpubr)
ggscatter(
  mergedfn, x = "Tulot", y = "KESK", 
  color = "VerotBraket"
) +
  stat_smooth(
    aes(color = VerotBraket), method = "lm",
    se = FALSE, fullrange = TRUE
  ) +
  stat_cor(
    aes(color = VerotBraket), 
    label.x.npc = 0.5, 
    label.y.npc = 0.3, hjust = 0
  )

library(ggpubr)
ggscatter(
  mergedfn, x = "Tulot", y = "KESK", 
  color = "P‰‰omatulotBraket"
) +
  stat_smooth(
    aes(color = P‰‰omatulotBraket), method = "lm",
    se = FALSE, fullrange = TRUE
  ) +
  stat_cor(
    aes(color = P‰‰omatulotBraket), 
    label.x.npc = 0.5, 
    label.y.npc = 0.3, hjust = 0
  )


ggscatter(
  mergedfn, x = "Tulot", y = "KESK", 
  color = "P‰‰omatulotBraket"
) +
  stat_smooth(
    aes(color = P‰‰omatulotBraket), method = "lm",
    se = FALSE, fullrange = TRUE
  ) +
  stat_cor(
    aes(color = P‰‰omatulotBraket), 
    label.x.npc = 0.5, 
    label.y.npc = 0.3, hjust = 0
  )


ggscatter(
  mergedfn, x = "P‰‰omatulot", y = "KESK", 
  color = "AnsiotulotBraket"
) +
  stat_smooth(
    aes(color = AnsiotulotBraket), method = "lm",
    se = FALSE, fullrange = TRUE
  ) +
  stat_cor(
    aes(color = AnsiotulotBraket), 
    label.x.npc = 0.5, 
    label.y.npc = 0.3, hjust = 0
  )



ggscatter(
  mergedfn, x = "Ansiotulot", y = "KESK", 
  color = "AnsiotulotBraket"
) +
  stat_smooth(
    aes(color = AnsiotulotBraket), method = "lm",
    se = FALSE, fullrange = TRUE
  ) +
  stat_cor(
    aes(color = AnsiotulotBraket), 
    label.x.npc = 0.5, 
    label.y.npc = 0.3, hjust = 0
  )

ggscatter(
  mergedfn, x = "Tulot", y = "KESK", 
  color = "TulotBraket"
) +
  stat_smooth(
    aes(color = TulotBraket), method = "lm",
    se = FALSE, fullrange = TRUE
  ) +
  stat_cor(
    aes(color = TulotBraket), 
    label.x.npc = 0.5, 
    label.y.npc = 0.3, hjust = 0
  )

ggscatter(
  mergedfn, x = "Tulot", y = "RKP", 
  color = "TulotBraket"
) +
  stat_smooth(
    aes(color = TulotBraket), method = "lm",
    se = FALSE, fullrange = TRUE
  ) +
  stat_cor(
    aes(color = TulotBraket), 
    label.x.npc = 0.5, 
    label.y.npc = 0.3, hjust = 0
  )

ggscatter(
  mergedfn, x = "Tulot", y = "RKP", 
  color = "Vaalipiiri"
) +
  stat_smooth(
    aes(color = Vaalipiiri), method = "lm",
    se = FALSE, fullrange = TRUE
  ) +
  stat_cor(
    aes(color = Vaalipiiri), 
    label.x.npc = 0.5, 
    label.y.npc = 0.3, hjust = 0
  )

ggscatter(
  mergedfn, x = "Tulot", y = "KESK", 
  color = "Vaalipiiri"
) +
  stat_smooth(
    aes(color = Vaalipiiri), method = "lm",
    se = FALSE, fullrange = TRUE
  ) +
  stat_cor(
    aes(color = Vaalipiiri), 
    label.x.npc = 0.5, 
    label.y.npc = 0.3, hjust = 0
  )


ggscatter(
  mergedfn, x = "P‰‰omatulot", y = "KESK", 
  color = "ValtionveroBraket"
) +
  stat_smooth(
    aes(color = ValtionveroBraket), method = "lm",
    se = FALSE, fullrange = TRUE
  ) +
  stat_cor(
    aes(color = ValtionveroBraket), 
    label.x.npc = 0.5, 
    label.y.npc = 0.3, hjust = 0
  )

ggscatter(
  mergedfn, x = "P‰‰omatulot", y = "KESK", 
  color = "TulotBraket"
) +
  stat_smooth(
    aes(color = TulotBraket), method = "lm",
    se = FALSE, fullrange = TRUE
  ) +
  stat_cor(
    aes(color = TulotBraket), 
    label.x.npc = 0.5, 
    label.y.npc = 0.3, hjust = 0
  )

ggscatter(
  mergedfn, x = "P‰‰omatulot", y = "KESK", 
  color = "AnsiotulotBraket"
) +
  stat_smooth(
    aes(color = AnsiotulotBraket), method = "lm",
    se = FALSE, fullrange = TRUE
  ) +
  stat_cor(
    aes(color = AnsiotulotBraket), 
    label.x.npc = 0.5, 
    label.y.npc = 0.3, hjust = 0
  )
