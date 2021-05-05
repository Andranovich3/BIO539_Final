#Clear working environment
rm(list=ls())

#Load this packages
library(akima)
library(fields)
library(gvlma)
library(AICcmodavg)

#Read in the Polar Bear data

data1 = read.csv("PBearMeasurementData_EDITED.csv")
dim(data1)
head(data1)

#Changing M/F readings to 1 and 0 for easier analysis
#Female = 0
#Male = 1

data1$Sex[data1$Sex == "F"] = 0
data1$Sex[data1$Sex == "M"] = 1

#Changing Populations to 1 and 0 for easier analysis
#CS = 0
#SB = 1

data1$Population[data1$Population == "CS"] = 0
data1$Population[data1$Population == "SB"] = 1

###############################
#Make two plots on one figure:
#Plot the response variable (Mass) by the independent variable (Age)
#Plot the response variable (Mass) by the independent variable (Total_Length)

par(mfrow=c(2,1))
plot(Mass~Age, data = data1, pch=20, cex=1, xlab = "Age",
     ylab="Mass")

plot(Mass~Total_Length, data = data1, pch=20, cex=1, xlab = "Total Length",
     ylab="Mass")

#############################################
### Model Creation for biological factors ###
#############################################

#Model 0 - null model of no effect on Polar Bear Mass

model0 = lm(Mass~1, data = data1)
summary(model0)

#Model 1 - Age has an effect on Polar Bear Mass
#IV = Age ; DV = Mass

model1 = lm(Mass~Age, data = data1)
summary(model1)

#Model 2 - Total Length has an effect on Polar Bear Mass
#IV = Total_Length ; DV = Mass

model2 = lm(Mass~Total_Length, data = data1)
summary(model2)

#Model 3 - Heart Girth has an effect on Polar Bear Mass
#IV = Heart_Girth ; DV = Mass

model3 = lm(Mass~Heart_Girth, data = data1)
summary(model3)

#Model 4 - Skull_Width has an effect on Polar Bear Mass
#IV = Skull_Width ; DV = Mass

model4 = lm(Mass~Skull_Width, data = data1)
summary(model4)

################################################
### Model Creation for location/time factors ###
################################################

#Model A - Location of population has an effect on Polar Bear Mass
modelA = lm(Mass~Population, data = data1)
summary(modelA)

#Model B - Years since the study started has an effect on Polar Bear Mass
modelB = lm(Mass~Years_Since, data = data1)
summary(modelB)

plot(data1$Mass~data1$Years_Since)

#Model C - Sex has an effect on Polar Bear Mass
modelC = lm(Mass~Sex, data = data1)
summary(modelC)

###########################################################
### Model Creation for Additive/Interactions of factors ###
###########################################################

#Model 3C - Heart Girth and Sex have an additive effect on Polar Bear Mass
#IV = Skull_Width + Sex; DV = Mass

model3C = lm(Mass~Heart_Girth+Sex, data = data1)
summary(model3C)

#Model AC - Population and Sex have an additive effect on Polar Bear Mass
#IV = Population + Sex; DV = Mass

modelAC = lm(Mass~Population+Sex, data = data1)
summary(modelAC)

#Model 3AC - Heart Girth, Population, and Sex have an additive effect on Polar Bear Mass
#IV = Heart Girth + Population + Sex; DV = Mass

model3AC = lm(Mass~Heart_Girth+Population+Sex, data = data1)
summary(model3AC)

#Model 13AC - Age, Heart Girth, Population, and Sex have an additive effect on Polar Bear Mass
#IV = Age + Heart Girth + Population + Sex; DV = Mass

model13AC = lm(Mass~Age+Heart_Girth+Population+Sex, data = data1)
summary(model13AC)

#####################################
### AIC Table and Model Selection ###
#####################################

#Package all the models fitted into a list
modelList = list(model0, model1, model2, model3, model4, modelA,
                 modelB, modelC, model3C, modelAC, model3AC, model13AC)

#Add the names of each model, in the order they are listed in modelList
Modnames = c("Null Model", "Age Model", "Total_Length Model", "Heart_Girth Model",
             "Skull_Width Model", "Population Model", "Study Duration Model",
             "Sex Model", "Heart + Sex Model", "Population+Sex Model",
             "Heart+Pop+Sex Model", "Age+Heart+Pop+Sex Model")

#Construct the AIC table and output it as a csv file, which you can use in a report
aicTable1 = aictab(modelList, Modnames, digits=4)
aicTable1

#Complete a model-averaged prediction based on the AIC Table
#Shows that Heart Girth is the driving factor in all models
newData = data1[,c(1,3:8)]
Model_Averaged_Predictions = modavgPred(modelList, newdata = newData, Modnames)
plot(data1$Mass, predict(model3,type="response"), pch = 20, col = "blue", cex = 0.5,
     xlab = "Observed Mass", ylab = "Predicted Mass",
     main = "Model Prediction Comparisons\n for Polar Bear Mass")
points(data1$Mass, Model_Averaged_Predictions$mod.avg.pred, pch = 20, col = "green", cex = 0.5)
legend("bottomright", legend= c("Model3 Prediction","Model Averaged Prediction"),
       pch=c(20,20), col=c("blue", "green"), cex = 0.75)

#####################################################
### Visually comparing populations using boxplots ###
#####################################################

years = c("1983", "1984", "1985", "1986", "1987", "1988", "1989", "1990",
          "1991", "1992", "1993", "1994", "1996", "1997", "1998",
          "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006",
          "2007", "2008", "2009", "2010", "2011")

MassAVG_byFactors = aggregate(Mass ~ YEAR + Sex + Population, FUN = mean, data = data1)

MassAVG_byFactors$Epoch = MassAVG_byFactors$YEAR
MassAVG_byFactors$Epoch[MassAVG_byFactors$YEAR < "1992"] <- 1
MassAVG_byFactors$Epoch[MassAVG_byFactors$YEAR > "1991" & MassAVG_byFactors$YEAR < "2002"] <- 2
MassAVG_byFactors$Epoch[MassAVG_byFactors$YEAR > "2001"] <- 3

#Average Mass by Year
bp = barplot(MassAVG_byYear$x, names.arg = years,
        xlab = "Year", ylab = "Average Body Mass of Captured Polar Bear",
        main = "Average Body Mass of\n Captured Polar Bears by Year")
text(bp, MassAVG_byYear$x+5, labels=as.character(round(MassAVG_byYear$x, digits=2)),
     cex = 0.5)

#Average Mass by Epoch (1 = early, 2 = middle, 3 = recent)
boxplot(MassAVG_byYear$x ~ MassAVG_byYear$Epoch, xaxt = "n",
        xlab = "Epoch", ylab = "Body Mass (kg)",
        main = "Comparison of Average Body Mass\n by Standardized Epoch")
axis(1, at = c(1:3), labels = c("1983-1991", "1992-2001", "2002-2011"))

#Average Mass by Sex and Epoch
boxplot(Mass ~ Sex + Epoch, xaxt = "n",
        xlab = "Sex + Epoch", ylab = "Body Mass (kg)",
        main = "Comparison of Average Body Mass\n by Sex and Epoch",
        data = MassAVG_byFactors)
axis(1, at = c(1:6), labels = c("Female\n1983-1991", "Male\n1983-1991",
                                "Female\n1992-2001", "Male\n1992-2001",
                                "Female\n2002-2011", "Male\n2002-2011"))
