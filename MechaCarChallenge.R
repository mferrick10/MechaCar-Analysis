library(tidyverse)

mechaCar_data <- read.csv('MechaCar_mpg.csv', stringsAsFactors = F) # Loading Car data from csv

mechaCar_matrix <- as.matrix(mechaCar_data[,c("vehicle.length","ground.clearance","mpg","AWD","vehicle.weight","spoiler.angle")]) # Create a matrix from dataframe

cor(mechaCar_matrix) # correlation matrix


mult_reg_mechaCar <- lm(mpg~vehicle.length + vehicle.weight + spoiler.angle + ground.clearance + AWD, data=mechaCar_data) # creates a multiple linear Regression

summary(mult_reg_mechaCar)# Summary of the Multiple Linear Regression

insignificatVariables <- lm(mpg~vehicle.weight + AWD + spoiler.angle,data = mechaCar_data)# Variables that had high p- values in the above statistical test
summary(insignificatVariables)


significantVariables <- lm(mpg~vehicle.length + ground.clearance, data=mechaCar_data) # variables that had low p-values in the above statistical test
summary(significantVariables)


ggplot(mechaCar_data,aes(x=spoiler.angle,y=mpg))+geom_point()+geom_smooth(method="lm") # linear model scatter plots of insignificant variables
ggplot(mechaCar_data,aes(x=AWD,y=mpg))+geom_point()+geom_smooth(method="lm")
ggplot(mechaCar_data,aes(x=vehicle.weight,y=mpg))+geom_point()+geom_smooth(method="lm")

ggplot(mechaCar_data,aes(x=vehicle.length,y=mpg))+geom_point()+geom_smooth(method="lm") # linear model scatter plots of significant varaibles
ggplot(mechaCar_data,aes(x=ground.clearance,y=mpg))+geom_point()+geom_smooth(method="lm")


ggplot(mechaCar_data,aes(x=vehicle.length,y=mpg,size=ground.clearance,color=AWD))+geom_point()+geom_smooth(se=FALSE, method="lm")# linear model scatter plot of significant variables with additional significant variables


suspensionCoil_Data <- read.csv("Suspension_Coil.csv", stringsAsFactors = FALSE) # Loading in suspension coil data to dataframe

psi <- suspensionCoil_Data$PSI # selecting the PSI data column

summary(psi) # Summary statistics of PSI

var(psi) # Varaince of PSI

sd(psi) # Standard Deviation of PSI

sample_psi <- suspensionCoil_Data %>% sample_n(20) # Sample from suspension coil population

ggplot(suspensionCoil_Data,aes(x=log10(PSI)))+geom_density() # distriubution of suspoension coil population

ggplot(sample_psi,aes(log10(PSI)))+geom_density() #distribution of selected sample from suspensioncoil population

t.test(log10(sample_psi$PSI),mu=mean(log10(suspensionCoil_Data$PSI))) # T test comparing means of sample and population data of suspension coils
