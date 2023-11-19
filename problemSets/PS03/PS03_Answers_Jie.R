#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)
library(ggplot2)
# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/incumbents_subset.csv")


#Question1
#(1)
#Create a regression about voteshare and difflog
reg_model<- lm(voteshare~difflog,data=inc.sub)
summary(reg_model)

#(2)
# Create scatterplot with regression line and customized colors
scatter_1 <- ggplot(data = inc.sub, mapping = aes(x = difflog, y = voteshare)) +
  geom_point(color = "#B4BDFF") +
  geom_smooth(method = "lm", se = FALSE, color = "#E5D4FF") +
  labs(title = "Scatterplot with Regression Line",
       x = "difflog",
       y = "voteShare") +
  scale_color_manual(values = c("#B4BDFF", "#E5D4FF"))
print(scatter_1)


#(3)
#save the residuals in a separate object
res_1 <- residuals(reg_model)
res_sep <- str(res_1)
summary(res_sep)


#(4)
#Write the prediction equation
coefficients_1 <- coef(reg_model)
# Write prediction equation
pre_equation_1 <- paste("voteshare =",
                             round(coefficients_1[1], 4),"+",
                             round(coefficients_1[2], 4)," * difflog")
cat(pre_equation_1)


#Question2
#(1) 
#Creat a regression about presvote and difflog
reg_model_2 <- lm(presvote~difflog,data = inc.sub)
summary(reg_model_2)

#(2)
# Create scatterplot with regression line and customized colors
scatter_2 <- ggplot(data = inc.sub, mapping = aes(x = difflog, y = presvote)) +
  geom_point(color = "#B4BDFF") +
  geom_smooth(method = "lm", se = FALSE, color = "#E5D4FF") +
  labs(title = "Scatterplot with Regression Line",
       x = "difflog",
       y = "presvote") +
  scale_color_manual(values = c("#B4BDFF", "#E5D4FF"))
print(scatter_2)

#(3)
#save the residuals in a separate object
res_2 <- residuals(reg_model_2)
res_sep <- str(res_2)
summary(res_sep)

#(4)
#write the prediction equation
cofficients_2 <- coef(reg_model_2)
prediction_equation_2 <- paste("presvote=",
                               round(cofficients_2[1],4),"+",
                               round(cofficients_2[2],4),"*difflog")
cat(prediction_equation_2)
  

#Question 3
#3.1 run a regression about voteshare and presvote
reg_3 <- lm(voteshare~presvote,data = inc.sub)
summary(reg_3)

#3.2 make a scatterplot
# Create scatterplot with regression line
scatter_3 <- ggplot(data = inc.sub,mapping = aes(x = presvote,y = voteshare))+
             geom_point()+
             geom_smooth(method = "lm",se = FALSE)+
             labs(title = "scatterplot with regression line",
                  x = "presvote", 
                  y = "voteshare")
# Print the scatterplot
print(scatter_3)

#3.3 write the prediction equation
residual_3 <- resid(reg_3)
print(residual_3)


#Question 4
#4.1 run a regression about residual_1 and residual_3
reg_4 <- lm(residuals_1~residuals_2,data = inc.sub)
print(reg_4)

#4.2 make a scatterplot 
# Create scatterplot with regression line
scatter_4 <- ggplot(data = inc.sub,mapping = aes(x = residuals_2,y = residuals_1))+
             geom_point()+
             geom_smooth(method = "lm",se = FALSE)+
             labs(title = "scatterplot with regression line",x = "residuals_2",y = "residuals_1")
# Print the scatterplot
print(scatter_4)


#4.3 write the prediction equation
coefficients_4 <- coef(reg_4)
prediction_quation_4 <- paste("residuals_1=",round(coefficients_4[1],4),"+",round(coefficients_4[2],4),"*residuals_2")
cat(prediction_quation_4)


#Question5
#5.1 run a regeression about voteshare, difflog and presvote
multreg_5 <- lm(voteshare~difflog+presvote,data = inc.sub)
print(multreg_5)

#5.2 write the prediction equation
coefficients_5 <- coef(multreg_5)
prediction_equation_5 <- paste("voteshare=",round(coefficients_5[1],4),"+",
                               round(coefficients_5[2],4),"*difflog","+",
                               round(coefficients_5[3],4),"*presvote")
cat(prediction_equation_5)

