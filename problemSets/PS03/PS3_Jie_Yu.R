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
ggplot(data = inc.sub, aes(x = difflog, y = voteshare)) +
geom_point(color = "#B4BDFF") +
#Add scatterplot points  
geom_smooth(method = "lm", se = FALSE, color = "#E5D4FF") +
  #Add regression line
  labs(title = "Difflog Campaign Spending",
       x = "difflog",
       y = "voteShare")


#(3)
#save the residuals in a separate object
res_1 <- residuals(reg_model)
res_sep_1 <- str(res_1)
summary(res_sep_1)


#(4)
# Extracting the intercept and slope
intercept <- round(coef(reg_model)[1],4)  
slope <- round(coef(reg_model)[2],4)

# Printing the intercept and slope
cat("Final Model: votehare=", intercept, "+", slope, "* difflog\n")




#Question2
#(1) 
#Creat a regression about presvote and difflog
reg_model_2 <- lm(presvote~difflog,data = inc.sub)
summary(reg_model_2)

#(2)
# Create scatterplot with regression line and customized colors
ggplot(data = inc.sub, aes(x = difflog, y = presvote)) +
  geom_point(color = "#B4BDFF") +
  #Add scatterplot points  
  geom_smooth(method = "lm", se = FALSE, color = "#E5D4FF") +
  #Add regression line
  labs(title = "Difflog Campaign Spending",
       x = "difflog",
       y = "presvote")

#(3)
#save the residuals in a separate object
res_2 <- residuals(reg_model_2)
res_sep_2 <- str(res_2)
summary(res_sep_2)

#(4) write the prediction equation
# Extracting the intercept and slope
intercept <- round(coef(reg_model_2)[1],4)  
slope <- round(coef(reg_model_2)[2],4)

# Printing the intercept and slope
cat("Final Model: presvote=", intercept, "+", slope, "* difflog\n")

#Question 3
#(1) 
#Creat a regression about presvote and difflog
reg_model_3 <- lm(voteshare~presvote,data = inc.sub)
summary(reg_model_3)

#(2)
#Create scatterplot with regression line and customized colors
ggplot(data = inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point(color = "#B4BDFF") +
  #Add scatterplot points  
  geom_smooth(method = "lm", se = FALSE, color = "#E5D4FF") +
  #Add regression line
  labs(title = "Difflog Campaign Spending",
       x = "presvote",
       y = "voteshare")

#(3) 
# Extracting the intercept and slope
intercept <- round(coef(reg_model_3)[1],4)  
slope <- round(coef(reg_model_3)[2],4)

# Printing the intercept and slope
cat("Final Model: voteshare =", intercept, "+", slope, "* presvote\n")


#Question 4
#(1)
#Create a regression about residual_1 and residual_3
reg_model_4<- lm(res_1~res_2,data = inc.sub)
summary(reg_model_4)

#(2)
#Create scatterplot with regression line and customized colors
ggplot(data = inc.sub, aes(x = res_2, y = res_1)) +
  geom_point(color = "#B4BDFF") +
  #Add scatterplot points  
  geom_smooth(method = "lm", se = FALSE, color = "#E5D4FF") +
  #Add regression line
  labs(title = "Difflog Campaign Spending",
       x = "res_2",
       y = "res_1")


#(3) 
# Extracting the intercept and slope
intercept <- round(coef(reg_model_4)[1],4)  
slope <- round(coef(reg_model_4)[2],4)

# Printing the intercept and slope
cat("Final Model: res_1=", intercept, "+", slope, "* res_2\n")

#Question5
#(1)
#Create a regeression about voteshare, difflog and presvote
multreg_5 <- lm(voteshare~difflog+presvote,data = inc.sub)
summary(multreg_5)

#(2) 
#write the prediction equation
# Extracting the intercept and slope
intercept <- round(coef(multreg_5)[1],4)  
slope_1 <- round(coef(multreg_5)[2],4)
slope_2 <- round(coef(multreg_5)[3],4)

# Printing the intercept and slope
cat("Final Model: votehare=", intercept, "+", slope_1, "* difflog", "+",slope_2, "* presvote\n")
