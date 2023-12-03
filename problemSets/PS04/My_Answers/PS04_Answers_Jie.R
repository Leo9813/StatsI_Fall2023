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
lapply(c("ggplot2", "stargazer"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

############## Question 1 ##############

# First, run the following commands

install.packages(car); library(car)
data(Prestige); help(Prestige)
#Q1 a
# Create a new variable  
Prestige$professional <- ifelse(Prestige$type %in% "prof", 1,
                                ifelse(Prestige$type %in% c("bc","wc"), 0,NA))
                                  
head(Prestige)

#Q1 b
# Run the linear model with prestige as the outcome and income, professional, and their interaction as predictors
model <- lm(prestige ~ income + professional + income*professional, data = Prestige)
summary(model)
stargazer(model, no.space = TRUE)

#Q2 a
pvalue_1 <- 1-pt(2.625,df=128)
print(pvalue_1)

#Q2 b
pvalue_2<- 1-pt(3.231,df=128)
print(pvalue_2)
