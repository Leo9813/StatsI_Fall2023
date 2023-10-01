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



# here is where you load any necessay packages
 
library(ggplot2)

lapply(c(" "),  pkgTest)



#####################
# Problem 1
####################
#Question 1'
IQ_scores <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
t_test<- t.test(IQ_scores, conf.level = 0.90)
print(t_test$conf.int)

#Question 2'
cat("Test Statistic (t):", t_test$statistic, "\n")
cat("Degrees of Freedom:", t_test$parameter, "\n")
cat("p-value:", t_test$p.value, "\n")
cat("At a significance level of 0.05, we", 
    ifelse(t_test$p.value < 0.05, "reject", "fail to reject"), 
    "the null hypothesis. There is", 
    ifelse(t_test$p.value < 0.05, "enough", "not enough"), 
    "evidence to conclude that school students' average IQ is higher than the national average.\n")

#####################
# Problem 2
#####################
#Question 1'
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2023/main/datasets/expenditure.txt", header=T)
data <- expenditure
pairs(data[c("Y", "X1", "X2", "X3")], main="Scatterplot Matrix")
correlations <- cor(data[c("Y", "X1", "X2", "X3")])
print(correlations)
#Correlation between Y and X1: Positive correlation, indicating that as X1 increases, Y tends to increase.
#Correlation between Y and X2: A weaker positive correlation compared to X1, indicating some positive relationship.
#Correlation between Y and X3: There appears to be a relatively weaker correlation between Y and X3, which is less linear than the other relationships.

#Question 2'
ggplot(data, aes(x =factor(Region) , y = Y)) +
  geom_boxplot() +
  labs(x = "Region", y = "Per Capita Expenditure") 
  theme_minimal()
#West has thehighest per capita expenditure on housing assistance
  
#Question 3'
ggplot(data, aes(x = X1, y = Y, color = factor(Region), shape = factor(Region))) +
  geom_point() +
    labs(x = "Per Capita Income", y = "Per Capita Expenditure") +
    scale_color_manual(values = c("red", "blue", "yellow", "black")) +
    scale_shape_manual(values = c(16, 17, 18, 19)) +
    theme_minimal()
#looking at the chart, I can see that there is a positive correlation between the data points, i.e. states with higher "Per Capita Income" tend to also have higher "Per Capita Expenditure" and vice versa.
#The positive correlation between Per Capita Income Per and Capita Expenditure in the South District, North District and Northwest District is stronger than that in the West District.

# -------------------------------------------------------------------------

