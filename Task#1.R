#to get the current the directory
getwd() #display current directore

#Setting the working directory
setwd("C:/Users/Ali Raza/Desktop/Sparks Foundation/Task#1") #is used to set the working directory

#Read the given data sets for tasks or given problem
#the given data sets is based on student scores
s_scores <- read.csv("student_scores - student_scores.csv")

#to check the data sets
head(s_scores)

#to check the detail of data sets
summary(s_scores)


#Loading required R packages
#Load required packages:
# 
#  ggplot2: for create plot
#  tidyverse for data manipulation and visualization
# Installed ggrepel for used ggpubr
#  ggpubr: creates easily a publication ready-plot
library(ggplot2)
library(tidyverse)
library(ggpubr)

#set the theme
theme_set(theme_pubr())

#Visualization:
#   Create a scatter plot displaying the hours and scores. by geom_point()
#   Add a smoothed line by stat_smooth()
colnames(s_scores) #to check columns name

ggplot(s_scores, aes(x= Hours, y= Scores)) +
  geom_point() + 
  stat_smooth() +
  ggtitle("Hours VS Scores") + 
  theme(legend.title = element_text(size = 9, face = "bold")
        )
# find the correlation between hours and scores
# compute correlation in R by using cor() 

cor(s_scores$Hours, s_scores$Scores)

#The correlation coefficient measures the level of the association between two variables x and y. Its value ranges between -1 (perfect negative correlation: when x increases, y decreases) and +1 (perfect positive correlation: when x increases, y increases).
#
#A value closer to 0 suggests a weak relationship between the variables. A low correlation (-0.2 < x < 0.2) probably suggests that much of variation of the outcome variable (y) is not explained by the predictor (x). In such case, we should probably look for better predictor variables.
#
#In our example, the correlation coefficient is large enough, so we can continue by building a linear model of y as a function of x.

#
#Computation
#

# In R language lm() is a function that can be use to determine the beta coeffiecie
# of the linear models

linear_model <- lm(Scores~Hours, data = s_scores)

#check linear model

linear_model

# display regression line

ggplot(s_scores, aes(x = Hours, y = Scores)) +
  geom_point() +
  stat_smooth(method = lm)

#Model Summary
summary(linear_model)

#Standard errors and confidence intervals

confint(linear_model)

#Residual standard error (RSE).

sigma(linear_model)*100/mean(s_scores$Scores)

#predict the score on 9.25 hours
predict(linear_model, data.frame(Hours = 9.25) )

