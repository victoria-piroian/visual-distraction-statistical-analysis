#R Final project

#Install/Packages 
install.packages("ggplot2")
#Download the libraries
library(ggplot2)
library(car)

#Set working Directory
setwd("~/Downloads") 
#Load datasets
Test_data<-read.csv("R_CVS_data.csv", sep = ",", header = T)

####Find the Mean, Median and Standard Deviation of the Independent Variables#####
mean(Test_data$Time,na.rm = TRUE)
mean(Test_data$Percent,na.rm = TRUE)

median(Test_data$Time,na.rm = TRUE)
median(Test_data$Percent,na.rm = TRUE)

sd(Test_data$Time,na.rm = TRUE)
sd(Test_data$Percent,na.rm = TRUE)

##################Levels and Percentages##################
#Graph the data in a Scatterplot
scatter_accuracy <- ggplot(Test_data, aes(Levels, Percent)) + 
  geom_point() + 
  labs(x = "Difficulty Levels", y = "Percentages (%)", title = "Observered Accuracy for the Different Levels")+ theme(plot.title = element_text(hjust = 0.5))
scatter_accuracy

#add a smoothing line to see what the trends look like
#ignoring the span function since it does not impact the line significantly
#created a linear regression line 
scatter_accuracy_model<- scatter_accuracy + geom_smooth(method = "lm",colour = "Red")
scatter_accuracy_model

#Correlation/Significance Value (p-test on t-values)
#using pearson because comparing two vector points and not examining ranks 
cor.test(Test_data$Levels, Test_data$Percent, method = "pearson")


##Linear regression model 
#map the dependent variable onto the independent variable 
# since there is limited points and it is a simple model, used linear regression 
model_percentage <- lm(Percent ~ Levels, data = Test_data) #creates the model 
summary(model_percentage) #shows info about the model created

##ANOVA Test 
#estimating the quantitative dependent variable change in relation to the independent variable
anova(model_percentage)
summary(anova(model_percentage))#gives the quartly description with max and min

#to have all 4 graphs on one plot 
par(mfrow=c(2,2))
plot(model_percentage)# if you want to see the model individually run this alone
par(mfrow=c(1,1))

###################Levels and Time distribution ##################

#ScatterPlot
scatter_time <- ggplot(Test_data, aes(Levels, Time)) + 
  geom_point() + 
  labs(x = "Difficulty Levels", y = "Time (s)", title = "Observered Reaction Time for the Different Levels") + theme(plot.title = element_text(hjust = 0.5))
scatter_time

#created a linear regression line in the model
scatter_time_model<- scatter_time + geom_smooth(method = "lm", colour = "Red")
scatter_time_model

#Correlation/Significance Value (p-test on t-values)
#using pearson because comparing two vector points and not examining ranks 
cor.test(Test_data$Levels, Test_data$Time, method = "pearson")


##Linear regression
model_time = lm(Time ~ Levels, data = Test_data)
summary(model_time)

#ANOVA TEST
anova(model_time)
summary(anova(model_time))#gives the quartly description with max and min


#to have all 4 graphs on one plot 
par(mfrow=c(2,2))
plot(model_time)# if you want to see the model individually run this alone
par(mfrow=c(1,1))





