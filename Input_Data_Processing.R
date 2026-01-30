## R Project 
## Taking the data collected from the 3 levels of testing 
## modelling the distribution and other relevant features 

#############################################################################
#install this package to read excel files 
install.packages("readxl")

#imported Libraries
library(readxl)
library(ggplot2)


#change working directory
setwd("~/Downloads") 
#loads the data into a table table 

Test_data = read_excel('R_Data_Excel.xlsx')
# create a new table to change the data
Test_data_remove <- Test_data
#remove all blank columns and columns that are not needed
#Test_data_remove[c(1,2,8,9,10,11,12,13,14,15,16)] <- NULL   

#Separate the data into the 3 levels 
Data_level_1 <- Test_data_remove[1:8,]
Data_level_2 <- Test_data_remove[14:21,]
Data_level_3 <- Test_data_remove[26:33,]


#############################################################################
##Probability Calculations associated with each accuracy 

##baseline probabilities 
# This is the probability that a person randomly guesses the differences  
# Level 1 --> 5 images and 3 differences  
level1 <- (1/5)^3

#Level 2 --> 7 images and 4 differences 
level2 <- (1/7)^4

#Level 3 --> 10 images and 5 differences 
level3 <- (1/10)^5

##########################################################################################################################################################
## binomial distribution probabilities

############################Level 1############################

list_Level_1 <- c()

for(i in 0:3) {
  # 10 different images, 6 were different and 4 were the same images 
  # probability is 6/10 for noticing a difference
  accuracy_probs <- dbinom(i, size=3, prob=0.6)
  list_Level_1 <- c(list_Level_1, accuracy_probs)
}

plot(y=list_Level_1,x=c(0,1,2,3), type ="l",col="black",xlab="Number of Correctly Spotted Differences",
     ylab= "Probability", main = "Predicted Accuracy Weighting for Level 1", xlim=c(0,3), ylim=c(0,0.5))

#############################Level 2############################

list_Level_2 <- c()

for(i in 0:4) {
  # 14 different images, 8 were different and 6 were the same images 
  # probability is 8/14 for noticing a difference
  accuracy_probs2 <- dbinom(i, size=4, prob=8/14)
  list_Level_2 <- c(list_Level_2, accuracy_probs2)
}

#plot the accuracy weight distribution 
plot(y=list_Level_2,x=c(0,1,2,3,4), type = "l",col="black",xlab="Number of Correctly Spotted Differences",
     ylab= "Probability", main = "Predicted Accuracy Weighting for Level 2", xlim=c(0,4), ylim=c(0,0.5))

#############################Level 3############################

list_Level_3 <- c()

for(i in 0:5) {
  # 20 different images, 10 were different and 10 were the same images 
  # probability is 10/20 for noticing a difference
  accuracy_probs3 <- dbinom(i, size=5, prob=0.5)
  list_Level_3 <- c(list_Level_3, accuracy_probs3)
}

plot(y=list_Level_3,x=c(0,1,2,3,4,5), type = "l",col="black",xlab="Number of Correctly Spotted Differences",
     ylab= "Probability", main = "Predicted Accuracy Weighting for Level 3", xlim=c(0,5), ylim=c(0,0.5))

###plot the probabilities together
#level 1 data
plot(x=c(0,1,2,3), list_Level_1, type="l", col="purple", lty=1, xlim=c(0,5), ylim=c(0,1), 
     ylab="Probability", xlab="Number of Correctly Spotted Differences",
     main = "Predicted Accuracy for the Different Levels") 
#add level 2 data
#points(x=c(0,1,2,3,4), list_Level_2,col="blue")
lines(x=c(0,1,2,3,4), list_Level_2, col="blue",lty=2)
#add level 3 data
#points(x=c(0,1,2,3,4,5),list_Level_3,col="dark green",pch="+")
lines(x=c(0,1,2,3,4,5), list_Level_3, col=" dark green", lty=3)
#add an legend
legend(0,1,legend=c("Level 1","Level 2","Level 3"), col=c("purple","blue"," dark green"),
       lty=c(1,2,3))

##########################################################################################################################################################
#graphing the observed data

#######################Level 1#######################
#create a list of all the observed values 
obs_level_1<- c()
for(i in 0:8) {
  if(i == 0){
    obs_level_1<- c(0, obs_level_1)
  }
  obs_level_1<- c(Data_level_1[i,6], obs_level_1)
}
#change the list value into a vector
vec_obs_1<- unlist(obs_level_1)

#create a DataFrame for the data
#rm(df2)    #to drop the Dataframe
df_1 <- data.frame(People=c(1,1,1,1,1,1,1,1,0.00000001), Correct=c(vec_obs_1))
# create the Bar Graph to Represent the spread of data
#change colour to fill to make it all filled in
plot_1<-ggplot(data=df_1, aes(x=Correct, y=People, colour= Correct)) +
  geom_bar(stat="identity",fill="grey")+theme_minimal()
#print the graph and add formating features 
print(plot_1+ labs(title = "Observered Accuracy For Level 1", x = "Correctly Identified Differences ", 
              y = "Number of People")+ theme(plot.title = element_text(hjust = 0.5)))

#######################Level 2#######################
obs_level_2<- c()
for(i in 0:8) {
    #do this to have 0 appear as an option
   if(i == 0){
     obs_level_2<- c(0, obs_level_2)
  }
  obs_level_2<- c(Data_level_2[i,6], obs_level_2)
}
#do this to have 1 appear as an option but have no value associated with it
#obs_level_2<-append(obs_level_2,"1")

#change the list value into a vector
vec_obs_2<- unlist(obs_level_2)
print(obs_level_2)
#create a DataFrame for the data
df_2<- data.frame(People=c(1,1,1,1,1,1,1,1,0.00000001,0.00000001), Correct=c(vec_obs_2))
# create the Bar Graph to Represent the spread of data
#change colour to fill to make it all filled in
plot_2<-ggplot(data=df_2, aes(x=Correct, y=People, colour= Correct)) +
  geom_bar(stat="identity",fill="grey")
#print the graph and add formating features 
print(plot_2+ labs(title = "Observed Accuracy For Level 2", x = "Correctly Identified Differences ", 
              y = "Number of People")+ theme(plot.title = element_text(hjust = 0.5)))

#######################Level 3#######################
obs_level_3<- c()
for(i in 0:8) {
  if(i == 0){
    obs_level_3<- c(0, obs_level_3)
  }
  obs_level_3<- c(Data_level_3[i,6], obs_level_3)
}
#change the list value into a vector
vec_obs_3<- unlist(obs_level_3)
#create a DataFrame for the data
df_3 <- data.frame(People=c(1,1,1,1,1,1,1,1,0.00000001), Correct=c(vec_obs_3))
# create the Bar Graph to Represent the spread of data
#change colour to fill to make it all filled in
plot_3<-ggplot(data=df_3, aes(x=Correct, y=People, colour= Correct)) +
  geom_col(stat="identity",fill="grey")
#print the graph and add formating features 
print(plot_3+ labs(title = "Observed Accuracy For Level 3", x = "Correctly Identified Differences ", 
              y = "Number of People")+ theme(plot.title = element_text(hjust = 0.5)))
##########################################################################################################################################################

#Determine the probability of the events 

#multiple the number of people got got the corresponding correctness to the probability of it happening

#######################Level 1#######################
# get the number of people at each instance

Probs_of_event_1<- c()  #probability of the number of people getting the same accuracy 
Percent_of_people_1<- c() #how many people got the same accuracy
Conditional_probs_1<- c()#conditional probability of the event happening

counter_1 <- 0
#Check all the people an all their accuracy and collect the values
for(i in 0:3){
  for(j in 1:8){
    b = obs_level_1[j] == i
    if(b == TRUE){
      counter_1 = counter_1 + 1
    }
  }
  if(counter_1 == 0){
    Probs_of_event_1<- c(Probs_of_event_1, 0.00000001)
    Percent_of_people_1<- c(Percent_of_people_1, 0.00000001)
    Conditional_probs_1 <-c(Conditional_probs_1, 0.00000001)
  }
  else{
    cont_dist_1<- ((list_Level_1[i+1])*counter_1)/ ((8-counter_1)*(1-list_Level_1[i+1])+(list_Level_1[i+1])*counter_1)
    Percent_of_people_1<- c(Percent_of_people_1, counter_1/8)
    Conditional_probs_1 <-c(Conditional_probs_1, cont_dist_1)
    Probs_of_event_1<- c(Probs_of_event_1, (list_Level_1[i+1])^counter_1)
    counter_1 = 0
  }
}
##plot the different types of data
plot(y=Percent_of_people_1, x=c(0,1,2,3), type = "l",col=" dark green",xlab="Number of Correctly Spotted Differences",
     ylab= "Probability", main = "Accuracy for Level 1", xlim=c(0,3), ylim=c(0,1))
lines(seq(0,3), c(level1,level1,level1,level1), type = "l", col="red", lty=3)#baseline
lines(x=c(0,1,2,3), list_Level_1, col=" blue", lty=3)
lines(x=c(0,1,2,3), Probs_of_event_1, col=" purple", lty=2)
lines(x=c(0,1,2,3), Conditional_probs_1, col = "black", lty=4)
legend("topleft",legend=c("Level 1 Observered Distribution", "Baseline Distribution", "Predicted Distribution", "Probability of Event Happening", "Conditional Probability"),cex=0.7, col=c("dark green","red"," blue","purple", "black"),
       lty=c(1,3,3,2,4))

#######################Level 2#######################
# get the number of people at each instance

Probs_of_event_2<- c()  #probability of the number of people getting the same accuracy 
Percent_of_people_2<- c() #how many people got the same accuracy
Conditional_probs_2<- c()#conditional probability of the event happening

counter_2 <- 0
#nested for loop to go through all the score and possible scores 
for(i in 0:4){
  for(j in 1:8){
    b = obs_level_2[j] == i
    if(b == TRUE){
      counter_2 = counter_2 + 1
    }
  }
  if(counter_2 == 0){
    #set it to a really small number so it is still seen on the graph 
    Probs_of_event_2<- c(Probs_of_event_2, 0.00000001)
    Percent_of_people_2<- c(Percent_of_people_2, 0.00000001)
    Conditional_probs_2 <-c(Conditional_probs_2, 0.00000001)
  }
  else{
    Probs_of_event_2<- c(Probs_of_event_2, counter_2/8)
    cont_dist_2<- ((list_Level_2[i+1])*counter_2)/ ((8-counter_2)*(1-list_Level_2[i+1])+(list_Level_2[i+1])*counter_2)
    Conditional_probs_2 <-c(Conditional_probs_2,  cont_dist_2)
    Percent_of_people_2<- c(Percent_of_people_2, (list_Level_2[i+1])^counter_2)
    counter_2 = 0
  }
}

plot(y=Percent_of_people_2, x=c(0,1,2,3,4), type = "l",col=" dark green",xlab="Number of Correctly Spotted Differences",
     ylab= "Probability", main = "Accuracy for Level 2", xlim=c(0,4), ylim=c(0,1))
lines(seq(0,4), c(level2,level2,level2,level2,level2), type = "l", col="red", lty=3)#baseline
lines(x=c(0,1,2,3,4), list_Level_2, col=" blue", lty=3)
lines(x=c(0,1,2,3,4), Probs_of_event_2, col=" purple", lty=2)
lines(x=c(0,1,2,3,4), Conditional_probs_2, col = "black", lty=4)
legend("topleft",legend=c("Level 2 Actual Distribution", "Baseline", "Predicted Distribution", "Probability of Event Happening", "Conditional Probability"),cex=0.7, col=c("dark green","red"," blue","purple", "black"),
       lty=c(1,3,3,2,4))

####################### Level 3 #######################
# get the number of people at each instance

Probs_of_event_3<- c()  #probability of the number of people getting the same accuracy 
Percent_of_people_3<- c() #how many people got the same accuracy
Conditional_probs_3<- c()#conditional probability of the event happening

counter_3 <- 0

#Check all the people an all their accuracy and collect the values
for(i in 0:5){
  for(j in 1:8){
    a = obs_level_1[j] == i
    if(a == TRUE){
    counter_3= counter_3 + 1
    }
  }
  if(counter_3 == 0){
    Probs_of_event_3<- c(Probs_of_event_3, 0.00000001)
    Percent_of_people_3<- c(Percent_of_people_3, 0.00000001)
    Conditional_probs_3 <-c(Conditional_probs_3, 0.00000001)
  }
  else{
    Percent_of_people_3<- c(Percent_of_people_3, counter_3/8)
    cont_dist_3<- ((list_Level_3[i+1])*counter_3)/
      ((8-counter_3)*(1-list_Level_3[i+1])+(list_Level_3[i+1])*counter_3)
    Conditional_probs_3 <-c(Conditional_probs_3, cont_dist_3)
    Probs_of_event_3<- c(Probs_of_event_3, (list_Level_3[i+1])^counter_3)
    counter_3 = 0
  }
}

##plot the different types of data
plot(y=Percent_of_people_3, x=c(0,1,2,3,4,5), type = "l",col=" dark green",xlab="Number of Correctly Spotted Differences",
     ylab= "Probability", main = "Accuracy for Level 3", xlim=c(0,5), ylim=c(0,1))

lines(seq(0,5), c(level3,level3,level3,level3,level3,level3), type = "l", col="red")#baseline value of just guessing
lines(x=c(0,1,2,3,4,5), list_Level_3, col=" blue", lty=3)
lines(x=c(0,1,2,3,4,5), Probs_of_event_3, col=" purple", lty=2)
lines(x=c(0,1,2,3,4,5), Conditional_probs_3, col = "black", lty=4)
legend("topleft",legend=c("Level 3 Observered Distribution", "Baseline Distribution", "Predicted Distribution", "Probability of Event Happening", "Conditional Probability"),cex=0.7, col=c("dark green","red"," blue","purple", "black"),
       lty=c(1,3,3,2,4))

##########################################################################################################################################################
###################################################################################################
#Time verse Accuracy 

#######################Level 1#######################
#create a list of all the observed times  
time_level_1<- c()
for(i in 1:8) {
  time_level_1<- c(Data_level_1[i,7], time_level_1)
}
print(time_level_1)
print(vec_obs_1)
vec_obs_1_1 <- vec_obs_1[-9]
#change the list value into a vector
vec_time_obs_1<- unlist(time_level_1)
print(length(vec_time_obs_1))
print(vec_obs_1_1)
#create a DataFrame for the data
#rm(df2)    #to drop the Dataframe
df_1_1 <- data.frame(Time=c(vec_time_obs_1), Correct=c(vec_obs_1_1))

# create the Scatter Plot to Represent the spread of data
#change colour to fill to make it all filled in
plot_1_1<- ggplot(data=df_1_1, aes(x=Correct, y=Time)) +geom_point()

plot_1_1<- plot_1_1 + geom_smooth(formula = y ~ s(x, bs = "cs"), method = "lm", colour = "Red")
#print the graph and add formating features 
print(plot_1_1+ labs(title = "Observered Accuracy For Level 1", x = "Correctly Identified Differences ", 
                 y = "Time Elapsed (seconds)")+ theme(plot.title = element_text(hjust = 0.5)))

model_ = lm(Time ~ Correct, data = df_1_1)
summary(model_)
anova(model_)
plot(model_)

#######################Level 2#######################
#create a list of all the observed times  
time_level_2<- c()
for(i in 1:8) {
  time_level_2<- c(Data_level_2[i,7], time_level_2)
}
#run twice to make the values equal
#vec_obs_2 <- vec_obs_2[-9]
vec_obs_2_2 <- vec_obs_2
#change the list value into a vector
vec_time_obs_2<- unlist(time_level_2)


#create a DataFrame for the data
#rm(df2)    #to drop the Dataframe
df_2_2 <- data.frame(Time=c(vec_time_obs_2), Correct=c(vec_obs_2_2))

# create the Scatter Plot to Represent the spread of data
#change colour to fill to make it all filled in
plot_2_2<- ggplot(data=df_2_2, aes(x=Correct, y=Time)) +geom_point()

plot_2_2<- plot_2_2 + geom_smooth(formula = y ~ s(x, bs = "cs"), method = "lm", colour = "Red")
#print the graph and add formating features 
print(plot_2_2+ labs(title = "Observered Accuracy For Level 2", x = "Correctly Identified Differences ", 
                     y = "Time Elapsed (seconds)")+ theme(plot.title = element_text(hjust = 0.5)))

model_ = lm(Time ~ Correct, data = df_2_2)
summary(model_)
anova(model_)
plot(model_)
#######################Level 3#######################
#create a list of all the observed times  
time_level_3<- c()
for(i in 1:8) {
  time_level_3<- c(Data_level_3[i,7], time_level_3)
}
print(length(time_level_3))
print(length(vec_obs_3_3))
vec_obs_3_3 <- vec_obs_3[-9]
#change the list value into a vector
vec_time_obs_3<- unlist(time_level_3)
print(length(vec_time_obs_3))
print(vec_obs_3_3)
#create a DataFrame for the data
#rm(df2)    #to drop the Dataframe
df_3_3 <- data.frame(Time=c(vec_time_obs_3), Correct=c(vec_obs_3_3))

# create the Scatter Plot to Represent the spread of data
#change colour to fill to make it all filled in
plot_3_3<- ggplot(data=df_3_3, aes(x=Correct, y=Time)) +geom_point()

plot_3_3<- plot_3_3 + geom_smooth(formula = y ~ s(x, bs = "cs"), method = "lm", colour = "Red")
#print the graph and add formating features 
print(plot_3_3+ labs(title = "Observered Accuracy For Level 3", x = "Correctly Identified Differences ", 
                     y = "Time Elapsed (seconds)")+ theme(plot.title = element_text(hjust = 0.5)))

model_ = lm(Time ~ Correct, data = df_3_3)
summary(model_)
anova(model_)
plot(model_)
##########################################################################################################################################################














