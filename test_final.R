library(readxl)
library(janitor)
library(e1071)
library(plyr)
library(lubridate)
library(caret)
library(mlbench)
library(ggplot2)
library(caTools)
library(rpart)
library(chron)
library(ipred)
library(randomForest)
library(neuralnet)
library(pROC)
library(fastDummies)
library(FSelector)
library(mlr)
library(stringr)
library(xgboost)

data_test <- read_excel("Test_set.xlsx")


#Doing same data prepartions (as did on train data) on test data:
test=data_test
View(test)
summary(test)
str(test)
dim(test)


#Missing Values Analysis:
sum(is.na(test))
mv_test=data.frame(apply(test, 2, function(x){sum(is.na(x))}))
mv_test


#Variable 1:Airline:
unique(test$Airline)

#Plotting frequecy count of each airline in test:
ggplot(test,aes(x=test$Airline,fill=test$Airline))+
  geom_bar(position="dodge")+labs(title = "Counts of each Airline")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#True jet is not at all given in the test$Airline. Also, removing levels in test which I removed in train.
#Subset:
test=subset(test, test$Airline != "Vistara Premium economy" & test$Airline != "Jet Airways Business" & test$Airline  != "Multiple carriers Premium economy" )
unique(test$Airline)


#Variable 2: Date_of_Journey: 
unique(test$Date_of_Journey)
#Replacing same dates od different formats (1/03/2019 and 01/03/2019) to same format:
test$Date_of_Journey <- str_replace_all(test$Date_of_Journey, "01/03/2019",  "1/03/2019")
test$Date_of_Journey <- str_replace_all(test$Date_of_Journey, "03/03/2019",  "3/03/2019")
test$Date_of_Journey <- str_replace_all(test$Date_of_Journey, "06/03/2019",  "6/03/2019")
test$Date_of_Journey <- str_replace_all(test$Date_of_Journey, "09/03/2019",  "9/03/2019")
unique(test$Date_of_Journey)
str(test$Date_of_Journey)
test$Date_of_Journey=str_replace_all(test$Date_of_Journey, "[/]",  "-")
unique(test$Date_of_Journey)


#Variable 3: Source:
unique(test$Source)
#unique(train$Source) #same source in train & test.


#Variable 4: Destination:
unique(test$Destination)
#unique(train$Destination) #same destination in train & test.
#Replacing New Delhi to Delhi:
test$Destination=str_replace_all(test$Destination, "New Delhi",  "Delhi")
unique(test$Destination)


#Variable 5: Route:
unique(test$Route)   #Not Important variable. Since, Total_Stops variable explains the same.


#Variable 6: Dep_Time:
unique(test$Dep_Time)
#Combining Date_of_Journey & Dep_Time to new variable: #derived variable:
test$departure=paste(test$Date_of_Journey, test$Dep_Time, sep=' ') #paste date & time together.
test$departure=as.POSIXlt(test$departure, format = "%d-%m-%Y %H:%M")
test=test[ order(test$departure , decreasing = FALSE ),]
class(test$departure)


#Variable 7: Arrival_Time:
unique(test$Arrival_Time)
arrival_test=data.frame(str_split_fixed(test$Arrival_Time, " ", 2)) #just to see the variables seperately, I've formed this dataframe.
test=data.frame(test,arrival_test$X2)


#Variable 8 :Duration:
unique(test$Duration)
str(test$Duration)
#Duration is in categorical format. It has to be changed to numeric. 
#So, trying to remove the h and m from the variable.
test$dur=str_replace_all(test$Duration, "h ",  ".") #derived variable
test$dur=str_replace_all(test$dur, "m",  "")
test$dur=str_replace_all(test$dur, "h",  ".00")
class(test$dur)

test$dur1=hm(test$dur)
str(test$dur1)
sum(is.na(test$dur1))
class(test$dur1)
summary(test$dur1)

test$duration=round(as.duration(test$dur1)/dhours(1)) #important


#Variable 9: Total_Stops:
unique(test$Total_Stops)


#Variable 10: Additional Info:
unique(test$Additional_Info)


#Derived Variable:
#Extract the hour and day data from the request time
test$dep_hour <- format(test$departure, "%H")
#Creating morning, day, evening, night, midnight timestamp using dep_hour variable:
str(test$dep_hour)
test$dep_hour=as.numeric(test$dep_hour)
test$dep_time_slot = ifelse(test$dep_hour < 5, "Pre_Morning", ifelse(test$dep_hour < 10,"Morning",ifelse(test$dep_hour < 17,"Day_Time",ifelse(test$dep_hour < 22,"Evening","Late_Night"))))
test$dep_time_slot=as.factor(test$dep_time_slot)
summary(test$dep_time_slot)


#Creating dep_day (date+month):
test$dep_day <- format(test$departure, "%d%b")


summary(test)


test1=test


#Dropping some variable:
colnames(test1)
test1$Route=NULL
test1$Date_of_Journey=NULL
test1$Dep_Time=NULL
test1$Arrival_Time=NULL
test1$Duration=NULL
test1$dur=NULL
test1$dep_hour=NULL
test1$Additional_Info=NULL
test1$dur1=NULL
test1$departure=NULL
write.csv(test1,"test_new.csv",row.names=F)

#Creating Dummy variables for categorical data:
#install.packages("fastDummies")
library(fastDummies)
colnames(train1)
test2=dummy_cols(test1, select_columns = c("Airline","Source","Destination","Total_Stops","dep_time_slot","dep_day" ),
                 remove_first_dummy = FALSE)


#Omitting the variables in test that are omitted in train during dummy variable creation:
test2$`Airline_Multiple carriers`=NULL
test2$Source_Delhi=NULL
test2$Destination_Cochin=NULL
test2$`Total_Stops_1 stop`=NULL
test2$dep_time_slot_Pre_Morning=NULL
test2$dep_day_01Mar=NULL


test3=test2
colnames(test3)


test3=test3[, -c(1:5)]
test3=test3[,-c(2,3)]


summary(test3)
str(test3)
dim(test3)
sum(is.na(test3))
test3=na.omit(test3)
#Thus formed the test data for testing.


colnames(train3)
colnames(test3)
write.csv(test3,"test_final.csv",row.names=F)
