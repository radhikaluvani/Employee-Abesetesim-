rm(list=ls(all=T))
setwd("C:/Users/Neelam/Desktop/Workradhika/project2")
getwd()

#Libraries imported
library(xlsx)
library(ggplot2)
library(gridExtra)
library(DMwR)
library(corrgram)
library(forecast)
library(outliers)
library(plyr)
library(tseries) #for stationarity
library(lmtest) #for testing autocorrelation

#importing employee absenteeism data set
data = read.csv(file.choose(), header =T)

#exploring data
data[1:113, 'Year'] = 2007
data[114:358, 'Year'] = 2008
data[359:570, 'Year'] = 2009
data[571:740, 'Year'] = 2010

#Checking data type and structure
head(data, 10)
str(data)
names(data)
summary(data)
ggplot(data , aes(x = data$Disciplinary.failure ))+
  geom_histogram(binwidth = 1 , fill = "grey" ,  colour = "black")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$ID ))+
  geom_histogram(binwidth = 1 , fill = "grey" ,  colour = "black")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$Reason.for.absence ))+
  geom_histogram(binwidth = 1 , fill = "grey" ,  colour = "black")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$Month.of.absence ))+
  geom_histogram(binwidth = 1 , fill = "grey" ,  colour = "black")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$Year ))+
  geom_histogram(binwidth = 1 , fill = "purple" ,  colour = "white")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))


ggplot(data , aes(x = data$Transportation.expense))+
  geom_histogram(binwidth = 1 , fill = "white" ,  colour = "navyblue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$Distance.from.Residence.to.Work))+
  geom_histogram(binwidth = 1 , fill = "navyblue" ,  colour = "navyblue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$Service.time))+
  geom_histogram(binwidth = 1 , fill = "navyblue" ,  colour = "navyblue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))


ggplot(data , aes(x = data$Age))+
  geom_histogram(binwidth = 1 , fill = "orange" ,  colour = "orange")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$Hit.target))+
  geom_histogram(binwidth = 1 , fill = "orange" ,  colour = "orange")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))


ggplot(data , aes(x = data$Education))+
  geom_histogram(binwidth = 1 , fill = "orange" ,  colour = "orange")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$Son))+
  geom_histogram(binwidth = 1 , fill = "navyblue" ,  colour = "navyblue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$Social.drinker))+
  geom_histogram(binwidth = 1 , fill = "navyblue" ,  colour = "navyblue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$Social.smoker))+
  geom_histogram(binwidth = 1 , fill = "navyblue" ,  colour = "navyblue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$Pet))+
  geom_histogram(binwidth = 1 , fill = "navyblue" ,  colour = "navyblue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$Weight))+
  geom_histogram(binwidth = 1 , fill = "navyblue" ,  colour = "navyblue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$Height))+
  geom_histogram(binwidth = 1 , fill = "navyblue" ,  colour = "navyblue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$Body.mass.index))+
  geom_histogram(binwidth = 1 , fill = "navyblue" ,  colour = "navyblue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

ggplot(data , aes(x = data$Absenteeism.time.in.hours))+
  geom_histogram(binwidth = 1 , fill = "navyblue" ,  colour = "navyblue")+
  ggtitle("Histogram Analysis") +  theme(text=element_text(size=15))

data = data[1:737,]
data[67, 'Month.of.absence'] = 10



factor_var = c('ID',"Reason.for.absence", "Month.of.absence","Day.of.the.week", "Seasons", "Disciplinary.failure",
                     "Education", "Son", "Social.drinker", "Social.smoker", "Pet")
for(i in factor_var)
{
  data[,i] = as.factor(data[,i])
}

#Confirming the changes type
str(data)






#*****************Missing value Analysis******************************
#Missing value Analysis
#Creating a data frame with missing value count
missing_val = data.frame(apply(data,2,function(x){sum(is.na(x))}))
#Adding a columns with features and traget variable name
missing_val$Columns = row.names(missing_val)
#Renaming the missing value columns with 'missing_percentage' 
names(missing_val)[1] =  "missing_percentage"
#Calculating the percentage
missing_val$missing_percentage = (missing_val$missing_percentage/nrow(data)) * 100
#Changing the position of 'missing_percentage'
missing_val = missing_val[order(-missing_val$missing_percentage),]
#Removing the first redundant column
row.names(missing_val) = NULL
#Changing the position of columns
missing_val = missing_val[,c(2,1)]
#Saving the missing percentages in a csv
#write.csv(missing_val, "Miising_perc.csv", row.names = F)


ggplot(data = missing_val[1:10,], aes(x=reorder(Columns, -missing_percentage),y = missing_percentage))+
  geom_bar(stat = "identity",fill = "black",size=25)+ xlab("Parameter")+
  ggtitle("Missing data percentage") + theme_bw()




# kNN Imputation
data = knnImputation(data, k = 3)
sum(is.na(data))



#Confirming there are no na's left
sum(is.na(data))

#Structure of data after imputing
str(data)

#Summary of data
summary(data)


#********************Outlier Analysis**********************

numeric_index = sapply(data,is.numeric) #selecting only numeric

numeric_data = data[,numeric_index]

cnames = colnames(data)

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "Absenteeism.time.in.hours"), data = subset(data))+ 
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "purple" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="Absenteeism.time.in.hours")+
           ggtitle(paste("Box plot of Absenteeism.time.in.hours for",cnames[i])))
}

# ## Plotting together
gridExtra::grid.arrange(gn1,gn2,gn3,gn11,ncol=4)
gridExtra::grid.arrange(gn4,gn7,gn12,ncol=3)
gridExtra::grid.arrange(gn5,gn6, ncol=2)*
gridExtra::grid.arrange(gn8,gn9,gn10,ncol=3)
gridExtra::grid.arrange(gn13,gn14,gn15, ncol=3)
gridExtra::grid.arrange(gn16,gn17,gn18, ncol=3)
gridExtra::grid.arrange(gn20,gn21,gn22,gn19, ncol=4)


# KnnImputation
for(i in cnames)
{
  cnames
  val = data[,i][data[,i] %in% boxplot.stats(data[,i])$out]
  print(length(val))
  data[,i][data[,i] %in% val] = NA
}
data = knnImputation(data, k = 3)

#Confirming that there is no missing value left
sum(is.na(data))

#Checking the structure again
str(data)
summary(data)
write.csv(data, "CleanedData.csv", row.names = T)

##*********************Correlation Plot************************

corrgram(data, order = T,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")
##################################Feature Scaling################################################
#Normality check
qqnorm(data$Body.mass.index)
hist(data$Absenteeism.time.in.hours)

#Normalisation

#for(i in cnames){
 # print(i)
  #data[,i] = (data[,i] - min(data[,i]))/
   # (max(data[,i] - min(data[,i])))
#}

# #Standardisation
# for(i in cnames){
#   print(i)
#   data[,i] = (data[,i] - mean(data[,i]))/sd(data[,i])
# }



#Checking the correlation with churn using anova to finding the corrleation between the target continous and categorical features
#From the correlated variable

avo1 = aov(data$Absenteeism.time.in.hours ~ data$Day.of.the.week)
avo2 = aov(data$Absenteeism.time.in.hours ~ data$Seasons)
avo3 = aov(data$Absenteeism.time.in.hours ~ data$Disciplinary.failure)
avo4 = aov(data$Absenteeism.time.in.hours ~ data$Education)
avo5 = aov(data$Absenteeism.time.in.hours ~ data$Son)
avo6 = aov(data$Absenteeism.time.in.hours ~ data$Social.drinker)
avo7 = aov(data$Absenteeism.time.in.hours ~ data$Social.smoker)
avo8 = aov(data$Absenteeism.time.in.hours ~ data$Pet)

#***************Feature selection with density plot***********************
g1 = ggplot(data, aes(x = Transportation.expense)) + geom_density()
g2 = ggplot(data, aes(x = Distance.from.Residence.to.Work)) + geom_density()
g3 = ggplot(data, aes(x = Age)) + geom_density()
g4 = ggplot(data, aes(x = Service.time)) + geom_density()
g5 = ggplot(data, aes(x = Work.load.Average.day)) + geom_density()
g6 = ggplot(data, aes(x = Hit.target)) + geom_density()
g7 = ggplot(data, aes(x = Weight)) + geom_density()
g8 = ggplot(data, aes(x = Height)) + geom_density()
g9 = ggplot(data, aes(x = Body.mass.index)) + geom_density()
g10 = ggplot(data, aes(x = Absenteeism.time.in.hours)) + geom_density()

grid.arrange(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, ncol=2)





#important variables for absenestism Disciplinary failure, Reason of Absence, Social Drinker, Social Smoker and Son. 

aggre.absent.desciplinary_failure =  ddply(data, c( "Disciplinary.failure"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))

#Count for Reason of absence
g11 = ggplot(data, aes(x = Reason.for.absence, fill = Reason.for.absence)) + geom_bar(stat = 'count')  +
  geom_label(stat='count',aes(label=..count..), size=5) + labs(ggtitle("Count for Reason of Absence")) +
  theme_grey(base_size = 18)
g11
#Reasons for absence according to day or week, seasons and month of absence
g12 = ggplot(data, aes(x = Reason.for.absence, fill = Day.of.the.week)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..)) + labs(ggtitle("Reason of absence based day of week"))+
  theme_grey()
g12

g13 = ggplot(data, aes(x = Reason.for.absence, fill = Seasons)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..)) + labs(ggtitle("Reason of absence based on seasons of all the years"))+
  facet_grid(Year~.)
theme_grey()
g13

g14 = ggplot(data, aes(x = Reason.for.absence, fill = Month.of.absence)) + geom_bar(stat = 'count', position = 'dodge')+
  geom_label(stat = 'count', aes(label = ..count..))+ labs(ggtitle("Count of reason for absence per year")) +
  facet_grid(Year~.) +
  theme_grey()
g14




#Aggregating the absentense hours based on ID
aggre.absent.ID =  ddply(data, c( "ID"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))
#Aggregating the absendtense hours based on Reason of Absence and ID 
aggre.absent.id.reasons=  ddply(data, c("ID", "Reason.for.absence"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))



#Aggregating based on sons, smoker, drinker
aggre.absent.son=  ddply(data, c("Son"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))

aggre.absent.drinker=  ddply(data, c("Social.drinker"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))

aggre.absent.smoker =  ddply(data, c("Social.smoker"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))

#Modelling

#Aggregating the data based on Absenteeism time in hour per month for an year
aggre.absent.hours.months =  ddply(data, c("Year", "Month.of.absence"), function(x) colSums(x[c("Absenteeism.time.in.hours")]))


tsdata = ts(aggre.absent.hours.months$Absenteeism.time.in.hours, frequency = 12,start = c(2007, 7))
autoplot(tsdata)
write.csv(aggre.absent.hours.months, "Absentese_per_month11.csv", row.names = T)

#Diving into train and test; 80:20 split
train = window(tsdata, start = c(2007,7), end = c(2009,12))
test = window(tsdata, start = c(2010))

#Decomposition
plot(decompose(tsdata))


#Check for stationarity
#There is non - stationarity in the dataset
adf.test(tsdata, k = 1)#Stationarity in the dataset  

#Test for autocorrelation
dwtest(tsdata[-37] ~ tsdata[-1])
set.seed(123)

#***********************Applying time series linear model with trend*********************

#Training the model
fit =  tslm(train~ season + trend)

#Summary of the fit
summary(fit)

#AIC score of tslm
AIC(fit)

#Residuals
checkresiduals(fit)

#Lets forcast for next 18 months
forecast_tslm = forecast(fit, h = 24)
autoplot(forecast_tslm)

#Lets check the accuracy
accuracy(forecast_tslm, test)


#**************************Applyting auto.arima****************************

#Checkingthe ACF and PACF PLOT
tsdisplay(train)

#Training the model
arima_fit = auto.arima(train, trace = T)

#Forecasting
arimafore = forecast(arima_fit, h =24)
autoplot(arimafore)

#Accuracy
accuracy(arimafore, test)

#Residuals
checkresiduals(arima_fit)
