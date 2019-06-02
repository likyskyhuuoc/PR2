library(RcmdrMisc)
library(stringr)
library(Hmisc)
library(corrplot)
library(MVN)
library(gvlma)
library(stargazer)
library(easyGgplot2)
library(psych)
library(car)
library(caret)
library(mgcv)
library(lmtest)
library(rpart)
library(rpart.plot)



#Load the datasets Casa Palacio-Puerto del Rosario2011.
Fuerteventura_2011_1 <- read.csv("Casa Palacio-Puerto del Rosario20111.csv",encoding="UTF-8")
Fuerteventura_2011_2 <- read.csv("Casa Palacio-Puerto del Rosario20112.csv",encoding="UTF-8")
Fuerteventura_2011_3 <- read.csv("Casa Palacio-Puerto del Rosario20113.csv",encoding="UTF-8")
Fuerteventura_2011_4 <- read.csv("Casa Palacio-Puerto del Rosario20114.csv",encoding="UTF-8")
#Combine the datasets.
Fuerteventura_2011 <- cbind(cbind(Fuerteventura_2011_1,Fuerteventura_2011_2),cbind(Fuerteventura_2011_3,Fuerteventura_2011_4))
#Create a new variable to assign the location of weather station.
Fuerteventura_2011$YEAR <- "2011"

#Load the datasets Casa Palacio-Puerto del Rosario2012.
Fuerteventura_2012_1 <- read.csv("Casa Palacio-Puerto del Rosario20121.csv",encoding="UTF-8")
Fuerteventura_2012_2 <- read.csv("Casa Palacio-Puerto del Rosario20122.csv",encoding="UTF-8")
Fuerteventura_2012_3 <- read.csv("Casa Palacio-Puerto del Rosario20123.csv",encoding="UTF-8")
Fuerteventura_2012_4 <- read.csv("Casa Palacio-Puerto del Rosario20124.csv",encoding="UTF-8")
#Combine the datasets.
Fuerteventura_2012 <- cbind(cbind(Fuerteventura_2012_1,Fuerteventura_2012_2),cbind(Fuerteventura_2012_3,Fuerteventura_2012_4))
#Create a new variable to assign the location of weather station.
Fuerteventura_2012$YEAR <- "2012"


#Load the datasets Casa Palacio-Puerto del Rosario2013.
Fuerteventura_2013_1 <- read.csv("Casa Palacio-Puerto del Rosario20131.csv",encoding="UTF-8")
Fuerteventura_2013_2 <- read.csv("Casa Palacio-Puerto del Rosario20132.csv",encoding="UTF-8")
Fuerteventura_2013_3 <- read.csv("Casa Palacio-Puerto del Rosario20133.csv",encoding="UTF-8")
Fuerteventura_2013_4 <- read.csv("Casa Palacio-Puerto del Rosario20134.csv",encoding="UTF-8")
#Combine the datasets.
Fuerteventura_2013 <- cbind(cbind(Fuerteventura_2013_1,Fuerteventura_2013_2),cbind(Fuerteventura_2013_3,Fuerteventura_2013_4))
#Create a new variable to assign the location of weather station.
Fuerteventura_2013$YEAR <- "2013"


#Load the datasets Casa Palacio-Puerto del Rosario2014.
Fuerteventura_2014_1 <- read.csv("Casa Palacio-Puerto del Rosario20141.csv",encoding="UTF-8")
Fuerteventura_2014_2 <- read.csv("Casa Palacio-Puerto del Rosario20142.csv",encoding="UTF-8")
Fuerteventura_2014_3 <- read.csv("Casa Palacio-Puerto del Rosario20143.csv",encoding="UTF-8")
Fuerteventura_2014_4 <- read.csv("Casa Palacio-Puerto del Rosario20144.csv",encoding="UTF-8")
#Combine the datasets.
Fuerteventura_2014 <- cbind(cbind(Fuerteventura_2014_1,Fuerteventura_2014_2),cbind(Fuerteventura_2014_3,Fuerteventura_2014_4))
#Create a new variable to assign the location of weather station.
Fuerteventura_2014$YEAR <- "2014"


#Load the datasets Casa Palacio-Puerto del Rosario2015.
Fuerteventura_2015_1 <- read.csv("Casa Palacio-Puerto del Rosario20151.csv",encoding="UTF-8")
Fuerteventura_2015_2 <- read.csv("Casa Palacio-Puerto del Rosario20152.csv",encoding="UTF-8")
Fuerteventura_2015_3 <- read.csv("Casa Palacio-Puerto del Rosario20153.csv",encoding="UTF-8")
Fuerteventura_2015_4 <- read.csv("Casa Palacio-Puerto del Rosario20154.csv",encoding="UTF-8")
#Combine the datasets.
Fuerteventura_2015 <- cbind(cbind(Fuerteventura_2015_1,Fuerteventura_2015_2),cbind(Fuerteventura_2015_3,Fuerteventura_2015_4))
#Create a new variable to assign the location of weather station.
Fuerteventura_2015$YEAR <- "2015"


#Load the datasets Casa Palacio-Puerto del Rosario2016.
Fuerteventura_2016_1 <- read.csv("Casa Palacio-Puerto del Rosario20161.csv",encoding="UTF-8")
Fuerteventura_2016_2 <- read.csv("Casa Palacio-Puerto del Rosario20162.csv",encoding="UTF-8")
Fuerteventura_2016_3 <- read.csv("Casa Palacio-Puerto del Rosario20163.csv",encoding="UTF-8")
Fuerteventura_2016_4 <- read.csv("Casa Palacio-Puerto del Rosario20164.csv",encoding="UTF-8")
#Combine the datasets.
Fuerteventura_2016 <- cbind(cbind(Fuerteventura_2016_1,Fuerteventura_2016_2),cbind(Fuerteventura_2016_3,Fuerteventura_2016_4))
#Create a new variable to assign the location of weather station.
Fuerteventura_2016$YEAR <- "2016"


#Load the datasets Casa Palacio-Puerto del Rosario2017.
Fuerteventura_2017_1 <- read.csv("Casa Palacio-Puerto del Rosario20171.csv",encoding="UTF-8")
Fuerteventura_2017_2 <- read.csv("Casa Palacio-Puerto del Rosario20172.csv",encoding="UTF-8")
Fuerteventura_2017_3 <- read.csv("Casa Palacio-Puerto del Rosario20173.csv",encoding="UTF-8")
Fuerteventura_2017_4 <- read.csv("Casa Palacio-Puerto del Rosario20174.csv",encoding="UTF-8")
#Combine the datasets.
Fuerteventura_2017 <- cbind(cbind(Fuerteventura_2017_1,Fuerteventura_2017_2),cbind(Fuerteventura_2017_3,Fuerteventura_2017_4))
#Create a new variable to assign the location of weather station.
Fuerteventura_2017$YEAR <- "2017"


#Load the datasets Casa Palacio-Puerto del Rosario2018.
Fuerteventura_2018_1 <- read.csv("Casa Palacio-Puerto del Rosario20181.csv",encoding="UTF-8")
Fuerteventura_2018_2 <- read.csv("Casa Palacio-Puerto del Rosario20182.csv",encoding="UTF-8")
Fuerteventura_2018_3 <- read.csv("Casa Palacio-Puerto del Rosario20183.csv",encoding="UTF-8")
Fuerteventura_2018_4 <- read.csv("Casa Palacio-Puerto del Rosario20184.csv",encoding="UTF-8")
#Combine the datasets.
Fuerteventura_2018 <- cbind(cbind(Fuerteventura_2018_1,Fuerteventura_2018_2),cbind(Fuerteventura_2018_3,Fuerteventura_2018_4))
#Create a new variable to assign the location of weather station.
Fuerteventura_2018$YEAR <- "2018"



clean_data <- function(dataset){

  
  #Rename some variables
  dataset$DATE <- dataset$Fecha
  dataset$SO2 <- dataset[ , which(str_detect(names(dataset), pattern = "SO2"))]
  dataset$NO <- dataset[ , which(str_detect(names(dataset), pattern = "NO..μg.m3."))]
  dataset$NO2 <- dataset[ , which(str_detect(names(dataset), pattern = "NO2..μg.m3."))]
  dataset$NOX <- dataset[ , which(str_detect(names(dataset), pattern = "NOX..μg.m3."))]
  dataset$PM10 <- dataset[ , which(str_detect(names(dataset), pattern = "PM10"))]
  dataset$CO <- dataset[ , which(str_detect(names(dataset), pattern = "CO"))]
  dataset$PM2.5 <- dataset[ , which(str_detect(names(dataset), pattern = "PM2.5"))]
  dataset$O3 <- dataset[ , which(str_detect(names(dataset), pattern = "O3"))]
  dataset$VV <- dataset[ , which(str_detect(names(dataset), pattern = "VV"))]
  dataset$DD <- dataset[ , which(str_detect(names(dataset), pattern = "DD"))]
  dataset$TMP <- dataset[ , which(str_detect(names(dataset), pattern = "TMP"))]
  dataset$HR <- dataset[ , which(str_detect(names(dataset), pattern = "HR"))]
  dataset$PRB <- dataset[ , which(str_detect(names(dataset), pattern = "PRB"))]
  
  #Rename some columns
  dataset$DATE <- dataset$Fecha
  dataset$HOUR <- dataset$Hora
  
  #Include the necessary variables into the dataset.
  dataset <-  dataset[,c("DATE","HOUR","YEAR","SO2","NO","NO2","NOX","O3","CO","PM10","PM2.5","VV","DD","TMP","HR","PRB")]
  

  
  return(dataset)
}

#Conduct the data cleaning process for each of the datasets.
Fuerteventura_2011 <- clean_data(Fuerteventura_2011)
Fuerteventura_2012 <- clean_data(Fuerteventura_2012)
Fuerteventura_2013 <- clean_data(Fuerteventura_2013)
Fuerteventura_2014 <- clean_data(Fuerteventura_2014)
Fuerteventura_2015 <- clean_data(Fuerteventura_2015)
Fuerteventura_2016 <- clean_data(Fuerteventura_2016)
Fuerteventura_2017 <- clean_data(Fuerteventura_2017)
Fuerteventura_2018 <- clean_data(Fuerteventura_2018)


#Bind the observations from different datasets in one unique dataset.
provisional1 <- rbind(Fuerteventura_2011,Fuerteventura_2012)
provisional2 <- rbind(Fuerteventura_2013,Fuerteventura_2014)
provisional3 <- rbind(Fuerteventura_2015,Fuerteventura_2016)
provisional4 <- rbind(Fuerteventura_2017,Fuerteventura_2018)
provisional5 <- rbind(provisional1,provisional2)
provisional6 <- rbind(provisional3,provisional4)
Canarydataset <-rbind(provisional5,provisional6)

#Configure levels for the variable "YEAR".
Canarydataset$YEAR <- factor(Canarydataset$YEAR,levels = c("2011","2012","2013","2014","2015","2016","2017","2018"))

#Create a new variable "MONTH".
Canarydataset$MONTH <- ifelse(str_detect(Canarydataset$DATE, pattern = "-01-"),"January",
                              ifelse(str_detect(Canarydataset$DATE, pattern = "-02-"),"February",
                                     ifelse(str_detect(Canarydataset$DATE, pattern = "-03-"),"March",
                                            ifelse(str_detect(Canarydataset$DATE, pattern = "-04-"),"April",
                                                   ifelse(str_detect(Canarydataset$DATE, pattern = "-05-"),"May",
                                                          ifelse(str_detect(Canarydataset$DATE, pattern = "-06-"),"June",
                                                                 ifelse(str_detect(Canarydataset$DATE, pattern = "-07-"),"July",
                                                                        ifelse(str_detect(Canarydataset$DATE, pattern = "-08-"),"August",
                                                                               ifelse(str_detect(Canarydataset$DATE, pattern = "-09-"),"September",
                                                                                      ifelse(str_detect(Canarydataset$DATE, pattern = "-10-"),"October",
                                                                                             ifelse(str_detect(Canarydataset$DATE, pattern = "-11-"),"November",
                                                                                                           "December")))))))))))

#Configure levels for the variable "MONTH".
Canarydataset$MONTH <- factor(Canarydataset$MONTH,levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))

#Detect outliers and replace them by NA.
outlier_SO2 <- which(Canarydataset$SO2 %in% boxplot.stats(Canarydataset$SO2)$out)
Canarydataset[outlier_SO2,"SO2"] <- NA

outlier_NO <- which(Canarydataset$NO %in% boxplot.stats(Canarydataset$NO)$out)
Canarydataset[outlier_NO,"NO"] <- NA

outlier_NO2 <- which(Canarydataset$NO2 %in% boxplot.stats(Canarydataset$NO2)$out)
Canarydataset[outlier_NO2,"NO2"] <- NA

outlier_PM10 <- which(Canarydataset$PM10 %in% boxplot.stats(Canarydataset$PM10)$out)
Canarydataset[outlier_PM10,"PM10"] <- NA

outlier_NOX <- which(Canarydataset$NOX %in% boxplot.stats(Canarydataset$NOX)$out)
Canarydataset[outlier_NOX,"NOX"] <- NA

outlier_CO <- which(Canarydataset$CO %in% boxplot.stats(Canarydataset$CO)$out)
Canarydataset[outlier_CO,"CO"] <- NA

outlier_PM2.5 <- which(Canarydataset$PM2.5 %in% boxplot.stats(Canarydataset$PM2.5)$out)
Canarydataset[outlier_PM2.5,"PM2.5"] <- NA

outlier_O3 <- which(Canarydataset$O3 %in% boxplot.stats(Canarydataset$O3)$out)
Canarydataset[outlier_O3,"O3"] <- NA

outlier_VV <- which(Canarydataset$VV %in% boxplot.stats(Canarydataset$VV)$out)
Canarydataset[outlier_VV,"VV"] <- NA

outlier_DD <- which(Canarydataset$DD %in% boxplot.stats(Canarydataset$DD)$out)
Canarydataset[outlier_DD,"DD"] <- NA

outlier_TMP <- which(Canarydataset$TMP %in% boxplot.stats(Canarydataset$TMP)$out)
Canarydataset[outlier_TMP,"TMP"] <- NA

outlier_HR <- which(Canarydataset$HR %in% boxplot.stats(Canarydataset$HR)$out)
Canarydataset[outlier_HR,"HR"] <- NA

outlier_PRB <- which(Canarydataset$PRB %in% boxplot.stats(Canarydataset$PRB)$out)
Canarydataset[outlier_PRB,"PRB"] <- NA

#Replace the missing values with the mean values in each of the numerical variables.
Canarydataset$SO2[is.na(Canarydataset$SO2)] <- mean(Canarydataset$SO2,na.rm=T)
Canarydataset$NO[is.na(Canarydataset$NO)] <- mean(Canarydataset$NO,na.rm=T)
Canarydataset$NO2[is.na(Canarydataset$NO2)] <- mean(Canarydataset$NO2,na.rm=T)
Canarydataset$PM10[is.na(Canarydataset$PM10)] <- mean(Canarydataset$PM10,na.rm=T)
Canarydataset$NOX[is.na(Canarydataset$NOX)] <- mean(Canarydataset$NOX,na.rm=T)
Canarydataset$CO[is.na(Canarydataset$CO)] <- mean(Canarydataset$CO,na.rm=T)
Canarydataset$PM2.5[is.na(Canarydataset$PM2.5)] <- mean(Canarydataset$PM2.5,na.rm=T)
Canarydataset$O3[is.na(Canarydataset$O3)] <- mean(Canarydataset$O3,na.rm=T)
Canarydataset$VV[is.na(Canarydataset$VV)] <- mean(Canarydataset$VV,na.rm=T)
Canarydataset$DD[is.na(Canarydataset$DD)] <- mean(Canarydataset$DD,na.rm=T)
Canarydataset$TMP[is.na(Canarydataset$TMP)] <- mean(Canarydataset$TMP,na.rm=T)
Canarydataset$HR[is.na(Canarydataset$HR)] <- mean(Canarydataset$HR,na.rm=T)
Canarydataset$PRB[is.na(Canarydataset$PRB)] <- mean(Canarydataset$PRB,na.rm=T)


#Output the cleaned dataset.
#write.csv(Canarydataset,file = "C:\\Users\\Xiaowei\\Desktop\\UOC学习\\Tipología y ciclo de vida de los datos\\Pr2\\20190528\\R code\\Canarydataset.csv",row.names = FALSE,fileEncoding = "UTF-8")



#Concentration of SO2 - μg / m³
summary(Canarydataset$SO2)
ggplot(Canarydataset, aes(x=SO2))+geom_histogram(binwidth=1,fill="#EE8420")


#Concentration of NO - μg / m³
summary(Canarydataset$NO)
ggplot(Canarydataset, aes(x=NO))+geom_histogram(binwidth=1,fill="#EE8420")


#Concentration of NO2 - μg / m³
summary(Canarydataset$NO2)
ggplot(Canarydataset, aes(x=NO2))+geom_histogram(binwidth=1,fill="#EE8420")


#Concentration of NOX - μg / m³
summary(Canarydataset$NOX)
ggplot(Canarydataset, aes(x=NOX))+geom_histogram(binwidth=1,fill="#EE8420")


#Concentration of CO - mg / m³
summary(Canarydataset$CO)
ggplot(Canarydataset, aes(x=CO))+geom_histogram(binwidth=0.1,fill="#EE8420")


#Concentration of O3 - μg / m³
summary(Canarydataset$O3)
ggplot(Canarydataset, aes(x=O3))+geom_histogram(binwidth=1,fill="#EE8420")


#Particulate matter (PM10) - μg / m³
summary(Canarydataset$PM10)
ggplot(Canarydataset, aes(x=PM10))+geom_histogram(binwidth=1,fill="#EE8420")


#Particulate matter (PM2.5) - μg / m³
summary(Canarydataset$PM2.5)
ggplot(Canarydataset, aes(x=PM2.5))+geom_histogram(binwidth=1,fill="#EE8420")


#Wind speed - m / s
summary(Canarydataset$VV)
ggplot(Canarydataset, aes(x=VV))+geom_histogram(binwidth=1,fill="#26C4BB")


#Wind direction - Grd
summary(Canarydataset$DD)
ggplot(Canarydataset, aes(x=DD))+geom_histogram(binwidth=20,fill="#26C4BB")


#Average temperature - ºC
summary(Canarydataset$TMP)
ggplot(Canarydataset, aes(x=TMP))+geom_histogram(binwidth=1,fill="#26C4BB")


#Relative humidity -%
summary(Canarydataset$HR)
ggplot(Canarydataset, aes(x=HR))+geom_histogram(binwidth=1,fill="#26C4BB")


#Barometric pressure - mb
summary(Canarydataset$PRB)
ggplot(Canarydataset, aes(x=PRB))+geom_histogram(binwidth=1,fill="#26C4BB")


#Merge in one plot
SO2_plot <- ggplot(Canarydataset, aes(x=SO2))+geom_histogram(binwidth=1,fill="#EE8420")
NO_plot <- ggplot(Canarydataset, aes(x=NO))+geom_histogram(binwidth=1,fill="#EE8420")
NO2_plot <-ggplot(Canarydataset, aes(x=NO2))+geom_histogram(binwidth=1,fill="#EE8420")
NOX_plot <-ggplot(Canarydataset, aes(x=NOX))+geom_histogram(binwidth=1,fill="#EE8420")
CO_plot <-ggplot(Canarydataset, aes(x=CO))+geom_histogram(binwidth=0.1,fill="#EE8420")
O3_plot <-ggplot(Canarydataset, aes(x=O3))+geom_histogram(binwidth=1,fill="#EE8420")
PM10_plot <-ggplot(Canarydataset, aes(x=PM10))+geom_histogram(binwidth=1,fill="#EE8420")
PM2.5_plot <-ggplot(Canarydataset, aes(x=PM2.5))+geom_histogram(binwidth=1,fill="#EE8420")
VV_plot <-ggplot(Canarydataset, aes(x=VV))+geom_histogram(binwidth=1,fill="#26C4BB")
DD_plot <- ggplot(Canarydataset, aes(x=DD))+geom_histogram(binwidth=20,fill="#26C4BB")
TMP_plot <- ggplot(Canarydataset, aes(x=TMP))+geom_histogram(binwidth=1,fill="#26C4BB")
HR_plot <- ggplot(Canarydataset, aes(x=HR))+geom_histogram(binwidth=1,fill="#26C4BB")
PRB_plot <- ggplot(Canarydataset, aes(x=PRB))+geom_histogram(binwidth=1,fill="#26C4BB")
ggplot2.multiplot(SO2_plot,NO_plot,NO2_plot,NOX_plot,CO_plot,O3_plot,PM10_plot,PM2.5_plot,VV_plot,DD_plot,TMP_plot,HR_plot,PRB_plot, cols=3)

#Descriptive statistics
numSummary(Canarydataset[,c("SO2","NO","NO2","NOX","O3","CO","PM10","PM2.5","VV","DD","TMP","HR","PRB"
), drop=FALSE], statistics=c("mean", "sd", "quantiles", "skewness", "kurtosis"), 
quantiles=c(0,.25,.5,.75,1), type="2")

#Correlation plot
numerical_items <- Canarydataset[,c("SO2","NO","NO2","NOX","O3","CO","PM10","PM2.5","VV","DD","TMP","HR","PRB")]
correlation_numerical_items <- cor(numerical_items)
corrplot.mixed(correlation_numerical_items, order = "original")

#Boxplots for each year
year_plot1 <- ggplot(Canarydataset, aes(x=YEAR, y=SO2)) + geom_boxplot()+geom_smooth()
year_plot2 <- ggplot(Canarydataset, aes(x=YEAR, y=NO)) + geom_boxplot()+geom_smooth()
year_plot3 <- ggplot(Canarydataset, aes(x=YEAR, y=NO2)) + geom_boxplot()+geom_smooth()
year_plot4 <- ggplot(Canarydataset, aes(x=YEAR, y=NOX)) + geom_boxplot()+geom_smooth()
year_plot5 <- ggplot(Canarydataset, aes(x=YEAR, y=O3)) + geom_boxplot()+geom_smooth()
year_plot6 <- ggplot(Canarydataset, aes(x=YEAR, y=CO)) + geom_boxplot()+geom_smooth()
year_plot7 <- ggplot(Canarydataset, aes(x=YEAR, y=PM10)) + geom_boxplot()+geom_smooth()
year_plot8 <- ggplot(Canarydataset, aes(x=YEAR, y=PM2.5)) + geom_boxplot()+geom_smooth()
year_plot9 <- ggplot(Canarydataset, aes(x=YEAR, y=VV)) + geom_boxplot()+geom_smooth()
year_plot10 <- ggplot(Canarydataset, aes(x=YEAR, y=DD)) + geom_boxplot()+geom_smooth()
year_plot11 <- ggplot(Canarydataset, aes(x=YEAR, y=TMP)) + geom_boxplot()+geom_smooth()
year_plot12 <- ggplot(Canarydataset, aes(x=YEAR, y=HR)) + geom_boxplot()+geom_smooth()
year_plot13 <- ggplot(Canarydataset, aes(x=YEAR, y=PRB)) + geom_boxplot()+geom_smooth()
ggplot2.multiplot(year_plot1,year_plot2,year_plot3,year_plot4,year_plot5,year_plot6,year_plot7,year_plot8,year_plot9,year_plot10,year_plot11,year_plot12,year_plot13, cols=3)

#Boxplots for each month
month_plot1 <- ggplot(Canarydataset, aes(x=MONTH, y=SO2)) + geom_boxplot()+geom_smooth()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
month_plot2 <-ggplot(Canarydataset, aes(x=MONTH, y=NO)) + geom_boxplot()+geom_smooth()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
month_plot3 <-ggplot(Canarydataset, aes(x=MONTH, y=NO2)) + geom_boxplot()+geom_smooth()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
month_plot4 <-ggplot(Canarydataset, aes(x=MONTH, y=NOX)) + geom_boxplot()+geom_smooth()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
month_plot5 <-ggplot(Canarydataset, aes(x=MONTH, y=O3)) + geom_boxplot()+geom_smooth()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
month_plot6 <-ggplot(Canarydataset, aes(x=MONTH, y=CO)) + geom_boxplot()+geom_smooth()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
month_plot7 <-ggplot(Canarydataset, aes(x=MONTH, y=PM10)) + geom_boxplot()+geom_smooth()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
month_plot8 <-ggplot(Canarydataset, aes(x=MONTH, y=PM2.5)) + geom_boxplot()+geom_smooth()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
month_plot9 <-ggplot(Canarydataset, aes(x=MONTH, y=VV)) + geom_boxplot()+geom_smooth()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
month_plot10 <-ggplot(Canarydataset, aes(x=MONTH, y=DD)) + geom_boxplot()+geom_smooth()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
month_plot11 <-ggplot(Canarydataset, aes(x=MONTH, y=TMP)) + geom_boxplot()+geom_smooth()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
month_plot12 <-ggplot(Canarydataset, aes(x=MONTH, y=HR)) + geom_boxplot()+geom_smooth()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
month_plot13 <-ggplot(Canarydataset, aes(x=MONTH, y=PRB)) + geom_boxplot()+geom_smooth()+theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggplot2.multiplot(month_plot1,month_plot2,month_plot3,month_plot4,month_plot5,month_plot6,month_plot7,month_plot8,month_plot9,month_plot10,month_plot11,month_plot12,month_plot13, cols=3)

#Scatter plot(VV and pollution indicators)
VV_plot1 <- ggplot(Canarydataset, aes(x=VV, y=SO2)) + geom_point()+geom_smooth()
VV_plot2 <- ggplot(Canarydataset, aes(x=VV, y=NO)) + geom_point()+geom_smooth()
VV_plot3 <- ggplot(Canarydataset, aes(x=VV, y=NO2)) + geom_point()+geom_smooth()
VV_plot4 <- ggplot(Canarydataset, aes(x=VV, y=NOX)) + geom_point()+geom_smooth()
VV_plot5 <- ggplot(Canarydataset, aes(x=VV, y=O3)) + geom_point()+geom_smooth()
VV_plot6 <- ggplot(Canarydataset, aes(x=VV, y=CO)) + geom_point()+geom_smooth()
VV_plot7 <- ggplot(Canarydataset, aes(x=VV, y=PM10)) + geom_point()+geom_smooth()
VV_plot8 <- ggplot(Canarydataset, aes(x=VV, y=PM2.5)) + geom_point()+geom_smooth()
ggplot2.multiplot(VV_plot1,VV_plot2,VV_plot3,VV_plot4,VV_plot5,VV_plot6,VV_plot7,VV_plot8, cols=4)


#Scatter plot(DD and pollution indicators)
DD_plot1 <- ggplot(Canarydataset, aes(x=DD, y=SO2)) + geom_point()+geom_smooth()
DD_plot1 <- ggplot(Canarydataset, aes(x=DD, y=NO)) + geom_point()+geom_smooth()
DD_plot1 <- ggplot(Canarydataset, aes(x=DD, y=NO2)) + geom_point()+geom_smooth()
DD_plot1 <- ggplot(Canarydataset, aes(x=DD, y=NOX)) + geom_point()+geom_smooth()
DD_plot1 <- ggplot(Canarydataset, aes(x=DD, y=O3)) + geom_point()+geom_smooth()
DD_plot1 <- ggplot(Canarydataset, aes(x=DD, y=CO)) + geom_point()+geom_smooth()
DD_plot1 <- ggplot(Canarydataset, aes(x=DD, y=PM10)) + geom_point()+geom_smooth()
DD_plot1 <- ggplot(Canarydataset, aes(x=DD, y=PM2.5)) + geom_point()+geom_smooth()
ggplot2.multiplot(DD_plot1,DD_plot2,DD_plot3,DD_plot4,DD_plot5,DD_plot6,DD_plot7,DD_plot8, cols=4)
  
#Scatter plot(TMP and pollution indicators)
TMP_plot1 <- ggplot(Canarydataset, aes(x=TMP, y=SO2)) + geom_point()+geom_smooth()
TMP_plot2 <- ggplot(Canarydataset, aes(x=TMP, y=NO)) + geom_point()+geom_smooth()
TMP_plot3 <- ggplot(Canarydataset, aes(x=TMP, y=NO2)) + geom_point()+geom_smooth()
TMP_plot4 <- ggplot(Canarydataset, aes(x=TMP, y=NOX)) + geom_point()+geom_smooth()
TMP_plot5 <- ggplot(Canarydataset, aes(x=TMP, y=O3)) + geom_point()+geom_smooth()
TMP_plot6 <- ggplot(Canarydataset, aes(x=TMP, y=CO)) + geom_point()+geom_smooth()
TMP_plot7 <- ggplot(Canarydataset, aes(x=TMP, y=PM10)) + geom_point()+geom_smooth()
TMP_plot8 <- ggplot(Canarydataset, aes(x=TMP, y=PM2.5)) + geom_point()+geom_smooth()
ggplot2.multiplot(TMP_plot1,TMP_plot2,TMP_plot3,TMP_plot4,TMP_plot5,TMP_plot6,TMP_plot7,TMP_plot8, cols=4)

#Scatter plot(HR and pollution indicators)
HR_plot1 <- ggplot(Canarydataset, aes(x=HR, y=SO2)) + geom_point()+geom_smooth()
HR_plot2 <- ggplot(Canarydataset, aes(x=HR, y=NO)) + geom_point()+geom_smooth()
HR_plot3 <- ggplot(Canarydataset, aes(x=HR, y=NO2)) + geom_point()+geom_smooth()
HR_plot4 <- ggplot(Canarydataset, aes(x=HR, y=NOX)) + geom_point()+geom_smooth()
HR_plot5 <- ggplot(Canarydataset, aes(x=HR, y=O3)) + geom_point()+geom_smooth()
HR_plot6 <- ggplot(Canarydataset, aes(x=HR, y=CO)) + geom_point()+geom_smooth()
HR_plot7 <- ggplot(Canarydataset, aes(x=HR, y=PM10)) + geom_point()+geom_smooth()
HR_plot8 <- ggplot(Canarydataset, aes(x=HR, y=PM2.5)) + geom_point()+geom_smooth()
ggplot2.multiplot(HR_plot1,HR_plot2,HR_plot3,HR_plot4,HR_plot5,HR_plot6,HR_plot7,HR_plot8, cols=4)

#Scatter plot(PRB and pollution indicators)
PRB_plot1 <- ggplot(Canarydataset, aes(x=PRB, y=SO2)) + geom_point()+geom_smooth()
PRB_plot2 <- ggplot(Canarydataset, aes(x=PRB, y=NO)) + geom_point()+geom_smooth()
PRB_plot3 <- ggplot(Canarydataset, aes(x=PRB, y=NO2)) + geom_point()+geom_smooth()
PRB_plot4 <- ggplot(Canarydataset, aes(x=PRB, y=NOX)) + geom_point()+geom_smooth()
PRB_plot5 <- ggplot(Canarydataset, aes(x=PRB, y=O3)) + geom_point()+geom_smooth()
PRB_plot6 <- ggplot(Canarydataset, aes(x=PRB, y=CO)) + geom_point()+geom_smooth()
PRB_plot7 <- ggplot(Canarydataset, aes(x=PRB, y=PM10)) + geom_point()+geom_smooth()
PRB_plot8 <- ggplot(Canarydataset, aes(x=PRB, y=PM2.5)) + geom_point()+geom_smooth()
ggplot2.multiplot(PRB_plot1,PRB_plot2,PRB_plot3,PRB_plot4,PRB_plot5,PRB_plot6,PRB_plot7,PRB_plot8, cols=4)

#Normality test
Canarydataset_NUME <- Canarydataset[,4:16]
MVN::mvn(Canarydataset_NUME)


#Homogeneity test
leveneTest(Canarydataset$SO2,Canarydataset$YEAR)
leveneTest(Canarydataset$NO,Canarydataset$YEAR)
leveneTest(Canarydataset$NO2,Canarydataset$YEAR)
leveneTest(Canarydataset$NOX,Canarydataset$YEAR)
leveneTest(Canarydataset$O3,Canarydataset$YEAR)
leveneTest(Canarydataset$CO,Canarydataset$YEAR)
leveneTest(Canarydataset$PM10,Canarydataset$YEAR)
leveneTest(Canarydataset$PM2.5,Canarydataset$YEAR)
leveneTest(Canarydataset$VV,Canarydataset$YEAR)
leveneTest(Canarydataset$DD,Canarydataset$YEAR)
leveneTest(Canarydataset$TMP,Canarydataset$YEAR)
leveneTest(Canarydataset$HR,Canarydataset$YEAR)
leveneTest(Canarydataset$PRB,Canarydataset$YEAR)

leveneTest(Canarydataset$SO2,Canarydataset$MONTH)
leveneTest(Canarydataset$NO,Canarydataset$MONTH)
leveneTest(Canarydataset$NO2,Canarydataset$MONTH)
leveneTest(Canarydataset$NOX,Canarydataset$MONTH)
leveneTest(Canarydataset$O3,Canarydataset$MONTH)
leveneTest(Canarydataset$CO,Canarydataset$MONTH)
leveneTest(Canarydataset$PM10,Canarydataset$MONTH)
leveneTest(Canarydataset$PM2.5,Canarydataset$MONTH)
leveneTest(Canarydataset$VV,Canarydataset$MONTH)
leveneTest(Canarydataset$DD,Canarydataset$MONTH)
leveneTest(Canarydataset$TMP,Canarydataset$MONTH)
leveneTest(Canarydataset$HR,Canarydataset$MONTH)
leveneTest(Canarydataset$PRB,Canarydataset$MONTH)

#Kruskal-Wallis test
kruskal.test(SO2 ~ YEAR,data= Canarydataset)
kruskal.test(NO ~ YEAR,data= Canarydataset)
kruskal.test(NO2 ~ YEAR,data= Canarydataset)
kruskal.test(NOX ~ YEAR,data= Canarydataset)
kruskal.test(CO ~ YEAR,data= Canarydataset)
kruskal.test(O3 ~ YEAR,data= Canarydataset)
kruskal.test(PM10 ~ YEAR,data= Canarydataset)
kruskal.test(PM2.5 ~ YEAR,data= Canarydataset)
kruskal.test(VV ~ YEAR,data= Canarydataset)
kruskal.test(DD ~ YEAR,data= Canarydataset)
kruskal.test(TMP ~ YEAR,data= Canarydataset)
kruskal.test(HR ~ YEAR,data= Canarydataset)
kruskal.test(PRB ~ YEAR,data= Canarydataset)

kruskal.test(SO2 ~ MONTH,data= Canarydataset)
kruskal.test(NO ~ MONTH,data= Canarydataset)
kruskal.test(NO2 ~ MONTH,data= Canarydataset)
kruskal.test(NOX ~ MONTH,data= Canarydataset)
kruskal.test(CO ~ MONTH,data= Canarydataset)
kruskal.test(O3 ~ MONTH,data= Canarydataset)
kruskal.test(PM10 ~ MONTH,data= Canarydataset)
kruskal.test(PM2.5 ~ MONTH,data= Canarydataset)
kruskal.test(VV ~ MONTH,data= Canarydataset)
kruskal.test(DD ~ MONTH,data= Canarydataset)
kruskal.test(TMP ~ MONTH,data= Canarydataset)
kruskal.test(HR ~ MONTH,data= Canarydataset)
kruskal.test(PRB ~ MONTH,data= Canarydataset)

#Establish a training set and a verification set.
set.seed(1)
sample <- sample.int(n = nrow(Canarydataset), size = floor(0.50*nrow(Canarydataset)), replace = F)
Canarydataset_training <- Canarydataset[sample, ]
Canarydataset_verification  <- Canarydataset[-sample, ]

#Lineal regression
lineal_model_SO2 <- lm(log(SO2+1) ~ NO+NO2+NOX+O3+PM2.5+VV,data=Canarydataset_training)
summary(gvlma(lineal_model_SO2))

lineal_model_NO <- lm(log(NO+1) ~ NO2+NOX+O3+HR+PRB,data=Canarydataset_training)
summary(gvlma(lineal_model_NO))

lineal_model_NO2 <- lm(log(NO2+1) ~ SO2+NO+NOX+O3+HR+PRB,data=Canarydataset_training)
summary(gvlma(lineal_model_NO2))

lineal_model_NOX <- lm(log(NOX+1) ~ SO2+NO+NO2+O3+HR+PRB,data=Canarydataset_training)
summary(gvlma(lineal_model_NOX))

lineal_model_O3 <- lm(log(O3+1) ~ SO2+NO+NO2+NOX+HR+PRB,data=Canarydataset_training)
summary(gvlma(lineal_model_O3))

lineal_model_CO <- lm(log(CO+1) ~ NO+NO2+NOX+PM2.5+DD+HR,data=Canarydataset_training)
summary(gvlma(lineal_model_CO))

lineal_model_PM10 <- lm(log(PM10+1) ~ NO+PM2.5+DD+TMP+HR+PRB,data=Canarydataset_training)
summary(gvlma(lineal_model_PM10))

lineal_model_PM2.5 <- lm(log(PM2.5+1)~ SO2+PM10+VV+DD+TMP,data=Canarydataset_training)
summary(gvlma(lineal_model_PM2.5))

#stargazer(lineal_model_SO2, lineal_model_NO, lineal_model_NO2, lineal_model_NOX, lineal_model_O3, lineal_model_CO, lineal_model_PM10, lineal_model_PM2.5,type = "html",title = "Lineal Regression Results",star.char = c("+", "*", "**", "***"), star.cutoffs = c(.1, .05, .01, .001), single.row = TRUE, digits = 3)

#GAM regression
GAM_model_SO2 <- gam(SO2 ~ s(NO)+s(NO2)+s(NOX)+s(O3)+s(PM2.5)+s(VV),data=Canarydataset_training)
summary(GAM_model_SO2)

GAM_model_NO <- gam(NO ~ s(NO2)+s(NOX)+s(O3)+s(HR)+s(PRB),data=Canarydataset_training)
summary(GAM_model_NO)

GAM_model_NO2 <- gam(NO2 ~ s(SO2)+s(NO)+s(NOX)+s(O3)+s(HR)+s(PRB),data=Canarydataset_training)
summary(GAM_model_NO2)

GAM_model_NOX <- gam(NOX ~ s(SO2)+s(NO)+s(NO2)+s(O3)+s(HR)+s(PRB),data=Canarydataset_training)
summary(GAM_model_NOX)

GAM_model_O3 <- gam(O3 ~ s(SO2)+s(NO)+s(NO2)+s(NOX)+s(HR)+s(PRB),data=Canarydataset_training)
summary(GAM_model_O3)

GAM_model_CO <- gam(CO ~ s(NO)+s(NO2)+s(NOX)+s(PM2.5)+s(DD)+s(HR),data=Canarydataset_training)
summary(GAM_model_CO)

GAM_model_PM10 <- gam(PM10 ~ s(NO)+s(PM2.5)+s(DD)+s(TMP)+s(HR)+s(PRB),data=Canarydataset_training)
summary(GAM_model_PM10)

GAM_model_PM2.5 <- gam(PM2.5 ~  s(SO2)+s(PM10)+s(VV)+s(DD)+s(TMP),data=Canarydataset_training)
summary(GAM_model_PM2.5)

stargazer(GAM_model_SO2, GAM_model_NO, GAM_model_NO2, GAM_model_NOX, GAM_model_O3, GAM_model_CO, GAM_model_PM10, GAM_model_PM2.5,type = "html",title = "GAM Results", single.row = TRUE, digits = 3,out="C:\\Users\\Xiaowei\\Desktop\\UOC学习\\Tipología y ciclo de vida de los datos\\Pr2\\20190601\\Plots\\GAMresults.html")

#Regression tree
tree_model_SO2 <- rpart(SO2 ~NO+NO2+NOX+O3+PM2.5+VV,data=Canarydataset_training)
rpart.plot(tree_model_SO2)

tree_model_NO <- rpart(NO ~NO2+NOX+O3+HR+PRB,data=Canarydataset_training)
rpart.plot(tree_model_NO)

tree_model_NO2 <- rpart(NO2 ~SO2+NO+NOX+O3+HR+PRB,data=Canarydataset_training)
rpart.plot(tree_model_NO2)

tree_model_NOX <- rpart(NOX ~SO2+NO+NO2+O3+HR+PRB,data=Canarydataset_training)
rpart.plot(tree_model_NOX)

tree_model_O3 <- rpart(O3 ~SO2+NO+NO2+NOX+HR+PRB,data=Canarydataset_training)
rpart.plot(tree_model_O3)

tree_model_CO <- rpart(CO ~NO+NO2+NOX+PM2.5+DD+HR,data=Canarydataset_training)
rpart.plot(tree_model_CO)

tree_model_PM10 <- rpart(PM10 ~NO+PM2.5+DD+TMP+HR+PRB,data=Canarydataset_training)
rpart.plot(tree_model_PM10)

tree_model_PM2.5 <- rpart(PM2.5 ~ SO2+PM10+VV+DD+TMP,data=Canarydataset_training)
rpart.plot(tree_model_PM2.5)


#Model comparasion
#SO2
#Predict values using verification set.
SO2_predict_GAM <- predict(GAM_model_SO2,newdata = Canarydataset_verification)
SO2_predict_tree <- predict(tree_model_SO2,newdata = Canarydataset_verification)
SO2_true <- Canarydataset_verification$SO2

#Using NMSE to evaluate the model performance.
nmse_SO2_GAM <- mean((SO2_predict_GAM - SO2_true)^2)/mean((mean(SO2_true)-SO2_true)^2)
nmse_SO2_GAM
nmse_SO2_tree <-  mean((SO2_predict_tree - SO2_true)^2)/mean((mean(SO2_true)-SO2_true)^2)
nmse_SO2_tree

#Visualisation of the predictive results of these two models.
SO2_data <- data.frame(SO2_predict_GAM = SO2_predict_GAM,
                       SO2_predict_tree = SO2_predict_tree,
                       SO2_true = SO2_true)
plot_SO2_GAM <- ggplot(SO2_data,aes(SO2_predict_GAM,SO2_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("GAM Model(SO2)")
plot_SO2_tree <- ggplot(SO2_data,aes(SO2_predict_tree,SO2_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("Regression tree Model(SO2)")
ggplot2.multiplot(plot_SO2_GAM, plot_SO2_tree, cols=2)

#NO
#Predict values using verification set.
NO_predict_GAM <- predict(GAM_model_NO,newdata = Canarydataset_verification)
NO_predict_tree <- predict(tree_model_NO,newdata = Canarydataset_verification)
NO_true <- Canarydataset_verification$NO

#Using NMSE to evaluate the model performance.
nmse_NO_GAM <- mean((NO_predict_GAM - NO_true)^2)/mean((mean(NO_true)-NO_true)^2)
nmse_NO_GAM
nmse_NO_tree <-  mean((NO_predict_tree - NO_true)^2)/mean((mean(NO_true)-NO_true)^2)
nmse_NO_tree

#Visualisation of the predictive results of these two models.
NO_data <- data.frame(NO_predict_GAM = NO_predict_GAM,
                       NO_predict_tree = NO_predict_tree,
                       NO_true = NO_true)
plot_NO_GAM <- ggplot(NO_data,aes(NO_predict_GAM,NO_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("GAM Model(NO)")
plot_NO_tree <- ggplot(NO_data,aes(NO_predict_tree,NO_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("Regression tree Model(NO)")
ggplot2.multiplot(plot_NO_GAM, plot_NO_tree, cols=2)

#NO2
#Predict values using verification set.
NO2_predict_GAM <- predict(GAM_model_NO2,newdata = Canarydataset_verification)
NO2_predict_tree <- predict(tree_model_NO2,newdata = Canarydataset_verification)
NO2_true <- Canarydataset_verification$NO2

#Using NMSE to evaluate the model performance.
nmse_NO2_GAM <- mean((NO2_predict_GAM - NO2_true)^2)/mean((mean(NO2_true)-NO2_true)^2)
nmse_NO2_GAM
nmse_NO2_tree <-  mean((NO2_predict_tree - NO2_true)^2)/mean((mean(NO2_true)-NO2_true)^2)
nmse_NO2_tree

#Visualisation of the predictive results of these two models.
NO2_data <- data.frame(NO2_predict_GAM = NO2_predict_GAM,
                       NO2_predict_tree = NO2_predict_tree,
                       NO2_true = NO2_true)
plot_NO2_GAM <- ggplot(NO2_data,aes(NO2_predict_GAM,NO2_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("GAM Model(NO2)")
plot_NO2_tree <- ggplot(NO2_data,aes(NO2_predict_tree,NO2_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("Regression tree Model(NO2)")
ggplot2.multiplot(plot_NO2_GAM, plot_NO2_tree, cols=2)

#NOX
#Predict values using verification set.
NOX_predict_GAM <- predict(GAM_model_NOX,newdata = Canarydataset_verification)
NOX_predict_tree <- predict(tree_model_NOX,newdata = Canarydataset_verification)
NOX_true <- Canarydataset_verification$NOX

#Using NMSE to evaluate the model performance.
nmse_NOX_GAM <- mean((NOX_predict_GAM - NOX_true)^2)/mean((mean(NOX_true)-NOX_true)^2)
nmse_NOX_GAM
nmse_NOX_tree <-  mean((NOX_predict_tree - NOX_true)^2)/mean((mean(NOX_true)-NOX_true)^2)
nmse_NOX_tree

#Visualisation of the predictive results of these two models.
NOX_data <- data.frame(NOX_predict_GAM = NOX_predict_GAM,
                       NOX_predict_tree = NOX_predict_tree,
                       NOX_true = NOX_true)
plot_NOX_GAM <- ggplot(NOX_data,aes(NOX_predict_GAM,NOX_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("GAM Model(NOX)")
plot_NOX_tree <- ggplot(NOX_data,aes(NOX_predict_tree,NOX_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("Regression tree Model(NOX)")
ggplot2.multiplot(plot_NOX_GAM, plot_NOX_tree, cols=2)

#O3
#Predict values using verification set.
O3_predict_GAM <- predict(GAM_model_O3,newdata = Canarydataset_verification)
O3_predict_tree <- predict(tree_model_O3,newdata = Canarydataset_verification)
O3_true <- Canarydataset_verification$O3

#Using NMSE to evaluate the model performance.
nmse_O3_GAM <- mean((O3_predict_GAM - O3_true)^2)/mean((mean(O3_true)-O3_true)^2)
nmse_O3_GAM
nmse_O3_tree <-  mean((O3_predict_tree - O3_true)^2)/mean((mean(O3_true)-O3_true)^2)
nmse_O3_tree

#Visualisation of the predictive results of these two models.
O3_data <- data.frame(O3_predict_GAM = O3_predict_GAM,
                       O3_predict_tree = O3_predict_tree,
                       O3_true = O3_true)
plot_O3_GAM <- ggplot(O3_data,aes(O3_predict_GAM,O3_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("GAM Model(O3)")
plot_O3_tree <- ggplot(O3_data,aes(O3_predict_tree,O3_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("Regression tree Model(O3)")
ggplot2.multiplot(plot_O3_GAM, plot_O3_tree, cols=2)

#CO
#Predict values using verification set.
CO_predict_GAM <- predict(GAM_model_CO,newdata = Canarydataset_verification)
CO_predict_tree <- predict(tree_model_CO,newdata = Canarydataset_verification)
CO_true <- Canarydataset_verification$CO

#Using NMSE to evaluate the model performance.
nmse_CO_GAM <- mean((CO_predict_GAM - CO_true)^2)/mean((mean(CO_true)-CO_true)^2)
nmse_CO_GAM
nmse_CO_tree <-  mean((CO_predict_tree - CO_true)^2)/mean((mean(CO_true)-CO_true)^2)
nmse_CO_tree

#Visualisation of the predictive results of these two models.
CO_data <- data.frame(CO_predict_GAM = CO_predict_GAM,
                       CO_predict_tree = CO_predict_tree,
                       CO_true = CO_true)
plot_CO_GAM <- ggplot(CO_data,aes(CO_predict_GAM,CO_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("GAM Model(CO)")
plot_CO_tree <- ggplot(CO_data,aes(CO_predict_tree,CO_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("Regression tree Model(CO)")
ggplot2.multiplot(plot_CO_GAM, plot_CO_tree, cols=2)

#PM10
#Predict values using verification set.
PM10_predict_GAM <- predict(GAM_model_PM10,newdata = Canarydataset_verification)
PM10_predict_tree <- predict(tree_model_PM10,newdata = Canarydataset_verification)
PM10_true <- Canarydataset_verification$PM10

#Using NMSE to evaluate the model performance.
nmse_PM10_GAM <- mean((PM10_predict_GAM - PM10_true)^2)/mean((mean(PM10_true)-PM10_true)^2)
nmse_PM10_GAM
nmse_PM10_tree <-  mean((PM10_predict_tree - PM10_true)^2)/mean((mean(PM10_true)-PM10_true)^2)
nmse_PM10_tree

#Visualisation of the predictive results of these two models.
PM10_data <- data.frame(PM10_predict_GAM = PM10_predict_GAM,
                       PM10_predict_tree = PM10_predict_tree,
                       PM10_true = PM10_true)
plot_PM10_GAM <- ggplot(PM10_data,aes(PM10_predict_GAM,PM10_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("GAM Model(PM10)")
plot_PM10_tree <- ggplot(PM10_data,aes(PM10_predict_tree,PM10_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("Regression tree Model(PM10)")
ggplot2.multiplot(plot_PM10_GAM, plot_PM10_tree, cols=2)

#PM2.5
#Predict values using verification set.
PM2.5_predict_GAM <- predict(GAM_model_PM2.5,newdata = Canarydataset_verification)
PM2.5_predict_tree <- predict(tree_model_PM2.5,newdata = Canarydataset_verification)
PM2.5_true <- Canarydataset_verification$PM2.5

#Using NMSE to evaluate the model performance.
nmse_PM2.5_GAM <- mean((PM2.5_predict_GAM - PM2.5_true)^2)/mean((mean(PM2.5_true)-PM2.5_true)^2)
nmse_PM2.5_GAM
nmse_PM2.5_tree <-  mean((PM2.5_predict_tree - PM2.5_true)^2)/mean((mean(PM2.5_true)-PM2.5_true)^2)
nmse_PM2.5_tree

#Visualisation of the predictive results of these two models.
PM2.5_data <- data.frame(PM2.5_predict_GAM = PM2.5_predict_GAM,
                       PM2.5_predict_tree = PM2.5_predict_tree,
                       PM2.5_true = PM2.5_true)
plot_PM2.5_GAM <- ggplot(PM2.5_data,aes(PM2.5_predict_GAM,PM2.5_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("GAM Model(PM2.5)")
plot_PM2.5_tree <- ggplot(PM2.5_data,aes(PM2.5_predict_tree,PM2.5_true))+geom_point()+geom_abline(slope=1, intercept =0, color="red")+ggtitle("Regression tree Model(PM2.5)")
ggplot2.multiplot(plot_PM2.5_GAM, plot_PM2.5_tree, cols=2)
