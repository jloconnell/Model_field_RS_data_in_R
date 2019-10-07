##Simple linear regression in R
##
##Code from the lecture on simulating data for linear modeling:
##TRY: play with increasing the sd of either x or e and see how that changes the scatter and model fit
##     what is the relationship between scatter/noise and R2?
set.seed(1)
n<-30; b0<-0; b1<-2
x <- rnorm(n, mean=0.1, sd=0.25)
e <- rnorm(n, mean=0, sd=0.5)
y <- b0+b1*x+e

summary(m<-lm(y~x))

plot(x,y, ylim=c(-4,4))
abline(b0,b1, lwd=2)


##Lets try it with our actual data
##Set the working directory; change path to your actual working directory
##Remember to make sure all the folder separaters are "/" not "\"
setwd("/home/jessica/UGA/Teaching/Model_field_RS_data_in_R/R_code/") 

##load the libraries we need; install these first if necessary
##to install missing packages try: install.packages("packagename", dependencies=TRUE)
library(pls) #for plsr regression
library(caret) #for generating random testing and training datasets of specified size
library(tidyverse) ##for loading tidyverse libraries
library(readxl) ##for reading excel files

##import the data and store as named objects; 
##here, the data are comma deliminated text files stored in your working directory; 
dat<-read.csv (file = "data/my_tidy_spectra.csv", header=TRUE, stringsAsFactors = F)
field<- read_excel("data/field_data.xlsx")

##look at the data
names(dat)
names(field)
head(field)
dat[1:5,1:6]

##combine the field data and the spectral data with merge
##first make a merge key based on plot and date, need to put date in a common format in both datasets first
##dat$date is already the default format so don't need to specify a format; field$date is not in the default format
dat$date<-as.Date(dat$date)
field$date<-as.Date(field$date, format="%m%d%Y")
field$key<-paste(field$plot, field$date)
##remove the columns we created the merge key out of, so they aren't duplicated in the final dataframe
field<-subset(field, select=-c(plot, date))
##create the same key in dat
dat$key<-paste(dat$plot, dat$date)
##check that key format is right
head(dat$key); head(field$key)
dat<-merge( field, dat, by="key")
##review the data
names(dat)
dat[1:5, 1:10]
dim(dat)

###variables in the dat data set are as follows:
##plot, a unique loction identifier
##date, the date the data were collected; note, if you multiple dates, you'd need to merge on plot and date, not just plot
##veg_height_form: character data, tall, medium, or short vegetation
##avg_veg_height_cm: the measured average vegetation height in the plot
##aboveground_biomass_g: aboveground biomass, weight in grams/m2
##band1..219; spectral reflectance for each band and plot

###DATA SETUP####
##create the spectral bands we need, following the band guidelines in lab 1
dat$red<-dat$X670
##in lab 1, you explored NIR as the mean of several bands; we can create that below
dat$nir<-rowMeans(dat[, c("X790", "X800","X810", "X820")])
##calculate ndvi and rename the biomass column something easier to type
dat$ndvi<-(dat$nir-dat$red)/(dat$nir+dat$red)
dat$biomass<-dat$aboveground_biomass_g

par(mar=c(4,4.5,0.5,0.5), oma=c(0,0.5,0.5,0.5),mfrow=c(1,1))
plot(dat$ndvi, dat$biomass, pch=19, cex=1.1, cex.lab=1.5, col=factor(dat$date))
legend("topright", legend=unique(dat$date), col=unique(factor(dat$date)), pch=19)

# the p =xx below determines the proprotion of data to train on, here 70%
trainIndex <- createDataPartition(dat$biomass, p = 0.7, list = FALSE, times = 1)
as.numeric(trainIndex)
train<-dat[trainIndex,c("plot","date","biomass", "red", "nir", "ndvi")]
test<-dat[-trainIndex,c("plot","date","biomass", "red", "nir", "ndvi")]
head(train);head(test)
dim(train); dim(test)

###MODEL THE DATA####
summary(m<-lm(biomass~ndvi, data=train))
##no great relationship because of the variation by date
##These are wetland plant data, why do you think the relationship with NDVI changes so much over time?

##lets check for normality of residuals, 
##an assumption of linear regression to be sure to get unbiased estimates
##Note, if the there are departures from normality a common correction 
##is to transform your data by taking either the sqrt or log of the x or y variables
##ex: summary(m<-lm(biomass~log(ndvi), data=train))
resids <- rstandard(m) 
qqnorm(resids)
qqline(resids)

##standardize the names for predicted and observed values
##this makes it easier to reuse this code 
pred<-predict(m, newdata=train)
observ<-train$biomass

##calculate RMSE and save it
rmse<- round(sqrt(sum((pred-observ)^2)/length(pred)),1)

##get R2 from the model and save it
##we're pulling in a function from the broom package; since we didn't call library(broom), 
##we can get the function by telling R which library it's in with library::function
##glance provides quick access to the summary stats for some kinds of models
mod.sum<-broom::glance(m)
mod.sum
##we just want the R2
r2<-as.numeric(round(mod.sum[1],2))

##we want the x and y axis to have the same lengths, 
##so lets find the max and min values for both x and y to use to set the axis limits
max<-max(c(pred, observ))
min<-min(c(pred, observ))

##plot the data and add a 1:1 line
##the par line below sets the inner and outer margins and how many panels to have in the plot (mfrow)
par(mar=c(4,4.5,1.5,0.5), ## how many lines outside the plot on the c(bottom, left, top, right) sides
    oma=c(0,0.5,0.5,0.5),## how many lines outside the inner margins on the c(bottom, left, top, right) sides
    mfrow=c(1,1)) ## how many rows, columns
plot(observ, pred, ylim=c(min,max), xlim=c(min,max), main="Training data")
abline(0,1)

##label the plot with the model summary stats
mtext(text= bquote(R^2 == .(r2) * "," ~~ RMSE == .(rmse)* " g"), side=3, line =-3, adj =0.05)

###Testing Data fit#####
##Now Repeat for the testing data
##we copied the training code down here, but only had to replace train with test in a few places
pred<-predict(m, newdata=test)
observ<-test$biomass

##calculate RMSE and save it
rmse<- round(sqrt(sum((pred-observ)^2)/length(pred)),1)

##get R2 from the model and save it
mod.sum<-broom::glance(m); mod.sum
r2<-as.numeric(round(mod.sum[1],2))
max<-max(c(pred, observ))
min<-min(c(pred, observ))
plot(observ, pred, ylim=c(min,max), xlim=c(min,max), main="Testing data")
abline(0,1)
mtext(text= bquote(RMSE == .(rmse) * " g"), side=3, line =-3, adj =0.05)

##note that the RMSE for the testing data typically is worse. Is the RMSE worse this time? Why or why not?
##did we come up with a useful model for predicting biomass? 
##Look at the range in biomass to decide if the error is small or large
summary(dat$biomass)