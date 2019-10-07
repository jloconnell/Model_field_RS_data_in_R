##PLSR in R; for more info see http://mevik.net/work/software/pls.html

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
dat<-read.csv (file = "data/my_tidy_spectra_band_numbers.csv", header=TRUE)
wave <- read.csv(file="data/hyperspectral_bands_and_wavelengths.csv", header = TRUE)
field<- read_excel("data/field_data.xlsx")
##clean up field by removing columns we don't need now; we took care of the read number in the last exercise
names(field)
head(field)

##combine the field data and the spectral data with merge
##first make a merge key based on plot and date, need to put date in a common format in both datassets first
dat$date<-as.Date(dat$date)
field$date<-as.Date(field$date, format="%m%d%Y")
field$key<-paste(field$plot, field$date)
field<-subset(field, select=-c(plot, date))
dat$key<-paste(dat$plot, dat$date)
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

#####Step 1: Subset the data if needed to get homogeneous cases (these data don't need this, but yours may)
#####ex: of reasons to subset: more than 1 species, wildly different water depths, or other factors that can obscure your target relationship
#dat<-dat[dat$species=="",]

##STEP 2: PLSR requires a special data set up; these next lines create this
#first, extract the response variable (the thing we'd like to predict)
response<-dat$aboveground_biomass_g
#extract the columns containing dat bands
bands<-as.matrix(dat[,7:ncol(dat)]) 
#make sure you got the right columns by looking at the first few lines of data
head(bands)[,c(1:5, ncol(bands))]
#rename the bands by wavelength (nm) for better plotting
colnames(bands)<-wave$wave
#create the PLSR dataframe; the I() below makes a protected dataframe needed for PLSR
forpls<-data.frame(response,I(bands))
#remove any NA's before proceeding, for example because of bad scans or bad field data
forpls<-forpls[complete.cases(forpls),]

###STEP 3: create training and testing datasets####
##PLSR usually makes a model on training dataset and verifies on a test dataset

#subset the data by random rows into testing and training datasets
#for reproducibility, set a seed
set.seed(1)
# the p =xx below determines the proprotion of data to train on, here 70%
trainIndex <- createDataPartition(forpls$response, p = 0.7, list = FALSE, 
                                  times = 1)
forpls.train<-forpls[trainIndex,]
forpls.test<-forpls[-trainIndex,]
dim(forpls.train); dim(forpls.test)

###STEP 4: run PLSR####
###PLSR REGRESSION
###Note, the number of components can't exceed the number of observations or bands
###also, if you're curious how the summary is calculated, try pls:::summary.mvr
summary(m1 <- plsr(response ~ bands, ncomp = 20, data = forpls.train, validation = "LOO", scale=T, center=T))
plot(RMSEP(m1), legendpos ="topright")
##identify the number of components corresponding to the first local minima
##in RMSEP and store as "nc"; 
#after this, this is the only part of code below you really have to change each 
#time you run a new model, ie. you should store the right number as "nc"
nc<-3
#in my example data the right number to store as nc is 3; its where the first steep  valley in the plot is
#in my example data Explained Variance in Y for 3 components is 63.39% and RMSEP is 219.5 grams 
#in my example, the explained variance in x for 3 components is 97%, almost all the x variance
#How does this compare to the linear model? Note, we have all the dates included here

##Pull out some summary stats, in case you need them
##variation explained in x 
explvar(m1)
##cumulative explained variation in x by each sucessive component
cumsum(explvar(m1))

##cumulative variaiton explained in y
##I'm not aware of a way to extract the individual variation explained in Y
##for each component, but you can do the math
##Component 1 will always explain the most 1 Y variation
pls::R2(m1, estimate = "train", intercept = FALSE)

##the rest of this code runs it self; just send to the R terminal (highlight the code and press CTRL+ENTER)
##and examine output
##plot the predicted vs measured values from the training model
plot(m1, ncomp=nc, asp =1, line =TRUE)
abline(h=0)

#Examine the loadings plots for each band; 
#These tell how much each band is contributing to the components (eg, the transformed x variables)
#Bands that are contributing the most to the component will have a high peak or valley in the plot
#Component 1 will always contain most of the Explained variance in Y; 
#For X variation explained, later components still typically explain less and less, but which one is most important is more variable
#Exp. Var for x for components is () in the legend, but you'd typically 
#want to report the explained variance in Y in a paper, which is not in the legend
plot(m1, "loadings", comps=1:3, legendpos = "topright", xlab = "nm", labels="numbers", 
     xaxp=c(500,2500,10),ylim=c(-0.2, 0.4))
abline(h=0)
#In the example data I provided, green, NIR and some longer wavelengths are most important
#Component 1 is mirroring the vegetation spectral reflectance, 
#Component 2 mirrors component 1, but is comparatively over emphasizing 
#the green and NIR wavelengths as well as wavelengths around 1600 nm
##Component 3 places greater emphasis on some of the noisy regions of the spectra
#This could help filter out the influence of these regions and 
#we might perhaps get a better fit if we had cleaned the data to remove or smooth those regions first

###STEP5: validate the selected model on the testing dataset and plot the outcome####
rmse<-RMSEP(m1,newdata=forpls.test)$val[nc+1]
p<-predict(m1, ncomp=nc, newdata=forpls.test)
plot(forpls.test$response, p, xlab="measured response variable", 
     ylab ="predicted response variable", ylim=c(min(forpls.test$response),max(forpls.test$response)))
abline(0,1)
mtext("testing data", side=3, adj=0.01, line=-1)
mtext(paste("RMSE =", round(rmse, 1), "g"), side=3, adj=0.01, line=-3)
#In the example data I provided, the testing model is not perfect, 
#but is about as good as the training model; 
#i.e. training data has RMSEP of 219 g for 3 components and testing has 
#RMSEP of 203 g for 3 components, roughly the same
#note RMSEP is Root Mean Square Error of Prediction, and the units are 
#the units of your response variable, here grams
#Note also that where we do worst is for the highest biomass plots
#These tall plots were adjacent to a creek and likely had the highest soil moisture.
#How might this make predicting them tricky?



###STEP 6: save a plot of the out come, if you want####
##This code will put a pdf figure in the output folder
rmse.train<-RMSEP(m1)$val[(nc+1)*2] ## I know this is weird but you can verify it yourself by playing with the peices of this code, the model summaries are reported strangely in this package
rmse.test<-RMSEP(m1,newdata=forpls.test)$val[nc+1]

pdf(file="output/myresult.pdf", height=8, width=4)
par(mar=c(4,4.5,0.5,0.5), oma=c(0,0.5,1.5,0.5),mfrow=c(2,1))
plot(m1, ncomp=nc, line =TRUE, main="", xlab = "", 
     ylab= "Predicted biomass (g)", col="black", cex=1.2, 
     cex.lab=1.2, lwd=1.3, ylim=c(min(forpls.train$response),max(forpls.train$response)))
mtext(paste("Training data: RMSEP =", round(rmse.train, 0), "g"), side =3, line =-1, adj=0.05)
mtext("A", line= 0.1, side=3, adj=1, cex=1.15)
plot(forpls.test$response, p, xlab = "Measured biomass (g)", 
     ylab= "Predicted biomass (g)", col="black", cex=1.2,
     cex.lab=1.2, lwd=1.3, ylim=c(min(forpls.train$response),max(forpls.train$response)))
abline(0,1)
mtext(paste("Testing data: RMSEP =", round(rmse.test, 0), "g"), side =3, line =-1, adj=0.05)
mtext("B", line= 0.1, side=3, adj=1, cex=1.15)
dev.off()  

##perhaps also write out a loadings plot
pdf(file="output/loadings.pdf", height=4.5, width=5)
par(mar=c(4,4.5,0.5,0.5), oma=c(0,0.5,1.5,0.5),mfrow=c(1,1))
plot(m1, "loadings", comps=1:3, legendpos = "topleft", xlab = "nm", labels="numbers", xaxp=c(500,2500,10))
abline(h=0)
dev.off()

###So which method yielded a model with a better prediction? PLSR or simple linear modeling?
###When might PLSR be most useful?
####Try on your own, repeat code for vegetation height and see what is different/similar
