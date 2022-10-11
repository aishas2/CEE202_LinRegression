#DATA SOURCE: World Bank DataBank
#install.packages("data.table") #already ran and installed on my computer
library("data.table")

mydata = read.csv ("TEAM08_CEE202_Proj_data.csv") #read data to dataframe I called mydata
mydata = as.data.table(mydata) # convert to data.table to be used correctly by the data.table package

colnames(mydata) = c("Country", "Code", "CO2Emm_kt", "GDP_US") #Change column names to one word

mydata$GDP_USbil = mydata$GDP_US/(10^9) #Convert GDP to units of US dollars to billion US dollars
mydata$GDP_US = NULL #delete GDP_US column

mydata$CO2Emm_Mt = mydata$CO2Emm_kt/(10^3) #Convert CO2 Emissions from kt to Mt
mydata$CO2Emm_kt = NULL #delete CO2Emm_kt column

mydata$Code = NULL #delete the unneeded ISO Country Codes column

head(mydata)

#install.packages("moments") #already ran and installed on my computer
library("moments")

#summarize CO2Emm_Mt variable
CO2Emm_mean = mean(mydata$CO2Emm_Mt)  
CO2Emm_median = median(mydata$CO2Emm_Mt) 
CO2Emm_var = var(mydata$CO2Emm_Mt)  
CO2Emm_sd = sd(mydata$CO2Emm_Mt)  
CO2Emm_range = max(mydata$CO2Emm_Mt) - min(mydata$CO2Emm_Mt)  
CO2Emm_IQR = IQR(mydata$CO2Emm_Mt)
CO2Emm_quantile = quantile(mydata$CO2Emm_Mt, c(.10,.30, .50,.70, .90))  
CO2Emm_skewness = skewness(mydata$CO2Emm_Mt)    
CO2Emm_kurtosis = kurtosis(mydata$CO2Emm_Mt)  
CO2Emm_summary = summary(mydata$CO2Emm_Mt)

#summarize GDP_USbil variable
GDP_mean = mean(mydata$GDP_USbil)  
GDP_median = median(mydata$GDP_USbil) 
GDP_var = var(mydata$GDP_USbil)  
GDP_sd = sd(mydata$GDP_USbil)  
GDP_range = max(mydata$GDP_USbil) - min(mydata$GDP_USbil)  
GDP_IQR = IQR(mydata$GDP_USbil)
GDP_quantile = quantile(mydata$GDP_USbil, c(.10,.30, .50,.70, .90))  
GDP_skewness = skewness(mydata$GDP_USbil)    
GDP_kurtosis = kurtosis(mydata$GDP_USbil)  
GDP_summary = summary(mydata$GDP_USbil)

#relationship between GDP and carbon emissions
covariance = cov(mydata$CO2Emm_Mt, mydata$GDP_USbil)
correlation = cor(mydata$CO2Emm_Mt, mydata$GDP_USbil)

###Plotting variables
CO2Emm_histogram = hist(mydata$CO2Emm_Mt, breaks = 50, main = 'CO2 Emissions (Megatons) by Country in 2018 Histogram', xlab = 'CO2 Emissions (Megatons)', freq = T, col= 'darkorange1', border = 'brown')   #Creates histogram of CO2 data
abline(v = CO2Emm_mean, col = "black", lwd = 2,lty = 2) #add dashed line at mean

CO2Emm_boxplot = boxplot(mydata$CO2Emm_Mt, main = 'CO2 Emissions (Megatons) by Country in 2018 Boxplot', horizontal = F, col = 'darkorange1', border = 'brown', xlab = 'CO2 Emissions (Megatons)')   #Creates box plot of CO2 data
CO2Emm_boxplot_NoOutliers = boxplot(mydata$CO2Emm_Mt, main = 'CO2 Emissions (Megatons) by Country in 2018 Boxplot Without Outliers', horizontal = F, ylim=c(0,150), col='darkorange1', border = 'brown', xlab = 'CO2 Emissions (Megatons)') #Creates box plot of CO2 data with no outliers
#add text to CO2 boxplot
text (x=0.7, y=66.333, label = "3rd quartile (66.33)")
text (x=0.7, y=11.585, label = "median (11.59)")
text (x=0.7, y= 2.328, label = "1st quartile (2.32)")
text (x=1.3, y= 0.01, label = "Min (0.01)")
text (x=0.7, y= 151.67, label = "Max (151.67)")

CO2Emm_QQplot = qqnorm(mydata$CO2Emm_Mt, frame = F,  main = 'CO2 Emissions (Megatons) by Country in 2018 QQ Normal Plot', col='darkorange1') #plots normal QQ plot for CO2 Emissions
qqline(mydata$CO2Emm_Mt, col = "brown", lwd = 2) #plots reference line on QQ plot

CO2_CumFreq <- plot.ecdf(mydata$CO2Emm_Mt, verticals = T, xlab="CO2 Emissions (Megatons)", ylab="relative cumulative frequency", main="CO2 Emissions (Megatons) by Country in 2018 Relative Cumulative Frequency", col='darkorange1') #plot CO2 relative cumulative frequency

# GDP histogram
GDP_histogram = hist(mydata$GDP_USbil, 
                     main="GDP (billion USD) by Country in 2018 Histogram", 
                     xlab="GDP (billion USD)", 
                     border="darkgreen", 
                     col="green",
                     xlim=c(0,1e+3),
                     las=1, 
                     breaks=1000)
abline(v = GDP_mean, col = "black", lwd = 2, lty = 2) #add dashed line at mean

#GDP boxplot with max value shown
GDP_boxplot<- boxplot(mydata$GDP_USbil, horizontal=F,  range=1e2, outline = T, ylim=c(4.259e-2, 20650), ylab="GDP (billion USD)", main="GDP (billion USD) by Country in 2018 Boxplot", col = 'green', border = 'darkgreen')

# example of adding some text to plot, I had to do trial and error with the location along x and y, here x=1 means in the middle of the boxplot
text (x=1.3, y= 4.259e-2, label = "Min (4.259e-2)")
text (x=0.7, y= 20611.9, label = "Max (2.06e+4 )")

###GDP boxplot
#name boxplot
GDP_boxplot_NoMax<- boxplot(mydata$GDP_USbil, horizontal=F,  range=1e2, outline = T, ylim=c(4.259e-2, 500), ylab="GDP (billion USD)", main="GDP (billion USD) by Country in 2018 Boxplot Without Max", col = 'green', border = 'darkgreen')

# example of adding some text to plot, I had to do trial and error with the location along x and y, here x=1 means in the middle of the boxplot
text (x=0.7, y=236.5, label = "3rd quartile (236.5)")
text (x=0.7, y=3.817e+1, label = "median (38.17e+1)")
text (x=0.7, y= 1.009e+1, label = "1st quartile (10.09)")
text (x=1.3, y= 4.259e-2, label = "Min (4.259e-2)")
text (x=0.7, y= 5e+3, label = "Max (2.060e+4 )")

#See what informational data are in GDP_boxplot_NoMax
#print (GDP_boxplot_NoMax)
#compare with these informational data with output of summary()
#print ("output of summary")
#summary(mydata$GDP_USbil)

GDP_QQplot = qqnorm(mydata$GDP_USbil, frame = F,  main = 'GDP (billion USD) by Country in 2018 QQ Normal Plot', col='green') #plots normal QQ plot for GDP
qqline(mydata$GDP_USbil, col = "darkgreen", lwd = 2) #plots reference line on QQ plot

GDP_CumFreq <- plot.ecdf(mydata$GDP_USbil, verticals = T, xlab="GDP (billion USD)", ylab="relative cumulative frequency", main="GDP (billion USD) by Country in 2018 Relative Cumulative Frequency", col='green') #plot GDP relative cumulative frequency

scatterplot = plot(mydata$CO2Emm_Mt, mydata$GDP_USbil, main="GDP vs CO2 Emissions Scatterplot",xlab="CO2 Emissions (Mt) ", ylab="GDP (billion USD) ", pch=19, ylim = c(0,5000), xlim = c(.01,1000)) #plots scatter plot of GDP vs CO2 Emissions
abline(lm(mydata$GDP_USbil ~ mydata$CO2Emm_Mt, data = mydata), col = "blue") #plots line of best fit


#simple linear regression of the two variables and evaluating significance of coefficients
class(mydata$GDP_USbil) #class is numeric
class (mydata$CO2Emm_Mt) #class is numeric
lmodel = lm(mydata$GDP_USbil ~ mydata$CO2Emm_Mt) #fit data to a linear regression model
summary(lmodel) #displays summary of SLR
attributes(lmodel) #shows what is stored in the lmodel data structure
lmodel$coef #shows the headings of lmodel data
confint(lmodel, level = 0.95) #shows confidence intervals of lmodel coefficients
cor.test( ~ mydata$GDP_USbil + mydata$CO2Emm_Mt, method = "pearson", conf.level = 0.95) #tests for correlation 

#check the assumptions of lmodel
par(mfrow=c(2,2))
plot(lmodel)

#create data table removing most influential pts
data_new = mydata[-c(179,36,83,63,75,139,80,135,73,78,132,72,76),]

#new scatterplot with SLR line
scatterplot = plot(data_new$CO2Emm_Mt, data_new$GDP_USbil, main="GDP vs CO2 Emissions Scatterplot",xlab="CO2 Emissions (Mt) ", ylab="GDP (billion USD) ", pch=19
                   , ylim = c(0,5000), xlim = c(.01,1000)) #plots scatter plot of GDP vs CO2 Emissions
abline(lm(data_new$GDP_USbil ~ data_new$CO2Emm_Mt, data = data_new), col = "red") #plots line of best fit

#simple linear regression of 2 variables and evaluating significance of coefficients
lmodel_new = lm(data_new$GDP_USbil ~ data_new$CO2Emm_Mt) #fit data to a linear regression model
summary(lmodel_new) #displays summary of SLR
attributes(lmodel_new) #shows what is stored in the lmodel data structure
lmodel_new$coef #shows the headings of lmodel data
confint(lmodel_new, level = 0.95) #shows confidence intervals of lmodel coefficients
cor.test( ~ data_new$GDP_USbil + data_new$CO2Emm_Mt, method = "pearson", conf.level = 0.95) #tests for correlation 

#check the assumptions of lmodel with no most influential pts
par(mfrow=c(2,2))
plot(lmodel_new)