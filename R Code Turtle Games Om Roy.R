Turtle Games are a game manufacturer and 
## retailer. They manufacture and sell their own products, along with sourcing
## and selling products manufactured by other companies. Their product range 
## includes books, board games, video games and toys. They have a global 
## customer base and have a business objective of improving overall sales 
##performance by utilising customer trends. 

## In particular, Turtle Games wants to understand:
## - how customers accumulate loyalty points 
## - how useful are remuneration and spending scores data 
## - can social data (e.g. customer reviews) be used in marketing 
##     campaigns 
## - what is the impact on sales per product (Week 4)
## - the reliability of the data 
## - if there is any possible relationship(s) in sales between North America,
##     Europe, and global sales 
################################################################################

 EDA using R

## The sales department of Turtle games prefers R to Python. As wecan perform
## data analysis in R, wewill explore and prepare the data set for analysis by
## utilising basic statistics and plots. 

# 1. Load and explore the data
df<-read.csv('turtle_sales.csv')
# Install and import Tidyverse.
library(tidyverse)

# Import the data set.
df<-data.frame(df)

# Print the data frame.
summary(df)
dim(df)
head(df)
as_tibble(df)
print(df)

# Create a new data frame from a subset of the sales data frame.
df1<-subset(df,select=-c(Ranking,Year,Genre,Publisher))




# View the data frame.
head(df1)
dim(df1)
tail(df1)
as_tibble(df1)
# View the descriptive statistics.
summary(df1)
sum(is.na(df1))
################################################################################

# 2. Review plots to determine insights into the data set.

## 2a) Scatterplots
# Create scatterplots.
library(ggplot2)
#from inspection points above 15 on both axes look like outliers
ggplot(data=df1,aes(x=NA_Sales,y=EU_Sales))+geom_point(alpha=1,color='red')+geom_abline()+xlim(0,15)+ylim(0,12)
ggplot(data=df1,aes(x=Global_Sales,y=EU_Sales))+geom_point(alpha=1,color='green')+xlim(0,30)+ylim(0,11)+geom_abline()
ggplot(data=df1,aes(x=NA_Sales,y=Global_Sales))+geom_point(alpha=1,color='blue')+geom_abline()+xlim(0,26)+ylim(0,37.5)
## 2b) Histograms
# Create histograms.
ggplot(df1,aes(x=NA_Sales))+geom_histogram()
ggplot(df1,aes(x=EU_Sales))+geom_histogram(bins=30)
ggplot(df1,aes(x=Global_Sales))+geom_histogram()
## 2c) Boxplots
ggplot(df1,aes(x=Global_Sales))+geom_boxplot(outlier.colour="red", outlier.shape=10,
                                                                             outlier.size=3, notch=FALSE)
ggplot(df1,aes(x=NA_Sales))+geom_boxplot(outlier.colour="red", outlier.shape=10,
                                             outlier.size=3, notch=FALSE)
ggplot(df1,aes(x=EU_Sales))+geom_boxplot(outlier.colour="red", outlier.shape=10,
   
                                                                                   outlier.size=3, notch=FALSE)



#Remove Outliers
Q1 <- quantile(df1$NA_Sales, .25)
Q3 <- quantile(df1$NA_Sales, .75)
IQR <- IQR(df1$NA_Sales)



no_outliers <- subset(df1, df1$NA_Sales > (Q1 - 1.5*IQR) & df1$NA_Sales < (Q3 + 1.5*IQR))
Q1e <- quantile(no_outliers$EU_Sales, .25)
Q3e<- quantile(no_outliers$EU_Sales, .75)
IQRe<- IQR(no_outliers$EU_Sales)



no_outliers1 <- subset(no_outliers, no_outliers$EU_Sales > (Q1 - 1.5*IQRe) & no_outliers$EU_Sales < (Q3 + 1.5*IQRe))

Q1g <- quantile(no_outliers1$Global_Sales, .25)
Q3g<- quantile(no_outliers1$Global_Sales, .75)
IQRg<- IQR(no_outliers1$Global_Sales)
df2<-subset(no_outliers1, no_outliers1$Global_Sales > (Q1 - 1.5*IQRg) & no_outliers1$Global_Sales < (Q3 + 1.5*IQRg))

#check box plots and no outlier dataframe
dim(df2)
ggplot(df2,aes(x=Global_Sales))+geom_boxplot(outlier.colour="red", outlier.shape=10,
                                             outlier.size=3, notch=FALSE)
ggplot(df2,aes(x=EU_Sales))+geom_boxplot(outlier.colour="red", outlier.shape=10,
                                             outlier.size=3, notch=FALSE)
ggplot(df2,aes(x=NA_Sales))+geom_boxplot(outlier.colour="red", outlier.shape=10,   outlier.size=3, notch=FALSE)

                                         #outliers greatly reduced                         
)###############################################################################

)# 3. Observations and insights


## We import the tidyverse library. There are 9 columns and 352 observations.
#We create a subset excluding the unnecessary columns.
#We now import the ggplot2 library for data visualization.
#We visually adjust the axes limits to account for outliers.







###############################################################################
###############################################################################


## Utilising R, we will explore, prepare and explain the normality of the data 

## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. 
## Instructions

# 1. Load and explore the data

# View data frame created in Week 4.
head(df2)
dim(df2)
as_tibble(df2)
# Check output: Determine the min, max, and mean values.
df2sales<-subset(df2,select = c(EU_Sales,NA_Sales,Global_Sales))
df2sales
min(df2$NA_Sales)
min(df2sales$EU_Sales)
min(df2sales$Global_Sales)
max(df2sales$NA_Sales)
max(df2sales$EU_Sales)
max(df2sales$Global_Sales)
mean(df2sales$NA_Sales)
mean(df2sales$EU_Sales)
mean(df2sales$Global_Sales)

# View the descriptive statistics.
summary(df2sales)
#Product is just an index
###############################################################################

# 2. Determine the impact on sales per product_id.

## 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
sum_per_prod<-df2%>%group_by(Product)%>%summarise(sum_per_product=sum(NA_Sales+EU_Sales+Global_Sales))

# View the data frame.

sum_per_prod
# Explore the data frame.
summary(sum_per_prod)
head(sum_per_prod)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.

ggplot(df2,aes(x=EU_Sales,y=NA_Sales))+geom_point()+geom_abline()
ggplot(df2,aes(x=EU_Sales,y=Global_Sales))+geom_point()+geom_abline()
ggplot(df2,aes(x=Global_Sales,y=NA_Sales))+geom_point()+geom_abline()
# Create histograms.
ggplot(df2,aes(x=NA_Sales))+geom_histogram()
ggplot(df2,aes(x=EU_Sales))+geom_histogram()
ggplot(df2,aes(x=Global_Sales))+geom_histogram()
# Create boxplots.


###############################################################################


# 3. Determine the normality of the data set.

## 3a) Create Q-Q Plots
# Create Q-Q Plots.

qqnorm(df2$NA_Sales,)
qqline(df2$NA_Sales,col='red')
qqnorm(df2$EU_Sales,)
qqline(df2$EU_Sales,col='blue')
qqnorm(df2$Global_Sales,)
qqline(df2$Global_Sales,col='green')


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(df2$NA_Sales)
shapiro.test(df2$EU_Sales)
shapiro.test(df2$Global_Sales)
#all columns are non-normal

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(df2sales)
kurtosis(df2sales)
#Let us use sqrt transf for global sales
#log10 for eu and na sales
#remove 0's to perform log transf
df3<-df2
sum(is.na(df2))
df3[df3==0]<-NA
sum(is.na(df3))
df4<-na.omit(df3)
install.packages('rcompanion')
library(rcompanion)
dftest<-df4
dfunc<-df2
T_tuk =
  transformTukey(df4$EU_Sales,
                 plotit=FALSE)
normeu<-((dftest$EU_Sales))^0.45
shapiro.test(normeu)
T_tuk1=transformTukey(dftest$NA_Sales,plotit=FALSE)
normna<-((dftest$NA_Sales))^0.475
shapiro.test((normna))
T_tuk2=transformTukey(dftest$Global_Sales,plotit=FALSE)
normg<-((dftest$Global_Sales))^0.7
shapiro.test(normg)
dftest<-transform(dftest,NA_Sales=NA_Sales^0.475,EU_Sales=EU_Sales^0.45,Global_Sales=Global_Sales^0.7)
dim(dftest)
dim(dfunc)
#We have created a dataframe without outliersand 0 values and  transformed to as close
#to normal as possible so we can perform statistical inference.
#We have kept a copy of the unchanged dataframe as well.

## 3d) Determine correlation
# Determine correlation.
plotDensityHistogram(dftest$NA_Sales)
plotDensityHistogram(dftest$EU_Sales)
plotDensityHistogram(dftest$Global_Sales)
cor(dfunc$NA_Sales,dfunc$EU_Sales)
cor(dftest$NA_Sales,dftest$EU_Sales)
cor(dfunc$NA_Sales,dfunc$Global_Sales)
cor(dftest$NA_Sales,dftest$Global_Sales)
cor(dfunc$Global_Sales,dfunc$EU_Sales)
cor(dftest$Global_Sales,dftest$EU_Sales)
#While more normally distributed Higher correlation between variables
#indicates that unchanged dataframe 
#would be more suitable for multiple linear regression
#in the future
###############################################################################
install.packages('BSDA')
library(BSDA)
#As no transformation can make the data normal
#We compute T-test to test whether the mean of 
#the Sales columns are equal to another column
#H0:XX_Sales=YY_Sales
#HA:XX_Sales!=YY_Sales
t.test(dfunc$NA_Sales,conf.level = 0.95,mu=mean(dfunc$EU_Sales))
t.test(dfunc$Global_Sales,conf.level = 0.95,mu=mean(dfunc$NA_Sales))
t.test(dfunc$EU_Sales,conf.level = 0.95,mu=mean(dfunc$Global_Sales))
#All tests statistically significant that the mean sales in each region
#is not the same

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.


#The summary function in R gives corresponding outputs
#We also used the group_by function to determine the sum of sales per product ID.
#We still see the heavy right skew. Let us see if we can transform the data to a normal distribution.
#The low Shapiro-Test P-value for all the different sales variables we reject the null hypothesis of a null distribution for all of them.
#We us the transformTurkey package to find the transformation that makes the variables as close to a normal distribution as possible. We raise the EU sales to a power of 0.45, the NA sales to a power of 0.475 and global sales to a power of 0.7.
#We then re-run the Shapiro-Test and see that unfortunately the data is still not normally distributed.
#There is no possible transformation that make the variables normal.
#The correlation of the NA sales with EU sales is 0.504 which implies moderate positive correlation.




## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, we need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and our
## previous analysis, we will then provide recommendations to 
## Turtle Games based on:
##   - Do we have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would our suggestions and recommendations be to the business?
##   - If needed, how would we improve the model(s)?
##   - Explain your answers.




# 1. Load and explore the data
# View data frame created in Week 5.
dftest

# Determine a summary of the data frame.
summary(dftest)

###############################################################################

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
# Create a linear regression model on the original data.

dfsales1<-subset(dftest,select=c(Global_Sales,EU_Sales,NA_Sales))
cor(dfsales1)
plot(dfsales1$EU_Sales,dfsales1$Global_Sales)
plot(dfsales1$NA_Sales,dfsales1$Global_Sales)
model1<-lm(Global_Sales~NA_Sales,data=dfsales1)
model2<-lm(Global_Sales~EU_Sales,data=dfsales1)


## 2b) Create a plot (simple linear regression)
# Basic visualisation.
model1
summary(model1)
model2
summary(model2)
plot(model1$residuals)
plot(model2$residuals)
coefficients(model1)
coefficients(model2)
abline(coefficients(model1))
abline(coefficients(model2))

# Predictions based on given values



SalesForecast=data.frame('NA_Sales'=c(34.02,3.93,2.73,2.26,22.08),EU_Sales=c(23.8,1.56,0.65,0.97,0.52))

SalesForecast
predict(model1,newdata=SalesForecast)
predict(model2,newdata=SalesForecast)

###############################################################################


#Would a multiple linear regression model be better?

cor(dfsales1)
install.packages('psych')


library(psych)
corPlot(dfsales1,cex=2)
modela=lm(Global_Sales~NA_Sales+EU_Sales,data=dfsales1)
summary(modela)
#Modelfit perfectly as there is multicollinearity
#between independant variables
#simple linear regression is a better choice
#In particular model 1 using NA_Sales as
#it has a higher R-Squared score of 78.72%
#EU and NA sales are significant
#but cannot be used together 
str(dfsales1)

plot(predict(model1),                             
     dfsales1$GlobalSales,
     xlab = "Predicted Values",
     ylab = "Observed Values",
     main='Model 1 (NA)')

     
     


plot(predict(model2),                                
     dfsales1$GlobalSales,
     xlab = "Predicted Values",
     ylab = "Observed Values",fig.dim = c(2, 2),
     main='Model 2 (EU)')
dev.off







