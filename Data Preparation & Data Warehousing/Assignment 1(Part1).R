#Part 1
#-----------------------------------------------------------------
install.packages("dplyr")
install.packages("plyr")
library(readr)
library(dplyr)
library(plyr)

#read the data set
bank_df <- read_delim("bank-additional-full.csv", delim = ";")

#select specific columns from the original data frame
df <- bank_df[,c("age", "education", "previous", "pdays", "y")]
View(df)

#change the value of 999 to NA as a missing values
df$pdays = na_if(df$pdays, 999)
#it has a lot of missing values (39673), so we can`t work with this column
sum(is.na(df$pdays))

#plot histogram of pdays
hist(df$pdays, main="Histogram for number of days",
     xlab="Days", border="black", col="blue", breaks=10)

#Transform the data values of the education field into numeric values
df$education <- revalue(df$education, replace= c("illiterate" = 0,
                                                 "basic.4y" = 4,
                                                 "basic.6y" = 6,
                                                 "basic.9y" = 9,
                                                 "high.school" = 12,
                                                 "professional.course" = 14,
                                                 "university.degree" = 16,
                                                 "unknown" = "Missing" ))

#change the value of Missing to NA as a missing values
df$education = na_if(df$education, "Missing")
unique(df$education)
sum(is.na(df$education))

#mean of age 
mean(df$age)
#median of age
median(df$age)
#function to calac the mode
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
#mode of age
getmode(df$age)


#Five number Summary 
#minimum
min(df$age)
#maximum
max(df$age)
#First quartile:
quantile(df$age, 0.25)
#Second quartile or median:
quantile(df$age, 0.5)
#Third quartile:
quantile(df$age, 0.75)

summary(df$age)

#Boxplot on age 
boxplot(df$age, main="Boxplot of Age", xlab="Age (year)", horizontal = TRUE)
text(x=fivenum(df$age),labels=fivenum(df$age),y=1.3)

#Q-Q Plot of age (quantile)
qqnorm(quantile(df$age))

#Standardize the age variable 
df$age_z = scale(x = df$age)
View(df)

#Boxplot on age after standardization 
boxplot(df$age_z, main="Boxplot of Age after standardization ", xlab="Age", horizontal = TRUE)
text(x=fivenum(round(df$age_z,digits=1)),labels=fivenum(round(df$age_z,digits=2)),y=1.3)

#Obtain a listing of all records that are outliers according to the field age_z
age_outliers <-  df[ which(df$age_z < -3 | df$age_z > 3), ]
age_outliers
View(age_outliers)
