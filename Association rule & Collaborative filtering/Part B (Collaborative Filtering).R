library(lsa)
library(readr)
library(recommenderlab)

courses <- read_csv("course.csv")
View(courses)
courses[is.na(courses)] = 0
#We can now remove user ids
ratingmat = as.matrix(courses[,-1])
cosine(ratingmat)

#create dataframe 
user <- c('LN','MH','JH','EN','DU','FL','GL', 'AH' ,'SA','RW','BA','MG','AF','KG','DS')
SQL <- c(4,3,2,4,4 ,NA,NA,NA,NA,NA,NA,NA,NA,NA,4)
Spatial <- c(NA ,4,2 ,NA ,4,4,4,3,NA,NA,NA,NA,NA,NA,NA)
PA1 <- c(NA,NA,NA,NA,NA,NA,NA,NA, 4,2,4,4,4,3,NA)
DM_IN_R <- c(NA,NA,NA ,4,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,2)
PYTHON <- c(3,4,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
FORECAST <- c(2 ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,4,NA,NA,NA)
R_PROG <- c(4 ,NA,NA ,4 ,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,4)
HADOOP <- c(NA,NA,NA,NA,NA,NA,NA,NA,NA,4, NA,NA ,NA,NA,NA)
REGRESSION <- c(2 ,NA,NA,3,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA ,NA)
df <- data.frame(user,SQL,Spatial,PA1,DM_IN_R,PYTHON,FORECAST,R_PROG,HADOOP,REGRESSION)

#convert to matrix and remove first column
ratingmat2 = as.matrix(df[,-1])
#Convert ratings matrix to real rating matrx which makes it dense
ratingmat2 = as(ratingmat2, "realRatingMatrix")
#Create Recommender Model. The parameters are IBCF and Cosine similarity.
rec_mod = Recommender(ratingmat2, method = "IBCF", param=list(method="Cosine", k=9)) 
#Obtain top 3 recommendations for 4th user entry in dataset
Top_3_pred = predict(rec_mod, ratingmat2[4], n=3)
#Convert the recommendations to a list
Top_3_List = as(Top_3_pred, "list")
Top_3_List

