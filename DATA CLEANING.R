load("cleaned.rda")

source("DataAnalyticsFunctions.R")

rm<-cbind("id","member_id","addr_state","pymnt_plan","sub_grade","last_pymnt_d","next_pymnt_d","last_credit_pull_d","earliest_cr_line")
new_DT<-DT[,!colnames(DT) %in% rm]
new_DT<-new_DT[grep(paste(as.character(2015),collapse="|"), new_DT$issue_d),]
new_DT<-new_DT[,!colnames(new_DT) %in% "issue_d"]


#new_DT$loan_status<-factor(new_DT$loan_status)
colnames(new_DT)[which(names(new_DT) == "loan_status")] <- "default"
new_DT<-new_DT[!new_DT[,"emp_length"]=="n/a",]


source("support.R")
scan(new_DT)
detail(new_DT)


#Try data
# Use 1% of our records to try our model
load("Cleaned.rda")
set.seed(13)
n<-nrow(DT)
nfold<-100
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
try <- which(foldid==100)
datatry <- DT[try,]


# Running a logistic regression
set.seed(1)
nfold <- 2 
n <- nrow(new_DT) 
### create a vector of fold memberships (random order)
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]

model.logistic <-glm(default ~., data=new_DT, subset=which(foldid==1), family="binomial")
summary(model.logistic)

#R2
1 - model.logistic$deviance/model.logistic$null.deviance

## Holdout Sample
OOS <- data.frame(logistic=NA) 

### Set the second part for testing (first for training)
k <- 2
### Set the other part for training (if not k)
train <- which(foldid!=k) # train on all but fold `k'
## which returns the indices 序列 of the elements which satisfy the condition 
test  <- which(foldid==k) # test on fold k

model.logistic <-glm(default~., data=new_DT, subset=train,family="binomial")
pred.logistic             <- predict(model.logistic, newdata=new_DT[-train,], type="response")
OOS$logistic <- R2(y=new_DT$default[-train], pred=pred.logistic, family="binomial")
OOS

## K fold cross validation
nfold <- 4
set.seed(13)
n <- nrow(new_DT) 
foldid <- rep(1:nfold,each=ceiling(n/nfold))[sample(1:n)]
### create an empty dataframe of results
OOS <- data.frame(logistic=rep(NA,nfold), null=rep(NA,nfold)) 
### Use a for loop to run through the nfold trails
## we want each group to be the test group one by one.
for(k in 1:nfold){ 
  train <- which(foldid!=k) # train on all but fold `k'
  ## fit the two regressions and null model
  model.logistic <-glm(default~., data=new_DT, subset=train,family="binomial")
  model.null <-glm(default~1, data=new_DT, subset=train,family="binomial")
  ## get predictions
  pred.logistic <- predict(model.logistic, newdata=new_DT[-train,], type="response")
  pred.null <- predict(model.null, newdata=new_DT[-train,], type="response")
  ## calculate and log R2
  # Logistic
  OOS$logistic[k] <- R2(y=new_DT$default[-train], pred=pred.logistic, family="binomial")
  OOS$logistic[k]
  #Null
  OOS$null[k] <- R2(y=new_DT$default[-train], pred=pred.null, family="binomial")
  OOS$null[k]
  ## We will loop this nfold times (I setup for 5)
  print(paste("Iteration",k,"of",nfold,"(thank you for your patience)"))
}

OOS  


# calculate means for each column


plot( c( 0, 1 ), c(0, 1), type="n", xlim=c(0,1), ylim=c(0,1), bty="n", xlab = "False positive rate", ylab="True positive rate")
lines(c(0,1),c(0,1), lty=2)  ## the true positive rate = false positive rate
val<- .5
values <- FPR_TPR( (model.logistic$fitted >= val) , model.logistic$y )
points( values$FPR , values$TPR )  ## the point is above the line so good model
ACC.model.logistic <- values$ACC
ACC.model.logistic
text( values$FPR+.12, values$TPR+.05, labels=c("LR"))


# Running a correlation matrix
drop <- c("id","member_id","term","grade","sub_grade","emp_length","home_ownership","verification_status","issue_d","pymnt_plan","purpose","addr_state"
          ,"earliest_cr_line","initial_list_status","last_pymnt_d","next_pymnt_d","last_credit_pull_d","application_type","verification_status_joint")
Data_for_matrrix <- DT[,!(names(DT) %in% drop)]
cormatrix<-cor(Data_for_matrrix)
installpkg("corrplot")
library(corrplot)
corrplot(cormatrix, method = "square")


