setwd("J:/SanDiskSecureAccessV2.0/UMUC MS 630/Assignments/Assignment 4")
phishing <- read_csv("J:/SanDiskSecureAccessV2.0/UMUC MS 630/Assignments/Assignment 4/phishing.csv")


dir()

View(phishing)

dim(phishing)

##Import in libraries for R
library(readr)
library("Amelia", lib.loc="~/R/win-library/3.3")
library("neuralnet", lib.loc="~/R/win-library/3.3")


ph1=phishing

summary(ph1)
#missmap(ph1, main="Missing Values")

apply(ph1, 2, function(ph1) sum(is.na(ph1)))


str(ph1)

ph2=ph1

keep.vars<- c("having_IP_Address", "URL_Length", "Shortining_Service", "having_At_Symbol", 
              "double_slash_redirecting", "Prefix_Suffix", "having_Sub_Domain", "SSLfinal_State", 
              "Domain_registeration_length", "Favicon", "port", "HTTPS_token", "Request_URL", 
              "URL_of_Anchor", "Links_in_tags", "SFH", "Submitting_to_email", "Abnormal_URL", "Redirect", 
              "on_mouseover", "RightClick", "popUpWidnow", "Iframe", "age_of_domain", "DNSRecord", 
              "web_traffic","Page_Rank", "Google_Index", "Links_pointing_to_page", "Statistical_report", "Result")


#
# I was having troubles with the model converging and tried to determine where the problem was coming 
# from 
#keep.varsa<- c("having_IP_Address", "URL_Length", "Shortining_Service", "having_At_Symbol", 
#               "double_slash_redirecting", "Prefix_Suffix", "having_Sub_Domain", "SSLfinal_State", 
#               "Domain_registeration_length", "Favicon", "Result")

#keep.varsa11<- c("having_IP_Address", "URL_Length", "Shortining_Service", "Result")

#keep.varsa22<- c("having_At_Symbol", "double_slash_redirecting", "Result")
              
#keep.varsa2<- c("Prefix_Suffix", "having_Sub_Domain", "SSLfinal_State", 
#              "Domain_registeration_length", "Favicon", "Result")


#keep.varsb<- c("port", "HTTPS_token", "Request_URL", 
#              "URL_of_Anchor", "Links_in_tags", "SFH", "Submitting_to_email", "Abnormal_URL", "Redirect", 
#              "on_mouseover", "Result")

#keep.varsc<- c("on_mouseover", "RightClick", "popUpWidnow", "Iframe", "age_of_domain", "DNSRecord", 
#              "web_traffic","Page_Rank", "Google_Index", "Links_pointing_to_page", "Statistical_report", "Result")
#

#keep.varsd<- c("Domain_registeration_length", "Favicon", "port", "HTTPS_token", "Request_URL", 
#              "URL_of_Anchor", "Links_in_tags", "SFH", "Submitting_to_email", "Abnormal_URL", "Redirect", 
#              "on_mouseover", "RightClick", "popUpWidnow", "Iframe", "age_of_domain", "DNSRecord", 
#              "web_traffic","Page_Rank", "Google_Index", "Links_pointing_to_page", "Statistical_report", "Result")

set.seed(32)
int <- sample(2, nrow(ph2), replace = TRUE, prob = c(0.7, 0.3))
summary(ph2[, names(ph2) %in% keep.vars])
summary(ph2[, !names(ph2) %in% keep.vars])

train <- ph2[int == 1, names(ph2) %in% keep.vars]
test <- ph2[int == 2, names(ph2) %in% keep.vars]

target.name<- "Result"

my.formula <- as.formula(paste("Result ~ ", 
                               paste(names(train)[!names(train) %in% target.name], collapse = ' + ')))

##nn <- neuralnet(my.formula, data = train, hidden = c(10), linear.output = FALSE)
nn <- neuralnet(my.formula, data = train, hidden = c(2), linear.output = FALSE, stepmax = 100e5)

names(nn)
nn$call
nn$model.list

nn$response

nn$covariate

nn$weights
nn$startweights

nn$result.matrix
nn$net.result[[1]][1:10]

plot(nn)

mypredict<-compute(nn, nn$covariate)$net.result
mypredict<-apply(mypredict, c(1), round)

mypredict

##Confusion Marix for Training Set 
table(mypredict, train$Result)
table(mypredict, train$Result, dnn=c("predicted", "actual"))
sum(mypredict == train$Result)/length(train$Result)

## Confustion Matrix for Test Set 
## Evaluation of model on test data
cpreds1 <- compute(nn, test[ , !names(test) %in% c('Result')])


cpreds1 <- apply(cpreds1$net.result, 1, round)
## table(cpreds1, test$class)
table(cpreds1, test$Result, dnn=c("predicted", "actual") )
sum(cpreds1 == test$Result)/length(test$Result) 


######################## ROUND 2 (10)
nn2 <- neuralnet(my.formula, data = train, hidden = c(10), linear.output = FALSE, stepmax = 100e5)



names(nn2)
nn2$call
nn2$model.list

nn2$response

nn2$covariate

nn2$weights
nn2$startweights

nn2$result.matrix
nn2$net.result[[1]][1:10]

plot(nn2)

##############Testing Generalized weights with Respect to each covariate
### Section will be added at the end of nn3

library("gplots", lib.loc="~/R/win-library/3.3")

par(mfrow=c(2,2))
gwplot(nn2, selected.covariate = "having_IP_Address",
       min = -1, max = 1, col="blue")

gwplot(nn2, selected.covariate = "URL_Length",
       min = -1, max = 1, col="red")

gwplot(nn2, selected.covariate = "Shortining_Service",
       min = -1, max = 1, col="purple")

gwplot(nn2, selected.covariate = "having_At_Symbol",
       min = -1, max = 1, col="dark cyan")


##Set 2 of 8
par(mfrow=c(2,2))
gwplot(nn2, selected.covariate = "double_slash_redirecting",
       min = -1, max = 1, col="blue")

gwplot(nn2, selected.covariate = "Prefix_Suffix",
       min = -1, max = 1, col="red")

gwplot(nn2, selected.covariate = "having_Sub_Domain",
       min = -1, max = 1, col="purple")

gwplot(nn2, selected.covariate = "SSLfinal_State",
       min = -1, max = 1, col="dark cyan")


############ 3 pf 8
par(mfrow=c(2,2))
gwplot(nn2, selected.covariate = "Domain_registeration_length",
       min = -1, max = 1, col="blue")

gwplot(nn2, selected.covariate = "Favicon",
       min = -1, max = 1, col="red")

gwplot(nn2, selected.covariate = "port",
       min = -1, max = 1, col="purple")

gwplot(nn2, selected.covariate = "HTTPS_token",
       min = -1, max = 1, col="dark cyan")


############ 4 pf 8
par(mfrow=c(2,2))
gwplot(nn2, selected.covariate = "Request_URL",
       min = -1, max = 1, col="blue")

gwplot(nn2, selected.covariate = "URL_of_Anchor",
       min = -1, max = 1, col="red")

gwplot(nn2, selected.covariate = "Links_in_tags",
       min = -1, max = 1, col="purple")

gwplot(nn2, selected.covariate = "Submitting_to_email",
       min = -1, max = 1, col="dark cyan")


############ 5 pf 8
par(mfrow=c(2,2))
gwplot(nn2, selected.covariate = "Abnormal_URL",
       min = -1, max = 1, col="blue")

gwplot(nn2, selected.covariate = "Redirect",
       min = -1, max = 1, col="red")

gwplot(nn2, selected.covariate = "on_mouseover",
       min = -1, max = 1, col="purple")

gwplot(nn2, selected.covariate = "RightClick",
       min = -1, max = 1, col="dark cyan")



############ 6 pf 8
par(mfrow=c(2,2))
gwplot(nn2, selected.covariate = "popUpWidnow",
       min = -1, max = 1, col="blue")

gwplot(nn2, selected.covariate = "Iframe",
       min = -1, max = 1, col="red")

gwplot(nn2, selected.covariate = "age_of_domain",
       min = -1, max = 1, col="purple")

gwplot(nn2, selected.covariate = "DNSRecord",
       min = -1, max = 1, col="dark cyan")


############ 7 pf 8
par(mfrow=c(2,2))
gwplot(nn2, selected.covariate = "DNSRecord",
       min = -1, max = 1, col="blue")

gwplot(nn2, selected.covariate = "Page_Rank",
       min = -1, max = 1, col="red")

gwplot(nn2, selected.covariate = "Google_Index",
       min = -1, max = 1, col="purple")



############ 8 pf 8
par(mfrow=c(2,2))
gwplot(nn2, selected.covariate = "Links_pointing_to_page",
       min = -1, max = 1, col="blue")

gwplot(nn2, selected.covariate = "Statistical_report",
       min = -1, max = 1, col="red")






mypredict2<-compute(nn2, nn2$covariate)$net.result
mypredict2<-apply(mypredict2, c(1), round)

mypredict2


## Confustion Matrix for Test Set 
## Evaluation of model on test data
cpreds2 <- compute(nn2, test[ , !names(test) %in% c('Result')])


cpreds2 <- apply(cpreds2$net.result, 1, round)

## table(cpreds2, test$class)
table(cpreds2, test$Result, dnn=c("predicted", "actual") )
sum(cpreds2 == test$Result)/length(test$Result) 

   
#################################Round 3  10 by 10 Node########################################

nn3 <- neuralnet(my.formula, data = train, hidden = c(10,10), linear.output = FALSE, stepmax = 100e5)


names(nn3)
nn3$call
nn3$model.list

nn3$response

nn3$covariate

nn3$weights
nn3$startweights

nn3$result.matrix
nn3$net.result[[1]][1:10]

plot(nn3)

mypredict3<-compute(nn3, nn3$covariate)$net.result
mypredict3<-apply(mypredict3, c(1), round)

mypredict3

##Confusion Marix for Training Set 
table(mypredict3, train$Result)
table(mypredict3, train$Result, dnn=c("predicted", "actual"))
sum(mypredict3 == train$Result)/length(train$Result)

## Confustion Matrix for Test Set 
## Evaluation of model on test data
cpreds3 <- compute(nn3, test[ , !names(test) %in% c('Result')])


cpreds3 <- apply(cpreds3$net.result, 1, round)
## table(cpreds1, test$class)
table(cpreds3, test$Result, dnn=c("predicted", "actual") )
sum(cpreds3 == test$Result)/length(test$Result) 


#####################################Rouind 4 NN_BP#########################

nn4_bp <- neuralnet(my.formula, data = train, hidden = c(10,10),
                 learningrate = 0.50,
                 algorithm = "rprop-", 
                 linear.output = FALSE, 
                 stepmax = 100e5)
                

#nn4_sag <- neuralnet(my.formula, data = train, hidden = 2,
#                    learningrate = 0.01,
#                    algorithm = "slr", 
#                    linear.output = FALSE, 
#                    stepmax = 100e5)


names(nn4_bp)
nn4_bp$call
nn4_bp$model.list

nn4_bp$response

nn4_bp$covariate

nn4_bp$weights
nn4_bp$startweights

nn4_bp$result.matrix
nn4_bp$net.result[[1]][1:10]

plot(nn4_bp)

mypredict4<-compute(nn4_bp, nn4_bp$covariate)$net.result
mypredict4<-apply(mypredict4, c(1), round)

mypredict4

##Confusion Marix for Training Set 
table(mypredict4, train$Result)
table(mypredict4, train$Result, dnn=c("predicted", "actual"))
sum(mypredict4 == train$Result)/length(train$Result)

## Confustion Matrix for Test Set 
## Evaluation of model on test data
cpreds4 <- compute(nn4_bp, test[ , !names(test) %in% c('Result')])


cpreds4 <- apply(cpreds4$net.result, 1, round)
## table(cpreds4, test$class)
table(cpreds4, test$Result, dnn=c("predicted", "actual") )
sum(cpreds4 == test$Result)/length(test$Result) 






##########################RPROP-#############


nn5_rpn <- neuralnet(my.formula, data = train, hidden = c(10, 10)
                    learningrate = 0.50,
                    algorithm = "rprop-", 
                    linear.output = FALSE, 
                    stepmax = 100e5)




names(nn5_rpn)
nn5_rpn$call
nn5_rpn$model.list

nn5_rpn$response

nn5_rpn$covariate

nn5_rpn$weights
nn5_rpn$startweights

nn5_rpn$result.matrix
nn5_rpn$net.result[[1]][1:10]

plot(nn5_rpn)

mypredict5<-compute(nn5_rpn, nn5_rpn$covariate)$net.result
mypredict5<-apply(mypredict5, c(1), round)

mypredict5

##Confusion Marix for Training Set 
table(mypredict5, train$Result)
table(mypredict5, train$Result, dnn=c("predicted", "actual"))
sum(mypredict5 == train$Result)/length(train$Result)

## Confustion Matrix for Test Set 
## Evaluation of model on test data
cpreds5 <- compute(nn5_rpn, test[ , !names(test) %in% c('Result')])


cpreds5 <- apply(cpreds5$net.result, 1, round)
## table(cpreds5, test$class)
table(cpreds5, test$Result, dnn=c("predicted", "actual") )
sum(cpreds5 == test$Result)/length(test$Result) 






#########################################backprop with 10 hidden

nn6_bp <- neuralnet(my.formula, data = train, hidden = 2,
                     learningrate = 0.01,
                     algorithm = "backprop", 
                     linear.output = FALSE, 
                     stepmax = 100e5)




names(nn6_bp)
nn6_bp$call
nn6_bp$model.list

nn6_bp$response

nn6_bp$covariate

nn6_bp$weights
nn6_bp$startweights

nn6_bp$result.matrix
nn6_bp$net.result[[1]][1:10]

plot(nn6_bp)

mypredict6<-compute(nn6_bp, nn6_bp$covariate)$net.result
mypredict6<-apply(mypredict6, c(1), round)

mypredict6

##Confusion Marix for Training Set 
table(mypredict6, train$Result)
table(mypredict6, train$Result, dnn=c("predicted", "actual"))
sum(mypredict6 == train$Result)/length(train$Result)

## Confustion Matrix for Test Set 
## Evaluation of model on test data
cpreds6 <- compute(nn6_bp, test[ , !names(test) %in% c('Result')])


cpreds6 <- apply(cpreds6$net.result, 1, round)
## table(cpreds5, test$class)
table(cpreds6, test$Result, dnn=c("predicted", "actual") )
sum(cpreds6 == test$Result)/length(test$Result) 









leakprofile(nn)
#create a pretty color vector for the bar plot
cols<-colorRampPalette(c('lightgreen','lightblue'))(ph2$Result)

#use the function on the model created above
par(mar=c(3,4,1,1),family='serif')
ph2$Result('y',nn,col=cols,ylab='Rel. importance',ylim=c(-1,1))