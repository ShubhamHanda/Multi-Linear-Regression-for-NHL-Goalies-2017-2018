# reading the file input add file path
autismClass_SH <- read.csv("C:/Users/Dell/Documents/Conestoga/mathematics for data anlytics/Assignment 4/AutismClass.csv")
#use sink to calculate table outputs
#sink("Shubham_results.txt")

# installing the required libraries
library(dplyr)
#install.packages("gclus")
library(gclus)

install.packages("pastecs")  
library("pastecs")   

if(!require(ggplot2)){install.packages("ggplot2")}
library('ggplot2')
if(!require(farff)){install.packages("farff")}
library("farff")

# just checking the unique values present in ethnicity

print(unique(autismClass_SH$Ethnicity))

# transforming the data by converting into numerical values 
#i have assignment each category a numerical value
#where as the missing values will automatically get 0

autismClass_SH$Ethnicity_SH <- '0' 
autismClass_SH$Ethnicity_SH[autismClass_SH$Ethnicity=='Black'] <- 1
autismClass_SH$Ethnicity_SH[autismClass_SH$Ethnicity=='Middle Eastern '] <- 2
autismClass_SH$Ethnicity_SH[autismClass_SH$Ethnicity=='South Asian'] <- 3
autismClass_SH$Ethnicity_SH[autismClass_SH$Ethnicity=='Asian'] <- 4
autismClass_SH$Ethnicity_SH[autismClass_SH$Ethnicity=='White-European'] <- 5
autismClass_SH$Ethnicity_SH[autismClass_SH$Ethnicity=='Hispanic'] <- 6
autismClass_SH$Ethnicity_SH[autismClass_SH$Ethnicity=='Turkish'] <- 7
autismClass_SH$Ethnicity_SH[autismClass_SH$Ethnicity=='Latino'] <- 8
autismClass_SH$Ethnicity_SH[autismClass_SH$Ethnicity=='Pasifika'] <- 9
autismClass_SH$Ethnicity_SH[autismClass_SH$Ethnicity=='Others' || autismClass_SH$Ethnicity=="others"] <- 10
autismClass_SH$Ethnicity_SH = as.numeric(autismClass_SH$Ethnicity_SH)

autismClass_SH$Rel_SH <- 0
autismClass_SH$Rel_SH[autismClass_SH$Rel=='Self'] <- 1
autismClass_SH$Rel_SH[autismClass_SH$Rel=='Relative'] <- 2
autismClass_SH$Rel_SH[autismClass_SH$Rel=='Parent'] <- 3
autismClass_SH$Rel_SH[autismClass_SH$Rel=='Health care professional'] <- 4
autismClass_SH$Rel_SH[autismClass_SH$Rel=='Others'] <- 5
autismClass_SH$Rel_SH = as.numeric(autismClass_SH$Rel_SH)

autismClass_SH$gender_SH <- 0 
autismClass_SH$gender_SH[autismClass_SH$gender=='f'] <- 1
autismClass_SH$gender_SH = as.numeric(autismClass_SH$gender_SH)

autismClass_SH$Jaundic_SH <- 0 
autismClass_SH$Jaundic_SH[autismClass_SH$Jaundic=='yes'] <- 1
autismClass_SH$Jaundic_SH = as.numeric(autismClass_SH$Jaundic_SH)

autismClass_SH$app_SH <- 0 
autismClass_SH$app_SH[autismClass_SH$App=='yes'] <- 1
autismClass_SH$app_SH = as.numeric(autismClass_SH$app_SH)

autismClass_SH$autism_SH<- 0 
autismClass_SH$autism_SH[autismClass_SH$Autism=='YES'] <- 1
autismClass_SH$autism_SH = as.numeric(autismClass_SH$autism_SH)

# where the age is na i have replaced with the median value by calculating it
# this is done because age cannot be replaced with 0 or any other hypothetical value
# as it will affect the data very much

autismClass_SH$Age_SH = as.numeric(autismClass_SH$Age)
autismClass_SH[["Age_SH"]][is.na(autismClass_SH[["Age_SH"]])] <- 27

# calculating summary of the table

summary(autismClass_SH)
stat.desc(autismClass_SH)

# calculating all the univariante graphs of all variables which have numeric types

# histogram

par(mfrow=c(2,2)) 

sapply(names(autismClass_SH), function(cname){
  if (is.numeric(autismClass_SH[[cname]]))
    print(hist(autismClass_SH[[cname]], main=cname))
})

par(mfrow=c(1,1))

# boxplot

par(mfrow=c(2,2)) 

sapply(names(autismClass_SH), function(cname){
  if (is.numeric(autismClass_SH[[cname]]))
    print(boxplot(autismClass_SH[[cname]], main=cname))
})

par(mfrow=c(1,1))

#barplot , not a good graph to use in this case

par(mfrow=c(2,2)) 

sapply(names(autismClass_SH), function(cname){
  if (is.numeric(autismClass_SH[[cname]]))
    print(barplot(autismClass_SH[[cname]], main=cname))
})

par(mfrow=c(1,1))

# scatterplot can be used to identify relationships
# see age it as one outlier

par(mfrow=c(2,2)) 

sapply(names(autismClass_SH), function(cname){
  if (is.numeric(autismClass_SH[[cname]]))
    print(plot(autismClass_SH[[cname]], main=cname))
})

par(mfrow=c(1,1))


par(mfrow=c(2,2)) 

# running normalcy tests for all variables

shapiro.test(autismClass_SH$X.1)
shapiro.test(autismClass_SH$X)
shapiro.test(autismClass_SH$A01)
shapiro.test(autismClass_SH$A02)
shapiro.test(autismClass_SH$A03)
shapiro.test(autismClass_SH$A04)
shapiro.test(autismClass_SH$A05)
shapiro.test(autismClass_SH$A06)
shapiro.test(autismClass_SH$A07)
shapiro.test(autismClass_SH$A08)
shapiro.test(autismClass_SH$A09)
shapiro.test(autismClass_SH$A10)
shapiro.test(autismClass_SH$Age)
shapiro.test(autismClass_SH$gender_SH)
shapiro.test(autismClass_SH$app_SH)
shapiro.test(autismClass_SH$autism_SH)
shapiro.test(autismClass_SH$Jaundic_SH)
shapiro.test(autismClass_SH$Res)
shapiro.test(autismClass_SH$Ethnicity_SH)
shapiro.test(autismClass_SH$Rel_SH)

# independence tests

chisq.test(autismClass_SH$X.1)
chisq.test(autismClass_SH$X)
chisq.test(autismClass_SH$A01)
chisq.test(autismClass_SH$A02)
chisq.test(autismClass_SH$A03)
chisq.test(autismClass_SH$A04)
chisq.test(autismClass_SH$A05)
chisq.test(autismClass_SH$A06)
chisq.test(autismClass_SH$A07)
chisq.test(autismClass_SH$A08)
chisq.test(autismClass_SH$A09)
chisq.test(autismClass_SH$A10)
chisq.test(autismClass_SH$Age_SH)
chisq.test(autismClass_SH$gender_SH)
chisq.test(autismClass_SH$app_SH)
chisq.test(autismClass_SH$autism_SH)
chisq.test(autismClass_SH$Jaundic_SH)
chisq.test(autismClass_SH$Res)
chisq.test(autismClass_SH$Ethnicity_SH)
chisq.test(autismClass_SH$Rel_SH)

# entering all the  numerical variables in a new table , this will help us calculate
# multiple correlations and model data without selecting variables one at a time

str(autismClass_SH)
NEW_autismClass_SH <- autismClass_SH %>%
  select(autism_SH,Jaundic_SH,gender_SH,X.1,X,A01,A02,A03,A04,A05,A06,A07,A08,A09,A10,Age_SH,app_SH,Ethnicity_SH,Rel_SH,Res)

par(mfrow=c(2,2)) 

# calculating outliers
# boxplots can easily tell  us outliers graphically

sapply(names(NEW_autismClass_SH), function(cname){
  print(boxplot(autismClass_SH[[cname]], main=cname))
})

par(mfrow=c(1,1))

# calculating all the outliers
# by printing all the values which are > 98 % 
# i have assigned these values with a random value 

outliers_age_SH<-quantile(NEW_autismClass_SH$Age_SH,0.98)
OUTLIER_TABLE_NEW_autismClass_SH <- as.data.frame(NEW_autismClass_SH$Age_SH)
OUTLIER_TABLE_NEW_autismClass_SH$Outlier <- 'NO'
OUTLIER_TABLE_NEW_autismClass_SH$Outlier[NEW_autismClass_SH$Age_SH> outliers_age_SH] <-'YES'

NEW_autismClass_SH$Age_SH <- with(NEW_autismClass_SH, ifelse(Age_SH >=55, 55,Age_SH))

# plotting after removal of  age 
boxplot(NEW_autismClass_SH$Age_SH)
# calculating co relations for all the variables

cor_SH <- cor(NEW_autismClass_SH, method='spearman')
round(cor_SH, 2)

# calculating co realtions graphically

Cor_Sh_col<-NEW_autismClass_SH
autismClass_SH.r <- abs(cor(Cor_Sh_col))                  
autismClass_SH.c <- dmat.color(autismClass_SH.r)              
autismClass_SH.o <- order.single(autismClass_SH.r)          
cpairs(Cor_Sh_col, panel.colors=autismClass_SH.c, gap=.5, main="Key Variables Ordered and Coloured by Correlation")

# as you can see from co realtions A05 A06 A09 Res have the highest impact on corelations

# calculating predictions on effect of each impactful variable 
# corelations with categorical data- grpahical

cross <- table(NEW_autismClass_SH$Res, NEW_autismClass_SH$autism_SH)

barplot(prop.table(cross,2), xlab='Class',ylab='Frequency',main="effect of autisim by predictor Res",
        col=c("darkblue","darkred")
        ,legend=rownames(cross), args.legend = list(x = "topleft"))


table(NEW_autismClass_SH$Res, NEW_autismClass_SH$autism_SH)   # contigency table
prop.table(table(NEW_autismClass_SH$Res, NEW_autismClass_SH$autism_SH), margin=1)*100 
summary(table(NEW_autismClass_SH$Res, NEW_autismClass_SH$autism_SH))
chisq.test(NEW_autismClass_SH$Res, NEW_autismClass_SH$autism_SH) # running chi sq test to check dependecy

# Effect of A05 on autism graphically

cross <- table(NEW_autismClass_SH$A05, NEW_autismClass_SH$autism_SH)

barplot(prop.table(cross,2), xlab='Class',ylab='Frequency',main="effect of autisim by predictor A05",
        col=c("darkblue","darkred")
        ,legend=rownames(cross), args.legend = list(x = "topleft"))

table(NEW_autismClass_SH$A05, NEW_autismClass_SH$autism_SH)   #Contingency Table
prop.table(table(NEW_autismClass_SH$A05, NEW_autismClass_SH$autism_SH), margin=1)*100  #Contingency Table Pct.
summary(table(NEW_autismClass_SH$A05, NEW_autismClass_SH$autism_SH))   #Chi-Sq
chisq.test(NEW_autismClass_SH$A05, NEW_autismClass_SH$autism_SH) 

# Effect of A06 on autism graphically

cross_1 <- table(NEW_autismClass_SH$A06, NEW_autismClass_SH$autism_SH)

barplot(prop.table(cross_1,2), xlab='Class',ylab='Frequency',main="effect of autisim by predictor A05",
        col=c("darkblue","darkred")
        ,legend=rownames(cross_1), args.legend = list(x = "topleft"))

table(NEW_autismClass_SH$A06, NEW_autismClass_SH$autism_SH)
prop.table(table(NEW_autismClass_SH$A06, NEW_autismClass_SH$autism_SH), margin=1)*100  #Contingency Table Pct.
summary(table(NEW_autismClass_SH$A06, NEW_autismClass_SH$autism_SH))
chisq.test(NEW_autismClass_SH$A06, NEW_autismClass_SH$autism_SH) 

# as age is not categorical effect is calculated using Age on autism

boxplot(Age_SH~autism_SH, data=NEW_autismClass_SH ) 




# Effect of A09 on autism graphically

cross_2 <- table(NEW_autismClass_SH$A09, NEW_autismClass_SH$autism_SH)

barplot(prop.table(cross_2,2), xlab='Class',ylab='Frequency',main="effect of autisim by predictor A09",
        col=c("darkblue","darkred")
        ,legend=rownames(cross_2), args.legend = list(x = "topleft"))

table(NEW_autismClass_SH$A09, NEW_autismClass_SH$autism_SH) 
prop.table(table(NEW_autismClass_SH$A09, NEW_autismClass_SH$autism_SH), margin=1)*100  #Contingency Table Pct.
summary(table(NEW_autismClass_SH$A09, NEW_autismClass_SH$autism_SH))
chisq.test(NEW_autismClass_SH$A09, NEW_autismClass_SH$autism_SH) 

### Res ###

cross_3 <- table(NEW_autismClass_SH$Res, NEW_autismClass_SH$autism_SH)

barplot(prop.table(cross_3,2), xlab='Class',ylab='Frequency',main="effect of autisim by predictor Res",
        col=c("darkblue","darkred")
        ,legend=rownames(cross), args.legend = list(x = "topleft"))

table(NEW_autismClass_SH$Res, NEW_autismClass_SH$autism_SH) 
prop.table(table(NEW_autismClass_SH$Res, NEW_autismClass_SH$autism_SH), margin=1)*100  #Contingency Table Pct.
summary(table(NEW_autismClass_SH$Res, NEW_autismClass_SH$autism_SH))
chisq.test(NEW_autismClass_SH$Res, NEW_autismClass_SH$autism_SH) 




dy = density(NEW_autismClass_SH$autism_SH[NEW_autismClass_SH$A09==1], from=min(NEW_autismClass_SH$autism_SH), to=max(NEW_autismClass_SH$autism_SH))
dn = density(NEW_autismClass_SH$autism_SH[NEW_autismClass_SH$A09==0], from=min(NEW_autismClass_SH$autism_SH), to=max(NEW_autismClass_SH$autism_SH))
sm = dy$y + dn$y

par(mfrow=c(3,1))

par(mfrow=c(3,1))
autism_f <- factor(NEW_autismClass_SH$autism_SH, labels = c("1", "0"))
autism_f
a09_f <- factor(NEW_autismClass_SH$A09, labels = c("1", "0"))
a09_f
spineplot(a09_f~autism_f, main="Splineplot")
#Spline Plot  can help us calculate graphical
 
# density plot
autism_f <- factor(NEW_autismClass_SH$autism_SH, labels = c("1", "0"))
autism_f
a09_f <- factor(NEW_autismClass_SH$A09, labels = c("1", "0"))
a09_f
plot(dy, xlim=range(NEW_autismClass_SH$autism_SH), ylim=c(0, 0.4), col="red", main="Densities",
     xlab="Mtha")
lines(dn, col="blue")
lines(dy$x, sm, col="black")
legend("topright", legend=c("Autism yes","NO","Sum"), lty=1, 
       col=c("red","blue","black"))
##Conditional Density
autism_f <- factor(NEW_autismClass_SH$autism_SH, labels = c("1", "0"))
autism_f
a09_f <- factor(NEW_autismClass_SH$A09, labels = c("1", "0"))
a09_f
cdplot(a09_f~autism_f, main="Conditional density plot")
par(mfrow=c(1,1))


####Res####

dy = density(NEW_autismClass_SH$autism_SH[NEW_autismClass_SH$A05==1], from=min(NEW_autismClass_SH$autism_SH), to=max(NEW_autismClass_SH$autism_SH))
dn = density(NEW_autismClass_SH$autism_SH[NEW_autismClass_SH$A05==0], from=min(NEW_autismClass_SH$autism_SH), to=max(NEW_autismClass_SH$autism_SH))
sm = dy$y + dn$y

par(mfrow=c(3,1))

par(mfrow=c(3,1))
autism_f <- factor(NEW_autismClass_SH$autism_SH, labels = c("1", "0"))
autism_f
A05_f <- factor(NEW_autismClass_SH$A05, labels = c("1", "12"))
A05_f
spineplot(A05_f~autism_f, main="Splineplot")
#Spline Plot  can help us calculate graphical

# density plot
autism_f <- factor(NEW_autismClass_SH$autism_SH, labels = c("1", "0"))
autism_f
a05_f <- factor(NEW_autismClass_SH$A05, labels = c("1", "0"))
a05_f
plot(dy, xlim=range(NEW_autismClass_SH$autism_SH), ylim=c(0, 0.4), col="red", main="autism",
     xlab="Mtha")
lines(dn, col="blue")
lines(dy$x, sm, col="black")
legend("topright", legend=c("autism yes","autism no","Sum"), lty=1, 
       col=c("red","blue","black"))
##Conditional Density
autism_f <- factor(NEW_autismClass_SH$autism_SH, labels = c("1", "0"))
autism_f
a05_f <- factor(NEW_autismClass_SH$A05, labels = c("1", "0"))
a05_f
cdplot(a05_f~autism_f, main="Conditional density plot")
par(mfrow=c(1,1))



# baseline model  with all variables
 
autism_lm = glm(autism_SH~+Jaundic_SH+gender_SH+Res+X.1+X+A01+A02+A03+A04+A05+A06+A07+A08+A09+A10+Age_SH+app_SH+Ethnicity_SH+Rel_SH,
            data=NEW_autismClass_SH, na.action=na.omit)
autism_lm
summary(autism_lm)

autism_SH_fit<-predict(autism_lm)
autism_SH_res<-residuals(autism_lm)


par(mfrow = c(2, 2))  
plot(autism_lm) 
par(mfrow = c(1, 1))  

shapiro.test(autism_SH_fit)


# backward
Bck_autism_lm = step(autism_lm, direction="backward")

Bck_autism_lm
summary(Bck_autism_lm)


Bck_autism_SH_fit<-predict(Bck_autism_lm)
Bck_autism_SH_res<-residuals(Bck_autism_lm)


par(mfrow = c(2, 2))  
plot(Bck_autism_lm) 
par(mfrow = c(1, 1))  

shapiro.test(Bck_autism_SH_fit)

# forward
Fwd_autism_lm = step(autism_lm, direction="forward")

Fwd_autism_lm
summary(Fwd_autism_lm)

Fwd_autism_SH_fit<-predict(Fwd_autism_lm)
Fwd_autism_SH_res<-residuals(Fwd_autism_lm)


par(mfrow = c(2, 2))  
plot(Fwd_autism_lm) 
par(mfrow = c(1, 1))  

shapiro.test(Fwd_autism_SH_fit)

# model by manually omiting variables x and x.1

autism_lm_1 = glm(autism_SH~gender_SH+Age_SH+A01+A02+A03+A04+A05+A06+A07+A08+A09+A10+Age_SH+Ethnicity_SH+Res,
                  data=NEW_autismClass_SH, na.action=na.omit)
autism_lm_1
summary(autism_lm_1)

autism_SH_fit_1<-predict(autism_lm_1)
autism_SH_res_1<-residuals(autism_lm_1)


par(mfrow = c(2, 2))  
plot(autism_lm_1) 
par(mfrow = c(1, 1))  

shapiro.test(autism_SH_fit_1)

# stepwise criteria model 

autism_lm_2<-step(autism_lm_1)
autism_lm_2
summary(autism_lm_2)


autism_SH_fit_2<-predict(autism_lm_2)
autism_SH_res_2<-residuals(autism_lm_2)


par(mfrow = c(2, 2))  
plot(autism_lm_2) 
par(mfrow = c(1, 1))  

shapiro.test(autism_SH_fit_2)

# manually calculating the stepwise criteria that is calculating which variable must have been removed

autism_lm_3 = glm(autism_SH~gender_SH++A01+A02+A03+A04+A05+A06+A07+A08+A09+A10+Age_SH,
                  data=NEW_autismClass_SH, na.action=na.omit)
autism_lm_3
summary(autism_lm_3)

#removing x1 too and res because Res is highly correlated and might cause overfitting of data 
# i first included res then saw that while evaluation it was giving highly over perfect model 
# hence i removed it
autism_lm_4 = glm(autism_SH~gender_SH+A01+A02+A03+A04+A05+A06+A07+A08+A09+A10+Age_SH,
                  data=NEW_autismClass_SH, na.action=na.omit)
autism_lm_4
summary(autism_lm_4)

autism_SH_fit_4<-predict(autism_lm_4)
autism_SH_res_4<-residuals(autism_lm_4)


par(mfrow = c(2, 2))  
plot(autism_lm_4) 
par(mfrow = c(1, 1))  

shapiro.test(autism_SH_fit_4)

# i chose my model 5 i.e autism_SH_res_3 because it had the least no of variables
# and the least aic and the deviance is very low
# also res was highly co related so was making 
# a lot of variables removal was checked before choosing a final model
# evaluating models and making predictions
# making classifications


pred <- predict(autism_lm_3, newdata=NEW_autismClass_SH)
pred_y <- as.numeric(pred > 0)
true_y <- as.numeric(NEW_autismClass_SH$autism_SH==1)
true_pos <- (true_y==1) & (pred_y==1)
true_neg <- (true_y==0) & (pred_y==0)
false_pos <- (true_y==0) & (pred_y==1)
false_neg <- (true_y==1) & (pred_y==0)

conf_mat <- matrix(c(sum(true_pos), sum(false_pos),
                     sum(false_neg), sum(true_neg)),2,2)
colnames(conf_mat) <- c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat) <- c('Y = 1', 'Y = 0')
conf_mat

# precision recall specificty
autism_lm_3$Accuracy <- (conf_mat[1,1]+conf_mat[2,1])/sum(conf_mat)
autism_lm_3$Precis <- conf_mat[1,1]/sum(conf_mat[,1])
autism_lm_3$Recall <- conf_mat[1,1]/sum(conf_mat[1,])
autism_lm_3$Specif <- conf_mat[2,2]/sum(conf_mat[2,])

predict(autism_lm_3,NEW_autismClass_SH,type="response")
# roc Curve
# the bigger the slope the better

idx <- order(-pred)
recall <- cumsum(true_y[idx]==1)/sum(true_y==1)
specificity <- (sum(true_y==0) - cumsum(true_y[idx]==0))/sum(true_y==0)
roc_df <- data.frame(recall = recall, specificity = specificity)
ggplot(roc_df, aes(x=specificity, y=recall)) +
  geom_line(color='red') + 
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  geom_line(data=data.frame(x=(0:100)/100), aes(x=x, y=1-x), 
            linetype='dashed', color='blue')

# auc
# the higher the value the better it is 


AUC<- sum(roc_df$recall[-1] * diff(1-roc_df$specificity))
AUC


# evaluating general baseline model



pred_1 <- predict(autism_lm, newdata=NEW_autismClass_SH)
pred_y_1 <- as.numeric(pred_1 > 0)
true_y_1 <- as.numeric(NEW_autismClass_SH$autism_SH==1)
true_pos_1 <- (true_y_1==1) & (pred_y_1==1)
true_neg_1 <- (true_y_1==0) & (pred_y_1==0)
false_pos_1 <- (true_y_1==0) & (pred_y_1==1)
false_neg_1 <- (true_y_1==1) & (pred_y_1==0)

conf_mat_1 <- matrix(c(sum(true_pos), sum(false_pos),
                     sum(false_neg), sum(true_neg)),2,2)
colnames(conf_mat_1) <- c('Yhat = 1', 'Yhat = 0')
rownames(conf_mat_1) <- c('Y = 1', 'Y = 0')
conf_mat_1
predict(autism_lm,NEW_autismClass_SH,type="response")

# precision recall specificty
autism_lm$Accuracy <- (conf_mat[1,1]+conf_mat[2,1])/sum(conf_mat)
autism_lm$Precis <- conf_mat[1,1]/sum(conf_mat[,1])
autism_lm$Recall <- conf_mat[1,1]/sum(conf_mat[1,])
autism_lm$Specif <- conf_mat[2,2]/sum(conf_mat[2,])

# roc Curve
# the bigger the slope the better

idx_1 <- order(-pred)
recall_1 <- cumsum(true_y_1[idx_1]==1)/sum(true_y_1==1)
specificity_1 <- (sum(true_y_1==0) - cumsum(true_y_1[idx_1]==0))/sum(true_y_1==0)
roc_df_1 <- data.frame(recall = recall, specificity = specificity)
ggplot(roc_df_1, aes(x=specificity, y=recall)) +
  geom_line(color='red') + 
  scale_x_reverse(expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  geom_line(data=data.frame(x=(0:100)/100), aes(x=x, y=1-x), 
            linetype='dashed', color='blue')

# auc
# the higher the value the better it is 


AUC_1<- sum(roc_df_1$recall[-1] * diff(1-roc_df_1$specificity))
AUC_1



# general baseline model and model 5 were evaluated 
# it was seen that # model 5 is better because it has higher accuracy and less aic
# also deviance is less hence that model is better

#sink()


