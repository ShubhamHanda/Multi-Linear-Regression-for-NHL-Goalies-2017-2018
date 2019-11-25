# reading the file input add file path
NHL_SH <- read.csv("C:/Users/Dell/Documents/Conestoga/mathematics for data anlytics/Assignment 3/NHL Goalies 2017-18.csv")
sink("Shubham_results.txt")
trimws(NHL_SH)
library(dplyr)
install.packages("gclus")
library(gclus)


#Q1
#entering new columns transporting data
colnames(NHL_SH)[1] <- "Serial_Num"  
colnames(NHL_SH)[29] <- "SV_Per"
colnames(NHL_SH)[18] <- "Team"

NHL_SH$Salary = as.numeric(gsub('[$,]',"", NHL_SH$Salary)) #removing $ sign and converting into numeric values
NHL_SH$Salary[is.na(NHL_SH$Salary)] <- 0  #where salary is null 

# Assigning nationality into two categories
NHL_SH$CAN_Nationality_SH <- '0' 
NHL_SH$CAN_Nationality_SH[NHL_SH$Nat=='CAN'] <- '1' 

# assigning canadian teams and non into two categories   

NHL_SH$CAN_Team_SH <- '0'
NHL_SH$CAN_Team_SH[NHL_SH$Team=="WPG" | NHL_SH$Team=="EDM" | NHL_SH$Team=="TOR" | NHL_SH$Team=="MTL" | NHL_SH$Team=="VAN" | NHL_SH$Team=="OTT" | NHL_SH$Team=="CGY"]<- '1'



Summary_table_CAN_SH<-NHL_SH%>%
  select(CAN_Nationality_SH,SV_Per, GAA, Salary)%>%
  group_by(CAN_Nationality_SH)%>% summarise(Mean_SVPER_SH = mean(SV_Per), MEAN_GAA_SH=mean(GAA), MEAN_SALARY_SH = mean(Salary))
summary(Summary_table_CAN_SH)

Summary_table_CAN_TEAM_SH<-NHL_SH%>%
  select(CAN_Team_SH,SV_Per, GAA, Salary)%>%
  group_by(CAN_Team_SH)%>% summarise(Mean_SVPER_SH = mean(SV_Per), MEAN_GAA_SH=mean(GAA), MEAN_SALARY_SH = mean(Salary))
summary(Summary_table_CAN_TEAM_SH)

#summary charts grpahically

par(mfrow = c(3, 3))

plot(NHL_SH$Salary~NHL_SH$CAN_Nationality_SH,main="salary vs nationality")
plot(NHL_SH$Salary~NHL_SH$CAN_Team_SH,main="salary vs team")
plot(NHL_SH$GAA~NHL_SH$CAN_Nationality_SH,main="GAA vs nationality")
plot(NHL_SH$GAA~NHL_SH$CAN_Team_SH,main="GAA VS main")
plot(NHL_SH$SV_Per~NHL_SH$CAN_Nationality_SH,main="sv_per vs nationality")
plot(NHL_SH$SV_Per~NHL_SH$CAN_Team_SH,main="sv_per vs team")

par(mfrow = c(1, 1))

par(mfrow = c(3, 3))

boxplot(NHL_SH$Salary~NHL_SH$CAN_Nationality_SH,main="salary vs nationality")
boxplot(NHL_SH$Salary~NHL_SH$CAN_Team_SH,main="salary vs team")
boxplot(NHL_SH$GAA~NHL_SH$CAN_Nationality_SH,main="GAA vs nationality")
boxplot(NHL_SH$GAA~NHL_SH$CAN_Team_SH,main="GAA VS main")
boxplot(NHL_SH$SV_Per~NHL_SH$CAN_Nationality_SH,main="sv_per vs nationality")
boxplot(NHL_SH$SV_Per~NHL_SH$CAN_Team_SH,main="sv_per vs team")

par(mfrow = c(1, 1))

# calculating whether salary is normally distrubuted
qqnorm(NHL_SH$Salary,main='Salary is normal?')
qqline(NHL_SH$Salary)
# as data seems to be normal we can run t test

nhl_ttest_nat<-var.test(Salary~CAN_Nationality_SH,data=NHL_SH)
nhl_ttest_nat
#p value > 0.05 there is no significant difference

nhl_ttest<-var.test(Salary~CAN_Team_SH,data=NHL_SH)
nhl_ttest

#p value > 0.05 there is no significant difference

qqnorm(NHL_SH$GAA,main=' GAA is normal?')
qqline(NHL_SH$GAA)

# as data not normal

nhl_ttest_nat_gaa<-var.test(GAA~CAN_Nationality_SH,data=NHL_SH)
nhl_ttest_nat_gaa
#p value > 0.05 there is no significant difference

nhl_ttest_gaa<-var.test(GAA~CAN_Team_SH,data=NHL_SH)
nhl_ttest_gaa

# there is difference as <0.05

qqnorm(NHL_SH$SV_Per,main='SV_per is normal?')
qqline(NHL_SH$SV_Per)

# looks normal

nhl_ttest_nat_sv<-var.test(SV_Per~CAN_Nationality_SH,data=NHL_SH)
nhl_ttest_nat_sv
#p value > 0.05 there is no significant difference

nhl_ttest_sv<-var.test(SV_Per~CAN_Team_SH,data=NHL_SH)
nhl_ttest_sv

# there is no difference as >0.5

# calculating mean difference just to verify

Differnce_MEAN_SVPER_CAN_SH<- Summary_table_CAN_SH[1,2] - Summary_table_CAN_TEAM_SH[1,2]
Differnce_MEAN_GAA_CAN_SH <- Summary_table_CAN_SH[1,3] - Summary_table_CAN_TEAM_SH[1,3]
Differnce_MEAN_SALARY_CAN_SH <- Summary_table_CAN_SH[1,4] - Summary_table_CAN_TEAM_SH[1,4]

Differnce_MEAN_SVPER_NONCAN_SH <- Summary_table_CAN_SH[2,2] - Summary_table_CAN_TEAM_SH[2,2]
Differnce_MEAN_GAA_NONCAN_SH <- Summary_table_CAN_SH[2,3] - Summary_table_CAN_TEAM_SH[2,3]
Differnce_MEAN_SALARY_NONCAN_SH <- Summary_table_CAN_SH[2,3] - Summary_table_CAN_TEAM_SH[2,3]


#linear regression between salary of nationality and salary of other nationality


lr_SH<-lm(Salary[NHL_SH$CAN_Nationality_SH=='0']~Salary[NHL_SH$CAN_Nationality_SH=='1'], data=NHL_SH[2:50,],na.action=na.omit)
lr_SH
summary(lr_SH)
plot(lr_SH)


par(mfrow = c(1, 1))


#Q2 transforming data

NHL_SH$JULIAN_SH <- as.Date(NHL_SH$DOB,format="%Y/%m/%d")
NHL_SH$JULIAN_SH <- julian(NHL_SH$JULIAN_SH)+10000

NHL_SH$SHOOTS_SH <- '0' 
NHL_SH$SHOOTS_SH[NHL_SH$Sh=='R'] <- '1'

NHL_SH$STATUS_INT_SH <- '0' 
NHL_SH$STATUS_INT_SH[NHL_SH$Status=='RFA'] <- '1'

NHL_SH$AGENT_INT_SH <- '0' 
NHL_SH$AGENT_INT_SH[NHL_SH$Agt=='Johnson'] <- '1'
NHL_SH$AGENT_INT_SH[NHL_SH$Agt=='Kleinfelder'] <- '2'
NHL_SH$AGENT_INT_SH[NHL_SH$Agt=='Mortensen'] <- '3'
NHL_SH$AGENT_INT_SH[NHL_SH$Agt=='Pace'] <- '4'
NHL_SH$AGENT_INT_SH[NHL_SH$Agt=='Samuel'] <- '5'
NHL_SH$AGENT_INT_SH[NHL_SH$Agt=='Smith'] <- '6'
NHL_SH$AGENT_INT_SH[NHL_SH$Agt=='Twickerson'] <- '7'



# only entering those values where gp>2

NEW_NHL_SH <- NHL_SH %>%
  select(GP,JULIAN_SH,CAN_Nationality_SH,Ht,Wt,SHOOTS_SH,Debut,Seas,SV_Per,GAA,STATUS_INT_SH,Salary,AGENT_INT_SH)
NEW_NHL_SH <- filter(NEW_NHL_SH, GP > 2)

NEW_NHL_SH <- NEW_NHL_SH %>%
  select(JULIAN_SH,CAN_Nationality_SH,Ht,Wt,SHOOTS_SH,Debut,Seas,SV_Per,GAA,STATUS_INT_SH,Salary,AGENT_INT_SH)

NEW_NHL_SH$CAN_Nationality_SH <- as.numeric(gsub('[$,]', '', NEW_NHL_SH$CAN_Nationality_SH))
NEW_NHL_SH$AGENT_INT_SH <- as.numeric(gsub('[$,]', '', NEW_NHL_SH$AGENT_INT_SH))
NEW_NHL_SH$STATUS_INT_SH <- as.numeric(gsub('[$,]', '', NEW_NHL_SH$STATUS_INT_SH))
NEW_NHL_SH$SHOOTS_SH <- as.numeric(gsub('[$,]', '', NEW_NHL_SH$SHOOTS_SH))
str(NEW_NHL_SH)

summary(NEW_NHL_SH)

#Q3

par(mfrow=c(3,3))   


sapply(names(NEW_NHL_SH), function(cname){
  if (is.numeric(NEW_NHL_SH[[cname]] ))
    print(hist(NEW_NHL_SH[[cname]], main=cname))
})

par(mfrow=c(1,1))


cor_sh <- cor(NEW_NHL_SH, method='spearman')
round(cor_sh, 2)


cor_sh_col <- NEW_NHL_SH                    #Get subset of Denver
cor_sh_col.r <- abs(cor(cor_sh_col))                  #Create Correlations
cor_sh_col.c <- dmat.color(cor_sh_col.r)              #Assign Colors - NOTE - 657 colours in HEX
cor_sh_col.o <- order.single(cor_sh_col.r)            #Reorders variables by correlation
cpairs(cor_sh_col, cor_sh_col.o, panel.colors=cor_sh_col.c, gap=.5, main="Key Variables Ordered and Coloured by Correlation")

par(mfrow=c(3,3))


sapply(names(NEW_NHL_SH), function(cname){
  if (is.numeric(NEW_NHL_SH[[cname]]))
    print(boxplot(NEW_NHL_SH[[cname]], main=cname))
})

#checking for normalacy

shapiro.test(NEW_NHL_SH$Salary)
shapiro.test(NEW_NHL_SH$GAA)
shapiro.test(NEW_NHL_SH$SV_PER)
shapiro.test(NEW_NHL_SH$Wt)
shapiro.test(NEW_NHL_SH$SHOOTS_SH)
shapiro.test(NEW_NHL_SH$Ht)
shapiro.test(NEW_NHL_SH$Seas)


par(mfrow=c(3,3))

#Graphical Tests
qqnorm(NEW_NHL_SH$Salary, main="Salary")
qqline(NEW_NHL_SH$Salary)

qqnorm(NEW_NHL_SH$GAA, main="GAA")
qqline(NEW_NHL_SH$GAA)

qqnorm(NEW_NHL_SH$SV_Per, main="Sv_per")
qqline(NEW_NHL_SH$SV_Per)

qqnorm(NEW_NHL_SH$SHOOTS_SH, main="Shoots_SH")
qqline(NEW_NHL_SH$SHOOTS_SH)

qqnorm(NEW_NHL_SH$Ht, main="ht")
qqline(NEW_NHL_SH$Ht)

par(mfrow=c(1,1))


SV_PER_IQR_SH <- IQR(NEW_NHL_SH[['SV_Per']]) *1.5
SV_PER_Q1_SH <-0.902
SV_PER_Q3_SH <- 0.92
OUTLIER_TABLE_NEW_NHL_SH <- NEW_NHL_SH
OUTLIER_TABLE_NEW_NHL_SH$Outlier <- 'NO'
OUTLIER_TABLE_NEW_NHL_SH$Outlier[OUTLIER_TABLE_NEW_NHL_SH$SV_Per > SV_PER_IQR_SH + SV_PER_Q3_SH] <-'YES'
OUTLIER_TABLE_NEW_NHL_SH$Outlier[OUTLIER_TABLE_NEW_NHL_SH$SV_Per <  SV_PER_Q1_SH-SV_PER_IQR_SH] <-'YES'


#Q4

#baseline model for salary

NEW_NHL_SH_lm = lm(Salary ~ JULIAN_SH+CAN_Nationality_SH+Ht+Wt+SHOOTS_SH+Debut+Seas+SV_Per+GAA+STATUS_INT_SH+Salary+AGENT_INT_SH,data=NEW_NHL_SH, na.action=na.omit)

NEW_NHL_SH_lm
summary(NEW_NHL_SH)

#backward selection

Bck_NEW_NHL_SH_lm = step(NEW_NHL_SH_lm, direction="backward")

Bck_NEW_NHL_SH_lm
summary(Bck_NEW_NHL_SH_lm)

# forward selection
min_model <- lm(Salary ~ 1, data=NEW_NHL_SH, na.action=na.omit)
Fwd_NEW_NHL_SH_lm = step(min_model, direction="forward", scope =(
  ~ Salary + JULIAN_SH+CAN_Nationality_SH+Ht+Wt+SHOOTS_SH+Debut+Seas+SV_Per+GAA+STATUS_INT_SH+Salary+AGENT_INT_SH))

Fwd_NEW_NHL_SH_lm 
summary(Fwd_NEW_NHL_SH_lm)

# criteria based salary without shoots

NEW_NHL_SH_lm_c = lm(Salary ~ JULIAN_SH+CAN_Nationality_SH+Ht+Wt+Debut+Seas+SV_Per+GAA+STATUS_INT_SH+AGENT_INT_SH,data=NEW_NHL_SH, na.action=na.omit)

NEW_NHL_SH_lm_c
summary(NEW_NHL_SH_lm_c)

#predicting and plotting graphs
NHL_SH_fit<-predict(NEW_NHL_SH_lm)
NHL_SH_res<-residuals(NEW_NHL_SH_lm)

par(mfrow = c(2, 2))  
plot(NEW_NHL_SH_lm)  
par(mfrow = c(1, 1)) 

bck_NHL_SH_fit<-predict(Bck_NEW_NHL_SH_lm)
bck_NHL_SH_res<-residuals(Bck_NEW_NHL_SH_lm)

par(mfrow = c(2, 2))
plot(Bck_NEW_NHL_SH_lm)
par(mfrow = c(1, 1))

#forward selection

Fwd_NHL_SH_fit<-predict(Fwd_NEW_NHL_SH_lm)
Fwd_NHL_SH_res<-residuals(Fwd_NEW_NHL_SH_lm)


par(mfrow = c(2, 2))  
plot(Fwd_NEW_NHL_SH_lm)
par(mfrow = c(1, 1))

NEW_NHL_SH_lm_c_fit<-predict(NEW_NHL_SH_lm_c)
NEW_NHL_SH_lm_c_res<-residuals(NEW_NHL_SH_lm_c)

par(mfrow = c(2, 2))  
plot(NEW_NHL_SH_lm_c)
par(mfrow = c(1, 1))


shapiro.test(NEW_NHL_SH_lm_c_res)
shapiro.test(Fwd_NHL_SH_res)
shapiro.test(bck_NHL_SH_res)
shapiro.test(NHL_SH_res)
#baseline selection for Gaa

NEW_NHL_SH_gaa = lm(GAA ~ JULIAN_SH+CAN_Nationality_SH+Ht+Wt+SHOOTS_SH+Debut+Seas+SV_Per+STATUS_INT_SH+Salary+AGENT_INT_SH,data=NEW_NHL_SH, na.action=na.omit)

NEW_NHL_SH_gaa
summary(NEW_NHL_SH_gaa)

#forward selection for gaa

min_model_gaa <- lm(Salary ~ 1, data=NEW_NHL_SH, na.action=na.omit)
Fwd__NEW_NHL_SH_gaa = step(min_model, direction="forward", scope =(
  ~ GAA + JULIAN_SH+CAN_Nationality_SH+Ht+Wt+SHOOTS_SH+Debut+Seas+SV_Per+STATUS_INT_SH+Salary+AGENT_INT_SH))

Fwd__NEW_NHL_SH_gaa 
summary(Fwd__NEW_NHL_SH_gaa)


#backward selection for gaa

Bck_NEW_NHL_SH_gaa = step(NEW_NHL_SH_lm, direction="backward")

Bck_NEW_NHL_SH_gaa
summary(Bck_NEW_NHL_SH_gaa)

#criteria based selection for gaa without salary

NEW_NHL_SH_lm_c_gaa = lm(GAA ~ JULIAN_SH+CAN_Nationality_SH+Ht+Wt+Debut+Seas+SV_Per+STATUS_INT_SH+AGENT_INT_SH,data=NEW_NHL_SH, na.action=na.omit)

NEW_NHL_SH_lm_c_gaa
summary(NEW_NHL_SH_lm_c_gaa)


#check normalcy

Fwd__NEW_NHL_SH_gaa_fit<-predict(Fwd__NEW_NHL_SH_gaa)
Fwd__NEW_NHL_SH_gaa_res<-residuals(Fwd__NEW_NHL_SH_gaa)
par(mfrow = c(2, 2))  
plot(Fwd__NEW_NHL_SH_gaa)  
par(mfrow = c(1, 1))

Bck_NEW_NHL_SH_gaa_fit<-predict(Bck_NEW_NHL_SH_gaa)
Bck_NEW_NHL_SH_gaa_res<-residuals(Bck_NEW_NHL_SH_gaa)

par(mfrow = c(2, 2))  
plot(Bck_NEW_NHL_SH_gaa)  
par(mfrow = c(1, 1))  

NEW_NHL_SH_gaa_fit<-predict(NEW_NHL_SH_gaa)
NEW_NHL_SH_gaa_res<-residuals(NEW_NHL_SH_gaa)

par(mfrow = c(2, 2))  
plot(NEW_NHL_SH_gaa)
par(mfrow = c(1, 1))


NEW_NHL_SH_lm_c_gaa_fit<-predict(NEW_NHL_SH_lm_c_gaa)
NEW_NHL_SH_lm_c_gaa_res<-residuals(NEW_NHL_SH_lm_c_gaa)

par(mfrow = c(2, 2))  
plot(NEW_NHL_SH_lm_c_gaa)
par(mfrow = c(1, 1))


# to check how strong the value is

shapiro.test(NEW_NHL_SH_lm_c_gaa_res)
shapiro.test(NEW_NHL_SH_gaa_res)
shapiro.test(Bck_NEW_NHL_SH_gaa_res)
shapiro.test(Fwd__NEW_NHL_SH_gaa_res)




#another way of finding how strong our data is we can check how strongly independent our variables are
# this can be done through chi square test

chisq.test(NEW_NHL_SH)

#as the p value is very less we can say that all the variabes are independent

sink()

# end pls run by one 