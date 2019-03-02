# Importing the telecom final dataset from the directory
proj <- read.csv("telecomfinal.csv")
# OBJECTIVE:-
# To Analyze the Customer's Churn rate for the telecom company "MOBICOM" and 
# checking those factors which are responsible for the churning of customers.

# Creating a data quality report
library(dataQualityR)
num.file <- paste(tempdir(), "telecomfinal.csv", sep= "")
cat.file <- paste(tempdir(), "telecomfinal.csv", sep= "")
checkDataQuality(data= proj, out.file.num= num.file, out.file.cat= cat.file)

# Some basic sanity checks
library(dplyr)
options(scipen=999)

# Column names with serial number
colnames(proj)

# DECIDING TARGET VARIABLE
# Churn has to be calculated which is categorical variable with two input o and 1.
# which means "1"= churned and "o" = not churned
# TARGET VARIABLE or DEPENDENT VARIABLE = CHURN
summary(proj$churn)

# checking for unique values in each variable in proj
unique(proj)

# Getting descriptive statistics.
summary(proj)
# Some Variables have missing values.

# Checking for all missing values in all the columns
is.na(proj)

# Total missing values in complete dataset
sum(is.na(proj))

# To know the datatypes of all variables
str(proj)

# As we see csa, div type,childeren,carbuy,mailresp,proptype,solflag,workwoman
# occu1,mailorder,dwllsize,dwlltype, ethnic, marital, hndwebcap, refurb_new, area
# prizm_social_one, asl_flag, crclscod are factor variables.

#Retention calls include any calls from the customer regarding loyalty or retention, e.g. contract renewal, relating competitor's offer, etc.
# customers calling for complaining and company end up giving offers and retain them.
# Since retdays contins high missing values 
# missing value treatment for retdays and creating dummy variables
# some values in retdays have numeric numbers that accounts it to be a continuous variable

#Missing values for this variable can be assumed to mean there have been no retention calls made by the customer.				
summary(proj$retdays)
str(proj$retdays) # having integer form
is.na(proj$retdays)
sum(is.na(proj$retdays))
sort(unique(proj$retdays), na.last=F)
proj$retention <- ifelse(is.na(proj$retdays)==TRUE,0,1)
str(proj$retention) # having numeric forms
summary(proj$retention)
summary(proj$retdays)
# since new column retention has been added so checking for the column names
names(proj)

# After seeing the proportions of missing values in telecom data set,
# Removing variables which have the missing values greater than 20 percent
# and filtering it into a new dataset called "project"
project <- proj[,colMeans(is.na(proj))<=0.20]

# After looking into data dictionary
# BLCK_VCE_MEAN is not there which comes under a single variable drop_blck_mean
# drop_blk_mean = blck_dat_mean+BLCK_VCE_MEAN+DROP_DAT_MEAN+DROP_VCE_MEAN
# so taking blck_dat_mean makes no sense 
# removing blck_dat_mean
names(project)
project <- project[,-50]
names(project)

# So now project is my final dataset with 68 variables
# so factor variables now are csa,car_buy,ethnic,marital,hnd webcap,refurb_new,
#area,prizm social one,asl_flag,crclscod

# Exploring the data by doing profiling
# Deciling continuous variables basis target variable churn
str(project)

# RATIO OF 0's AND 1's IN COMPLETE DATASET
table(proj$churn)/nrow(proj)
table(project$churn)/nrow(project)
#cut off value for a customer likely to churn is 0.2392114
# finding the margin of churning which comes out to be 0.23%

#CONTINUOUS VARIABLE PROFILING AND DECILING
# profiling all the continuous variables according to churn variable
# Denoting all variables with x

# for variable mou_mean=x1 ##Mean number of monthly minutes of use

summary(project$mou_Mean) 
x1<- project%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
x1$N <- unclass(project%>%mutate(dec=ntile(mou_Mean,n=10))%>%count(dec)%>%unname())[[2]]
x1$perc_churn <- round(x1$n/x1$N,2)
x1$greaterthan <- unclass(project%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(min(mou_Mean)))[[2]]
x1$lesserthan <- unclass(project%>%mutate(dec=ntile(mou_Mean,n=10))%>%group_by(dec)%>%summarise(max(mou_Mean)))[[2]]
x1$variable <- rep("mou_mean",nrow(x1))
plot(project$mou_Mean,col="red")
boxplot(project$mou_Mean) 

# for variable totmrc_mean=x2 ##Monthly Recurring Charge is the base cost 
#of the calling plan regardless of actual minutes used.	

summary(project$totmrc_Mean)
x2<- project%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
x2$N <- unclass(project%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%count(dec)%>%unname())[[2]]
x2$perc_churn <- round(x2$n/x2$N,2)
x2$greaterthan <- unclass(project%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(min(totmrc_Mean)))[[2]]
x2$lesserthan <- unclass(project%>%mutate(dec=ntile(totmrc_Mean,n=10))%>%group_by(dec)%>%summarise(max(totmrc_Mean)))[[2]]
x2$variable <- rep("totmrc_mean",nrow(x2))
plot(project$totmrc_Mean)
boxplot(project$totmrc_Mean)

#for variable rev_range=x3 #Range of revenue (charge amount)

summary(project$rev_Range)
x3<- project%>%mutate(dec=ntile(rev_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
x3$N <- unclass(project%>%mutate(dec=ntile(rev_Range,n=10))%>%count(dec)%>%unname())[[2]]
x3$perc_churn <- round(x3$n/x3$N,2)
x3$greaterthan <- unclass(project%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(min(rev_Range)))[[2]]
x3$lesserthan <- unclass(project%>%mutate(dec=ntile(rev_Range,n=10))%>%group_by(dec)%>%summarise(max(rev_Range)))[[2]]
x3$variable <- rep("rev_range",nrow(x3))
plot(project$rev_Range)

# for variable mou_range=x4#Range of number of minutes of use

summary(project$mou_Range)
x4<- project%>%mutate(dec=ntile(mou_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
x4$N <- unclass(project%>%mutate(dec=ntile(mou_Range,n=10))%>%count(dec)%>%unname())[[2]]
x4$perc_churn <- round(x4$n/x4$N,2)
x4$greaterthan <- unclass(project%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_Range)))[[2]]
x4$lesserthan <- unclass(project%>%mutate(dec=ntile(mou_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_Range)))[[2]]
x4$variable <- rep("mou_range",nrow(x4))
plot(project$mou_Range)

# for variable change_mou =x5 $Percentage change in monthly minutes of use vs previous three month average

summary(project$change_mou)
x5<- project%>%mutate(dec=ntile(change_mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
x5$N <- unclass(project%>%mutate(dec=ntile(change_mou,n=10))%>%count(dec)%>%unname())[[2]]
x5$perc_churn <- round(x5$n/x5$N,2)
x5$greaterthan <- unclass(project%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(min(change_mou)))[[2]]
x5$lesserthan <- unclass(project%>%mutate(dec=ntile(change_mou,n=10))%>%group_by(dec)%>%summarise(max(change_mou)))[[2]]
x5$variable <- rep("change_mou",nrow(x5))
plot(project$change_mou)

# for variable drop_blk_mean =x6 #Mean number of dropped or blocked calls

summary(project$drop_blk_Mean)
x6<- project%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
x6$N <- unclass(project%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%count(dec)%>%unname())[[2]]
x6$perc_churn <- round(x6$n/x6$N,2)
x6$greaterthan <- unclass(project%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_blk_Mean)))[[2]]
x6$lesserthan <- unclass(project%>%mutate(dec=ntile(drop_blk_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_blk_Mean)))[[2]]
x6$variable <- rep("drop_blk_mean",nrow(x6))
plot(project$churn,project$drop_blk_Mean)
plot(project$drop_blk_Mean)

#for variable drop_vce_range=x7 # Range of number of dropped (failed) voice calls

summary(project$drop_vce_Range)
x7<- project%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
x7$N <- unclass(project%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
x7$perc_churn <- round(x7$n/x7$N,2)
x7$greaterthan <- unclass(project%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Range)))[[2]]
x7$lesserthan <- unclass(project%>%mutate(dec=ntile(drop_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Range)))[[2]]
x7$variable <- rep("drop_vce_range",nrow(x7))

# for variable owylis_vce_range=x8#Range of number of outbound wireless to wireless voice calls

summary(project$owylis_vce_Range)
x8<- project%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
x8$N <- unclass(project%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%count(dec)%>%unname())[[2]]
x8$perc_churn <- round(x8$n/x8$N,2)
x8$greaterthan <- unclass(project%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(min(owylis_vce_Range)))[[2]]
x8$lesserthan <- unclass(project%>%mutate(dec=ntile(owylis_vce_Range,n=10))%>%group_by(dec)%>%summarise(max(owylis_vce_Range)))[[2]]
x8$variable <- rep("owylis_vce_range",nrow(x8))

# for variable mou_opkv_range=x9 # Range of unrounded minutes of use of off-peak voice calls

summary(project$mou_opkv_Range)
x9<- project%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(churn,dec)%>%filter(churn==1)
x9$N <- unclass(project%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%count(dec)%>%unname())[[2]]
x9$perc_churn <- round(x9$n/x9$N,2)
x9$greaterthan <- unclass(project%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(min(mou_opkv_Range)))[[2]]
x9$lesserthan <- unclass(project%>%mutate(dec=ntile(mou_opkv_Range,n=10))%>%group_by(dec)%>%summarise(max(mou_opkv_Range)))[[2]]
x9$variable <- rep("mou_opkv'-range",nrow(x9))

# for variable months=x10 #Total number of months in service

summary(project$months)
x10<- project%>%mutate(dec=ntile(months,n=10))%>%count(churn,dec)%>%filter(churn==1)
x10$N <- unclass(project%>%mutate(dec=ntile(months,n=10))%>%count(dec)%>%unname())[[2]]
x10$perc_churn <- round(x10$n/x10$N,2)
x10$greaterthan <- unclass(project%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(min(months)))[[2]]
x10$lesserthan <- unclass(project%>%mutate(dec=ntile(months,n=10))%>%group_by(dec)%>%summarise(max(months)))[[2]]
x10$variable <- rep("months",nrow(x10))

# for variable totcalls=x11 # Total number of calls over the life of the customer

summary(project$totcalls)
x11<- project%>%mutate(dec=ntile(totcalls,n=10))%>%count(churn,dec)%>%filter(churn==1)
x11$N <- unclass(project%>%mutate(dec=ntile(totcalls,n=10))%>%count(dec)%>%unname())[[2]]
x11$perc_churn <- round(x11$n/x11$N,2)
x11$greaterthan <- unclass(project%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(min(totcalls)))[[2]]
x11$lesserthan <- unclass(project%>%mutate(dec=ntile(totcalls,n=10))%>%group_by(dec)%>%summarise(max(totcalls)))[[2]]
x11$variable <- rep("totcalls",nrow(x11))

# for variable eqpdays=x12 #Number of days (age) of current equipment

summary(project$eqpdays)
# removing the missing value value due to low number
misvalue <- which(is.na(project$eqpdays))
#now deleting the misvalue from project dataset
project <- project[-misvalue,]
summary(project$eqpdays)
x12 <- project%>%mutate(dec=ntile(eqpdays,n=10))%>%count(churn,dec)%>%filter(churn==1)
x12$N <- unclass(project%>%mutate(dec=ntile(eqpdays,n=10))%>%count(dec)%>%unname())[[2]]
x12$perc_churn <- round(x12$n/x12$N,2)
x12$greaterthan <- unclass(project%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(min(eqpdays)))[[2]]
x12$lesserthan <- unclass(project%>%mutate(dec=ntile(eqpdays,n=10))%>%group_by(dec)%>%summarise(max(eqpdays)))[[2]]
x12$variable <- rep("eqpdays",nrow(x12))
boxplot(project$eqpdays)

# for variable custcare_mean=x13 # Mean number of customer care calls
# less than 4 deciles

summary(project$custcare_Mean)
x13<- project%>%mutate(dec=ntile(custcare_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
x13$variable <- rep("custcare_mean",nrow(x13))
plot(project$custcare_Mean)
plot(project$churn,project$custcare_Mean,col="green")
hist(project$custcare_Mean)  # positive skewed

#for variable callwait_mean=x14 #Mean number of call waiting calls

summary(project$callwait_Mean)
x14<- project%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
x14$N <- unclass(project%>%mutate(dec=ntile(callwait_Mean,n=4))%>%count(dec)%>%unname())[[2]]
x14$perc_churn <- round(x14$n/x14$N,2)
x14$greaterthan <- unclass(project%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(min(callwait_Mean)))[[2]]
x14$lesserthan <- unclass(project%>%mutate(dec=ntile(callwait_Mean,n=4))%>%group_by(dec)%>%summarise(max(callwait_Mean)))[[2]]
x14$variable <- rep("callwait_mean",nrow(x14))

# for vriable iwylis_vce_mean=x15 #Mean number of inbound wireless to wireless voice calls

summary(project$iwylis_vce_Mean)
x15<- project%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(churn,dec)%>%filter(churn==1)
x15$N <- unclass(project%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%count(dec)%>%unname())[[2]]
x15$perc_churn <- round(x15$n/x15$N,2)
x15$greaterthan <- unclass(project%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(min(iwylis_vce_Mean)))[[2]]
x15$lesserthan <- unclass(project%>%mutate(dec=ntile(iwylis_vce_Mean,n=6))%>%group_by(dec)%>%summarise(max(iwylis_vce_Mean)))[[2]]
x15$variable <- rep("iwylis_vce_mean",nrow(x15)) # positive skewed
hist(project$iwylis_vce_Mean)

# for variable callwait_range=x16 # Range of number of call waiting calls
# median is zero which shows 50% is data is zero

summary(project$callwait_Range)
x16 <- project%>%mutate(dec=ntile(callwait_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)
x16$variable <- rep("callwait_range",nrow(x16))
plot(project$callwait_Mean)
hist(project$callwait_Range)# positive skewed

# for variable ccrndmou_range=x17 # Mean rounded minutes of use of customer care calls
# median is zero
# need to omitted
summary(project$ccrndmou_Range)
x17 <- project%>%mutate(dec=ntile(ccrndmou_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)
x17$varible <- rep("ccnrndmou_range",nrow(x17))

# for variable adjqty=x18 # Billing adjusted total number of calls over the life of the customer

summary(project$adjqty)
x18 <- project%>%mutate(dec=ntile(adjqty,n=10))%>%count(churn,dec)%>%filter(churn==1)
x18$N <- unclass(project%>%mutate(dec=ntile(adjqty,n=10))%>%count(dec)%>%unname())[[2]]
x18$perc_churn <- round(x18$n/x18$N,2)
x18$greaterthan <- unclass(project%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(min(adjqty)))[[2]]
x18$lesserthan <- unclass(project%>%mutate(dec=ntile(adjqty,n=10))%>%group_by(dec)%>%summarise(max(adjqty)))[[2]]
x18$variable <- rep("adjqty",nrow(x18))

# for variable  overrev_mean =x19 # Mean overage revenue

summary(project$ovrrev_Mean)
x19 <- project%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
x19$N <- unclass(project%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%count(dec)%>%unname())[[2]]
x19$perc_churn <- round(x19$n/x19$N,2)
x19$greaterthan <- unclass(project%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrrev_Mean)))[[2]]
x19$lesserthan <- unclass(project%>%mutate(dec=ntile(ovrrev_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrrev_Mean)))[[2]]
x19$variable <- rep("ovrrev_mean",nrow(x19))

# for variable rev_mean =x20 # Mean monthly revenue (charge amount)

summary(project$rev_Mean)
x20 <- project%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
x20$N <- unclass(project%>%mutate(dec=ntile(rev_Mean,n=10))%>%count(dec)%>%unname())[[2]]
x20$perc_churn <- round(x20$n/x20$N,2)
x20$greaterthan <- unclass(project%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(min(rev_Mean)))[[2]]
x20$lesserthan <- unclass(project%>%mutate(dec=ntile(rev_Mean,n=10))%>%group_by(dec)%>%summarise(max(rev_Mean)))[[2]]
x20$variable <- rep("rev_mean",nrow(x20))

# for variable overmouu_mean=x21 #Mean overage minutes of use

summary(project$ovrmou_Mean)
x21 <- project%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
x21$N <- unclass(project%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%count(dec)%>%unname())[[2]]
x21$perc_churn <- round(x21$n/x21$N,2)
x21$greaterthan <- unclass(project%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(min(ovrmou_Mean)))[[2]]
x21$lesserthan <- unclass(project%>%mutate(dec=ntile(ovrmou_Mean,n=4))%>%group_by(dec)%>%summarise(max(ovrmou_Mean)))[[2]]
x21$variable <- rep("ovrmou_mean",nrow(x21)) # positive skewed

# for variable comp_vce_mean=x22 #Mean number of completed voice calls

summary(project$comp_vce_Mean)
x22 <- project%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
x22$N <- unclass(project%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
x22$perc_churn <- round(x22$n/x22$N,2)
x22$greaterthan <- unclass(project%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(comp_vce_Mean)))[[2]]
x22$lesserthan <- unclass(project%>%mutate(dec=ntile(comp_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(comp_vce_Mean)))[[2]]
x22$variable <- rep("comp_vce_mean",nrow(x22))

# for variable plcd_vce_mean=x23 #Mean number of attempted voice calls placed

summary(project$plcd_vce_Mean)
x23 <- project%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
x23$N <- unclass(project%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
x23$perc_churn <- round(x23$n/x23$N,2)
x23$greaterthan <- unclass(project%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_vce_Mean)))[[2]]
x23$lesserthan <- unclass(project%>%mutate(dec=ntile(plcd_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_vce_Mean)))[[2]]
x23$variable <- rep("plcd-vce-'mean",nrow(x23))

# for variable avg3mou=x24 #Average monthly minutes of use over the previous three months

summary(project$avg3mou)
x24 <- project%>%mutate(dec=ntile(avg3mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
x24$N <- unclass(project%>%mutate(dec=ntile(avg3mou,n=10))%>%count(dec)%>%unname())[[2]]
x24$perc_churn <- round(x24$n/x24$N,2)
x24$greaterthan <- unclass(project%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(min(avg3mou)))[[2]]
x24$lesserthan <- unclass(project%>%mutate(dec=ntile(avg3mou,n=10))%>%group_by(dec)%>%summarise(max(avg3mou)))[[2]]
x24$variable <- rep("avg3mou",nrow(x24))

# for variable avgmou=x25 # Average monthly minutes of use over the life of the customer

summary(project$avgmou)
x25 <- project%>%mutate(dec=ntile(avgmou,n=10))%>%count(churn,dec)%>%filter(churn==1)
x25$N <- unclass(project%>%mutate(dec=ntile(avgmou,n=10))%>%count(dec)%>%unname())[[2]]
x25$perc_churn <- round(x25$n/x25$N,2)
x25$greaterthan <- unclass(project%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(min(avgmou)))[[2]]
x25$lesserthan <- unclass(project%>%mutate(dec=ntile(avgmou,n=10))%>%group_by(dec)%>%summarise(max(avgmou)))[[2]]
x25$variable <- rep("avgmou",nrow(x25))

# for variable avg3qty=x26 #Average monthly number of calls over the previous three months

summary(project$avg3qty)
x26 <- project%>%mutate(dec=ntile(avg3qty,n=10))%>%count(churn,dec)%>%filter(churn==1)
x26$N <- unclass(project%>%mutate(dec=ntile(avg3qty,n=10))%>%count(dec)%>%unname())[[2]]
x26$perc_churn <- round(x26$n/x26$N,2)
x26$greaterthan <- unclass(project%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(min(avg3qty)))[[2]]
x26$lesserthan <- unclass(project%>%mutate(dec=ntile(avg3qty,n=10))%>%group_by(dec)%>%summarise(max(avg3qty)))[[2]]
x26$variable <- rep("avg3qty",nrow(x26))

# for variable avgqty=x27 #Average monthly number of calls over the life of the customer

summary(project$avgqty)
x27 <- project%>%mutate(dec=ntile(avgqty,n=10))%>%count(churn,dec)%>%filter(churn==1)
x27$N <- unclass(project%>%mutate(dec=ntile(avgqty,n=10))%>%count(dec)%>%unname())[[2]]
x27$perc_churn <- round(x27$n/x27$N,2)
x27$greaterthan <- unclass(project%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(min(avgqty)))[[2]]
x27$lesserthan <- unclass(project%>%mutate(dec=ntile(avgqty,n=10))%>%group_by(dec)%>%summarise(max(avgqty)))[[2]]
x27$variable <- rep("avgqty",nrow(x27))

# for variable avg6mou=x28 #Average monthly minutes of use over the previous six months

summary(project$avg6mou)
x28 <- project%>%mutate(dec=ntile(avg6mou,n=10))%>%count(churn,dec)%>%filter(churn==1)
x28$N <- unclass(project%>%mutate(dec=ntile(avg6mou,n=10))%>%count(dec)%>%unname())[[2]]
x28$perc_churn <- round(x28$n/x28$N,2)
x28$greaterthan <- unclass(project%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(min(avg6mou)))[[2]]
x28$lesserthan <- unclass(project%>%mutate(dec=ntile(avg6mou,n=10))%>%group_by(dec)%>%summarise(max(avg6mou)))[[2]]
x28$variable <- rep("avg6mou",nrow(x28))

# for variable avg6qty=x29 # Average monthly number of calls over the previous six months

summary(project$avg6qty)
x29 <- project%>%mutate(dec=ntile(avg6qty,n=10))%>%count(churn,dec)%>%filter(churn==1)
x29$N <- unclass(project%>%mutate(dec=ntile(avg6qty,n=10))%>%count(dec)%>%unname())[[2]]
x29$perc_churn <- round(x29$n/x29$N,2)
x29$greaterthan <- unclass(project%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(min(avg6qty)))[[2]]
x29$lesserthan <- unclass(project%>%mutate(dec=ntile(avg6qty,n=10))%>%group_by(dec)%>%summarise(max(avg6qty)))[[2]]
x29$variable <- rep("avg6qty",nrow(x29))

# for variable roam_mean=x30 # Mean number of roaming calls
# contains less than 4 deciles , need to be omit.
summary(project$roam_Mean)
x30 <- project%>%mutate(dec=ntile(roam_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
x30$variable <- rep("roam_mean",nrow(x30)) # median is zero

# for variable recv_sms_mean=x31 # Mean number of received SMS calls
# contains less than 4 deciles , needs to be omit
summary(project$recv_sms_Mean)
x31 <- project%>%mutate(dec=ntile(recv_sms_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
x31$variable <- rep("recv_sms_mean",nrow(x31)) # median is zero

#for variable mou-pead_mean= x32 # Mean unrounded minutes of use of peak data calls
# median is zero
#  getting less than 4 deciles, need to be omit
summary(project$mou_pead_Mean)
x32 <- project%>%mutate(dec=ntile(mou_pead_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
x32$variable <- rep("mou_pead_mean",nrow(x32))

#for variable da_mean =x33 # Mean number of directory assisted calls

summary(project$da_Mean)
x33 <- project%>%mutate(dec=ntile(da_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
x33$N <- unclass(project%>%mutate(dec=ntile(da_Mean,n=4))%>%count(dec)%>%unname())[[2]]
x33$perc_churn <- round(x33$n/x33$N,2)
x33$greaterthan <- unclass(project%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(min(da_Mean)))[[2]]
x33$lesserthan <- unclass(project%>%mutate(dec=ntile(da_Mean,n=4))%>%group_by(dec)%>%summarise(max(da_Mean)))[[2]]
x33$variable <- rep("da_mean",nrow(x33))

#for variable da_range =x34 # Range of number of directory assisted calls

summary(project$da_Range)
x34 <- project%>%mutate(dec=ntile(da_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)
x34$N <- unclass(project%>%mutate(dec=ntile(da_Range,n=4))%>%count(dec)%>%unname())[[2]]
x34$perc_churn <- round(x34$n/x34$N,2)
x34$greaterthan <- unclass(project%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(min(da_Range)))[[2]]
x34$lesserthan <- unclass(project%>%mutate(dec=ntile(da_Range,n=4))%>%group_by(dec)%>%summarise(max(da_Range)))[[2]]
x34$variable <- rep("da_range",nrow(x34))

#for variable datovr_mean=x35 # Mean revenue of data overage
# conatains less than 4 deciles , needs to be omit
summary(project$datovr_Mean) # median is zero
x35 <- project%>%mutate(dec=ntile(datovr_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
x35$variable <- rep("datovr_mean",nrow(x35))

# for variable datovr_range=x36 # Range of revenue of data overage
# median is zero
# contains less tha 4 deciles , needs to be omit
summary(project$datovr_Range)
x36 <- project%>%mutate(dec=ntile(datovr_Range,n=4))%>%count(churn,dec)%>%filter(churn==1)
x36$variable <- rep("datovr_range",nrow(x36))

#for varible drop_dat_mean= x37 # Mean number of dropped (failed) data calls
# contains less than 4 deciles , needs to be omit
summary(project$drop_dat_Mean)
x37 <- project%>%mutate(dec=ntile(drop_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
x37$variable <- rep("drop_dat_mean",nrow(x37)) # median is zero

#for variable drop_vce_mean =x38 # Mean number of dropped (failed) voice calls

summary(project$drop_vce_Mean)
x38 <- project%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
x38$N <- unclass(project%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%count(dec)%>%unname())[[2]]
x38$perc_churn <- round(x38$n/x38$N,2)
x38$greaterthan <- unclass(project%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(min(drop_vce_Mean)))[[2]]
x38$lesserthan <- unclass(project%>%mutate(dec=ntile(drop_vce_Mean,n=10))%>%group_by(dec)%>%summarise(max(drop_vce_Mean)))[[2]]
x38$variable <- rep("drop_vce_mean",nrow(x38))

#for variable adjmou= x39 # Billing adjusted total minutes of use over the life of the customer

summary(project$adjmou)
x39 <- project%>%mutate(dec=ntile(adjmou,n=10))%>%count(churn,dec)%>%filter(churn==1)
x39$N <- unclass(project%>%mutate(dec=ntile(adjmou,n=10))%>%count(dec)%>%unname())[[2]]
x39$perc_churn <- round(x39$n/x39$N,2)
x39$greaterthan <- unclass(project%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(min(adjmou)))[[2]]
x39$lesserthan <- unclass(project%>%mutate(dec=ntile(adjmou,n=10))%>%group_by(dec)%>%summarise(max(adjmou)))[[2]]
x39$variable <- rep("adjmou",nrow(x39))

# for variable totrev=x40 # total revenue 

summary(project$totrev)
x40 <- project%>%mutate(dec=ntile(totrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
x40$N <- unclass(project%>%mutate(dec=ntile(totrev,n=10))%>%count(dec)%>%unname())[[2]]
x40$perc_churn <- round(x40$n/x40$N,2)
x40$greaterthan <- unclass(project%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(min(totrev)))[[2]]
x40$lesserthan <- unclass(project%>%mutate(dec=ntile(totrev,n=10))%>%group_by(dec)%>%summarise(max(totrev)))[[2]]
x40$variable <- rep("totrev",nrow(x40))

# for variable adjrev=x41 #Billing adjusted total revenue over the life of the customer

summary(project$adjrev)
x41 <- project%>%mutate(dec=ntile(adjrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
x41$N <- unclass(project%>%mutate(dec=ntile(adjrev,n=10))%>%count(dec)%>%unname())[[2]]
x41$perc_churn <- round(x41$n/x41$N,2)
x41$greaterthan <- unclass(project%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(min(adjrev)))[[2]]
x41$lesserthan <- unclass(project%>%mutate(dec=ntile(adjrev,n=10))%>%group_by(dec)%>%summarise(max(adjrev)))[[2]]
x41$variable <- rep("adjrev",nrow(x41))

# for variable avgrev=x42 # Average monthly revenue over the life of the customer

summary(project$avgrev)
x42 <- project%>%mutate(dec=ntile(avgrev,n=10))%>%count(churn,dec)%>%filter(churn==1)
x42$N <- unclass(project%>%mutate(dec=ntile(avgrev,n=10))%>%count(dec)%>%unname())[[2]]
x42$perc_churn <- round(x42$n/x42$N,2)
x42$greaterthan <- unclass(project%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(min(avgrev)))[[2]]
x42$lesserthan <- unclass(project%>%mutate(dec=ntile(avgrev,n=10))%>%group_by(dec)%>%summarise(max(avgrev)))[[2]]
x42$variable <- rep("avgrev",nrow(x42))

# for variable comp_dat-mean=x43 # Mean number of completed data calls
# has to relate with complete mean then omit.
summary(project$comp_dat_Mean)
x43 <- project%>%mutate(dec=ntile(comp_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
x43$variable <- rep("comp_dat_mean",nrow(x43)) # median is zero

# for variable  plcd_dat_mean=x44 #Mean number of attempted data calls placed
# has to relate with plcd_attempt_mean then omit.
summary(project$plcd_dat_Mean)
x44 <- project%>%mutate(dec=ntile(plcd_dat_Mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
x44$variable <- rep("plcd_dat_mean",nrow(x44)) # median is zero

# creating a variable plcd_atempt_mean=x45 called dummy variable 
# then deciling it .

project$plcd_atempted_mean <- project$plcd_dat_Mean + project$plcd_vce_Mean
summary(project$plcd_atempted_mean)
x45 <- project%>%mutate(dec=ntile(plcd_atempted_mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
x45$N <- unclass(project%>%mutate(dec=ntile(plcd_atempted_mean,n=10))%>%count(dec)%>%unname())[[2]]
x45$perc_churn <- round(x45$n/x45$N,2)
x45$greaterthan <- unclass(project%>%mutate(dec=ntile(plcd_atempted_mean,n=10))%>%group_by(dec)%>%summarise(min(plcd_atempted_mean)))[[2]]
x45$lesserthan <- unclass(project%>%mutate(dec=ntile(plcd_atempted_mean,n=10))%>%group_by(dec)%>%summarise(max(plcd_atempted_mean)))[[2]]
x45$variable <- rep("plcd_atempted_mean",nrow(x45))

# craeting a  variable completed_mean= x46 
# then deciling it

project$completed_mean <- project$comp_vce_Mean + project$comp_dat_Mean
summary(project$completed_mean)
x46 <- project%>%mutate(dec=ntile(completed_mean,n=10))%>%count(churn,dec)%>%filter(churn==1)
x46$N <- unclass(project%>%mutate(dec=ntile(completed_mean,n=10))%>%count(dec)%>%unname())[[2]]
x46$perc_churn <- round(x46$n/x46$N,2)
x46$greaterthan <- unclass(project%>%mutate(dec=ntile(completed_mean,n=10))%>%group_by(dec)%>%summarise(min(completed_mean)))[[2]]
x46$lesserthan <- unclass(project%>%mutate(dec=ntile(completed_mean,n=10))%>%group_by(dec)%>%summarise(max(completed_mean)))[[2]]
x46$variable <- rep("completed_mean",nrow(x46))

# for variable opk_dat_mean= x47 # Mean number of off-peak data calls
# median is zero
# conatins less than 4 deciles
summary(project$opk_dat_Mean)
x47 <- project%>%mutate(dec=ntile(opk_dat_Mean,n=4))%>%count(churn,dec)%>%filter(churn==1)
x47$variable <- rep("opk_dat_mean",nrow(x47))

# NOW THE CATEGORICAL VARIABLES WHICH ARE HAVING CONTINUOUS VALUES HAVE TO BE DECILED
# under continuous variable deciling.
# and these variables have to be converted to factor during model run.

# for variable age1=x48 #Age of first household member
#00 = default
#Other values signify a valid age.

summary(project$age1)
x48 <- project%>%mutate(dec=ntile(age1,n=6))%>%count(churn,dec)%>%filter(churn==1)
x48$N <- unclass(project%>%mutate(dec=ntile(age1,n=6))%>%count(dec)%>%unname())[[2]]
x48$perc_churn <- (x48$n/x48$N)
x48$greaterthan <- unclass(project%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(min(age1)))[[2]]
x48$lesserthan <- unclass(project%>%mutate(dec=ntile(age1,n=6))%>%group_by(dec)%>%summarise(max(age1)))[[2]]
x48$variable <- rep("age1",nrow(x48)) 

# for variable age2=x49 # Age of second household member
#00 = default
#Other values signify a valid age.
# conatisn less than 4 deciles , need s to be omit.
summary(project$age2)
x49 <- project%>%mutate(dec=ntile(age2,n=4))%>%count(churn,dec)%>%filter(churn==1)
x49$variable <- rep("age2",nrow(x49))                  

# for variable models=x50 #Number of models issued
# contains less than  4 deciles, needs to be omit
summary(project$models)
x50 <- project%>%mutate(dec=ntile(models,n=4))%>%count(churn,dec)%>%filter(churn==1)
x50$variable <- rep("models",nrow(x50))                   

# for variable hnd_price-x51 #Current handset price

summary(project$hnd_price)
x51 <- project%>%mutate(dec=ntile(hnd_price,n=10))%>%count(churn,dec)%>%filter(churn==1)
x51$N <- unclass(project%>%mutate(dec=ntile(hnd_price,n=10))%>%count(dec)%>%unname())[[2]]
x51$perc_churn <- round(x51$n/x51$N,2)
x51$greaterthan <- unclass(project%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(min(hnd_price)))[[2]]
x51$lesserthan <- unclass(project%>%mutate(dec=ntile(hnd_price,n=10))%>%group_by(dec)%>%summarise(max(hnd_price)))[[2]]
x51$variable <- rep("hnd_price",nrow(x51))

# for variable actvsubs=x52 # Number of active subscribers in household
# contains less than 4 deciles, needs to be omit
summary(project$actvsubs)
x52 <- project%>%mutate(dec=ntile(actvsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)
x52$variable <- rep("actvsubs",nrow(x52))

# for variable uniqsubs=x53 # Number of unique subscribers in the household
# getting less than 4 deciles, needs to be omit
summary(project$uniqsubs)
x53 <- project%>%mutate(dec=ntile(uniqsubs,n=4))%>%count(churn,dec)%>%filter(churn==1)
x53$variable <- rep("uniqsubs",nrow(x53))

# for variable forgntvl=x54 #Foreign travel dummy variable
#0 = No
#1 = Yes
# conatins less than 4 deciles , needs to be omit
summary(project$forgntvl)
x54 <- project%>%mutate(dec=ntile(forgntvl,n=4))%>%count(churn,dec)%>%filter(churn==1)
x54$variable <- rep("forgntvl",nrow(x54)) # median is zero

# for variable mtrcycle=x55 # Motorcycle indicator
# getting less than 4 deciles, needs to be omit.
summary(project$mtrcycle)
x55 <- project%>%mutate(dec=ntile(mtrcycle,n=4))%>%count(churn,dec)%>%filter(churn==1)
x55$variable <- rep("mtrcycle",nrow(x55))

# for variable truck=x56 #Truck indicator
# contains less than 4 deciles , needs to be omit
summary(project$truck)
x56 <- project%>%mutate(dec=ntile(truck,n=4))%>%count(churn,dec)%>%filter(churn==1)
x56$variable <- rep("truck",nrow(x56))

# To create a CONTx object by summing all appropriate x1 - x56 with exclusion of variables having less than 4 deciles.
CONTx <-rbind (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x14,x15,x18,x19,x20,x21,x22,x23,x24,x25,x26,x27,x28,x29,x33,x34,x38,x39,x40,x41,x42,x45,x46,x48,x51)

# saving deciled variable
write.csv(CONTx,"Deciled Continuous Variables with 10 deciles.csv",row.names = F)

# variables which were not deciled ,will show insignificant behaviour in the model
# so removing those variables
summary(project)
names(project)
project <- project[,-c(13,16,17,22,23,45,48:50,56:58,65,66)]
colnames(project)

######################
## CATEGORICAL VARIABLES PROFILING AND DECILING
## event rate for each level in a categorical variable

# for the variable crclscod=y1 #Credit class code
#A represents best rating, Z represents worst rating
# due to the low percent of churn which are less than 5% 
# which can show insignificant behaviour

summary(project$crclscod)
y1 <- project%>%count(churn,levels=crclscod)%>%filter(churn==1)
y1$N <- unclass(project%>%filter(crclscod%in%y1$levels)%>%count(crclscod))[[2]]
y1$perc_churn <- round(y1$n/y1$N,2)
y1$variable <- rep("crclscod",nrow(y1))
class(project$crclscod) # omitting it due to churn rate less than 5%
#20 observation shows less than 0.23 and having many levels may show
#insignificant nature during model running.

# for variable asl_flag=y2 #Account spending limit
#N = No
#Y = Yes
summary(project$asl_flag)
y2 <- project%>%count(churn,levels=asl_flag)%>%filter(churn==1)
y2$N <- unclass(project%>%filter(asl_flag%in%y2$levels)%>%count(asl_flag))[[2]]
y2$perc_churn <- round(y2$n/y2$N,2)
y2$variable <- rep("asl_flag",nrow(y2))
class(project$asl_flag) # trend in churn perc.

# for variable prizm_social_one=y3 # Social group letter only
#Based on degree of population density of area.
#C = City
#R = Rural
#S = Suburban
#T = Town
#U = Urban
summary(project$prizm_social_one)
y3 <- project%>%count(churn,levels=prizm_social_one)%>%filter(churn==1)
y3$N <- unclass(project%>%filter(prizm_social_one%in%y3$levels)%>%count(prizm_social_one))[[2]]
y3$perc_churn <- round(y3$n/y3$N,2)
y3$variable <- rep("prizm_social_one",nrow(y3))
class(project$prizm_social_one)

# for variable area=y4 #Geographic area
summary(project$area)
y4 <- project%>%count(churn,levels=area)%>%filter(churn==1)
y4$N <- unclass(project%>%filter(area%in%y4$levels)%>%count(area))[[2]]
y4$perc_churn <- round(y4$n/y4$N,2)
y4$variable <- rep("area",nrow(y4))
class(project$area)

# for variable refurb_new=y5 # Handset: refurbished or new
#N = New
#R = Refurbished
summary(project$refurb_new)
y5 <- project%>%count(churn,levels=refurb_new)%>%filter(churn==1)
y5$N <- unclass(project%>%filter(refurb_new%in%y5$levels)%>%count(refurb_new))[[2]]
y5$perc_churn <- round(y5$n/y5$N,2)
y5$variable <- rep("refurb_new",nrow(y5))
class(project$refurb_new)

# for variable hnd_webcap=y6 #Handset web capability
#WC = Web Capable
#WC = Web Capable Mini-Browser
#NA = Not Applicable
#UNKW = Unable to collect these data
summary(project$hnd_webcap)
y6 <- project%>%count(churn,levels=hnd_webcap)%>%filter(churn==1)
y6$N <- unclass(project%>%filter(hnd_webcap%in%y6$levels)%>%count(hnd_webcap))[[2]]
y6$perc_churn <- round(y6$n/y6$N,2)
y6$variable <- rep("hnd_webcap",nrow(y6))
class(project$hnd_webcap)

# for variable marital =y7 # Marital status
#Indicates if anyone in the household is married.
#U = Unknown
#M = Married
#S = Single
#B = Inferred single
#A = Inferred married
summary(project$marital)
y7 <- project%>%count(churn,levels=marital)%>%filter(churn==1)
y7$N <- unclass(project%>%filter(marital%in%y7$levels)%>%count(marital))[[2]]
y7$perc_churn <- round(y7$n/y7$N,2)
y7$variable <- rep("marital",nrow(y7))
class(project$marital)

# for variable ethnic=y8 # Ethnicity roll-up code
#B = Asian (non-Oriental)
#D = Southern European
#F = French
#G = German
#H = Hispanic
#I = Italian
#J = Jewish
#M = Miscellaneous
#N = Northern European
#O = Asian
#P = Polynesian
#R = Arab
#S = Scottish / Irish
#U = Unknown
#Z = African-American
summary(project$ethnic)
y8 <- project%>%count(churn,levels=ethnic)%>%filter(churn==1)
y8$N <- unclass(project%>%filter(ethnic%in%y8$levels)%>%count(ethnic))[[2]]
y8$perc_churn <- round(y8$n/y8$N,2)
y8$variable <- rep("ethnic",nrow(y8))
class(project$ethnic)

# for variable car_buy=y9 # New or used car buyer
#Indicates a history of new car buying in the household.
summary(project$car_buy)
y9 <- project%>%count(churn,levels=car_buy)%>%filter(churn==1)
y9$N <- unclass(project%>%filter(car_buy%in%y9$levels)%>%count(car_buy))[[2]]
y9$perc_churn <- round(y9$n/y9$N,2)
y9$variable <- rep("car_buy",nrow(y9))
class(project$car_buy)

# for variable csa=y10 # Communications local service area
#Refers to specific location of the customer, usually indicating city.

summary(project$csa)
y10 <- project%>%count(churn,levels=csa)%>%filter(churn==1)
y10$N <- unclass(project%>%filter(csa%in%y10$levels)%>%count(csa))[[2]]
y10$perc_churn <- round(y10$n/y10$N,2)
y10$variable <- rep("csa",nrow(y10))
class(project$csa) # many levels showing less than 5% churn rate
# and having high levels may show insignificant nature during model running.

# now those categorical variable which do not have factor nature
# so also converting those into factor datatype

# for variable  retention=y11
#Retention calls include any calls from the customer regarding loyalty or retention, e.g. contract renewal, relating competitor's offer, etc.				
#Missing values for this variable can be assumed to mean there have been no retention calls made by the customer.				
summary(project$retention)
project$retention <- as.factor(project$retention)
y11 <- project%>%count(churn,levels=retention)%>%filter(churn==1)
y11$N <- unclass(project%>%filter(retention%in%y11$levels)%>%count(retention))[[2]]
y11$perc_churn <- round(y11$n/y11$N,2)
y11$variable <- rep("retention",nrow(y11))
class(project$retention)

# for variable age2=y12 # Age of second household member
#00 = default
#Other values signify a valid age.
summary(project$age2)
y12 <- project%>%count(churn,levels=age2)%>%filter(churn==1)
y12$N <- unclass(project%>%filter(age2%in%y12$levels)%>%count(age2))[[2]]
y12$perc_churn <- round(y12$n/y12$N,2)
y12$variable <- rep("age2",nrow(y12))
class(project$age2)

# for variable models=y13 # Number of models issued
summary(project$models)
y13 <- project%>%count(churn,levels=models)%>%filter(churn==1)
y13$N <- unclass(project%>%filter(models%in%y13$levels)%>%count(models))[[2]]
y13$perc_churn <- round(y13$n/y13$N,2)
y13$variable <- rep("models",nrow(y13))
class(project$models)

# for variable actvsubs=y14 # Number of active subscribers in household
summary(project$actvsubs)
y14 <- project%>%count(churn,levels=actvsubs)%>%filter(churn==1)
y14$N <- unclass(project%>%filter(actvsubs%in%y14$levels)%>%count(actvsubs))[[2]]
y14$perc_churn <- round(y14$n/y14$N,2)
y14$variable <- rep("actvsubs",nrow(y14))
class(project$actvsubs)

# for variable uniqsubs=y15 # Number of unique subscribers in the household
summary(project$uniqsubs)
y15 <- project%>%count(churn,levels=uniqsubs)%>%filter(churn==1)
y15$N <- unclass(project%>%filter(uniqsubs%in%y15$levels)%>%count(uniqsubs))[[2]]
y15$perc_churn <- round(y15$n/y15$N,2)
y15$variable <- rep("uniqsubs",nrow(y15))

# for variable mtrcycle =y16 # Motorcycle indicator
#Indicates motorcycle owner in a household.
summary(project$mtrcycle)
y16 <- project%>%count(churn,levels=mtrcycle)%>%filter(churn==1)
y16$N <- unclass(project%>%filter(mtrcycle%in%y16$levels)%>%count(mtrcycle))[[2]]
y16$perc_churn <- round(y16$n/y16$N,2)
y16$variable <- rep("mtrcycle",nrow(y16))

# for variable truck=y17 # Truck indicator
#Indicates a truck owner in a household.
summary(project$truck)
y17 <- project%>%count(churn,levels=truck)%>%filter(churn==1)
y17$N <- unclass(project%>%filter(truck%in%y17$levels)%>%count(truck))[[2]]
y17$perc_churn <- round(y17$n/y17$N,2)
y17$variable <- rep("truck",nrow(y17))

# for variable hnd_price=y18 #Current handset price

summary(project$hnd_price)
y18 <- project%>%count(churn,levels=hnd_price)%>%filter(churn==1)
y18$N <- unclass(project%>%filter(hnd_price%in%y18$levels)%>%count(hnd_price))[[2]]
y18$perc_churn <- round(y18$n/y18$N,2)
y18$variable <- rep("hnd_price",nrow(y18))
class(project$hnd_price)

# summing up all the variables from y1 to y19
cat_y <- rbind(y1,y2,y3,y4,y5,y6,y7,y8,y9,y10,y11)
catt_y <- rbind(y12,y13,y14,y15,y16,y17,y18)

# saving out deciled categorical variables 
write.csv(cat_y,"event rate for categorical variables.csv",row.names = F)
write.csv(catt_y,"event rate for categorical variables.csv",row.names = F)

summary(project)
colnames(project)

# variables having churn rate less 5%
project <- project[,-c(25,44)]
names(project)

#####################################################
# DATA PREPARATION
# (1) OUTLIERS TREATMENT

# For continuous variables first
# we have two methods to check for the outliers 
# graphical representation that is using plot
# boxplot
# boxplot tells more clearly about the outliers, it easily detects it.
colnames(project)
str(project)
#keeping continuoius variables only. 
# factor variables which needs to be excluded from list later
#retention,car_buy,ethnic,marital,hnd_webcap,refurb_new,area,prizm_socila_one
#asl_flag,age1,age2,models,hnd_price,hnd_price,area,forgntvl,mtrcycle,truck,churn,carbuy
#customer_id
names(project)
list <- names(project)
list

# Removing categorical varibles
list <- list[-c(25:42,50,51)]
list
boxplot(project$churn,project$prizm_social_one)
# Plotting outliers
par(mfrows=c(3,11))
for(i in 1:length(list))
{
  boxplot(project[,list[i]],main=list[i])
}
#warning()

# outlier treatment
for(i in 1:length(list))
{
  a <- boxplot(project[,list[i]],main=list[i])
  out <- a$out
  index <- which(project[,list[i]]%in%a$out)
  project[index,list[i]] <- mean(project[,list[i]],na.rm=T)
  rm(a)
  rm(out)
}

# checking after treatment
for(i in 1:length(list))
{
  boxplot(project[,list[i]],main=list[i])
}

for(i in 1:length(list))
{ plot(project[,list[i]],main=list[i])
}

dev.off()

# (2) Missing values treatmemnt
summary(project)
colnames(project)

# removing misisng values for first four columns because the number is less which can 
# be removed  in the case of large dataset

ms1 <- which(is.na(project[,c(1:5)]))
project <- project[-ms1,]
summary(project)

ms2 <- which(is.na(project$change_mou))
project <- project[-ms2,]
summary(project)

ms3 <- which(is.na(project$marital))
project <- project[-ms3,]
summary(project)

ms4 <- which(is.na(project$area))
project <- project[-ms4,]
summary(project)

project$hnd_price[is.na(project$hnd_price)] <- mean(project$hnd_price,na.rm=T)
summary(project)

project$avg6mou[is.na(project$avg6mou)] <- mean(project$avg6mou,na.rm=T)
summary(project)

project$avg6qty[is.na(project$avg6qty)] <- mean(project$avg6qty,na.rm=T)
summary(project)

str(project)

# for factor variables , Creating the separate category "MISSING" 
# for variable prizm_social_one
project$prizm_social_one_a <- ifelse(is.na(project$prizm_social_one),"MISSING",as.factor(project$prizm_social_one))
str(project$prizm_social_one_a)
project$prizm_social_one_a <- as.factor(project$prizm_social_one_a)
summary(project$prizm_social_one)
summary(project$prizm_social_one_a)
project$prizm_social_one_a <- factor(project$prizm_social_one_a,labels=c("C","R","S","T","U","MISISING"))
summary(project$prizm_social_one_a)
names(project)
project <- project[,-26]
summary(project)

# for variable hnd_webcap
project$hnd_webcap_a <- ifelse(is.na(project$hnd_webcap),"MISSING",as.factor(project$hnd_webcap))
str(project$hnd_webcap_a)
project$hnd_webcap_a <- as.factor(project$hnd_webcap_a)
summary(project$hnd_webcap)
summary(project$hnd_webcap_a)
project$hnd_webcap_a <- factor(project$hnd_webcap_a,labels=c("UNKW","WC","WCMB","MISSING"))
summary(project$hnd_webcap_a)
names(project)
project <- project[,-28]
names(project)

# Calculating churn rate in the data after missing value treat ment 
table(project$churn)/nrow(proj)
table(project$churn)/nrow(project)

str(project)

# Transformation of categorical variables to factor variables 
# and creating dummy variables 

# for variable age1, making categories (default, young,midage , old)
str(project$age1)
summary(project$age1)
project$age1_a <- ifelse(project$age1==0,"default",ifelse(project$age1<=30,"young",ifelse(project$age1>30 & project$age1<=55,"mid age","old")))
str(project$age1_a)
project$age1_a <- as.factor(project$age1_a)
summary(project$age1_a)
names(project)
project <- project[,-30]
colnames(project)

# for variable age2, making categories(deafult,young,midage,old)
str(project$age2)
project$age2_a <- ifelse(project$age2==0,"default",ifelse(project$age2<=30,"young",ifelse(project$age2>30 & project$age2<=55,"mid age","old")))
str(project$age2_a)
project$age2_a <-as.factor(project$age2_a)
summary(project$age2_a)
names(project)
project <- project[,-30]

# for other variables
str(project)
summary(project$models)
project$models <- as.factor(project$models)
summary(project$models)

summary(project$actvsubs)
project$actvsubs <- as.factor(project$actvsubs)
summary(project$actvsubs)

project$hnd_price <- as.factor(project$hnd_price)
summary(project$hnd_price)

summary(project$uniqsubs)
project$uniqsubs <- as.factor(project$uniqsubs)
summary(project$uniqsubs)

summary(project$forgntvl)
project$forgntvl <- as.factor(project$forgntvl)
summary(project$forgntvl)

summary(project$mtrcycle)
project$mtrcycle <- as.factor(project$mtrcycle)
summary(project$mtrcycle)

summary(project$truck)
project$truck <- as.factor(project$truck)
summary(project$truck)

str(project)
###################################################
## BUILDING LOGISTIC REGRESSION

# splitting the data set into training an dtesting data set
set.seed(200)
partition <- sample(nrow(project),0.70*nrow(project),replace=F)
training <- project[partition,]
testing <- project[-partition,]

# Calculating churn rate for both training and testing.
table(training$churn)/nrow(training)
table(testing$churn)/nrow(testing)
# almost near values of both training and testing

colnames(project)

# building a model
mod <- glm(churn~.,data=training[,-46],family="binomial")
summary(mod)
# coefficients estimates would be log odd of events
library(car)
# hence some variable showing significant nature

# mou_mean,totmrc_mean,rev_range,mou_range,change_mou,drop_blk_mean,
#drop_vce_range,mou_opkv_range,months,eqpdays,iwylis_vce_mean,
# overrev_mean,avgmou,avg3qty,avgqty,avg6mou,asl_flagY
# area(califnrth,central/sth,nrthflorida,nrthwst,southflorida,tennese area),
#refurb_newr,ethnic(c,n,o,s,u,z)
# hnd_prc(79.98,105.08,129.98,149.98,199.98,249.98),uniqsubs(2,3,4,5,6,7,9)
#truck1,adjmou,totrev,retention1
#complteted_mean,prizm_socil_onea(r,t),age1,age2(old)

#step(mod,direction="both")
# since step takes more time for individual step , doing it manually
#aic is 48137.61

# Creating dummy variables for factor variables with levels showing some significant
# behaviour to run further models

summary(training$age1_a)
training$age1_midage <- ifelse(training$age1_a=="mid age",1,0)
testing$age1_midage <- ifelse(testing$age1_a=="mid age",1,0)

training$age1_old <- ifelse(training$age1_a=="old",1,0)
testing$age1_old <- ifelse(testing$age1_a=="old",1,0)

training$age1_young <- ifelse(training$age1_a=="young",1,0)
testing$age1_young <- ifelse(testing$age1_a=="young",1,0)

# for variable age2_a
training$age2_old <- ifelse(training$age2_a=="old",1,0)
testing$age2_old <- ifelse(testing$age2_a=="old",1,0)

# for variable prizm_social_one_a
summary(training$prizm_social_one_a)

training$prizm_social_one_R <- ifelse(training$prizm_social_one_a=="R",1,0)
testing$prizm_social_one_R <- ifelse(testing$prizm_social_one_a=="R",1,0)

training$prizm_social_one_T <- ifelse(training$prizm_social_one_a=="T",1,0)
testing$prizm_social_one_T <- ifelse(testing$prizm_social_one_a=="T",1,0)

# for variable uniqsubs
summary(project$uniqsubs)
training$uniqsubs_2 <- ifelse(training$uniqsubs=="2",1,0)
testing$uniqsubs_2 <- ifelse(testing$uniqsubs=="2",1,0)


training$uniqsubs_3 <- ifelse(training$uniqsubs=="3",1,0)
testing$uniqsubs_3 <- ifelse(testing$uniqsubs=="3",1,0)


training$uniqsubs_4 <- ifelse(training$uniqsubs=="4",1,0)
testing$uniqsubs_4 <- ifelse(testing$uniqsubs=="4",1,0)

training$uniqsubs_5 <- ifelse(training$uniqsubs=="5",1,0)
testing$uniqsubs_5 <- ifelse(testing$uniqsubs=="5",1,0)

training$uniqsubs_6 <- ifelse(training$uniqsubs=="6",1,0)
testing$uniqsubs_6 <- ifelse(testing$uniqsubs=="6",1,0)

training$uniqsubs_7 <- ifelse(training$uniqsubs=="7",1,0)
testing$uniqsubs_7 <- ifelse(testing$uniqsubs=="7",1,0)

training$uniqsubs_9 <- ifelse(training$uniqsubs=="9",1,0)
testing$uniqsubs_9 <- ifelse(testing$uniqsubs=="9",1,0)

#for variable truck
summary(training$truck)

# for variable hnd_price
summary(training$hnd_price)
training$hnd_price_79.98 <- ifelse(training$hnd_price=="79.98999023",1,0)
testing$hnd_price_79.98 <- ifelse(testing$hnd_price=="79.98999023",1,0)

training$hnd_price_105.08 <- ifelse(training$hnd_price=="105.083038078331",1,0)
testing$hnd_price_105.08 <- (ifelse(testing$hnd_price=="105.08038078331",1,0))

training$hnd_price_129.98 <- ifelse(training$hnd_price=="129.9899902",1,0)
testing$hnd_price_129.98 <- ifelse(testing$hnd_price=="129.9899902",1,0)

training$hnd_price_149.98 <- ifelse(training$hnd_price=="149.9899902",1,0)
testing$hnd_price_149.98 <- ifelse(testing$hnd_price=="149.9899902",1,0)

training$hnd_price_199.98 <- ifelse(training$hnd_price=="199.9899902",1,0)
testing$hnd_price_199.98 <- ifelse(testing$hnd_price=="199.9899902",1,0)

training$hnd_price_249.98 <- ifelse(training$hnd_price=="249.9899902",1,0)
testing$hnd_price_249.98 <- ifelse(testing$hnd_price=="249.9899902",1,0)

# for  variable ethnic
summary(training$ethnic)
training$ethnic_C <- ifelse(training$ethnic=="C",1,0)
testing$ethnic_C <- ifelse(testing$ethnic=="C",1,0)

training$ethnic_N <- ifelse(training$ethnic=="N",1,0)
testing$ethnic_N <- ifelse(testing$ethnic=="N",1,0)

training$ethnic_O <- ifelse(training$ethnic=="O",1,0)
testing$ethnic_O <- ifelse(testing$ethnic=="O",1,0)

training$ethnic_S <- ifelse(training$ethnic=="S",1,0)
testing$ethnic_S <- ifelse(testing$ethnic=="S",1,0)

training$ethnic_U <- ifelse(training$ethnic=="U",1,0)
testing$ethnic_U <- ifelse(testing$ethnic=="U",1,0)

training$ethnic_Z <- ifelse(training$ethnic=="Z",1,0)
testing$ethnic_Z <- ifelse(testing$ethnic=="Z",1,0)

# for variable area
summary(training$area)

training$area_calnrth <- ifelse(training$area=="CALIFORNIA NORTH AREA",1,0)
testing$area_calnrth <- ifelse(testing$area=="CALIFORNIA NORTH AREA",1,0)

training$area_texas <- ifelse(training$area=="CENTRAL/SOUTH TEXAS AREA",1,0)
testing$area_texas <- ifelse(testing$area==" CENTRAL/SOUTH TEXAS AREA",1,0)

training$area_nrthflorida <- ifelse(training$area=="NORTH FLORIDA AREA",1,0)
testing$area_nrthflorida <- ifelse(testing$area=="NORTH FLORIDA AREA",1,0)

training$area_nrthwst <- ifelse(training$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)
testing$area_nrthwst <- ifelse(testing$area=="NORTHWEST/ROCKY MOUNTAIN AREA",1,0)

training$area_southflor <- ifelse(training$area=="SOUTH FLORIDA AREA",1,0)
testing$area_southflor <- ifelse(testing$area=="SOUTH FLORIDA AREA",1,0)

training$area_southwest <- ifelse(training$area=="SOUTHWEST AREA",1,0)
testing$area_southwest <- ifelse(testing$area=="SOUTHWEST AREA",1,0)

training$area_tenese <- ifelse(training$area=="TENNESSEE AREA",1,0)
testing$area_tenese <- ifelse(testing$area=="TENNESSEE AREA",1,0)

# for varaible asl_flag
summary(training$asl_flag)
training$asl_flag_y <- ifelse(training$asl_flag=="Y",1,0)
testing$asl_flag_y <- ifelse(testing$asl_flag=="Y",1,0)


# for variable refurb_new
summary(training$refurb_new)
training$refurb_new_r <- ifelse(training$refurb_new=="R",1,0)
testing$refurb_new_r <- ifelse(testing$refurb_new=="R",1,0)

# Again running the model manually by removing insignificant variable
colnames(training)
model1 <- glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou
              + drop_blk_Mean + drop_vce_Range + mou_opkv_Range + months + eqpdays
              + iwylis_vce_Mean + ovrrev_Mean + avgmou + avg3qty + avgqty + avg6mou
              + asl_flag_y + area_calnrth +area_texas +area_nrthflorida +area_nrthwst
              + area_southflor + area_southwest + refurb_new_r + ethnic_C
              + ethnic_N +ethnic_O + ethnic_S + ethnic_U + ethnic_Z + hnd_price_79.98
              + hnd_price_105.08 + hnd_price_129.98 + hnd_price_149.98 + hnd_price_199.98
              + hnd_price_249.98 + uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_5
              + uniqsubs_6 + uniqsubs_7 + uniqsubs_9 + truck + adjmou + totrev + retention
              + completed_mean + prizm_social_one_R + prizm_social_one_T  + age1_midage
              + age1_old + age1_young + age2_old ,data=training,family="binomial")
summary(model1)
# results-model is 49780 units deviant(from actual value) when it is null
# so 47950 units under rtesidual using the independent variables.
#AIC= 48060
library(car)
vif(model1)
# OPTIMISING THE MODEL
# since uniqsubs_5,uniqsubs_6,uniqsubs_9,truck are coming insignificant
# so removing these variables then rerunning the model
model2 <- glm(churn ~ mou_Mean + totmrc_Mean + rev_Range + mou_Range + change_mou
              + drop_blk_Mean + drop_vce_Range + mou_opkv_Range + months + eqpdays
              + iwylis_vce_Mean + ovrrev_Mean + avgmou + avg3qty + avgqty + avg6mou
              + asl_flag_y + area_calnrth +area_texas +area_nrthflorida +area_nrthwst
              + area_southflor + area_southwest + refurb_new_r + ethnic_C
              + ethnic_N +ethnic_O + ethnic_S + ethnic_U + ethnic_Z + hnd_price_79.98
              + hnd_price_105.08 + hnd_price_129.98 + hnd_price_149.98 + hnd_price_199.98
              + hnd_price_249.98 + uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_7  + adjmou
              + totrev + retention
              + completed_mean + prizm_social_one_R + prizm_social_one_T  + age1_midage
              + age1_old + age1_young + age2_old ,data=training,family="binomial")
summary(model2)
# AIC decreased
# here it comes the model with all significant variables having p value less than 5%
# It can be considered as final model 

# so maximum variables are significnt now with less than 0.5 p value
# CHECKING MULTICOLLINEARITY (VIF must be less than 5)
vif(model2)
# some variables show more than 5 vif values.
#mou_mean,avgmou,avg3qty,avg6mou

# lets run model with excluding the previoius four variables with high vif
model3 <- glm(churn ~ totmrc_Mean + rev_Range + mou_Range + change_mou
              + drop_blk_Mean + drop_vce_Range + mou_opkv_Range + months + eqpdays
              + iwylis_vce_Mean + ovrrev_Mean  + avgqty
              + asl_flag_y + area_calnrth +area_texas +area_nrthflorida +area_nrthwst
              + area_southflor + area_southwest + refurb_new_r + ethnic_C
              + ethnic_N +ethnic_O + ethnic_S + ethnic_U + ethnic_Z + hnd_price_79.98
              + hnd_price_105.08 + hnd_price_129.98 + hnd_price_149.98 + hnd_price_199.98
              + hnd_price_249.98 + uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_7  + adjmou
              + totrev + retention
              + completed_mean + prizm_social_one_R + prizm_social_one_T  + age1_midage
              + age1_old + age1_young + age2_old ,data=training,family="binomial")
summary(model3)
vif(model3)
# aic and residual deviance increased 
#adjmou comming insignificant
# rerunning model without adjmou
model4 <- glm(churn ~ totmrc_Mean + rev_Range + mou_Range + change_mou
              + drop_blk_Mean + drop_vce_Range + mou_opkv_Range + months + eqpdays
              + iwylis_vce_Mean + ovrrev_Mean  + avgqty
              + asl_flag_y + area_calnrth +area_texas +area_nrthflorida +area_nrthwst
              + area_southflor + area_southwest + refurb_new_r + ethnic_C
              + ethnic_N +ethnic_O + ethnic_S + ethnic_U + ethnic_Z + hnd_price_79.98
              + hnd_price_105.08 + hnd_price_129.98 + hnd_price_149.98 + hnd_price_199.98
              + hnd_price_249.98 + uniqsubs_2 + uniqsubs_3 + uniqsubs_4 + uniqsubs_7
              + totrev + retention
              + completed_mean + prizm_social_one_R + prizm_social_one_T  + age1_midage
              + age1_old + age1_young + age2_old ,data=training,family="binomial")
summary(model4)
# now since aic remains same and residual deviance has a small increment.
vif(model4)
# so model 4 can be finalised with all significant variables and no multicollinearity

# PERFORMANCE METRICS
# checking the performance of this model
# model testing with the testing partition
#obtaining predictions from my model by using predict functions
# it will predict the probabilities for the customer churning
# evaluating it by kappa2 and confusion matrix

pred <- predict(model4,type="response",newdata=testing)
head(pred)# gives predictions of my model and type response provide probability
# prediction for testing data
testing
#setting a cutoff
# assuming cutoff probabilities as per the churn rate in the dataset
table(project$churn)/nrow(project)
# proportion is 0.2392114

pred1 <- ifelse(pred>=0.2380871,1,0)
# getting will be churn
# getting 0 will not churn

library(lpSolve)
library(irr)
library(lattice)
library(caret)
kappa2(data.frame(testing$churn,pred1))
confusionMatrix(pred1,testing$churn,positive="1")
table(testing$churn)
(8525+2730)/(8525+1872+6289+2730)
#57.9% accuracy


#Misclassification rate(overall how often is it wrong)
#fp+fn/total
(6289+1872)/(8525+1872+6289+2730)

# True positive rate
#tp/actual yes
2730/(6289+2730)
#actual no
# tn/actual no
8525/(8525+1872)

#ROCR curve
library(ROCR)
predauc <- prediction(pred1,testing$churn)
predroc <- performance(predauc,measure="tpr",x.measure="fpr")
plot(predroc,col="blue")
abline(0,1,lty=8,col="green")
auc <- performance(predauc,measure="auc")
auc
auc <- auc@y.values[[1]]
#0.5843447 which is greater than 0.50
#the curve seems to be good which is above the green line.
# so model seems to be acceptable

# Doing customer targetting
# what range of probability score given by my model would make sure that i am able to target
# maximum number of churn out customers 
# checking for which customer would churn out and which customer would 
# not churn out.

#gains charts
library(gains)
testing$churn <- as.numeric(testing$churn)
gains(testing$churn,predict(model4,type="response",newdata = testing),groups=10)
# from the results , 42.1% customers are likely to churn as they come unde top 30% probabilities

testing$prob <- predict(model4,type="response",newdata=testing)
quantile(testing$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))


targetted <- testing[testing$prob>0.2755509 & testing$prob<=7446702,"Customer_ID"]
targetted # we can target those customers which ae likely to churn out.

####################################################################
########################################
#CAPSTONE PROJECT QUESTIONS AND ANSWERS
#Q-1 What are the top five factors driving likelihood of churn at Mobicom?
#A-1 
summary(model4)
head(sort(model4$coefficients,decreasing = T))
#Top Five Factors which are affecting the churn at Mobicom are
#A). uniqsubs_7 with a unit of (0.7240542)
# which means unit increment in level 7 of uniqsubs would lead to increment of 0.7240542 units
# in churn.

#B). retention1 with a unit of (0.6759334)
# which implies a unit increment in retention will lead to increment of 0.6759334 in churn.

#C). ethnic_O with a unit of (0.3123674)
# which implies a unit icrease in asian ethnicity leading to a increment of 0.3123674 in churn.

#D). area_nrthwst with a unit of (0.2873840)
# which implies variable NORTHWEST/ROCKY MOUNTAIN AREA will get increased by 0.2873840
# units in churn.

#E). area_southflor with a unit of (0.2795409)
# which implies SOUTH FLORIDA GEOGRAPHIC AREA , in which a unit increment will 
# lead to increment by 0.2795409 units in churn.

# since unique subscribers are playing a big role in churning 
# and uniqsubs_7 simply tells ws for 7 unique subscribers are there and they are churning
# out . so we can collect all these subsribers into a family pack with some special offers
# so that equal services and benefits get distributed amongs them.

#Now retention calls plays a internal role which needs to be modified for those customers
# who are facing the issue with the services and calling for moving out from the services
# special retention team has to be their with good soft skills and best to maintain
# customers retended with best plans strategy using the segments of customers
# with other plans .

#Now area plays a major role for the ARPU but here as customer churn rate 
# is higher in NORTHWEST/ROCKY MOUNTAIN AREA AND SOUTH FLORIDA, so special plans 
#should be rolled out for these customers as well as people with asian ethnicity.
# so a different strategy has to be maden to retend customers from these areas, as they may be 
# facing issues with internet, voice and data calls, so soft skills matters here
# as irate customers are their, technical department has to be informed and 
# for that time being customer should be given better discount and loyalty in services.

#######################################################
#######################################################
#Q-2).Validation of survey findings
#2A). whether "cost and billings" and "network and service quality" are important
# factors influencing the churn behaviour.

# some variables are associated with "Cost and Billing" and "Network and Service quality"
#variables associated with "cost and billing" are
#totmrc_mean which defines Monthly Recurring Charge is the base cost
#of the calling plan regardless of actual minutes used.	
summary(model4)
# as per the model if there is a unit increase in totmrc_mean then 
#it would get decrease by 0.00585827/unit in churn

#the billing amount is represent by variable rev_range which is range of revenue.
# as per the beta coefficient of rev_range  will get increment in churn by 0.00204796/unit on a unit increase.

# now overall revenue matters a lot for costing and billing as it is the sum of 
# data and voice
# from model ovrerev_mean , a unit increase in it will cause a increase 
#in churn by 0.00662439

#variable totrev  defines total revenue from all customer 
# from model , totrev , on increasing a unit will lead to a increase in churn 
# by 0.00024475

# so , obvious from the results,the increment in churn by these variables doesnt affect
# much and base plan charge seems to be good as they are the standard charges.
# so COST AND BILLING can not be considered as important factor for customer churn.
# beta coefficients values are close to zero.
########################
##variables associated with NETWORK AND SERVICE QUALITY
summary(model4)
#mou_range-Range of number of minutes of use
#On increment of unit in range of number of minutes of use , there is a 
#increase of 0.00025391units in churn

#change_mou-Percentage change in monthly minutes of use vs previous three month average
#On a unit increase in change_mou , there will be decrement in churn by 0.00062333 units.

#drop_blk_mean-Number of dropped or blocked calls is equal to the sum of blocked data and voice calls and dropped data and voice calls.
#on unit increase in drop_blk_mean, there is increment of churn by 0.00709848 units.

#drop_vce_range-Range of number of dropped (failed) voice calls
#on unit increase in drop_vce_range , there is a increase in churn by 0.01968546 units.

#mou_opkv_range-Range of unrounded minutes of use of off-peak voice calls
#on a unit increase in mou_opkv_range, decrease in churn by 0.00117811 units.

#iwylis_vce_mean-Mean number of inbound wireless to wireless voice calls
#on a unit increase in iwylis_vce_mean, decrease in churn by 0.01534377 units.

#avgqty-Average monthly number of calls over the life of the customer
#on a unit increase in avgqty, increase in churn by 0.00088293 units.

# retention-Number of days since last retention call
#on a unit increase in retention, there is increase in churn by 0.67593340 units.
# almost 67% 

#completed_mean-Completed number of calls is equal to the sum of completed data calls and completed voice calls.	
#on a unit increase in completed_mean, decrease in churn by 0.00194096 units.

# so in terms of NETWORK AND SERVICE QUALITY,variable retention is leading to a big churn.
# as customers calling for their problems facing in services , queries should be
# handled carefully with their grievances and checking their history , offers
# should be given to them.
#################################################

#2B) Are data usage connectivity issues turning out to be costly? In other words,
# is it leading to churn?
#A-2B). variables associated with data usage connectivity are
# comp_dat_mean, plcd_dat_mean,opk_dat_mean, blck_dat_mean,datovr_mean,datovr_range
#drop_dat_mean
#lets check for mean number of attempted data calls placed
#now these are the calls which actually got placed.
quantile(proj$plcd_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
# from the report it says around 15 % are the customers who are actually making data calls
# and using the internet.
# so customers are not using the internet that much so this can be a area of light to focus.
#so we need to influence customers to use data services which will also conclude to our'
#network quality connectivity which will reduce the customer churn and allows more customer
#to get invoved in data usage activity.

# for completed data calls
quantile(proj$comp_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
# so only 10% customers make complete data calls, which is less.

# mean no. of blocked or failed data calls
quantile(proj$blck_dat_Mean,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.82,0.84,0.85,0.90,1))
# now as compare to previous variables, its just 10% who are making internet calls
# which are either blocked or failed, so influencing more customers will also 
# allow ws to prove are network and data services efficiently better and it
# will help ws to improve download speed, data throtling and applications which do not work.
# From the survey reports, recommendations from family and friends are playing vital role.

###################################################
###################################################
#Q-3).Would you recommend rate plan migration as a proactive retention strategy?
#A-3). 
summary(model4)
# Now rate plan migration is a big challenge for ws to convert customers plan to optimal plans
# from this model the overrev_mean which is the sum of data and voice overrage revenues
#representing the overage revenue earned .
# the value of overrev_mean which is 0.00662439 doesnt shows any strong impact of overage billing 
# as an influencer of churn behaviour which simply means customers are on their respective plans giving a 
# good revenue and churn rate is also not impoacted at that level so as to go for rate paln migration.
# we need to focus on those customers whose needs like more minutes of use, more internet,
# so in such cases can be dealt separately.
# but overall rate plan migration might not help as proactive retention strategies.

##################################################
##################################################
#Q-4). What would be your recommendations on how to use this churn model for prioritisation of customers
# for a proactive retention campaigns in the future?
#A-4). Now proactive retention strategies can only be implemented after knowing the customers needs.
# factors which would play important role for proactive retention strategies in campaignimg are
# 1.minutes of use for both voice and data , so as per customer history we can find it out and target those 
# customers accordingly.
# 2.Associated overage charges
# 3. Monthly rates of their selevcted plans.

# With the help of gains chart we will get the idea for those customers which need to be prioritezed.
# gains charts
library(gains)
testing$churn <- as.numeric(testing$churn)
gains(testing$churn,predict(model4,type="response",newdata = testing),groups=10)
# Top 10% customers which are likely to churn are 16.4%
# Top 20% customers which are likely to churn are 29.4%
# Top 30% customers which are likely to churn are 42.1%

# High churn rate customers
testing$prob <- predict(model4,type="response",newdata=testing)
quantile(testing$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
# so as we can see top 30 % of the probabilities are between 0.2755509 and 0.7446702

# applying cutoff value to predict the customers who will likely to churn
likley <- predict(model4,type="response",newdata = testing)
likely <- ifelse(likley>=0.2755509,1,0)
table(likely,testing$churn)

# customer targeting with their customer id
targetted <- testing[testing$prob>0.2755509 & testing$prob<=0.7446702,"Customer_ID"]
targetted <- as.data.frame(targetted)
nrow(targetted)

# exporting the list of customers with their customer id
write.csv(targetted,"Customers_on_TARGET.csv",row.names = F)
# so from this target list we can focus on these customers with high churn probabilities
# with our retention strategies
# giving good offers and best customer service.

##############################################
##############################################
#Q-5).What would be the target segments for proactive retention campaigns?
# Falling ARPU forecast is also a concern and therefore , Mobicom would like to save their
# high revenues customers besides managing churn. Given a budget constraints of a contact 
#list of 20% of the subscriber pool, which subscriber should be prioritized if 
#"revenue saves" is also a priority besides controllinhg churn.
# In other words,controlling churn is the primary objective and revenue saves is the secondary
# objective.
#A-5). Now the major objectives are 
# To control the customer churn
# To save the high revnues

# finding predictions first
obser <- predict(model4,type="response",newdata=testing)
testing$prob <- predict(model4,type="response",newdata=testing)
quantile(testing$prob,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
# segmenting customers according to their probabilities
observed <- ifelse(obser<0.20,"low_score",ifelse(obser>=0.20 & obser<0.30,"medium_score","high_score"))
table(observed,testing$churn)

# for revenues
str(testing$churn)
quantile(testing$totrev,prob=c(0.10,0.20,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1))
rev_levels <- ifelse(testing$totrev<670.660,"low_revenue",ifelse(testing$totrev>=670.660&
                                                                   testing$totrev<1034.281,"medium_revenue","high_revenue"))
table(rev_levels)

table(obser,rev_levels) # high revenue customers can be focussed now.
# these are the levels of customers which need to be targetted.
# taking out target list
testing$prob_lvl <- ifelse(obser<0.20,"low_score",ifelse(obser>=0.20 & obser<0.30,"medium_score","high_score"))
testing$rev_levels <- ifelse(testing$totrev<670.660,"low_revenue",ifelse(testing$totrev>=670.660&
                                                                           testing$totrev<1034.281,"medium_revenue","high_revenue"))

# final list
targreport <- testing[testing$prob_lvl=="high_score"&testing$rev_levels=="high_revenue","Customer_ID"]
targreport <- data.frame(targreport)
nrow(targreport) # 1710 customers with high probability and high revenue.
# exporting file out
write.csv(targreport,"CUSTOMERS_WITH_HIGH_REVENUE_FOR TARGET.csv",row.names = F)

