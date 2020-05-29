rm(list=ls())

getwd()
setwd("E:\\Riz\\Edvancer\\Banking_Project")
train = read.csv("bank-full_train.csv",stringsAsFactors = F,na.strings = 'unknown')
test = read.csv("bank-full_test.csv",stringsAsFactors = F,na.strings = 'unknown')

test$y = NA

train$data = 'train'
test$data = 'test'
bank_proj = rbind(train,test)

library(dplyr)
glimpse(bank_proj)

#checking NA values
na_df = data.frame(sapply(bank_proj, function(x) sum(is.na(x))))
na_df

#Job
head(bank_proj$job)
table(bank_proj$job)
bank_proj$job[is.na(bank_proj$job)]="blue-collar"

#Education
head(bank_proj$education)
table(bank_proj$education)
bank_proj$education[is.na(bank_proj$education)]="secondary"

#Contact
head(bank_proj$contact)
table(bank_proj$contact)
bank_proj$contact[is.na(bank_proj$contact)]="cellular"

#Poutcome
head(bank_proj$poutcome)
table(bank_proj$poutcome)
bank_proj$poutcome[is.na(bank_proj$poutcome)]="failure"

#Checking outliers
outlier_limits = function(x,k){
  x_q1 = quantile(x,na.rm = T)[2]
  x_q3 = quantile(x,na.rm = T)[4]
  x_iqr = IQR(x,na.rm = T)
  ll = x_q1-k*x_iqr
  ul = x_q3+k*x_iqr
  x_limits = c(ll,ul)
  names(x_limits) = NULL
  return(x_limits)
}
#Outliers for banking.
ol_bal = outlier_limits(bank_proj$balance,1.5)
ol_bal
hist(bank_proj$balance,breaks = 50)
boxplot(bank_proj$balance)

sum(bank_proj$balance<ol_bal[1],na.rm = T)
bank_proj$balance[which(bank_proj$balance<ol_bal[1])] = ol_bal[1] #Flooring

sum(bank_proj$balance>ol_bal[2],na.rm = T)
bank_proj$balance[which(bank_proj$balance>ol_bal[2])] =ol_bal[2] # CAPPING

#Outliers for duration

ol_dur = outlier_limits(bank_proj$duration,1.5)
ol_dur
hist(bank_proj$duration,breaks = 50)
boxplot(bank_proj$duration)

sum(bank_proj$duration<ol_dur[1],na.rm = T)
#df_bank$balance[which(df_bank$balance<ol_bal[1])] = ol_bal[1] #Flooring

sum(bank_proj$duration>ol_dur[2],na.rm = T)
bank_proj$duration[which(bank_proj$duration>ol_dur[2])] =ol_dur[2] # CAPPING


#Binning cat variables
round(prop.table(table(bank_proj$job,bank_proj$y),1),2)

bank_proj$job = case_when(bank_proj$job %in% c('student','retired') ~ 'Job_A',
                          bank_proj$job %in% c('management','unemployed') ~ 'Job_B',
                          bank_proj$job %in% c('admin.','self-employed','technician') ~ 'Job_C',
                          TRUE ~ 'Job_D')
bank_proj=bank_proj %>% 
  mutate(month=gsub("jan",1,month),
         month=gsub("feb",2,month),
         month=gsub("mar",3,month),
         month=gsub("apr",4,month),
         month=gsub("may",5,month),
         month=gsub("jun",6,month),
         month=gsub("jul",7,month),
         month=gsub("aug",8,month),
         month=gsub("sep",9,month),
         month=gsub("oct",10,month),
         month=gsub("nov",11,month),
         month=gsub("dec",12,month))

bank_proj$month=as.numeric(bank_proj$month)

bank_proj$y = as.numeric(bank_proj$y=='yes')

#Dummy Coding
CreateDummies1 = function(data,var,freq_cutoff = 100){
  t = table(data[,var])
  t = t[t > freq_cutoff]
  t = sort(t)
  categories = names(t)[-1]
  
  for(cat in categories){
    name = paste(var,cat,sep='_')
    name = gsub(" ","",name)
    name = gsub("-","_",name)
    name = gsub("\\?","Q",name)
    name = gsub("<","LT_",name)
    name = gsub("\\+","",name)
    name = gsub(">","GT_",name)
    name = gsub("=","EQ_",name)
    name = gsub(",","",name)
    name = gsub("/","_",name)
    
    data[,name] = as.numeric(data[,var]==cat)
  }
  data[,var] = NULL
  return(data)
}

#cat_cols
cat_cols = names(bank_proj)[sapply(bank_proj, is.character)]

cat_cols = cat_cols[!cat_cols %in% c('data','y')]
cat_cols

for(i in cat_cols){
  bank_proj = CreateDummies1(bank_proj,i,50)
}
glimpse(bank_proj)

#separating train & Test

df_train = bank_proj %>% filter(data=='train') %>% select(-data)
df_test = bank_proj %>% filter(data=='test') %>% select(-data,-y)

#Creating validation data 
set.seed(123)
s = sample(1:nrow(df_train),0.8*nrow(df_train))
train_mod = df_train[s,]
train_val = df_train[-s,]

library(car)

for_vif=lm(y~.-ID,data=train_mod)
sort(vif(for_vif),decreasing = T)[1:3]

fit=glm(y~.-ID,data=train_mod,family="binomial")
summary(fit)
fit=step(fit)
formula(fit)

fit1 = glm(y ~ balance + duration + campaign + pdays + previous + job_Job_B + 
             job_Job_C + job_Job_D + marital_single + marital_married + 
             education_tertiary + education_secondary + default_no + housing_yes + 
             loan_no + poutcome_other + poutcome_failure, data = train_mod,family = 'binomial')
summary(fit1)


fit1 = glm(y ~ balance + duration + campaign + pdays + previous + job_Job_B + 
             job_Job_C + job_Job_D + marital_single + marital_married + 
             education_tertiary + education_secondary + housing_yes + loan_no 
             + poutcome_other + poutcome_failure, data = train_mod,family = 'binomial')
summary(fit1)

fit1 = glm(y ~ balance + duration + campaign + pdays  + job_Job_B + 
             job_Job_C + job_Job_D + marital_single + marital_married + 
             education_tertiary + education_secondary + housing_yes + loan_no 
           + poutcome_other + poutcome_failure, data = train_mod,family = 'binomial')
summary(fit1)

fit1 = glm(y ~ balance + duration + campaign + pdays  + job_Job_B + 
             job_Job_C + job_Job_D + marital_married + 
             education_tertiary + education_secondary + housing_yes + loan_no 
           + poutcome_other + poutcome_failure, data = train_mod,family = 'binomial')
summary(fit1)

#predict on act_train data to set a cuttoff
train_mod$score=predict(fit1,newdata=train_mod,type="response")
head(train_mod$score)

cutoff_data=NULL
cutoffs=round(seq(0,1,length=100),3)
cutoffs

for(cutoff in cutoffs){
  
  predicted=as.numeric(train_mod$score>cutoff)
  
  TP=sum(predicted==1 & train_mod$y==1)
  TN=sum(predicted==0 & train_mod$y==0)
  FP=sum(predicted==1 & train_mod$y==0)
  FN=sum(predicted==0 & train_mod$y==1)
  
  cutoff_data=rbind.data.frame(cutoff_data,c(cutoff,TP,FP,FN,TN))
  
}

colnames(cutoff_data)=c("cutoff","TP","FP","FN","TN")
View(cutoff_data)

P=sum(train_mod$y==1)
N=sum(train_mod$y==0)

cutoff_data=cutoff_data %>% 
  mutate(Sn=TP/P,
         Sp=TN/N,
         dist=sqrt((1-Sn)**2+(1-Sp)**2),
         P=FN+TP,
         N=TN+FP) %>% 
  mutate(KS=abs((TP/P)-(FP/N))) %>% 
  mutate(Accuracy=(TP+TN)/(P+N)) %>% 
  mutate(M=(8*FN+2*FP)/(P+N)) %>% 
  select(-P,-N)

KS_cutoff=cutoff_data$cutoff[which.max(cutoff_data$KS)][1]
KS_cutoff

train_val$score=predict(fit1,newdata=train_val,type="response")

table(as.numeric(train_val$score>KS_cutoff),train_val$y)

P=sum(train_val$y==1)
N=sum(train_val$y==0)
(602/P)-(1123/N)

test_last = predict(fit1,newdata = df_test,type='response')
head(test_last)
test_class = as.numeric(test_last>KS_cutoff)
head(test_class)

write.csv(test_class,'Riyaz_Ali_P5_Part2.csv',row.names = F)





























































