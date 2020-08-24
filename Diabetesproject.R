#######--------------------- Final Project ---------------------------------- ######
######------------------------ Diabetes 130-US hospitals for  years 1999-2008 Data Set ---------------#


#--------------------------------------------------- Data Preparation---------------------------------------------------#
# set working directory
setwd("D:\\MSDA\\Data_Mining2\\Final_Project")
# Import the dataset
diabetes = read.csv("diabetic_data1.csv", header = T)
str(diabetes)
# Removing the column encounter_id
diabetes = diabetes[, -1]
str(diabetes)
dim(diabetes)

#------Dealing with Missing Values:
# replace the "?" in the columns with NA
diabetes[diabetes=="?"] = NA
# Counting the amount of NA values in the dataset in general
sum(is.na(diabetes))
# Distribution of the NA's among the columns
allmisscols=sapply(diabetes, function(x) sum(length(which(is.na(x)))))
allmiss=as.table(allmisscols[allmisscols>0])
# percentage of missing values for each of these columns
allmiss_pct = round(allmiss*100/101766, 2)
barplot(allmiss_pct, ylim = c(0, 100), ylab = "Percentage of NA's", las=2, main = "Missing Values")
# detect variables having 30% or more missing values
abline(h=30, col="red")

# removing three variables having more than 30% of their data missing:  payer_code, medical_specialty and weight 
#diabetes = diabetes[,-c(diabetes$weight, diabetes$payer_code, diabetes$medical_specialty)]
diabetes = subset(diabetes, select = -c(weight, payer_code, medical_specialty))

###------------- Near Zero-Variance Variables(NZV)
# Zero-variables:
summary(diabetes$examide)
summary(diabetes$citoglipton)
# Near zero-variance:
library(caret)
x = nearZeroVar(diabetes)
# name of the variables with NZV
names(diabetes)[x]
# removing variables with NZV
diabetes = diabetes[, -x]


#---------Mutiple Encounters of a patient
# number of rows with duplicate patient_nbr
#nrow(diabetes[unique(diabetes$patient_nbr)])
nrow(diabetes[duplicated(diabetes$patient_nbr), ])

# removing duplicate of patient_nbr, by keeping the ones with max(time_in_hospital) among the duplicates. 
library(dplyr)
diabetes=diabetes %>%
  group_by(patient_nbr)%>%
  filter(row_number(desc(time_in_hospital))==1)

#-------- Categorical variables
diabetes$gender = factor(diabetes$gender)
summary(diabetes$gender)
# eliminate the unknown type of gender:
levels(diabetes$gender)=c('Female', 'Male', 'Female')

# consolidated variable Age from 10-level factor to 4
summary(diabetes$age)
levels(diabetes$age)=c("[0-30)","[0-30)","[0-30)","[30-60)","[30-60)","[30-60)","[60-80)","[60-80)","80+","80+")

# reduce the levels of admission type id
diabetes$admission_type_id=factor(diabetes$admission_type_id)
summary(diabetes$admission_type_id)
# change 8 levels to 3
levels(diabetes$admission_type_id) = c("urgent","urgent","elective","urgent","unknown","unknown","urgent","unknown")

# admission source
diabetes$admission_source_id = factor(diabetes$admission_source_id)
summary(diabetes$admission_source_id) # 25 levels
# reduce the levels of admission_source_id to 5 levels, r:referral, t:transfer, b:birth, o:other, u:unknown
levels(diabetes$admission_source_id)=c("r",	"r", "r", "t", "t",	"t",	"r", "o",	"u", "t",	"b",	"b",	"b",	"b",	
                                       "u",	"u",	"t",	"t",	"u", "u",	"t","b", "b",	"t")

# discharge_disposition_id variable
diabetes$discharge_disposition_id=factor(diabetes$discharge_disposition_id)
summary(diabetes$discharge_disposition_id)
# reduce the levels of discharge_disposition_id to 5 levels, d:discharge, h:hospice, u:unknown, o:other
levels(diabetes$discharge_disposition_id)=c("d","d",	"d",	"d","d","d","o","d","h","d","o","o","h","h",
                                           "d","d","d","u","o","o","o","d","d","d","u",	"u","d","d","d")


# Diagnostic information
#----Reduces levels for diag_1, diag_2, diag_3:
#-- for diag_1
diabetes$diag_1_fact = "NA"
dia2=as.character(diabetes$diag_1)
diabetes$diag_1_fact[(dia2>='390' & dia2<='459')|(dia2=='785')]="circulatory"
diabetes$diag_1_fact[(dia2>='460' & dia2<='519')|(dia2=='786')]="Respiratory"
diabetes$diag_1_fact[(dia2>='520' & dia2<='579')|(dia2=='787')]="Digestive"
diabetes$diag_1_fact[(dia2>='250' & dia2<='251')]="Diabetes"
diabetes$diag_1_fact[(dia2>='800' & dia2<='999')]="Injury"
diabetes$diag_1_fact[(dia2>='710' & dia2<='739')]="Musculoskeletal"
diabetes$diag_1_fact[(dia2>='580' & dia2<='629')|(dia2=='788')]="Genitourinary"
diabetes$diag_1_fact[(dia2>='140' & dia2<='239')]="Neoplasms"
diabetes$diag_1_fact[(diabetes$diag_1_fact)=='NA']="Other"
table(diabetes$diag_1_fact)
diabetes$diag_1_fact = factor(diabetes$diag_1_fact)

#-- for diag_2
diabetes$diag_2_fact = "NA"
dia2=as.character(diabetes$diag_2)
diabetes$diag_2_fact[(dia2>='390' & dia2<='459')|(dia2=='785')]="circulatory"
diabetes$diag_2_fact[(dia2>='460' & dia2<='519')|(dia2=='786')]="Respiratory"
diabetes$diag_2_fact[(dia2>='520' & dia2<='579')|(dia2=='787')]="Digestive"
diabetes$diag_2_fact[(dia2>='250' & dia2<='251')]="Diabetes"
diabetes$diag_2_fact[(dia2>='800' & dia2<='999')]="Injury"
diabetes$diag_2_fact[(dia2>='710' & dia2<='739')]="Musculoskeletal"
diabetes$diag_2_fact[(dia2>='580' & dia2<='629')|(dia2=='788')]="Genitourinary"
diabetes$diag_2_fact[(dia2>='140' & dia2<='239')]="Neoplasms"
diabetes$diag_2_fact[(diabetes$diag_2_fact)=='NA']="Other"
table(diabetes$diag_2_fact)
diabetes$diag_2_fact = factor(diabetes$diag_2_fact)

# for diag_3
diabetes$diag_3_fact = "NA"
dia2=as.character(diabetes$diag_3)
diabetes$diag_3_fact[(dia2>='390' & dia2<='459')|(dia2=='785')]="circulatory"
diabetes$diag_3_fact[(dia2>='460' & dia2<='519')|(dia2=='786')]="Respiratory"
diabetes$diag_3_fact[(dia2>='520' & dia2<='579')|(dia2=='787')]="Digestive"
diabetes$diag_3_fact[(dia2>='250' & dia2<='251')]="Diabetes"
diabetes$diag_3_fact[(dia2>='800' & dia2<='999')]="Injury"
diabetes$diag_3_fact[(dia2>='710' & dia2<='739')]="Musculoskeletal"
diabetes$diag_3_fact[(dia2>='580' & dia2<='629')|(dia2=='788')]="Genitourinary"
diabetes$diag_3_fact[(dia2>='140' & dia2<='239')]="Neoplasms"
diabetes$diag_3_fact[(diabetes$diag_3_fact)=='NA']="Other"
table(diabetes$diag_3_fact)
diabetes$diag_3_fact = factor(diabetes$diag_3_fact)

diabetes = subset(diabetes, select = -c(diag_1, diag_2, diag_3))

# remove the patient_nbr variable, it's an ID variable
diabetes=subset(diabetes, select = -c(patient_nbr))
# factor variable in the dataset
names(diabetes)[sapply(diabetes,is.factor)]

# numeric variables in the datasets:
names(diabetes)[sapply(diabetes,is.numeric)]


# Visualization of the numeric variables
op = par(mfrow=c(3,3))
hist(diabetes$time_in_hospital, col = 'lightblue')
hist(diabetes$num_lab_procedures, col = 'lightblue')
hist(diabetes$num_procedures, col = 'lightblue')
hist(diabetes$num_medications, col = 'lightblue')
hist(diabetes$number_outpatient, col = 'lightblue')
hist(diabetes$number_emergency, col = 'lightblue')
hist(diabetes$number_inpatient, col = 'lightblue')
hist(diabetes$number_diagnoses, col = 'lightblue')
par(op)


# check for outliers
op = par(mfrow=c(3,3))
boxplot(diabetes$time_in_hospital, col = 'lightblue', main='time in hospital')
boxplot(diabetes$num_lab_procedures, col = 'lightblue',main='number of labs prorcedures')
boxplot(diabetes$num_procedures, col = 'lightblue', main='number of procedures')
boxplot(diabetes$num_medications, col = 'lightblue', main = 'number of medications')
boxplot(diabetes$number_outpatient, col = 'lightblue', main='number out patient')
boxplot(diabetes$number_emergency, col = 'lightblue', main='number of emergency')
boxplot(diabetes$number_inpatient, col = 'lightblue', main='number_inpatient')
boxplot(diabetes$number_diagnoses, col = 'lightblue', main='number_diagnoses')
par(op)

summary(diabetes[,c('time_in_hospital','num_lab_procedures','num_procedures', 'num_medications',
                    'number_outpatient', 'number_emergency','number_inpatient','number_diagnoses')])

# Remove missing values in race:
diabetes=diabetes[!is.na(diabetes$race),]
summary(diabetes$race)

# prepared dataset
summary(diabetes)
str(diabetes)




#------------------------------------------------------------------------------------------------------------------
# Export the cleaned data in excel, so we can import it in python to do the unsupervised learning
write.table(diabetes, file="clean_diabetes.csv",sep=",",row.names=F)


###---------------------- PCA
# we now perform a PCA on the numerical variables
diabet_num = diabetes[,c('time_in_hospital','num_lab_procedures','num_procedures', 'num_medications',
                         'number_outpatient', 'number_emergency','number_inpatient','number_diagnoses')]
pr.out = prcomp(diabet_num, scale=TRUE)

# principal component loading:
pr.out$rotation
# principal components score vectors
dim(pr.out$x)
head(pr.out$x)
# plot the first two components
biplot(pr.out, scale=0)
# proportion of variance explained by each principal component
pve = (pr.out$sdev^2)/sum(pr.out$sdev^2)
pve

# plot the PVE explained by each components
plot(pve , xlab=" Principal Component ", ylab=" Proportion of Variance Explained ", ylim=c(0,1) ,type='b')
plot(cumsum (pve ), xlab=" Principal Component ", ylab ="
     Cumulative Proportion of Variance Explained ", ylim=c(0,1) ,type='b')