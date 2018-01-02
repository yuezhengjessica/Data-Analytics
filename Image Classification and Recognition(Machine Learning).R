
install.packages("imager")
library(imager)


#----------------------------------------------------------------------------------------------------
setwd("C:/Users/yuezh/Desktop/homework data/airplanes/train")
library(jpeg)
library(tidyr)
temp1=list.files(pattern = "*.jpg")
temp1
class(temp1)
myfiles1=lapply(temp1,readJPEG)
myfiles1[1]
dim(myfiles1[1])
class(myfiles1[1])
summary(myfiles1)
str(myfiles1)
head(myfiles1)
length(myfiles1)

class(myfiles1[1])
myfiles1[1]
max<-rep(NA,590)
for(i in 1:590){
  max[i]<-length(unlist(myfiles1[i]))
}
output<-matrix(rep(NA,590*max(max)),ncol=max(max),nrow=590)
for(i in 1:length(myfiles1)){
  temp<-unlist(myfiles1[i])
  temp2<-c(temp,rep(NA,max(max)-length(temp)))
  output[i,]<-temp2
}

output
dim(output)
class(output)
output[,1]

length(temp1)
class(temp1)
temp1
library(dplyr)

label<-read.csv("airplanes_data.csv",stringsAsFactors = FALSE)
label
length(temp1)
dim(label)
library(data.table)
setDT(label)
output2<-label[,which(label$Image %in% temp1==TRUE)]
output2
length(output2)
as.vector(output2)
label_t<-label[output2,]
label_t
dim(label_t)
label_train<-label_t[,c("Image","final")]
label_train
label_train1<-arrange(label_train,Image)
label_train1
label2<-label_train1[!duplicated(label_train1$Image),]
label2
dim(label2)

Airplane_train_data<-cbind(output,label2$final)
head(Airplane_train_data,1)
dim(Airplane_train_data)
str(Airplane_train_data)
class(Airplane_train_data)
class(output)
dim(output)
library(reshape2)
colnames(Airplane_train_data) <- 1:573535
rownames(Airplane_train_data) <- 1:590

model <- glm (final ~ ., data = Airplane_train_data, family = binomial)
logitMod <- glm(Airplane_train_data[,573535] ~., data=data.frame(Airplane_train_data), family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores

install.packages("e1071")
library(e1071)
s_model<-svm(Airplane_train_data$"573535"~.,data=Airplane_train_data,kernel="linear",type='C')
summary(s_model)
pred<-predict(s_model,newdata=apltest)
s_frame1=data.frame(predict=pred,real=test_act)
table(s_frame1)
28/39

#SVMKernel
svm_model<-svm(apl_result$final~.,data=apl_result,kernel=("radial"),type='C')
summary(svm_model)
pred<-predict(svm_model,newdata=apltest)
svm_frame1=data.frame(predict=pred,real=test_act)
table(svm_frame1)
#----------------------------------------------------------------------------------------------------------
setwd("C:/Users/yuezh/Desktop/homework data/airplanes/test")
AirTestFile=list.files(pattern = "*.jpg")
Datafiles_AirTest=lapply(AirTestFile,readJPEG)
length(Datafiles_AirTest)
class(Datafiles_AirTest)
AirTest_max<-rep(NA,590)
for(i in 1:590){
  AirTest_max[i]<-length(unlist(Datafiles_AirTest[i]))
}
AirTest_output<-matrix(rep(NA,210*max(AirTest_max)),ncol=max(AirTest_max),nrow=210)
for(i in 1:length(AirTest_max)){
  temp_AirTest<-unlist(Datafiles_AirTest[i])
  temp2_AirTest<-c(temp_AirTest,rep(NA,max(AirTest_max)-length(temp_AirTest)))
  AirTest_output[i,]<-temp2_AirTest
}

AirTest_output
View(AirTest_output)



#1)2load Airplane test csv
Airplane_test_label<-read.csv("airplanes_data.csv",stringsAsFactors = FALSE)
Airplane_test_label

library(data.table)
setDT(Airplane_test_label)
Airplane_test_output2<-Airplane_test_label[,which(Airplane_test_label$Image %in% AirTestFile==TRUE)]
Airplane_test_output2
length(Airplane_test_output2)%>%as.vector()
Airplane_test_label_t<-Airplane_test_label[Airplane_test_output2,]
Airplane_test_label_train<-Airplane_test_label_t[,c("Image","final")]

Airplane_test_label2<-Airplane_test_label_train[!duplicated(Airplane_test_label_train$Image),]
Airplane_test_label2
dim(Airplane_test_label2)
Airplane_test_data<-cbind(AirTest_output,Airplane_test_label2$final)
head(Airplane_test_data)

model <- glm (final ~ ., data = Airplane_train_data, family = binomial)
logitMod <- glm(Airplane_train_data[,573535] ~., data=data.frame(Airplane_train_data), family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores

install.packages("e1071")
library(e1071)
s_model<-svm(Airplane_train_data$"573535"~.,data=Airplane_train_data,kernel="linear",type='C')
summary(s_model)
pred<-predict(s_model,newdata=apltest)
s_frame1=data.frame(predict=pred,real=test_act)
table(s_frame1)
28/39

#SVMKernel
svm_model<-svm(apl_result$final~.,data=apl_result,kernel=("radial"),type='C')
summary(svm_model)
pred<-predict(svm_model,newdata=apltest)
svm_frame1=data.frame(predict=pred,real=test_act)
table(svm_frame1)


#2)1load Bonsai Train dataset
#-----------------------------------------------------------------------------------------------------
setwd("C:/Users/yuezh/Desktop/homework data/Bonsai/train")
Bonsai_Train_File=list.files(pattern = "*.jpg")
Bonsai_Train_File
Datafiles_BonsaiTrain=lapply(Bonsai_Train_File,readJPEG)
length(Datafiles_BonsaiTrain)
class(Datafiles_BonsaiTrain)
BonsaiTrain_max<-rep(NA,90)
for(i in 1:90){
  BonsaiTrain_max[i]<-length(unlist(Datafiles_BonsaiTrain[i]))
}
BonsaiTrain_output<-matrix(rep(NA,90*max(BonsaiTrain_max)),ncol=max(BonsaiTrain_max),nrow=90)
for(i in 1:length(BonsaiTrain_max)){
  temp_BonsaiTrain<-unlist(Datafiles_BonsaiTrain[i])
  temp2_BonsaiTrain<-c(temp_BonsaiTrain,rep(NA,max(BonsaiTrain_max)-length(temp_BonsaiTrain)))
  BonsaiTrain_output[i,]<-temp2_BonsaiTrain
}
BonsaiTrain_output
View(BonsaiTrain_output)

#2)1load Bonsai train csv
Bonsai_train_label<-read.csv("Bonsai_data.csv",stringsAsFactors = FALSE)
Bonsai_train_label

library(data.table)
setDT(Bonsai_train_label)
Bonsai_train_output2<-Bonsai_train_label[,which(Bonsai_train_label$Image %in% Bonsai_Train_File==TRUE)]
Bonsai_train_output2
length(Bonsai_train_output2)%>%as.vector()
Bonsai_train_label_t<-Bonsai_train_label[Bonsai_train_output2,]
Bonsai_train_label_train<-Bonsai_train_label_t[,c("Image","final")]
Bonsai_train_label2<-Bonsai_train_label_train[!duplicated(Bonsai_train_label_train$Image),]
Bonsai_train_label2
dim(Bonsai_train_label2)
Bonsai_train_data<-cbind(BonsaiTrain_output,Bonsai_train_label2$final)
head(Bonsai_train_data)

model <- glm (final ~ ., data = Airplane_train_data, family = binomial)
logitMod <- glm(Airplane_train_data[,573535] ~., data=data.frame(Airplane_train_data), family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores

install.packages("e1071")
library(e1071)
s_model<-svm(Airplane_train_data$"573535"~.,data=Airplane_train_data,kernel="linear",type='C')
summary(s_model)
pred<-predict(s_model,newdata=apltest)
s_frame1=data.frame(predict=pred,real=test_act)
table(s_frame1)
28/39

#SVMKernel
svm_model<-svm(apl_result$final~.,data=apl_result,kernel=("radial"),type='C')
summary(svm_model)
pred<-predict(svm_model,newdata=apltest)
svm_frame1=data.frame(predict=pred,real=test_act)
table(svm_frame1)

#2)2load Bonsai test dataset
#--------------------------------------------------------------------------------------------------------------------
setwd("C:/Users/yuezh/Desktop/homework data/Bonsai/test")
Bonsai_Test_File=list.files(pattern = "*.jpg")
Bonsai_Test_File
Datafiles_BonsaiTest=lapply(Bonsai_Test_File,readJPEG)
length(Datafiles_BonsaiTest)
class(Datafiles_BonsaiTest)
BonsaiTest_max<-rep(NA,38)
for(i in 1:38){
  BonsaiTest_max[i]<-length(unlist(Datafiles_BonsaiTest[i]))
}
BonsaiTest_output<-matrix(rep(NA,38*max(BonsaiTest_max)),ncol=max(BonsaiTest_max),nrow=38)
for(i in 1:length(BonsaiTest_max)){
  temp_BonsaiTest<-unlist(Datafiles_BonsaiTest[i])
  temp2_BonsaiTest<-c(temp_BonsaiTest,rep(NA,max(BonsaiTest_max)-length(temp_BonsaiTest)))
  BonsaiTest_output[i,]<-temp2_BonsaiTest
}
BonsaiTest_output


#2)1load Bonsai test csv
Bonsai_test_label<-read.csv("Bonsai_data.csv",stringsAsFactors = FALSE)
Bonsai_test_label

library(data.table)
setDT(Bonsai_test_label)
Bonsai_test_output2<-Bonsai_test_label[,which(Bonsai_test_label$Image %in% Bonsai_Test_File==TRUE)]
Bonsai_test_output2
length(Bonsai_test_output2)%>%as.vector()
Bonsai_test_label_t<-Bonsai_test_label[Bonsai_test_output2,]
Bonsai_test_label_train<-Bonsai_test_label_t[,c("Image","final")]
Bonsai_test_label2<-Bonsai_test_label_train[!duplicated(Bonsai_test_label_train$Image),]
Bonsai_test_label2
dim(Bonsai_test_label2)
Bonsai_test_data<-cbind(BonsaiTest_output,Bonsai_test_label2$final)
length(Bonsai_test_data)
head(Bonsai_test_data)

model <- glm (final ~ ., data = Airplane_train_data, family = binomial)
logitMod <- glm(Airplane_train_data[,573535] ~., data=data.frame(Airplane_train_data), family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores

install.packages("e1071")
library(e1071)
s_model<-svm(Airplane_train_data$"573535"~.,data=Airplane_train_data,kernel="linear",type='C')
summary(s_model)
pred<-predict(s_model,newdata=apltest)
s_frame1=data.frame(predict=pred,real=test_act)
table(s_frame1)
28/39

#SVMKernel
svm_model<-svm(apl_result$final~.,data=apl_result,kernel=("radial"),type='C')
summary(svm_model)
pred<-predict(svm_model,newdata=apltest)
svm_frame1=data.frame(predict=pred,real=test_act)
table(svm_frame1)

#3)1 load Faces train dataset
#----------------------------------------------------------------------------------------------------------------
setwd("C:/Users/yuezh/Desktop/homework data/Faces/train")
Faces_Train_File=list.files(pattern = "*.jpg")
Faces_Train_File
Datafiles_FacesTrain=lapply(Faces_Train_File,readJPEG)
length(Datafiles_FacesTrain)
class(Datafiles_FacesTrain)
FacesTrain_max<-rep(NA,311)
for(i in 1:311){
  FacesTrain_max[i]<-length(unlist(Datafiles_FacesTrain[i]))
}
FacesTrain_output<-matrix(rep(NA,311*max(FacesTrain_max)),ncol=max(FacesTrain_max),nrow=311)
for(i in 1:length(FacesTrain_max)){
  temp_FacesTrain<-unlist(Datafiles_FacesTrain[i])
  temp2_FacesTrain<-c(temp_FacesTrain,rep(NA,max(FacesTrain_max)-length(temp_FacesTrain)))
  FacesTrain_output[i,]<-temp2_FacesTrain
}
FacesTrain_output
View(FacesTrain_output)

#2)1load Faces Train csv
Faces_train_label<-read.csv("Faces_data.csv",stringsAsFactors = FALSE)
Faces_train_label

library(data.table)
setDT(Faces_train_label)
Faces_train_output2<-Faces_train_label[,which(Faces_train_label$Image %in% Faces_Train_File==TRUE)]
Faces_train_output2
length(Faces_train_output2)%>%as.vector()
Faces_train_label_t<-Faces_train_label[Faces_train_output2,]
Faces_train_label_train<-Faces_train_label_t[,c("Image","final")]
Faces_train_label2<-Faces_train_label_train[!duplicated(Faces_train_label_train$Image),]
Faces_train_label2
dim(Faces_train_label2)
Faces_train_data<-cbind(FacesTrain_output,Faces_train_label2$final)
length(Faces_train_data)
head(Faces_train_data)

model <- glm (final ~ ., data = Airplane_train_data, family = binomial)
logitMod <- glm(Airplane_train_data[,573535] ~., data=data.frame(Airplane_train_data), family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores

install.packages("e1071")
library(e1071)
s_model<-svm(Airplane_train_data$"573535"~.,data=Airplane_train_data,kernel="linear",type='C')
summary(s_model)
pred<-predict(s_model,newdata=apltest)
s_frame1=data.frame(predict=pred,real=test_act)
table(s_frame1)
28/39

#SVMKernel
svm_model<-svm(apl_result$final~.,data=apl_result,kernel=("radial"),type='C')
summary(svm_model)
pred<-predict(svm_model,newdata=apltest)
svm_frame1=data.frame(predict=pred,real=test_act)
table(svm_frame1)
#------------------------------------------------------------------------------------------------------
setwd("C:/Users/yuezh/Desktop/homework data/Faces/test")
FacesTestFile=list.files(pattern = "*.jpg")
Datafiles_FacesTest=lapply(FacesTestFile,readJPEG)
length(Datafiles_FacesTest)
class(Datafiles_FacesTest)
FacesTest_max<-rep(NA,124)
for(i in 1:124){
  FacesTest_max[i]<-length(unlist(Datafiles_FacesTest[i]))
}
FacesTest_output<-matrix(rep(NA,124*max(FacesTest_max)),ncol=max(FacesTest_max),nrow=124)
for(i in 1:length(FacesTest_max)){
  temp_FacesTest<-unlist(Datafiles_FacesTest[i])
  temp2_FacesTest<-c(temp_FacesTest,rep(NA,max(FacesTest_max)-length(temp_FacesTest)))
  FacesTest_output[i,]<-temp2_FacesTest
}

FacesTest_output
View(FacesTest_output)

#1)2load Faces test csv
Faces_test_label<-read.csv("Faces_data.csv",stringsAsFactors = FALSE)
Faces_test_label

library(data.table)
setDT(Faces_test_label)
Faces_test_output2<-Faces_test_label[,which(Faces_test_label$Image %in% FacesTestFile==TRUE)]
Faces_test_output2
length(Faces_test_output2)%>%as.vector()
Faces_test_label_t<-Faces_test_label[Faces_test_output2,]
Faces_test_label_t
Faces_test_label_train<-Faces_test_label_t[,c("Image","final")]
Faces_test_label_train
Faces_test_label2<-Faces_test_label_train[!duplicated(Faces_test_label_train$Image),]
Faces_test_label2
dim(Faces_test_label2)
Faces_test_data<-cbind(FacesTest_output,Faces_test_label2$final)
head(Faces_test_data)


model <- glm (final ~ ., data = Airplane_train_data, family = binomial)
logitMod <- glm(Airplane_train_data[,573535] ~., data=data.frame(Airplane_train_data), family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores

install.packages("e1071")
library(e1071)
s_model<-svm(Airplane_train_data$"573535"~.,data=Airplane_train_data,kernel="linear",type='C')
summary(s_model)
pred<-predict(s_model,newdata=apltest)
s_frame1=data.frame(predict=pred,real=test_act)
table(s_frame1)
28/39

#SVMKernel
svm_model<-svm(apl_result$final~.,data=apl_result,kernel=("radial"),type='C')
summary(svm_model)
pred<-predict(svm_model,newdata=apltest)
svm_frame1=data.frame(predict=pred,real=test_act)
table(svm_frame1)
#----------------------------------------------------------------------------------------------------
setwd("C:/Users/yuezh/Desktop/homework data/Motorbikes/train")
motorbikes_Train_File=list.files(pattern = "*.jpg")
motorbikes_Train_File
Datafiles_motorbikesTrain=lapply(motorbikes_Train_File,readJPEG)
length(Datafiles_motorbikesTrain)
class(Datafiles_motorbikesTrain)
motorbikesTrain_max<-rep(NA,575)
for(i in 1:575){
  motorbikesTrain_max[i]<-length(unlist(Datafiles_motorbikesTrain[i]))
}
motorbikesTrain_output<-matrix(rep(NA,575*max(motorbikesTrain_max)),ncol=max(motorbikesTrain_max),nrow=575)
for(i in 1:length(motorbikesTrain_max)){
  temp_motorbikesTrain<-unlist(Datafiles_motorbikesTrain[i])
  temp2_motorbikesTrain<-c(temp_motorbikesTrain,rep(NA,max(motorbikesTrain_max)-length(temp_motorbikesTrain)))
  motorbikesTrain_output[i,]<-temp2_motorbikesTrain
}
motorbikesTrain_output


#2)1load motorbike Train csv
motorbikes_train_label<-read.csv("motorbikes_data.csv",stringsAsFactors = FALSE)
motorbikes_train_label

library(data.table)
setDT(motorbikes_train_label)
motorbikes_train_output2<-motorbikes_train_label[,which(motorbikes_train_label$Image %in% motorbikes_Train_File==TRUE)]
motorbikes_train_output2
length(motorbikes_train_output2)%>%as.vector()
motorbikes_train_label_t<-motorbikes_train_label[motorbikes_train_output2,]
motorbikes_train_label_train<-motorbikes_train_label_t[,c("Image","final")]
motorbikes_train_label2<-motorbikes_train_label_train[!duplicated(motorbikes_train_label_train$Image),]
motorbikes_train_label2
dim(motorbikes_train_label2)
motorbikes_train_data<-cbind(motorbikesTrain_output,motorbikes_train_label2$final)
length(motorbikes_train_data)
dim(motorbikes_train_data)
head(motorbikes_train_data)


model <- glm (final ~ ., data = Airplane_train_data, family = binomial)
logitMod <- glm(Airplane_train_data[,573535] ~., data=data.frame(Airplane_train_data), family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores

install.packages("e1071")
library(e1071)
s_model<-svm(Airplane_train_data$"573535"~.,data=Airplane_train_data,kernel="linear",type='C')
summary(s_model)
pred<-predict(s_model,newdata=apltest)
s_frame1=data.frame(predict=pred,real=test_act)
table(s_frame1)
28/39

#SVMKernel
svm_model<-svm(apl_result$final~.,data=apl_result,kernel=("radial"),type='C')
summary(svm_model)
pred<-predict(svm_model,newdata=apltest)
svm_frame1=data.frame(predict=pred,real=test_act)
table(svm_frame1)
#------------------------------------------------------------------------------------------------------
setwd("C:/Users/yuezh/Desktop/homework data/motorbikes/test")
motorbikesTestFile=list.files(pattern = "*.jpg")
Datafiles_motorbikesTest=lapply(motorbikesTestFile,readJPEG)
length(Datafiles_motorbikesTest)
class(Datafiles_motorbikesTest)
motorbikesTest_max<-rep(NA,223)
for(i in 1:223){
  motorbikesTest_max[i]<-length(unlist(Datafiles_motorbikesTest[i]))
}
motorbikesTest_output<-matrix(rep(NA,223*max(motorbikesTest_max)),ncol=max(motorbikesTest_max),nrow=223)
for(i in 1:length(motorbikesTest_max)){
  temp_motorbikesTest<-unlist(Datafiles_motorbikesTest[i])
  temp2_motorbikesTest<-c(temp_motorbikesTest,rep(NA,max(motorbikesTest_max)-length(temp_motorbikesTest)))
  motorbikesTest_output[i,]<-temp2_motorbikesTest
}

motorbikesTest_output

#1)2load Faces test csv
motorbikes_test_label<-read.csv("motorbikes_data.csv",stringsAsFactors = FALSE)
motorbikes_test_label

library(data.table)
setDT(motorbikes_test_label)
motorbikes_test_output2<-motorbikes_test_label[,which(motorbikes_test_label$Image %in% motorbikesTestFile==TRUE)]
motorbikes_test_output2
length(motorbikes_test_output2)%>%as.vector()
motorbikes_test_label_t<-motorbikes_test_label[motorbikes_test_output2,]
motorbikes_test_label_t
motorbikes_test_label_train<-motorbikes_test_label_t[,c("Image","final")]
motorbikes_test_label_train
motorbikes_test_label2<-motorbikes_test_label_train[!duplicated(motorbikes_test_label_train$Image),]
motorbikes_test_label2
dim(motorbikes_test_label2)
motorbikes_test_data<-cbind(motorbikesTest_output,motorbikes_test_label2$final)
head(motorbikes_test_data)


model <- glm (final ~ ., data = Airplane_train_data, family = binomial)
logitMod <- glm(Airplane_train_data[,573535] ~., data=data.frame(Airplane_train_data), family=binomial(link="logit"))

predicted <- plogis(predict(logitMod, testData))  # predicted scores
predicted <- predict(logitMod, testData, type="response")  # predicted scores

install.packages("e1071")
library(e1071)
s_model<-svm(Airplane_train_data$"573535"~.,data=Airplane_train_data,kernel="linear",type='C')
summary(s_model)
pred<-predict(s_model,newdata=apltest)
s_frame1=data.frame(predict=pred,real=test_act)
table(s_frame1)
#SVMKernel
svm_model<-svm(apl_result$final~.,data=apl_result,kernel=("radial"),type='C')
summary(svm_model)
pred<-predict(svm_model,newdata=apltest)
svm_frame1=data.frame(predict=pred,real=test_act)
table(svm_frame1)
#---------------------------------------------------------------------------------------------------------
setwd("C:/Users/yuezh/Desktop/homework data/leopards/train")
leopards_Train_File=list.files(pattern = "*.jpg")
leopards_Train_File
Datafiles_leopardsTrain=lapply(leopards_Train_File,readJPEG)
length(Datafiles_leopardsTrain)
class(Datafiles_leopardsTrain)
leopardsTrain_max<-rep(NA,138)
for(i in 1:138){
  leopardsTrain_max[i]<-length(unlist(Datafiles_leopardsTrain[i]))
}
leopardsTrain_output<-matrix(rep(NA,138*max(leopardsTrain_max)),ncol=max(leopardsTrain_max),nrow=138)
for(i in 1:length(leopardsTrain_max)){
  temp_leopardsTrain<-unlist(Datafiles_leopardsTrain[i])
  temp2_leopardsTrain<-c(temp_leopardsTrain,rep(NA,max(leopardsTrain_max)-length(temp_leopardsTrain)))
  leopardsTrain_output[i,]<-temp2_leopardsTrain
}
leopardsTrain_output


#2)1load leopards Train csv
leopards_train_label<-read.csv("leopards_data.csv",stringsAsFactors = FALSE)
leopards_train_label

library(data.table)
setDT(leopards_train_label)
leopards_train_output2<-leopards_train_label[,which(leopards_train_label$Image %in% leopards_Train_File==TRUE)]
leopards_train_output2
length(leopards_train_output2)%>%as.vector()
leopards_train_label_t<-leopards_train_label[leopards_train_output2,]
leopards_train_label_train<-leopards_train_label_t[,c("Image","final")]
leopards_train_label2<-leopards_train_label_train[!duplicated(leopards_train_label_train$Image),]
leopards_train_label2
dim(leopards_train_label2)
leopards_train_data<-cbind(leopardsTrain_output,leopards_train_label2$final)
length(leopards_train_data)
dim(leopards_train_data)
head(leopards_train_data)

s_model<-svm(apl_result$final~.,data=apl_result,kernel="linear",type='C')
summary(s_model)
pred<-predict(s_model,newdata=apltest)
s_frame1=data.frame(predict=pred,real=test_act)
table(s_frame1)

svm_model<-svm(apl_result$final~.,data=apl_result,kernel=("radial"),type='C')
summary(svm_model)
pred<-predict(svm_model,newdata=apltest)
svm_frame1=data.frame(predict=pred,real=test_act)
table(svm_frame1)
setwd("C:/Users/yuezh/Desktop/homework data/leopards/test")
leopardsTestFile=list.files(pattern = "*.jpg")
Datafiles_leopardsTest=lapply(leopardsTestFile,readJPEG)
length(Datafiles_leopardsTest)
class(Datafiles_leopardsTest)
leopardsTest_max<-rep(NA,62)
for(i in 1:62){
  leopardsTest_max[i]<-length(unlist(Datafiles_leopardsTest[i]))
}
leopardsTest_output<-matrix(rep(NA,62*max(leopardsTest_max)),ncol=max(leopardsTest_max),nrow=62)
for(i in 1:length(leopardsTest_max)){
  temp_leopardsTest<-unlist(Datafiles_leopardsTest[i])
  temp2_leopardsTest<-c(temp_leopardsTest,rep(NA,max(leopardsTest_max)-length(temp_leopardsTest)))
  leopardsTest_output[i,]<-temp2_leopardsTest
}

leopardsTest_output
leopards_test_label<-read.csv("leopards_data.csv",stringsAsFactors = FALSE)
leopards_test_label

library(data.table)
setDT(leopards_test_label)
leopards_test_output2<-leopards_test_label[,which(leopards_test_label$Image %in% leopardsTestFile==TRUE)]
leopards_test_output2
length(leopards_test_output2)%>%as.vector()
leopards_test_label_t<-leopards_test_label[leopards_test_output2,]
leopards_test_label_t
leopards_test_label_train<-leopards_test_label_t[,c("Image","final")]
leopards_test_label_train
leopards_test_label2<-leopards_test_label_train[!duplicated(leopards_test_label_train$Image),]
leopards_test_label2
dim(leopards_test_label2)
leopards_test_data<-cbind(leopardsTest_output,leopards_test_label2$final)
head(leopards_test_data)
model10 <- glm (final ~ ., data = leopards_test_data, family = binomial)
predicted <- plogis(predict(model10, leopards_test_data))
library(neuralnet)
n <- names(leopards_test_data)
f <- as.formula(paste("final ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=leopards_test_data,hidden=c(5,3),linear.output=T)
install.packages("e1071")
library(e1071)
s_model<-svm(Airplane_train_data$"573535"~.,data=leopards_test_data,kernel="linear",type='C')
summary(s_model)
pred<-predict(s_model,newdata=leopards_test_data)
s_frame1=data.frame(predict=pred,real=test_act)
table(s_frame1)
svm_model<-svm(apl_result$final~.,data=leopards_test_data,kernel=("radial"),type='C')
summary(svm_model)
pred<-predict(svm_model,newdata=leopards_test_data)
svm_frame1=data.frame(predict=pred,real=leopards_test_data)
table(svm_frame1)
