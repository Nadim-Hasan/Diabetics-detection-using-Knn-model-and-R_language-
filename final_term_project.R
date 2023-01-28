

f_dataset <- read.csv("C:/Users/user/Desktop/PROJECT_NADIM/diabetes.csv",header=TRUE,sep=",")
f_dataset


summary(f_dataset)
str(f_dataset)



f_dataset$Pregnancies[f_dataset$Pregnancies ==0] = mean(f_dataset$Pregnancies,)
f_dataset$Glucose [f_dataset$Glucose ==0] = mean(f_dataset$Glucose,)
f_dataset$BloodPressure[f_dataset$BloodPressure ==0] = mean(f_dataset$BloodPressure,)
f_dataset$SkinThickness[f_dataset$SkinThickness ==0] = mean(f_dataset$SkinThickness,)
f_dataset$Insulin[f_dataset$Insulin ==0] = mean(f_dataset$Insulin,)
f_dataset$BMI [f_dataset$BMI ==0] = mean(f_dataset$BMI,)
f_dataset$Pedigree [f_dataset$Pedigree ==0] = mean(f_dataset$Pedigree,)
f_dataset$Age[f_dataset$Pregnancies ==0] = mean(f_dataset$Age,)

library(class)


noramalize_data <- function(x)
{
  nu= x-min(x)
  dn= max(x)-min(x)
  return(nu/dn)
}
make_data<-as.data.frame(lapply(f_dataset[1:8],noramalize_data))



sample_data <- sample(2,nrow(make_data),replace = TRUE ,prob = c(0.70,0.30))

train_data<- make_data[sample_data==1, 1:8]
test_data <- make_data[sample_data==2, 1:8]

train_datalabels <- f_dataset[sample_data==1,9]
test_datalabels <- f_dataset[sample_data==2,9]


prediction= knn(train = train_data,test = test_data,train_datalabels,k=5)
con_matrix= table(test_datalabels,prediction)

con_matrix

hist(con_matrix)
Accuracy= function(con_matrix)
{
  sum=0
  for(i in 1:nrow(con_matrix))
    sum=sum+con_matrix[i,i]
    return(sum/sum(con_matrix))
}

print(paste('ACCURACY OF THIS MODEL IS = ',Accuracy(con_matrix)*100,'%'))


list_k <- c(1:50)
arr_k_result <-c()

for(i in 1: length(list_k))
{
  prediction= knn(train = train_data,test = test_data,train_datalabels,k=i)
  con_matrix= table(test_datalabels,prediction)
  
  arr_k_result[i]<-Accuracy(con_matrix)
}

knn_rslt <- cbind(list_k ,arr_k_result)

colnames(knn_rslt)<-c("Value of k","      Accuracy")
knn_rslt <- as.data.frame(knn_rslt)


knn_rslt

plot(knn_rslt$`Value of k`,knn_rslt$`      Accuracy`,type="b",pch=16, col="green", lwd=1, xlab="VALUE OF K", ylab="ACCURACY", main="ACCURACY ON DIFFERENT K VALUS")









