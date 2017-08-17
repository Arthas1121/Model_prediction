
#library(kernlab)
#library(splines2)
#library(e1071)
#library(DMwR)
#library(nnet)
library(rminer)

soh_svm<-function(test,size=65,len=800,...)
{
  
  ###data
  data_all<-read.csv(sprintf('C:/Users/1/Documents/GitHub/Battery_project/Data/%d_predict.csv',size),header=T, sep=",")
  data_soh<-read.csv(sprintf('C:/Users/1/Documents/GitHub/Battery_project/Data/%d_intotal.csv',size),header=T, sep=",")
  data_soh<-as.matrix(data_soh)
  
  #################SVR method
  
  ##########data
  matri<-matrix(rep(0,400*len),nrow=len,byrow=T)
  m<-1
  
  #get data
  for(i in 1:len)
  {
    x<-0
    y<-0
    while(data_all[m,1]==i&&is.na(data_all[m,1])!=T)
    {
      y<-c(y,data_all[m,3])
      x<-c(x,data_all[m,2])
      m<-m+1
    }
    x<-x[-1]
    y<-y[-1]
    for(k in 1:length(y)) matri[i,k]<-y[k]
    matri[i,400]<-data_soh[i]
  }
  
  #########normalization
  #norm_data<-scale(matri[,1:399])
  #norm_data<-cbind(norm_data,matri[,400])
  
  
  
  #or without normalization
  norm_data<-matri[,1:400]
  
  #test data
  
  #test<-data_all[66990:67100,3] #202
  
  #adjust to test data
  
  nlen<-len-1
  new_data<-matrix(rep(0,nlen*length(test)),nrow=nlen,byrow=T)
  
  
  for(j in 1:nlen)
  {
    test_dif<-abs(matri[j+1,]-test[1])
    j_order<-order(test_dif)[1]
    for(k in 1:length(test))
    {
      new_data[j,k]<-norm_data[j+1,k+j_order-1]
    }
  }
  
  
  
  new_data<-cbind(new_data,matri[2:len,400])
  
  #devide into training set and test set
  #set.seed(1)
  #ind<-sample(2,799,replace=T,prob=c(0.7,0.3))
  #trainset<-new_data[ind==1,]
  #testset<-new_data[ind==2,]
  
  #SVM
  
  #model.svm<-svm(new_data[,length(test)+1]~.,new_data)
  
  #svr<-fit(trainset[,length(test)+1]~.,trainset[,-(length(test)+1)],model="svm",task = "reg")
  
  svr<-fit(new_data[,length(test)+1]~.,new_data[,-(length(test)+1)],model="svm",task = "reg")
  
  #soh.pred<-predict(svr,new_data[,-(length(test)+1)])
  
  #plot(soh.pred~new_data[,length(test)+1])
  
  test_ma<-rbind(test,new_data[,-(length(test)+1)])
  
  test_pred<-predict(svr,test_ma)
  
  paste(sprintf("SOH=%.2f",test_pred[1]*100),"%",sep="")
  
}






















