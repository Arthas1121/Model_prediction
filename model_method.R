
size=65
len=800

###data
data_all<-read.csv(sprintf('C:/Users/1/Documents/GitHub/Battery_project/Data/%d_predict.csv',size),header=T, sep=",")


for(i in 1:len)
{
  data_newT<-0
  data_newV<-0
  for(j in 1:10)
  {
    data_newT<-c(data_newT,data_all[j,2])
    data_newV<-c(data_newV,data_all[j,3])
    if(data_all[j,1]!=i) break
  }
  data_Vdif<-diff(data_all[,3])/(diff(data_all[,2]))
}

                               