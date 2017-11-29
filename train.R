setwd("C:/Users/me/Desktop/mid sem/DWM/mini project")
A=read.csv('movie.csv')

no_of_users=max(A[,1])
no_of_movies=max(A[,2])

Train=A
Test=data.frame()

Test_insices=c();


for(i in 1:no_of_users)
{
  X=which(A[,1]==i) 
    
  Y=sample(X, length(X)*0.2)
  
  Test_insices=append(Test_insices,Y)
  Test=rbind(Test,A[Y,])
 
 
}

Train=A[-Test_insices,]


write.csv(Test,file="Test.csv")
write.csv(Train,file="Train.csv")





