setwd("C:/Users/me/Desktop/mid sem/DWM/mini project")
#Test=read.csv('Test_result_weighted.csv')
Test=read.csv('Test_result.csv')

Test=Test[,-1]

accuracy=length(which(Test[,3] == Test[,5]))/nrow(Test)

MAE=0;
count=0;
#calculating mean average error
for(i in 1:max(Test[,1]))
{
  
  MAEi=0;
  user_i=which(Test[,1]==i);
  
  if(length(user_i)==0)
  {
    next;
  }
  evaluation_i=Test[user_i,];
  MAEi=sum( abs( evaluation_i[,3] - evaluation_i[,4] ) )
  MAEi=MAEi/nrow(evaluation_i)
  if(!is.na(MAEi))
  {
    MAE=MAE+MAEi;
    count=count+1;
  }
}

MAE=MAE/count;
print(MAE)

