setwd("C:/Users/me/Desktop/mid sem/DWM/mini project")
Train=read.csv('Train.csv')
Test=read.csv('Test.csv')

Test=Test[,-1]
Train=Train[,-1]


no_of_users=max(Train[,1])
no_of_movies=max(Train[,2])

deviation=matrix(nrow=no_of_movies,ncol=no_of_movies)
Test=cbind(Test,predicted_real=0)
Test=cbind(Test,predicted_int=0)


initial=Sys.time()

for(i in 1:nrow(Test))
{
  
  uid=Test[i,1]
  midi=Test[i,2]
  rating=0;
  
  U=which(Train[,1]==uid);
  weight=0;
  for(j in 1:length(U))
  {
    index=U[j];
    midj=Train[index,2];
    
    deviation_ij=0;
  
      M=Train[Train$movieId == midi, ]
      N=Train[Train$movieId == midj, ]
      common=merge(M,N,by="userId")
      weight=weight+nrow(common);
       if(is.na(deviation[midi,midj])){
      
      
      
      
      deviation_ij=sum(common[,3]-common[,5])
      
      if(nrow(common)==0) {
        deviation[midi,midj]=0;
        deviation[midj,midi]=0;
        deviation_ij=0;
        
      }
      else{
        deviation_ij=deviation_ij/nrow(common);
        deviation[midi,midj]=deviation_ij;
        deviation[midj,midi]=(-1*deviation_ij);
        
      }
    }
    else{
      
      deviation_ij=deviation[midi,midj];
      
    }

    
    d=Train[index,3];
    rating=rating+  (deviation_ij +d)* nrow(common) ; 
    
  }
 
  rating=rating/weight;
  int_rating=round(rating,digits=0)
  Test[i,4]=rating;
  Test[i,5]=int_rating;
  print(Test[i,3:5])
  
}

write.csv(Test,file="Test_result_weighted.csv")


MAE=0;
#calculating mean average error
for(i in 1:no_of_users)
{
  MAEi=0;
  user_i=which(Test[,1]==i);
  evaluation_i=Test[user_i,];
  MAEi=sum( abs( evaluation_i[,3] -evaluation_i[,4] ) )
  MAEi=MAEi/length(user_i);
  MAE=MAE+MAEi;
}

MAE=MAE/no_of_users;
print(MAE)
print(Sys.time()-initial)




