
library(caTools)

#nrow = 10
#ncol = 10
beta = 0.5
data <-numeric()
 # initial nbd matrix made up of all elemrnt 0.

start <- function(L,beta){
  
  S <- matrix(rbinom ( L* L , 1, beta), L,  L)
  return(S) 
}

# intial configuration matrix

#S <- matrix(rbinom (100,1,0.),nrow = 10, ncol = 10) # using binomial distribution to find configuration matrix havin element 0 and 1 
#S[ ] <- rbinom (100,1,0.4)


#for (k in 1:100){
  #if (runif(1)>0.5){
  #  data[k] = 1
 # }else {
 #   data[k] = 0}
 # }


#A<-matrix(data, nrow, ncol)

fn <-function(x){if(x==0){fout<- L
}else{
  fout<-x
}
return(fout)}

gn<-function(x){if(x==L+1){gout<-1
}else{
  gout<-x
}
return(gout)
}

A <- start(L,beta)
 nbd_counter <- function(A){
   nbd<-matrix(0,L,L)
     
 for (i in 1:L){
  for(j in 1:L){
   nbd[i,j] = A [fn(i-1),fn(j-1)]+A[fn(i-1),j]+A[fn(i-1),gn(j+1)]+A[i,gn(j+1)]+A[gn(i+1),gn(j+1)]+A[gn(i+1),j]+A[gn(i+1),fn(j-1)]+A[i,fn(j-1)]
  }
}
 return (nbd)}

#I <- start(L)
#D <- nbd_counter (I)# we need function to simulate to use large no of time 

Dynamics <- function(L,nsteps,beta){
  A = start(L,beta)
  storage <- array (0,c(L,L,nsteps)) # big bhakari layer by layer 1 conf matrix, layer by layer of conf matrix 
   for ( step in 1:nsteps){
       storage[, ,step] <- A
       nbd = nbd_counter (A)
       #print(nbd)
        for (i in 1:L){
          for(j in 1:L){
             if (A [i,j]==1){
               
                     if (nbd[i,j]<2){
                             A[i,j] <-0
                        } else if(nbd[i,j]==2 | nbd[i,j]==3 ){ 
                             A[i,j] <-1
                        } else if (nbd[i,j]>3){
                          A[i,j]<-0}
           }else if (A[i,j]==0){
                  if (nbd[i,j]==3){
                           A[i,j]<-1
                          }}}}}

      return(storage)
                      }
#.................................................................................................................

L=100
nsteps = 100
D = Dynamics(L,nsteps,beta)
write.gif (D, "video.gif", col= "jet",delay = 50)
                        
  
       
               
                
             
          
        
                                      #  array banako c matix banauchha  L* L matrix nstep ota




