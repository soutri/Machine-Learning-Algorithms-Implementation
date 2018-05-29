#add_data
MyData_raw <- read.csv(file="ionosphere.csv",header=FALSE, sep=",")
class(MyData_raw)
#setwd("D:/R-progrms")
b <-dim(MyData_raw)
#b
?subset
new_mydata<-subset(MyData_raw[b[2]])
q=b[2]-1
#q
MyData<-subset(MyData_raw[1:q])
#MyData
avg_error<-matrix(0,nrow=5,ncol=1)
for(w in 2:5)
{
  #set_data
  k=w
  stop_crit=0.001
  
  
  #initialize_cetroids
  set.seed(4)
  a <-dim(MyData)
  clust1 <- rnorm(k*a[2])
  centroid_mat <-matrix(clust1,nrow =k, ncol =a[2])
  
  #initialize label matrix
  label_matrix<-matrix('n',nrow=a[1],ncol=1)
  
  #initialize distance matrix
  dist_matrix <- matrix(-1,nrow=a[1],ncol=k)
  
  #initialize centroid_mat 2
  centroid_mat2 <-matrix(0,nrow=k,ncol = a[2])
  
  #initialize diff_matrix
  diff_matrix <-matrix(-10,nrow=k,ncol = 1)
  
  #initialize gob
  gob<-matrix('n',nrow =k,ncol=2)
  
  #initialize error_dat
  error_dat<-matrix(0,nrow = k,ncol=1)
  
  #initialize avg_error
  #avg_error<-matrix(0,nrow=1,ncol=1)
  
  
  for(o in 1:20)
  {
    
    #fill distance matrix
    for(i in 1:a[1])# no.of obs
    {
      for(j in 1:k)#no.cluster
      {
        #print(dist(rbind(MyData[i:i,],centroid_mat[j:j,])))
        dist_matrix[i:i,j:j]<-dist(rbind(MyData[i:i,],centroid_mat[j:j,]))
      }
      
      
    }
    
    #labeling the clusters
    for(s in 1:a[1])
    {
      for(t in 1:k)
      {
        if(dist_matrix[s:s,t:t]==min(dist_matrix[s:s,]))
        {
          label_matrix[s:s,1]=t
        }
        
      }
    }
    
    #copy to a new matrix
    for(x in 1:k)
    {
      for(y in 1:a[2])
      {
        centroid_mat2[x:x,y:y]=centroid_mat[x:x,y:y]
      }
    }
    
    #re-calculating new clusters
    for(v in 1:k) # no. of clusters
    {
      count=1
      for(u in 1:a[1])#no. of observations
      {
        if(label_matrix[u:u,1]==v) #check label value
        {
          centroid_mat[v:v,]=colSums(rbind(MyData[u:u,],centroid_mat[v:v,]),na.rm = TRUE)
          count=count+1
          
        }
        
      }
      centroid_mat[v:v,]=centroid_mat[v:v,]/count
    }
    
    #calculating dif
    for(e in 1:k)
    {
      
      diff_matrix[e:e]=dist(rbind(centroid_mat2[e:e,],centroid_mat[e:e,]))
      
      
    }
    
    #stopping criteria  
    stop_criteria=sum(diff_matrix)/dim(diff_matrix)[1]
    print(stop_criteria)
    if(stop_criteria<stop_crit)
    {
      print("Reached stopping criteria.Exiting Now..")
      break;
    }
    
  }
  
  #finding out good and bad for each frequency
  dat<-cbind(label_matrix,new_mydata)
  
  gob<-as.data.frame(table(dat))
  c<-dim(gob)
  
  good<-subset(gob[c[2]],gob[,2]=='g')
  names(good)[1]<-"good_freq"
  
  bad<-subset(gob[c[2]],gob[,2]=='b')
  names(bad)[1]<-"bad_freq"
  
  gob_new<-as.data.frame(rbind(cbind(bad,good)))
  newlab<-matrix('n',nrow =k,ncol=1)
  #error_dat<-matrix(0,nrow = k,ncol=1)

  
  for(l in 1:k)
  {
    if(gob_new[l:l,1]>gob_new[l:l,2])
    {
      newlab[l:l,1]='g'
      error_dat[l:l,1]<-gob_new[l:l,1]/sum(cbind(gob_new[l:l,1],gob_new[l:l,2]))
    }
    else
    {
      newlab[l:l,1]='b'
      error_dat[l:l,1]<-gob_new[l:l,1]/sum(cbind(gob_new[l:l,1],gob_new[l:l,2]))
    }
    
  }
  d<-dim(error_dat)
 
  p=0
  for(p in 1:d[1])
  {
    avg_error[k:k,1]<-colSums(rbind(error_dat[p:p,],avg_error[k:k,1]),na.rm= TRUE)
  }

  
  
  
  
}
?plot
plot(avg_error[,1],pch=20,xlab ="K" ,ylab = "Total Error",type='b',cex=2, main="K-Means on Ionosphere dataset")







