#add_data
MyData <- read.csv(file="dat.csv",header=FALSE, sep=",")
class(MyData)
#setwd("D:/R-progrms")

#set_data
k=2
stop_crit=0.001


#initialize_cetroids
set.seed(2)
a <-dim(MyData)
clust1 <- rnorm(k*a[2])
centroid_mat <-matrix(clust1,nrow =k, ncol =a[2])
?dist

#initialize label matrix
label_matrix<-matrix('n',nrow=a[1],ncol=1)

#initialize distance matrix
dist_matrix <- matrix(-1,nrow=a[1],ncol=k)

#initialize centroid_mat 2
centroid_mat2 <-matrix(0,nrow=k,ncol = a[2])

#initialize diff_matrix
diff_matrix <-matrix(-10,nrow=k,ncol = 1)

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

#dist(rbind(rbind(MyData[1,],centroid_mat[1,])))




#label the clusters according to their minimum matrix

?colnames
?min
#{
 # label_matrix[p]<-min((dist_matrix[p:p, ]))
#}
#label_matrix


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
      
      #for(w in 1:a[2]) # no. of variables
      #{
        #?colSums
        #print("entered")
        centroid_mat[v:v,]=colSums(rbind(MyData[u:u,],centroid_mat[v:v,]),na.rm = TRUE)
      #}
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

stop_criteria=sum(diff_matrix)/dim(diff_matrix)[1]
print(stop_criteria)


if(stop_criteria<stop_crit)
{
  print("Reached stopping criteria.Breaking now..")
  break;
}
  


}














