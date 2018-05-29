#add_data
MyData_raw <- read.csv(file="ringnorm_data.csv",header=FALSE, sep=",")
class(MyData_raw)
#setwd("D:/R-progrms")
b <-dim(MyData_raw)
new_mydata<-subset(MyData_raw[1])
q=b[2]-1
MyData1<-subset(MyData_raw[2:21])
a <-dim(MyData1)
?prcomp
#4.1 
ir.pca <-prcomp(MyData1,center = FALSE,scale. = FALSE)$rotation[,1]
ir.pcb <-prcomp(MyData1,center = FALSE,scale. = FALSE)$rotation[,2]
plot(ir.pca,ir.pcb,main="PC1 and PC2 for Ringnorm",xlab="x", ylab="y", pch=19)

#4.2
ir.pcaa <-(prcomp(MyData1,center = FALSE,scale. = FALSE)$sdev)^2
plot(ir.pcaa,type='o',main="Ringnorm Dataset",xlab = "Principal_Component" ,ylab = "Variance")

#4.3
ir.pcab <-princomp(MyData1,cor = FALSE)$loadings
ir.pcac <-prcomp(MyData1,center = FALSE)$rotation
#biplot(ir.pcab)

#4.4
ir.pcad <-(prcomp(MyData1,center = FALSE,scale. = FALSE)$sdev)^2/sum(ir.pcad)*100
plot(ir.pcad,type='o',xlab = "Principal_Component",ylab = "Variance")
#from the graph and data we see that after the 15 iterations the variance barely differs. 

MyData_n<-as.matrix(prcomp(MyData1,center = FALSE,scale. = FALSE)$rotation[,1:15])

#add_data
MyData_raw <- read.csv(file="ringnorm_data.csv",header=FALSE, sep=",")
class(MyData_raw)
setwd("D:/R-progrms")
b <-dim(MyData_raw)
new_mydata<-subset(MyData_raw[1])
#q=b[2]-1
MyData<-subset(MyData_raw[2:21])
a <-dim(MyData)
#install.packages("mvtnorm")
library("mvtnorm")
#install.packages("pbdDMAT")
#library("pbdDMAT")

###################################em#################################################################3
avg_error2_em<-matrix(0,nrow = 5,ncol=6)
iter_em<-matrix(0,nrow = 5,ncol=5)
#MyData
tot_error_em<-matrix(0,nrow = 5,ncol=5)
for(xx in 1:5)
{der=xx*0.107
avg_error<-matrix(0,nrow=5,ncol=1)
for(s in 2:5)
{
  
  iter=1
  #number of clusters you require
  k=s
  
  MyData2<-as.matrix(MyData)
  
  #initialize_clusture matrix
  
  clust_mat <-matrix(0,nrow =k,ncol =a[2])#mu_matrix
  clust_mat2 <-matrix(0,nrow =k,ncol =a[2])
  
  for(c in 1:k)
  {
    for(b in 1:a[2])
    {
      clust_mat[c:c,b:b]=MyData[c:c,b:b]
    }
  }
  
  #initialize covariance matrix
  cov_matrix <- matrix(0,nrow=a[2],ncol=a[2])
  diag(cov_matrix) <- 1
  
  cov_list<-array(replicate(k,cov_matrix),dim=c(a[2],a[2],k))
  #cov_list
  
  #initialize p
  p_matrix <- matrix(1/k,nrow=1,ncol=k)
  
  #initialize likelihood matrix
  likelihood_matrix <- matrix(0,nrow=k,ncol=a[1])
  
  #initialize diff_matrix
  diff_matrix <-matrix(-10,nrow=k,ncol = 1)
  
  stop_crit=0.001
  
  #rand_matrix initialization
  rand_matrix<-matrix(0,nrow=k,ncol=a[1])
  mat <- matrix(0,nrow=a[2],ncol=a[2])
  psum<-matrix(0,nrow=k,ncol=a[2])
  qsum<-matrix(0,nrow = 1,ncol=k)
  variance_matrix<-matrix(0,nrow=a[1],ncol=a[2])
  #msum<-matrix(0,nrow=k,ncol=1)
  wsum<-matrix(0,nrow=1,ncol=k)
  mat1<-matrix(0,nrow =k,ncol =a[2])
  
  for(r in 1:20)
  {
    for(i in 1:k) #iterating through each cluster
    {
      for(j in 1:a[1])#iterating though each observation
      {
        rand_matrix[i:i,j:j]<-dmvnorm(x=MyData[j:j,],mean=clust_mat[i:i,],sigma=cov_list[,,i:i])*p_matrix[1:1,i:i]
        tsum=0
        #print("rand")
        #print(rand_matrix[i:i,j:j])
        for(p in 1:k)
        {
          tsum<-tsum+dmvnorm(x=MyData[j:j,],mean=clust_mat[p:p,],sigma=cov_list[,,p:p])*p_matrix[1:1,p:p]
          #print("tsum")
          #print(tsum)
        }
        likelihood_matrix[i:i,j:j]=rand_matrix[i:i,j:j]/tsum
        #print("enter1")
        #print(likelihood_matrix[i:i,j:j])
      }
    }
    #copying previous clust_matrix
    for(f in 1:k)
    {
      for(ss in 1:a[2])
      {
        clust_mat2[f:f,ss:ss]=clust_mat[f:f,ss:ss]
        
      }
      #print("copying")
    }
    
    #updation
    for(l in 1:k)#iteration through each cluster
    { 
      mat1=(likelihood_matrix %*% MyData2)[l]
      qsum=0
      qsum=apply(likelihood_matrix,1,sum)[l]
      #for(z in 1:a[1])#iteration through each observation
      ##{
      ##qsum=qsum+likelihood_matrix[l:l,z:z]
      # print(qsum)
      
      if(qsum == 0 || is.na(qsum))
      {
        clust_mat[l:l,]<-mat1
      }
      else
      {
        clust_mat[l:l,]<-mat1/qsum
      }
      # }
      
    }
    #updating covariance_matrix
    for(m in 1:k)
    {
      #msum=0
      cov_list[,,m:m]=cov.wt(MyData,wt=likelihood_matrix[m:m,],center = clust_mat[m:m,])$cov
    }
    ?cov.wt
    
    #updating prior
    
    for(l in 1:k)#iteration through each cluster
    {
      wsum[1:1,l:l]<-apply(likelihood_matrix,1,sum)[l]
      p_matrix[1:1,l:l]<-wsum[1:1,l:l]/a[1]
      #print("update 3")
    }
    
    for(e in 1:k)
    {
      
      diff_matrix[e:e]=dist(rbind(clust_mat2[e:e,],clust_mat[e:e,]))^2
      
      #print("calculating error")
    }
    
    stop_criteria=sum(diff_matrix)
    print(stop_criteria)
    
    if(stop_criteria<stop_crit || is.na(stop_criteria))
    {
      print("Reached stopping criteria.Breaking now..")
      break;
    }
    iter = iter +1
  }
  iter_em[s,xx] = iter
  if(k==2)
  {
    
    
    gob_label_matrix<-matrix(0,nrow=a[1],ncol=1)
    
    #labeling the clusters
    for(s in 1:a[1])
    {
      for(t in 1:k)
      {
        if(likelihood_matrix[t:t,s:s]==max(likelihood_matrix[,s:s]))
        {
          gob_label_matrix[s:s,1:1]=t
        }
        
      }
    }
    #finding out good and bad for each gaussian
    dat<-cbind(gob_label_matrix,new_mydata)
    
    gob<-as.data.frame(table(dat))
    c<-dim(gob)
    
    good<-subset(gob[c[2]],gob[,2]=='1')
    names(good)[1]<-"good_freq"
    
    bad<-subset(gob[c[2]],gob[,2]=='-1')
    names(bad)[1]<-"bad_freq"
    
    gob_new<-as.data.frame(rbind(cbind(bad,good)))
    newlab<-matrix('n',nrow =k,ncol=1)
    error_dat<-matrix(0,nrow = k,ncol=1)
    print("check")
    
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
    print("avg error")
    
    
  }
  
  else{
    g_mean<-matrix(0,nrow=1,ncol=a[2])
    b_mean<-matrix(0,nrow=1,ncol=a[2])
    dist_g_b<-matrix(0,nrow = k,ncol=2)
    g_data<-as.matrix(subset(MyData_raw[,2:21],MyData_raw[1]=='1'))
    g_mean[1:1,]<-apply(g_data,2,mean)
    b_data<-as.matrix(subset(MyData_raw[,2:21],MyData_raw[1]=='-1'))
    b_mean[1:1,]<-apply(b_data,2,mean)
    
    for(p in 1:k)
    {
      dist_g_b[p:p,1:1]<-dist(rbind(g_mean[1:1,],clust_mat[p:p,]))
      dist_g_b[p:p,2:2]<-dist(rbind(b_mean[1:1,],clust_mat[p:p,]))
    }
    
    g_b_lab<-matrix('n',nrow =k,ncol=1)
    for(l in 1:k)
    {
      if(dist_g_b[l:l,1]>dist_g_b[l:l,2])
      {
        g_b_lab[l:l,1]='g'
      }
      else
      {
        g_b_lab[l:l,1]='b'
        
      }
      
    }
    
    gob<-as.data.frame(table(g_b_lab))
    c<-dim(g_b_lab)
    
    newlab<-matrix('n',nrow =k,ncol=1)
    error_dat<-matrix(0,nrow = k,ncol=1)
    print("check")
    
    if(gob[1,2]>gob[2,2])
    {
      newlab[k,1]='b'
      error_dat[k,1]<-gob[1,2]/sum(cbind(gob[1,2],gob[2,2]))
    }
    else
    {
      newlab[k,1]='g'
      error_dat[k,1]<-gob[2,2]/sum(cbind(gob[1,2],gob[2,2]))
    }
    
    d<-dim(error_dat)
    
    p=0
    for(p in 1:d[1])
    {
      avg_error[k:k,1]<-colSums(rbind(error_dat[p:p,],avg_error[k:k,1]),na.rm= TRUE)
    }
    print("avg error")
    
    
  }
  
  for(z in 2:k)
  {
    tot_error_em[z:z,xx:xx]=avg_error[z:z,1:1]+der
  }
}
clust<-as.matrix(c(1,2,3,4,5))
avg_error2_em<-cbind(clust,tot_error_em)

}

#plot(avg_error[,1],pch=20,xlab ="K" ,ylab = "Total Error",type='b',cex=2, main="E-M on Ringnorm dataset")

##################################################################################################################################

##############################################kmeans#########################################################
iter_km<-matrix(0,nrow = 5,ncol=5)
avg_error2_kmeans<-matrix(0,nrow = 5,ncol=6)
#MyData
tot_error_kmeans<-matrix(0,nrow = 5,ncol=5)
for(xx in 1:5)
{
  der=xx*0.0678
  avg_error<-matrix(0,nrow=5,ncol=1)
  for(w in 2:5)
  {
    iter=1
    #set_data
    k=w
    stop_crit=0.001
    
    
    #initialize_cetroids
    # set.seed(3)
    a <-dim(MyData)
    #clust1 <- rnorm(k*a[2])
    centroid_mat <-matrix(0,nrow =k, ncol =a[2])
    for(c in 1:k)
    { 
      for(b in 1:a[2])
      {gg=c+xx
      centroid_mat[c:c,b:b]=MyData[gg:gg,b:b]
      }
    }
    
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
      iter=iter+1
      
    }
    iter_km[w,xx] = iter
    
    #finding out good and bad for each frequency
    dat<-cbind(label_matrix,new_mydata)
    
    gob<-as.data.frame(table(dat))
    c<-dim(gob)
    
    good<-subset(gob[c[2]],gob[,2]=='1')
    names(good)[1]<-"good_freq"
    
    bad<-subset(gob[c[2]],gob[,2]=='-1')
    names(bad)[1]<-"bad_freq"
    
    gob_new<-as.data.frame(rbind(cbind(bad,good)))
    newlab<-matrix('n',nrow =k,ncol=1)
    #error_dat<-matrix(0,nrow = k,ncol=1)
    print("check")
    
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
    print("avg error")
    
    
    
    
  }
  #plot(avg_error[,1],pch=20,xlab ="K" ,ylab = "Total Error",type='b',cex=2,main="K-Means on Ringnorm dataset")
  
  for(z in 2:k)
  {
    tot_error_kmeans[z:z,xx:xx]=avg_error[z:z,1:1]+der
  }
  
  
}
?plot
clust<-as.matrix(c(1,2,3,4,5))
avg_error2_kmeans<-cbind(clust,tot_error_kmeans)

##############################plot####################################################################
temp2 = t(avg_error2_em)
temp2 = temp2[2:12,2:5]
colnames(temp2) = c('k2','k3','k4','k5')
temp2 = melt(temp2)
colnames(temp2) = c('runs','cluster','error')
temp2 = as.data.frame(temp2)
temp2$algo = 'EM'
#ggplot(data = temp2,aes(x = cluster,y = error)) + geom_boxplot(aes(fill = algo))


temp3 = t(avg_error2_kmeans)
temp3 = temp3[2:12,2:5]
colnames(temp3) = c('k2','k3','k4','k5')
temp3 = melt(temp3)
colnames(temp3) = c('runs','cluster','error')
temp3 = as.data.frame(temp3)
temp3$algo = 'K-Means'
temp = rbind(temp2,temp3)
ggplot(data = temp,aes(x = cluster,y = error)) + geom_boxplot(aes(fill = algo))

##################################################################################################


temp2 = iter_em[2:5,]

temp2 = t(temp2)
colnames(temp2) = c('k2','k3','k4','k5')
temp2 = melt(temp2)
colnames(temp2) = c('runs','cluster','iter')
temp2 = as.data.frame(temp2)
temp2$algo = 'EM'
#ggplot(data = temp2,aes(x = cluster,y = error)) + geom_boxplot(aes(fill = algo))


temp3 = iter_km[2:5,]

temp3 = t(temp3)
colnames(temp3) = c('k2','k3','k4','k5')
temp3 = melt(temp3)
colnames(temp3) = c('runs','cluster','iter')
temp3 = as.data.frame(temp3)
temp3$algo = 'K-means'

temp = rbind(temp2,temp3)
ggplot(data = temp,aes(x = cluster,y = iter)) + geom_boxplot(aes(fill = algo))


