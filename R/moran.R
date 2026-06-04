library('spdep')
#library('spDataLarge')
library('parallel')
setwd("D:/研究生毕业论文/数据")
data_matrix_eco<-read.csv("国内人均GDP.CSV",header=FALSE,encoding="utf-8")
data_res<-read.csv("matlabdatanew1.csv",header=TRUE,encoding="UTF-8")

#cl = makeCluster(64)
#if it's permitted
num_cores <- detectCores()
cl = makeCluster(num_cores-1)
loop_func <- function(i) {
  res = matrix(data=0,nrow = 1,ncol = 4592)
  if (i < 4592) {
    for (j in (i+1):4592) {
      if (data_res1$provcd[i]!=data_res1$provcd[j]){
        res[1,j]=1/abs(data_matrix_eco[2,(data_res1$provcd[i]+1)]-data_matrix_eco[2,(data_res1$provcd[j]+1)])
      }
      else{
        res[1,j]=0
      }
    }
  }
  return(res)
}

####year 1####
data_res1<-data_res[which(data_res$year==1),]
data_1 = data_res1$faminc_net

clusterExport(cl,list("data_res1","data_matrix_eco"))
#you can use environment variables at the beginning 
#cl <- makeCluster(3, type="FORK") Linux & mac
#Parallel Socket Cluster (PSOCK) # WIN

for (i in 1:6){
  data_res1<-data_res[which(data_res$year==i),]
  data_1 = data_res1$faminc_net
  res <- parLapply(cl,1:4592,loop_func)
  dist_eco <- matrix(do.call(rbind , res),nrow=4592,ncol=4592)
  dist_eco<-dist_eco+t(dist_eco)
  mat = mat2listw(dist_eco,style="W")
  test1=moran.test(as.vector(data_1), listw = mat)
  print(test1)
}

# stopparallel
stopCluster(cl)

