#######family#######
library(haven)
library(dplyr)
setwd("D:/数据和代码/数据")
#CFPS默认了-8为缺失值，在此将-8全部赋值为0，后续将家庭净收入<=0的样本剔除
data2010<-read_dta("cfps2010famecon_202008.dta")
data2012<-read_dta("cfps2012famecon_201906.dta")
data2014<-read_dta("cfps2014famecon_201906.dta")
data2016<-read_dta("cfps2016famecon_201807.dta")
data2018<-read_dta("cfps2018famecon_202101.dta")
data2020<-read_dta("cfps2020famecon_202306.dta")

data20101<-read_dta("cfps2010famconf_202008.dta")
data20101$chi<-0
data20101$chi[which(data20101$tb1b_a_p>0&data20101$tb1b_a_p<=16)]<-1
data20101$old<-0
data20101$old[which(data20101$tb1b_a_p>=60)]<-1
children<-aggregate.data.frame(data20101$chi,by=list(data20101$fid),sum)
old<-aggregate.data.frame(data20101$old,by=list(data20101$fid),sum)
data201011<-merge.data.frame(children,old,by=c("Group.1"))
colnames(data201011)<-c('fid','children','old')

data2010$faminc_net[which(data2010$faminc_net<0)]<-0
data2010$indinc_net[which(data2010$indinc_net<0)]<-0
data2010$fh304[which(data2010$fh304<0)]<-0
data2010$fh201_a_3[which(data2010$fh201_a_3<0)]<-0
data2010$fh201_a_5[which(data2010$fh201_a_5<0)]<-0
data2010$fh201_a_6[which(data2010$fh201_a_6<0)]<-0
data2010$fg2[which(data2010$fg2<0)]<-0
data2010$ff2[which(data2010$ff2<0)]<-0
data2010$ff301_a_2[which(data2010$ff301_a_2<0)]<-0
data2010$fe3[which(data2010$fe3 != 1)] <- 0
data2010_use<-cbind(data2010$fid,data2010$cid,data2010$provcd,data2010$countyid,data2010$faminc_net,data2010$indinc_net,data2010$finc,data2010$fproperty,
                    #基本信息
                    data2010$fh304,as.double(data2010$fe3),
                    #通信支出,参与企业经营
                    data2010$familysize,
                    #家庭规模，
                    data2010$fh201_a_3,data2010$fh201_a_5,data2010$fh201_a_6,data2010$fg2,
                    #银行借贷，亲戚借贷，民间借贷,别人欠自家的钱
                    data2010$ff2,data2010$ff301_a_2,
                    #存款，市值
                    as.vector(rep(1,14797)))
                    #年份
colnames(data2010_use)<-c('fid','cid','provcd','countyid','faminc_net','indinc_net','finc','fproperty',
                          'trco','company','familysize',
                          'bank_debit','fam_debit','peo_debit','debit_other',
                          'saving','marketvalue',
                          'year')
data2010_use<-merge.data.frame(data2010_use,data201011,by=c("fid"))

data20121<-read_dta("cfps2012famconf_092015.dta")
data20121$chi<-0
data20121$chi[which(data20121$tb1b_a_p>0&data20121$tb1b_a_p<=16)]<-1
data20121$old<-0
data20121$old[which(data20121$tb1b_a_p>=60)]<-1
children<-aggregate.data.frame(data20121$chi,by=list(data20121$fid10),sum)
old<-aggregate.data.frame(data20121$old,by=list(data20121$fid10),sum)
data201211<-merge.data.frame(children,old,by=c("Group.1"))
colnames(data201211)<-c('fid','children','old')

data2012$fincome2_adj[which(data2012$fincome2_adj<0)]<-0
data2012$fincome2_per_adj[which(data2012$fincome2_per_adj<0)]<-0
data2012$trco[which(data2012$trco<0)]<-0
data2012$bank_debts[which(data2012$bank_debts<0)]<-0
data2012$ind_debts[which(data2012$ind_debts<0)]<-0
data2012$ft802[which(data2012$ft802<0)]<-0
data2012$debit_other[which(data2012$debit_other<0)]<-0
data2012$savings[which(data2012$savings<0)]<-0
data2012$ft301[which(data2012$ft301<0)]<-0
data2012$ft401[which(data2012$ft401<0)]<-0
data2012$ft501[which(data2012$ft501<0)]<-0
data2012$ft601[which(data2012$ft601<0)]<-0
data2012$ft701[which(data2012$ft701<0)]<-0
data2012$fm1[which(data2012$fm1 != 1)] <- 0
data2012_use<-cbind(data2012$fid10,data2012$cid,data2012$provcd,data2012$countyid,data2012$fincome2_adj,data2012$fincome2_per_adj,data2012$wage_2_adj,data2012$fproperty_2,
                    #基本信息
                    data2012$trco,as.double(data2012$fm1),
                    #通信支出,是否从事经营
                    data2012$familysize,
                    #家庭规模
                    data2012$bank_debts,data2012$ind_debts,data2012$ft802,data2012$debit_other,
                    #银行贷款，非金融机构个人贷款，民间借贷,别人欠自家的钱，
                    data2012$savings,data2012$ft301+data2012$ft401+data2012$ft501+data2012$ft601+data2012$ft701,
                    #存款，市值
                    as.vector(rep(2,13315)))
                    #年份
colnames(data2012_use)<-c('fid','cid','provcd','countyid','faminc_net','indinc_net','finc','fproperty',
                          'trco','company','familysize',
                          'bank_debit','fam_debit','peo_debit','debit_other',
                          'saving','marketvalue',
                          'year')
data2012_use<-merge.data.frame(data2012_use,data201211,by=c("fid"))

data20141<-read_dta("cfps2014famconf_170630.dta")
data20141$chi<-0
data20141$chi[which(data20141$tb1y_a_p>=1998)]<-1
data20141$old<-0
data20141$old[which(data20141$tb1y_a_p<=1954&data20141$tb1y_a_p>0)]<-1
children<-aggregate.data.frame(data20141$chi,by=list(data20141$fid10),sum)
old<-aggregate.data.frame(data20141$old,by=list(data20141$fid10),sum)
data201411<-merge.data.frame(children,old,by=c("Group.1"))
colnames(data201411)<-c('fid','children','old')

data2014$fincome2[which(data2014$fincome2<0)]<-0
data2014$fincome2_per[which(data2014$fincome2_per<0)]<-0
data2014$trco[which(data2014$trco<0)]<-0
data2014$ft501[which(data2014$ft501<0)]<-0
data2014$ft1001[which(data2014$ft1001<0)]<-0
data2014$ft602[which(data2014$ft602<0)]<-0
data2014$debit_other[which(data2014$debit_other<0)]<-0
data2014$savings[which(data2014$savings<0)]<-0
data2014$ft201[which(data2014$ft201<0)]<-0
data2014$fm1[which(data2014$fm1 != 1)] <- 0
data2014_use<-cbind(data2014$fid10,data2014$cid14,data2014$provcd14,data2014$countyid14,data2014$fincome2,data2014$fincome2_per,data2014$fwage_2,data2014$fproperty_2,
                    #基本信息
                    data2014$trco,as.double(data2014$fm1),
                    #通信支出,是否从事公司经营
                    data2014$familysize,
                    #家庭规模
                    data2014$ft501,data2014$ft1001,data2014$ft602,data2014$debit_other,
                    #银行贷款,亲戚借贷,民间借贷,别人欠自家的
                    data2014$savings,data2014$ft201,
                    #存款，市值
                    as.vector(rep(3,13946)))
                    #年份
colnames(data2014_use)<-c('fid','cid','provcd','countyid','faminc_net','indinc_net','finc','fproperty',
                          'trco','company','familysize',
                          'bank_debit','fam_debit','peo_debit','debit_other',
                          'saving','marketvalue',
                          'year')
data2014_use<-merge.data.frame(data2014_use,data201411,by=c("fid"))

data20161<-read_dta("cfps2016famconf_201804.dta")
data20161$chi<-0
data20161$chi[which(data20161$tb1y_a_p>=2000)]<-1
data20161$old<-0
data20161$old[which(data20161$tb1y_a_p<=1956&data20161$tb1y_a_p>0)]<-1
children<-aggregate.data.frame(data20161$chi,by=list(data20161$fid10),sum)
old<-aggregate.data.frame(data20161$old,by=list(data20161$fid10),sum)
data201611<-merge.data.frame(children,old,by=c("Group.1"))
colnames(data201611)<-c('fid','children','old')

data2016$fincome2[which(data2016$fincome2_per<0)]<-0
data2016$trco[which(data2016$trco<0)]<-0
data2016$familysize16[which(data2016$familysize16<0)]<-0
data2016$ft501[which(data2016$ft501<0)]<-0
data2016$ft601[which(data2016$ft601<0)]<-0
data2016$ft602[which(data2016$ft602<0)]<-0
data2016$debit_other[which(data2016$debit_other<0)]<-0
data2016$savings[which(data2016$savings<0)]<-0
data2016$ft201[which(data2016$ft201<0)]<-0
data2016$fm1[which(data2016$fm1 != 1)] <- 0
data2016_use<-cbind(data2016$fid10,data2016$cid16,data2016$provcd16,data2016$countyid16,data2016$fincome2,data2016$fincome2_per,data2016$fwage_2,data2016$fproperty_2,
                    #基本信息
                    data2016$trco,as.double(data2016$fm1),
                    #通信支出,是否从事公司经营
                    data2016$familysize16,
                    #家庭规模
                    data2016$ft501,data2016$ft601,data2016$ft602,data2016$debit_other,
                    #银行贷款，亲戚贷款，民间贷款，别人欠自家的钱
                    data2016$savings,data2016$ft201,
                    #存款，市值
                    as.vector(rep(4,14019)))
                    #年份
colnames(data2016_use)<-c('fid','cid','provcd','countyid','faminc_net','indinc_net','finc','fproperty',
                          'trco','company','familysize',
                          'bank_debit','fam_debit','peo_debit','debit_other',
                          'saving','marketvalue',
                          'year')
data2016_use<-merge.data.frame(data2016_use,data201611,by=c("fid"))

data20181<-read_dta("cfps2018famconf_202008.dta")
data20181$chi<-0
data20181$chi[which(data20181$tb1y_a_p>=2002)]<-1
data20181$old<-0
data20181$old[which(data20181$tb1y_a_p<=1958&data20181$tb1y_a_p>0)]<-1
children<-aggregate.data.frame(data20181$chi,by=list(data20181$fid10),sum)
old<-aggregate.data.frame(data20181$old,by=list(data20181$fid10),sum)
data201811<-merge.data.frame(children,old,by=c("Group.1"))
colnames(data201811)<-c('fid','children','old')

data2018$fincome2[which(data2018$fincome2<0)]<-0
data2018$fincome2_per[which(data2018$fincome2_per<0)]<-0
data2018$trco[which(data2018$trco<0)]<-0
data2018$ft501[which(data2018$ft501<0)]<-0
data2018$ft601[which(data2018$ft601<0)]<-0
data2018$ft602[which(data2018$ft602<0)]<-0
data2018$debit_other[which(data2018$debit_other<0)]<-0
data2018$savings[which(data2018$savings<0)]<-0
data2018$ft201[which(data2018$ft201<0)]<-0
data2018$fm1[which(data2018$fm1 != 1)] <- 0
data2018_use<-cbind(data2018$fid10,data2018$cid18,data2018$provcd18,data2018$countyid18,data2018$fincome2,data2018$fincome2_per,data2018$fwage_2,data2018$fproperty_2,
                    #基本信息
                    data2018$trco,as.double(data2018$fm1),
                    #通信支出
                    data2018$familysize18,
                    #家庭规模
                    data2018$ft501,data2018$ft601,data2018$ft602,data2018$debit_other,
                    #银行贷款，亲友贷款，民间贷款，别人欠自家的
                    data2018$savings,data2018$ft201,
                    #存款,市值
                    as.vector(rep(5,14218)))
                    #年份
colnames(data2018_use)<-c('fid','cid','provcd','countyid','faminc_net','indinc_net','finc','fproperty',
                          'trco','company','familysize',
                          'bank_debit','fam_debit','peo_debit','debit_other',
                          'saving','marketvalue',
                          'year')
data2018_use<-merge.data.frame(data2018_use,data201811,by=c("fid"))


data20201<-read_dta("cfps2020famconf_202306.dta")
data20201$chi<-0
data20201$chi[which(data20201$tb1y_a_p>=2004&data20201$tb1y_a_p>0)]<-1
data20201$old<-0
data20201$old[which(data20201$tb1y_a_p<=1960&data20201$tb1y_a_p>0)]<-1
children<-aggregate.data.frame(data20201$chi,by=list(data20201$fid10),sum)
old<-aggregate.data.frame(data20201$old,by=list(data20201$fid10),sum)
data202011<-merge.data.frame(children,old,by=c("Group.1"))
colnames(data202011)<-c('fid','children','old')

data2020$fincome1[which(data2020$fincome1<0)]<-0
data2020$fincome1_per_p[which(data2020$fincome1_per_p<0)]<-0
data2020$trco[which(data2020$trco<0)]<-0
data2020$ft501[which(data2020$ft501<0)]<-0
data2020$ft601[which(data2020$ft601<0)]<-0
data2020$ft602[which(data2020$ft602<0)]<-0
data2020$debit_other[which(data2020$debit_other<0)]<-0
data2020$savings[which(data2020$savings<0)]<-0
data2020$ft201[which(data2020$ft201<0)]<-0
data2020$fm1[which(data2020$fm1 != 1)] <- 0
data2020_use<-cbind(data2020$fid10,data2020$cid20,data2020$provcd20,data2020$countyid20,data2020$fincome1,data2020$fincome1_per,data2020$fwage_1,data2020$fproperty_1,
                    #基本信息
                    data2020$trco,as.double(data2020$fm1),
                    #通信支出
                    data2020$familysize20,
                    #家庭规模
                    data2020$ft501,data2020$ft601,data2020$ft602,data2020$debit_other,
                    #银行贷款，亲友贷款，民间贷款，别人欠自家的
                    data2020$savings,data2020$ft201,
                    #存款,市值
                    as.vector(rep(6,11620)))
#年份
colnames(data2020_use)<-c('fid','cid','provcd','countyid','faminc_net','indinc_net','finc','fproperty',
                          'trco','company','familysize',
                          'bank_debit','fam_debit','peo_debit','debit_other',
                          'saving','marketvalue',
                          'year')
data2020_use<-merge.data.frame(data2020_use,data202011,by=c("fid"))

#####比对家户号fid，筛选连续被调查家庭#####
data_use2010<-data.frame(data2010_use)
data_use2010<-data_use2010[which(data_use2010$faminc_net>0),]
l1<-count(data_use2010,fid)
l2 <- l1[(l1$n!=1),]
vec<- l2$fid
record <- matrix(NA,nrow = nrow(l2),ncol = 2)
l=1
for (i in 1:nrow(data_use2010)){
  if ((data_use2010$fid[i] %in% vec) && (data_use2010$fid[i] %in% record[,2])){
    data_use2010$fid[i]=data_use2010$fid[i]/10000
  }
  if ((data_use2010$fid[i] %in% vec) && (data_use2010$fid[i] %in% record[,2]==FALSE)){
    record[l,1] <- i
    record[l,2] <- data_use2010$fid[i]
    l=l+1
  }
}
data_use2010<-data_use2010[which(data_use2010$fid>=110000),]

data_use2012<-data.frame(data2012_use)
data_use2012<-data_use2012[which(data_use2012$faminc_net>0),]
l1<-count(data_use2012,fid)
l2 <- l1[(l1$n!=1),]
vec<- l2$fid
record <- matrix(NA,nrow = nrow(l2),ncol = 2)
l=1
for (i in 1:nrow(data_use2012)){
  if ((data_use2012$fid[i] %in% vec) && (data_use2012$fid[i] %in% record[,2])){
    data_use2012$fid[i]=data_use2012$fid[i]/10000
  }
  if ((data_use2012$fid[i] %in% vec) && (data_use2012$fid[i] %in% record[,2]==FALSE)){
    record[l,1] <- i
    record[l,2] <- data_use2012$fid[i]
    l=l+1
  }
}
data_use2012<-data_use2012[which(data_use2012$fid>=110000),]

data_use2014<-data.frame(data2014_use)
data_use2014<-data_use2014[which(data_use2014$faminc_net>0),]
l1<-count(data_use2014,fid)
l2 <- l1[(l1$n!=1),]
vec<- l2$fid
record <- matrix(NA,nrow = nrow(l2),ncol = 2)
l=1
for (i in 1:nrow(data_use2014)){
  if ((data_use2014$fid[i] %in% vec) && (data_use2014$fid[i] %in% record[,2])){
    data_use2014$fid[i]=data_use2014$fid[i]/10000
  }
  if ((data_use2014$fid[i] %in% vec) && (data_use2014$fid[i] %in% record[,2]==FALSE)){
    record[l,1] <- i
    record[l,2] <- data_use2014$fid[i]
    l=l+1
  }
}
data_use2014<-data_use2014[which(data_use2014$fid>=110000),]

data_use2016<-data.frame(data2016_use)
data_use2016<-data_use2016[which(data_use2016$faminc_net>0),]
l1<-count(data_use2016,fid)
l2 <- l1[(l1$n!=1),]
vec<- l2$fid
record <- matrix(NA,nrow = nrow(l2),ncol = 2)
l=1
for (i in 1:nrow(data_use2016)){
  if ((data_use2016$fid[i] %in% vec) && (data_use2016$fid[i] %in% record[,2])){
    data_use2016$fid[i]=data_use2016$fid[i]/10000
  }
  if ((data_use2016$fid[i] %in% vec) && (data_use2016$fid[i] %in% record[,2]==FALSE)){
    record[l,1] <- i
    record[l,2] <- data_use2016$fid[i]
    l=l+1
  }
}
data_use2016<-data_use2016[which(data_use2016$fid>=110000),]

data_use2018<-data.frame(data2018_use)
data_use2018<-data_use2018[which(data_use2018$faminc_net>0),]
l1<-count(data_use2018,fid)
l2 <- l1[(l1$n!=1),]
vec<- l2$fid
record <- matrix(NA,nrow = nrow(l2),ncol = 2)
l=1
for (i in 1:nrow(data_use2018)){
  if ((data_use2018$fid[i] %in% vec) && (data_use2018$fid[i] %in% record[,2])){
    data_use2018$fid[i]=data_use2018$fid[i]/10000
  }
  if ((data_use2018$fid[i] %in% vec) && (data_use2018$fid[i] %in% record[,2]==FALSE)){
    record[l,1] <- i
    record[l,2] <- data_use2018$fid[i]
    l=l+1
  }
}
data_use2018<-data_use2018[which(data_use2018$fid>=110000),]

data_use2020<-data.frame(data2020_use)
data_use2020<-data_use2020[which(data_use2020$faminc_net>0),]
l1<-count(data_use2020,fid)
l2 <- l1[(l1$n!=1),]
vec<- l2$fid
record <- matrix(NA,nrow = nrow(l2),ncol = 2)
l=1
for (i in 1:nrow(data_use2020)){
  if ((data_use2020$fid[i] %in% vec) && (data_use2020$fid[i] %in% record[,2])){
    data_use2020$fid[i]=data_use2020$fid[i]/10000
  }
  if ((data_use2020$fid[i] %in% vec) && (data_use2020$fid[i] %in% record[,2]==FALSE)){
    record[l,1] <- i
    record[l,2] <- data_use2020$fid[i]
    l=l+1
  }
}
data_use2020<-data_use2020[which(data_use2020$fid>=110000),]

data_use<-rbind.data.frame(data_use2010,data_use2012,data_use2014,data_use2016,data_use2018,data_use2020)
l1<-count(data_use,fid)
l2 <- l1[(l1$n==6),]
vec<-l2$fid
data_use<-subset.data.frame(data_use, fid %in% vec,select = c('fid','cid','provcd','countyid','faminc_net','indinc_net','finc','fproperty',
                                                              'trco','company','familysize',
                                                              'bank_debit','fam_debit','peo_debit','debit_other',
                                                              'saving','marketvalue','children','old',
                                                              'year'))
data_use[is.na(data_use)]<-0
vec<-data_use$fid[which(data_use$year==1)]
for (i in 2:6){
  vec1<-data_use$fid[which(data_use$year==i)]
  vec<-intersect(vec,vec1)
}
data_use<-subset.data.frame(data_use, fid %in% vec,select = c('fid','cid','provcd','countyid','faminc_net','indinc_net','finc','fproperty',
                                                              'trco','company','familysize',
                                                              'bank_debit','fam_debit','peo_debit','debit_other',
                                                              'saving','marketvalue','children','old',
                                                              'year'))
write_dta(data_use,'data_usenew.dta')

#######adult#######
library(haven)
library(dplyr)
data2010_adult<-read_dta("cfps2010adult_202008.dta")
data2012_adult<-read_dta("cfps2012adult_201906.dta")
data2014_adult<-read_dta("cfps2014adult_201906.dta")
data2016_adult<-read_dta("cfps2016adult_201906.dta")
data2018_adult<-read_dta("cfps2018person_202012.dta")
data2020_adult<-read_dta("cfps2020person_202306.dta")

data2010_adult$qe1_best[which(data2010_adult$qe1_best==1)]<-0
data2010_adult$qe1_best[which(data2010_adult$qe1_best==2)]<-1
data2010_adult$qe1_best[which(data2010_adult$qe1_best!=1)]<-0
#原表中qe1=1,qe1=2未婚，已婚，此处改为1为已婚
data2010_adult$qc1[which(data2010_adult$qc1<=0)]<-1
data2010_adult$ku2[which(data2010_adult$ku2!=1)]<-0
data2010_adult$qp3[which(data2010_adult$qp3<0)]<-6
data2010_adult$qg3[which(data2010_adult$qg3<0)]<-0
data2010_adult$ku1[which(data2010_adult$ku1<0)]<-0
data2010_adult$qb1[which(data2010_adult$qb1<0)]<-0
#qp3缺失值默认-8，改成6为不知自己健康状况的
data2010_adult$qa701[which(data2010_adult$qa701<=0)]<-0
data2010_adult$qa701[which(data2010_adult$qa701>0)]<-1
#政治面貌
data2010_adult_use<-cbind.data.frame(data2010_adult$fid,data2010_adult$pid,data2010_adult$gender,2010-data2010_adult$qa1y_best,
                          data2010_adult$qe1_best,data2010_adult$qc1,data2010_adult$cfps2010eduy_best,
                          data2010_adult$ku2,data2010_adult$qp3,as.double(data2010_adult$qg3),as.double(data2010_adult$ku1),as.double(data2010_adult$qa701),as.vector(rep(1,33598)))
colnames(data2010_adult_use)<-c('fid','code','gender','age','marry','edulevel','eduyear','network','health','job','phone','party','year')


data2012_adult$cfps2012_age[which(data2012_adult$cfps2012_age<0)]<-0
#年龄低于0的视为0
data2012_adult$qe104[which(data2012_adult$qe104==1)]<-0
data2012_adult$qe104[which(data2012_adult$qe104==2)]<-1
data2012_adult$qe104[which(data2012_adult$qe104!=1)]<-0
#1为已婚
data2012_adult$edu2012[which(is.na(data2012_adult$edu2012))]<-1
#NA视为文盲
data2012_adult$eduy2012[which(is.na(data2012_adult$eduy2012))]<-0
#NA视为无教育经历
data2012_adult$qp201[which(data2012_adult$qp201<0)]<-6
#NA不知自己健康水平
data2012_adult$qg101[which(is.na(data2012_adult$qg101))]<-0
data2012_adult$qg101[which(data2012_adult$qg101!=1)]<-0

#NA为无工作
data2012_adult$cfps_party[which(data2012_adult$cfps_party!=1)]<-0
#筛选出党员
data2012_adult_use<-cbind.data.frame(data2012_adult$fid10,data2012_adult$pid,data2012_adult$cfps2012_gender_best,data2012_adult$cfps2012_age,
                          data2012_adult$qe104,data2012_adult$edu2012,data2012_adult$eduy2012,
                          as.vector(rep(0,35719)),as.double(data2012_adult$qp201),as.double(data2012_adult$qg101),as.vector(rep(0,35719)),as.double(data2012_adult$cfps_party),as.vector(rep(2,35719)))
colnames(data2012_adult_use)<-c('fid','code','gender','age','marry','edulevel','eduyear','network','health','job','phone','party','year')
for (i in 1:35719){
  data2012_adult_use$network[i]=max(data2010_adult_use$network[which(data2010_adult_use$fid==data2012_adult_use$fid[i])])
}
data2012_adult_use$network[which(is.infinite(data2012_adult_use$network))]<-0
for (i in 1:35719){
  data2012_adult_use$phone[i]=max(data2010_adult_use$phone[which(data2010_adult_use$fid==data2012_adult_use$fid[i])])
}
data2012_adult_use$phone[which(is.infinite(data2012_adult_use$phone))]<-0
data2012_adult_use$job[which(data2012_adult_use$job>1)]<-1


data2014_adult$cfps2014_age[which(data2014_adult$cfps2014_age<0)]<-0
#年龄低于0的视为0
data2014_adult$qea0[which(data2014_adult$qea0==1)]<-0
data2014_adult$qea0[which(data2014_adult$qea0==2)]<-1
data2014_adult$qea0[which(data2014_adult$qea0!=1)]<-0
#1为已婚
data2014_adult$cfps2014edu[which(data2014_adult$cfps2014edu<0|data2014_adult$cfps2014edu==9)]<-1
#不知道或缺失不读书视为文盲
data2014_adult$cfps2014eduy[which(is.na(data2014_adult$cfps2014eduy)|data2014_adult$cfps2014eduy<0)]<-0
#NA视为无教育经历
data2014_adult$ku2[which(is.na(data2014_adult$ku2))]<-0
data2014_adult$ku2[which(data2014_adult$ku2!=1)]<-0
#ku2代表是否上网，NA视为无法上网
data2014_adult$ku1m[which(is.na(data2014_adult$ku1m))]<-0
data2014_adult$ku1m[which(data2014_adult$ku1m!=1)]<-0
#ku1m代表是否用手机，NA为无法使用
data2014_adult$pg01[which(data2014_adult$pg01!=1)]<-0
#qg1为是否工作
data2014_adult$qp201[which(data2014_adult$qp201<0)]<-6
#NA不知自己健康水平
data2014_adult$cfps_party[which(data2012_adult$cfps_party!=1)]<-0
#党员身份
data2014_adult_use<-cbind.data.frame(data2014_adult$fid10,data2014_adult$pid,data2014_adult$cfps_gender,data2014_adult$cfps2014_age,
                          data2014_adult$qea0,data2014_adult$cfps2014edu,data2014_adult$cfps2014eduy,
                          data2014_adult$ku2,data2014_adult$qp201,as.double(data2014_adult$pg01),as.double(data2014_adult$ku1m),as.double(data2014_adult$cfps_party),as.vector(rep(3,37147)))
colnames(data2014_adult_use)<-c('fid','code','gender','age','marry','edulevel','eduyear','network','health','job','phone','party','year')
data2014_adult_use$network[which(is.na(data2014_adult_use$network))]<-0
for (i in 1:37147){
  data2014_adult_use$network[i]=max(data2012_adult_use$network[which(data2012_adult_use$fid==data2014_adult_use$fid[i])],
                                    data2014_adult_use$network[which(data2014_adult_use$fid==data2014_adult_use$fid[i])])
}


data2016_adult$cfps_gender[which(data2016_adult$cfps_gender<0)]<-0
#年龄低于0的视为0
data2016_adult$cfps_age[which(data2016_adult$cfps_age<0)]<-0
#年龄低于0的视为0
data2016_adult$qea0[which(data2016_adult$qea0==1)]<-0
data2016_adult$qea0[which(data2016_adult$qea0==2)]<-1
data2016_adult$qea0[which(data2016_adult$qea0!=1)]<-0
#1为已婚
data2016_adult$cfps2016edu[which(is.na(data2016_adult$cfps2016edu))]<-1
#不知道或缺失不读书视为文盲
data2016_adult$cfps2016eduy[which(is.na(data2016_adult$cfps2016eduy))]<-0
#NA视为无教育经历
data2016_adult$ku201[which(is.na(data2016_adult$ku201))]<-0
data2016_adult$ku202[which(is.na(data2016_adult$ku202))]<-0
data2016_adult$ku201[which(data2016_adult$ku201!=1)]<-0
data2016_adult$ku202[which(data2016_adult$ku202!=1)]<-0
#NA视为无法上网
data2016_adult$employ[which(data2016_adult$employ!=1)]<-0
#工作状态
data2016_adult$ku1m[which(is.na(data2016_adult$ku1m))]<-0
data2016_adult$ku1m[which(data2016_adult$ku1m!=1)]<-0
#是否使用手机
data2016_adult$qp201[which(data2016_adult$qp201<0)]<-6
#NA不知自己健康水平
data2016_adult$qn4001[which(data2016_adult$qn4001!=1)]<-0
#党员
data2016_adult_use<-cbind.data.frame(data2016_adult$fid10,data2016_adult$pid,data2016_adult$cfps_gender,data2016_adult$cfps_age,
                          data2016_adult$qea0,data2016_adult$cfps2016edu,data2016_adult$cfps2016eduy,
                          round((data2016_adult$ku201+data2016_adult$ku202+0.1)/2),data2016_adult$qp201,as.double(data2016_adult$employ),as.double(data2016_adult$ku1m),as.double(data2016_adult$qn4001),as.vector(rep(4,36892)))
colnames(data2016_adult_use)<-c('fid','code','gender','age','marry','edulevel','eduyear','network','health','job','phone','party','year')


data2018_adult$gender_update[which(data2018_adult$gender_update<0)]<-0
#年龄低于0的视为0
data2018_adult$age[which(data2018_adult$age<0)]<-0
#年龄低于0的视为0
data2018_adult$qea0[which(data2018_adult$qea0==1)]<-0
data2018_adult$qea0[which(data2018_adult$qea0==2)]<-1
data2018_adult$qea0[which(data2018_adult$qea0!=1)]<-0
#1为已婚
data2018_adult$cfps2018edu[which(is.na(data2018_adult$cfps2018edu))]<-1
#不知道或缺失不读书视为文盲
data2018_adult$cfps2018eduy[which(is.na(data2018_adult$cfps2018eduy))]<-0
#NA视为无教育经历
data2018_adult$qu201[which(is.na(data2018_adult$qu201))]<-0
data2018_adult$qu202[which(is.na(data2018_adult$qu202))]<-0
data2018_adult$qu201[which(data2018_adult$qu201!=1)]<-0
data2018_adult$qu202[which(data2018_adult$qu202!=1)]<-0
#NA视为无法上网
data2018_adult$employ[which(data2018_adult$employ!=1)]<-0
#就业状态
data2018_adult$qu1m[which(is.na(data2018_adult$qu1m))]<-0
data2018_adult$qu1m[which(data2018_adult$qu1m!=1)]<-0
#能否用手机
data2018_adult$qp201[which(data2018_adult$qp201<0)]<-6
#NA不知自己健康水平
data2018_adult$party[which(data2018_adult$party!=1)]<-0
#党员身份
data2018_adult_use<-cbind.data.frame(data2018_adult$fid10,data2018_adult$pid,data2018_adult$gender_update,data2018_adult$age,
                          data2018_adult$qea0,data2018_adult$cfps2018edu,data2018_adult$cfps2018eduy,
                          round((data2018_adult$qu201+data2018_adult$qu202+0.1)/2),data2018_adult$qp201,as.double(data2018_adult$employ),as.double(data2018_adult$qu1m),as.double(data2018_adult$party),as.vector(rep(5,37354)))
colnames(data2018_adult_use)<-c('fid','code','gender','age','marry','edulevel','eduyear','network','health','job','phone','party','year')


data2020_adult$gender[which(data2020_adult$gender<0)]<-0
#年龄低于0的视为0
data2020_adult$age[which(data2020_adult$age<0)]<-0
#年龄低于0的视为0
data2020_adult$qea0[which(data2020_adult$qea0==1)]<-0
data2020_adult$qea0[which(data2020_adult$qea0==2)]<-1
data2020_adult$qea0[which(data2020_adult$qea0!=1)]<-0
#1为已婚
data2020_adult$cfps2020edu[which(is.na(data2020_adult$cfps2020edu) | data2020_adult$cfps2020edu<0) ]<-1
#不知道或缺失不读书视为文盲
data2020_adult$cfps2020eduy[which(is.na(data2020_adult$cfps2020eduy) | data2020_adult$cfps2020eduy<0)]<-0
#NA视为无教育经历
data2020_adult$qu201[which(is.na(data2020_adult$qu201))]<-0
data2020_adult$qu202[which(is.na(data2020_adult$qu202))]<-0
data2020_adult$qu201[which(data2020_adult$qu201!=1)]<-0
data2020_adult$qu202[which(data2020_adult$qu202!=1)]<-0
#NA视为无法上网
data2020_adult$employ[which(data2020_adult$employ!=1)]<-0
#就业状态
data2020_adult$qu201[which(is.na(data2020_adult$qu201))]<-0
data2020_adult$qu201[which(data2020_adult$qu201!=1)]<-0
#能否用手机
data2020_adult$qp201[which(data2020_adult$qp201<0)]<-6
#NA不知自己健康水平
data2020_adult$party[which(data2020_adult$party!=1)]<-0
#党员身份
data2020_adult_use<-cbind.data.frame(data2020_adult$fid10,data2020_adult$pid,data2020_adult$gender,data2020_adult$age,
                                     data2020_adult$qea0,data2020_adult$cfps2020edu,data2020_adult$cfps2020eduy,
                                     round((data2020_adult$qu201+data2020_adult$qu202+0.1)/2),data2020_adult$qp201,as.double(data2020_adult$employ),as.double(data2020_adult$qu201),as.double(data2020_adult$party),as.vector(rep(6,28530)))
colnames(data2020_adult_use)<-c('fid','code','gender','age','marry','edulevel','eduyear','network','health','job','phone','party','year')


data_adult_use<-rbind.data.frame(data2010_adult_use,data2012_adult_use,data2014_adult_use,data2016_adult_use,data2018_adult_use,data2020_adult_use)
data_adult_use<-data_adult_use[data_adult_use$fid>0,]
data_adult_use1<-data_adult_use
data_adult_use<-subset.data.frame(data_adult_use, fid %in% vec,select = c('fid','code','gender','age','marry','edulevel','eduyear','network','health','job','phone','party','year'))
vec_data<-aggregate.data.frame(x=data_adult_use,by=list(data_adult_use$fid),FUN = 'min')
vec_code<-vec_data$code
data_adult_final<-subset.data.frame(data_adult_use, code %in% vec_code,select = c('fid','code','gender','age','marry','edulevel','eduyear','network','health','job','phone','party','year'))

#####比对家户号fid，筛选连续被调查家庭对应的户主个人，data_use可以读取data_usenew.dta文件获取#####
l1<-count(data_adult_final,fid)
l2 <- l1[(l1$n==6),]
vec_fid<-l2$fid
data_adult_final<-subset.data.frame(data_adult_final, fid %in% vec,select = c('fid','code','gender','age','marry','edulevel','eduyear','network','health','job','phone','party','year'))
data_final<-merge.data.frame(x=data_adult_final,y=data_use,by=c('fid','year'))

vec<-data_final$fid[which(data_final$year==1)]
for (i in 2:6){
  vec1<-data_final$fid[which(data_final$year==i)]
  vec<-intersect(vec,vec1)
}
data_final<-subset.data.frame(data_final, fid %in% vec,select = c('fid','cid','provcd','countyid','faminc_net','indinc_net','finc','fproperty',
                                                              'trco','company','familysize',
                                                              'bank_debit','fam_debit','peo_debit','debit_other',
                                                              'saving','marketvalue','children','old',
                                                              'code','gender','age','marry','edulevel','eduyear','network','health','job','phone','party',
                                                              'year'))
data_final[is.na(data_final)]<-0
#######datafinance（数字普惠金融指数）#######
library(haven)
library(dplyr)
#data_finance.csv是从北京大学数字普惠金融指数（PKU-DFIIC）2011_2020.xlsx直接复制粘贴指标过来的
data_finance<-read.table("data_finance.csv",sep=',',header=T,fileEncoding = "gbk")
data_finance$year[which(data_finance$year==2011)]=1
data_finance$year[which(data_finance$year==2012)]=2
data_finance$year[which(data_finance$year==2014)]=3
data_finance$year[which(data_finance$year==2016)]=4
data_finance$year[which(data_finance$year==2018)]=5
data_finance$year[which(data_finance$year==2020)]=6
data_res<-merge.data.frame(x=data_final,y=data_finance,by=c('provcd','year'))

vec<-data_res$fid[which(data_res$year==1)]
for (i in 2:6){
  vec1<-data_res$fid[which(data_res$year==i)]
  vec<-intersect(vec,vec1)
}
data_res<-subset.data.frame(data_res, fid %in% vec,select = c('fid','cid','provcd','countyid','faminc_net','indinc_net','finc','fproperty',
                                                              'trco','company','familysize',
                                                              'bank_debit','fam_debit','peo_debit','debit_other',
                                                              'saving','marketvalue','children','old',
                                                              'code','gender','age','marry','edulevel','eduyear','network','health','job','phone','party',
                                                              'year','index_aggregate','coverage_breadth','usage_depth',
                                                              'digitization_level','insurance','credit','payment'))

#######处理provcd，解决省代码不连续的问题######
for (i in 1:nrow(data_res)){
  if (data_res$provcd[i]<20){
    data_res$provcd[i] <- data_res$provcd[i]-10
  }
  if (data_res$provcd[i]<30 && data_res$provcd[i]>20){
    data_res$provcd[i] <- data_res$provcd[i]-15
  }
  if (data_res$provcd[i]<40 && data_res$provcd[i]>30){
    data_res$provcd[i] <- data_res$provcd[i]-22
  }
  if (data_res$provcd[i]<50 && data_res$provcd[i]>40){
    data_res$provcd[i] <- data_res$provcd[i]-25
  }
  if (data_res$provcd[i]<60 && data_res$provcd[i]>=50){
    data_res$provcd[i] <- data_res$provcd[i]-28
  }
  if (data_res$provcd[i]>60){
    data_res$provcd[i] <- data_res$provcd[i]-34
  }
}
data_res <- data_res[order(data_res$fid,data_res$year),]
write.csv(data_res,"matlabdatanew.CSV",row.names=F,fileEncoding='gbk')

####for separated matrix，省内省外分开######
data_res<-read.csv("matlabdatanew.CSV",header=T,fileEncoding='gbk')

sum_famincnet<-aggregate.data.frame(log(data_res$faminc_net),list(data_res$provcd,data_res$year),mean)
colnames(sum_famincnet)<-c('provcd','year','sum_famincnet')
data_res<-merge.data.frame(x=data_res,y=sum_famincnet,by=c('provcd','year'))

sum_finc<-aggregate.data.frame(log(data_res$finc+1),list(data_res$provcd,data_res$year),mean)
colnames(sum_finc)<-c('provcd','year','sum_finc')
data_res<-merge.data.frame(x=data_res,y=sum_finc,by=c('provcd','year'))

sum_fproperty<-aggregate.data.frame(log(data_res$fproperty+1),list(data_res$provcd,data_res$year),mean)
colnames(sum_fproperty)<-c('provcd','year','sum_fproperty')
data_res<-merge.data.frame(x=data_res,y=sum_fproperty,by=c('provcd','year'))
data_res <- data_res[order(data_res$fid,data_res$year),]
write.csv(data_res,"matlabdatanew1.CSV",row.names=F,fileEncoding='gbk')#需要在excel里面按（家户号，年份）字典式排序

data_res<-read.csv("matlabdatanew1.CSV",header=TRUE,encoding="gbk")
dist_data<-read.csv("matrix_geonew.CSV",header=FALSE,fileEncoding="gbk")
num <- nrow(data_res)/6
for (t in 1:6){
  data_res1<-data_res[which(data_res$year==t),]
  dist_geo<-matrix(0,nrow = num,ncol = num)
  for (i in 1:(num-1)){
    for (j in (i+1):num){
      dist_geo[i,j]<-as.double(as.character(dist_data[(data_res1$provcd[i]+1),(data_res1$provcd[j]+1)]))
    }
  }
  dist_geo<-dist_geo+t(dist_geo)
  write.csv(dist_geo,sprintf("matrix_geo%d.CSV",t),row.names=F)
}

data_matrix_eco<-read.csv("国内人均GDP.CSV",header=FALSE)
for (t in 1:6){
  data_res1<-data_res[which(data_res$year==t),]
  dist_eco<-matrix(0,nrow = num,ncol = num)
  for (i in 1:(num-1)){
    for (j in (i+1):num){
      if (data_res1$provcd[i]!=data_res1$provcd[j]){
        dist_eco[i,j]=1/abs(data_matrix_eco[t+1,(data_res1$provcd[i]+1)]-data_matrix_eco[t+1,(data_res1$provcd[j]+1)])
      }
      else{
        dist_eco[i,j]=0
      }
    }
  }
  dist_eco<-dist_eco+t(dist_eco)
  write.csv(dist_eco,sprintf("matrix_eco%d.CSV",t),row.names=F)
}


######hangzhou######
data_res<-read.csv("matlabdatanew1.CSV",header=T)
dist_hangzhou <- read.csv("省级距离.csv",sep=',',header=F,fileEncoding='gbk')
dist_hz <- rep(0,length(data_res$provcd))
for (i in 1:length(data_res$provcd)){
  dist_hz[i] <- dist_hangzhou$V2[data_res$provcd[i]]
}
data_res$dist_hz<-dist_hz
write.csv(data_res,"matlabdatanew1.csv",row.names=F)

######famincnet#####
data_res<-read.csv("matlabdatanew1.CSV",header=T)
famincnet_lag<-rep(0,27552)
for (i in 1:27552){
  if (i%%6!=1){
    famincnet_lag[i]<-data_res$faminc_net[i-1]
  } 
}
data_res<-cbind.data.frame(data_res,famincnet_lag)

sum_famincnet_lag<-rep(0,27552)
for (i in 1:27552){
  if (i%%6!=1){
    sum_famincnet_lag[i]<-data_res$sum_famincnet[i-1]
  } 
}
data_res<-cbind.data.frame(data_res,sum_famincnet_lag)

#####finc#####
finc_lag<-rep(0,27552)
for (i in 1:27552){
  if (i%%6!=1){
    finc_lag[i]<-data_res$finc[i-1]
  } 
}
data_res<-cbind.data.frame(data_res,finc_lag)

sum_finc_lag<-rep(0,27552)
for (i in 1:27552){
  if (i%%6!=1){
    sum_finc_lag[i]<-data_res$sum_finc[i-1]
  } 
}
data_res<-cbind.data.frame(data_res,sum_finc_lag)

#####fproperty######
fproperty_lag<-rep(0,27552)
for (i in 1:27552){
  if (i%%6!=1){
    fproperty_lag[i]<-data_res$fproperty[i-1]
  } 
}
data_res<-cbind.data.frame(data_res,fproperty_lag)

sum_fproperty_lag<-rep(0,27552)
for (i in 1:27552){
  if (i%%6!=1){
    sum_fproperty_lag[i]<-data_res$sum_fproperty[i-1]
  } 
}
data_res<-cbind.data.frame(data_res,sum_fproperty_lag)


#####calculate special index#####
data_res$child_ratio<-data_res$children/data_res$familysize
data_res$child_ratio<-ifelse(data_res$child_ratio>1,data_res$children/(data_res$children+2),data_res$child_ratio)
data_res$old_ratio<-data_res$old/data_res$familysize
data_res$old_ratio<-ifelse(data_res$old_ratio >1,1,data_res$old_ratio)

data_res$edu1<-ifelse(data_res$edulevel == 1,1,0)
data_res$edu2<-ifelse(data_res$edulevel == 2,1,0)
data_res$edu3<-ifelse(data_res$edulevel == 3,1,0)
data_res$edu4<-ifelse(data_res$edulevel == 4,1,0)
data_res$edu5<-ifelse(data_res$edulevel == 5,1,0)
data_res$edu6<-ifelse(data_res$edulevel == 6,1,0)
data_res$edu7<-ifelse(data_res$edulevel == 7,1,0)

data_res$hea1<-ifelse(data_res$health == 1,1,0)
data_res$hea2<-ifelse(data_res$health == 2,1,0)
data_res$hea3<-ifelse(data_res$health == 3,1,0)
data_res$hea4<-ifelse(data_res$health == 4,1,0)
data_res$hea5<-ifelse(data_res$health == 5,1,0)
data_res$hea6<-ifelse(data_res$health == 6,1,0)

write.csv(data_res,"matlabdatanew1.CSV",row.names=F,fileEncoding='gbk')
data_restore<-data_res

data_res<-read.csv("matlabdatanew1.CSV",header=T,fileEncoding='gbk')
##储存一份副本，接下去筛选finc和fproperty
######筛选finc########
data_res<-data_restore
fid_ls<-unique(data_res$fid[which(data_res$finc==0)])
data_res<-subset.data.frame(data_res,!(data_res$fid %in% fid_ls))
write.csv(data_res,"matlabdatanew1finc.CSV",row.names=F,fileEncoding='gbk')

######计算finc矩阵######
data_res<-read.csv("matlabdatanew1finc.CSV",header=T,fileEncoding='gbk')
dist_data<-read.csv("matrix_geonew.CSV",header=FALSE,fileEncoding="gbk")
num <- nrow(data_res)/6
for (t in 2:6){
  data_res1<-data_res[which(data_res$year==t),]
  dist_geo<-matrix(0,nrow = num,ncol = num)
  for (i in 1:(num-1)){
    for (j in (i+1):num){
      dist_geo[i,j]<-as.double(as.character(dist_data[(data_res1$provcd[i]+1),(data_res1$provcd[j]+1)]))
    }
  }
  dist_geo<-dist_geo+t(dist_geo)
  write.csv(dist_geo,sprintf("matrixfinc_geo%d.CSV",t),row.names=F)
}

data_matrix_eco<-read.csv("国内人均GDP.CSV",header=FALSE)
for (t in 2:6){
  data_res1<-data_res[which(data_res$year==t),]
  dist_eco<-matrix(0,nrow = num,ncol = num)
  for (i in 1:(num-1)){
    for (j in (i+1):num){
      if (data_res1$provcd[i]!=data_res1$provcd[j]){
        dist_eco[i,j]=1/abs(data_matrix_eco[t+1,(data_res1$provcd[i]+1)]-data_matrix_eco[t+1,(data_res1$provcd[j]+1)])
      }
      else{
        dist_eco[i,j]=0
      }
    }
  }
  dist_eco<-dist_eco+t(dist_eco)
  write.csv(dist_eco,sprintf("matrixfinc_eco%d.CSV",t),row.names=F)
}

##筛选fproperty
data_res<-data_restore
fid_ls1<-data_res$fid[data_res$fproperty==0 & data_res$year==1]
fid_ls2<-data_res$fid[data_res$fproperty==0 & data_res$year==2]
fid_ls3<-data_res$fid[data_res$fproperty==0 & data_res$year==3]
fid_ls4<-data_res$fid[data_res$fproperty==0 & data_res$year==4]
fid_ls5<-data_res$fid[data_res$fproperty==0 & data_res$year==5]
fid_ls6<-data_res$fid[data_res$fproperty==0 & data_res$year==6]
vector1<-cbind.data.frame(unique(data_res$fid),rep(0,4592))
for (i in 1:4592){
  if (vector1[i,1] %in% fid_ls1){
    vector1[i,2] = vector1[i,2]+1
  }
  if (vector1[i,1] %in% fid_ls2){
    vector1[i,2] = vector1[i,2]+1
  }
  if (vector1[i,1] %in% fid_ls3){
    vector1[i,2] = vector1[i,2]+1
  }
  if (vector1[i,1] %in% fid_ls4){
    vector1[i,2] = vector1[i,2]+1
  }
  if (vector1[i,1] %in% fid_ls5){
    vector1[i,2] = vector1[i,2]+1
  }
  if (vector1[i,1] %in% fid_ls6){
    vector1[i,2] = vector1[i,2]+1
  }
}
vec_fid <- vector1$`unique(data_res$fid)`[vector1$`rep(0, 4592)`<4]
data_res <- subset.data.frame(data_res,(data_res$fid %in% vec_fid))
write.csv(data_res,"matlabdatanew1fproperty.CSV",row.names=F,fileEncoding='gbk')

######计算property矩阵######
data_res<-read.csv("matlabdatanew1fproperty.CSV",header=T,fileEncoding='gbk')
dist_data<-read.csv("matrix_geonew.CSV",header=FALSE,fileEncoding="gbk")
num <- nrow(data_res)/6
for (t in 2:6){
  data_res1<-data_res[which(data_res$year==t),]
  dist_geo<-matrix(0,nrow = num,ncol = num)
  for (i in 1:num-1){
    for (j in (i+1):num){
      dist_geo[i,j]<-as.double(as.character(dist_data[(data_res1$provcd[i]+1),(data_res1$provcd[j]+1)]))
    }
  }
  dist_geo<-dist_geo+t(dist_geo)
  write.csv(dist_geo,sprintf("matrixfproperty_geo%d.CSV",t),row.names=F)
}

data_matrix_eco<-read.csv("国内人均GDP.CSV",header=FALSE)
for (t in 2:6){
  data_res1<-data_res[which(data_res$year==t),]
  dist_eco<-matrix(0,nrow = num,ncol = num)
  for (i in 1:(num-1)){
    for (j in (i+1):num){
      if (data_res1$provcd[i]!=data_res1$provcd[j]){
        dist_eco[i,j]=1/abs(data_matrix_eco[t+1,(data_res1$provcd[i]+1)]-data_matrix_eco[t+1,(data_res1$provcd[j]+1)])
      }
      else{
        dist_eco[i,j]=0
      }
    }
  }
  dist_eco<-dist_eco+t(dist_eco)
  write.csv(dist_eco,sprintf("matrixfproperty_eco%d.CSV",t),row.names=F)
}


#####zhejiang-province plot######
library(ggplot2)
library(gridExtra)
data_res<-read.csv("matlabdatanew1.CSV",header=T,fileEncoding='gbk')

data_zhejiang<-subset.data.frame(data_res, provcd == 11,select = c('fid','cid','provcd','countyid','faminc_net','year'))
mean_famincnet_zhejiang<-aggregate.data.frame(data_zhejiang$faminc_net,list(data_zhejiang$year),mean)

data_anhui<-subset.data.frame(data_res, provcd == 12,select = c('fid','cid','provcd','countyid','faminc_net','year'))
mean_famincnet_anhui<-aggregate.data.frame(data_anhui$faminc_net,list(data_anhui$year),mean)

x<-c(2010,2012,2014,2016,2018,2020)
y<-mean_famincnet_zhejiang$x
data1<-cbind.data.frame(x,y)

ggplot(data1, aes(x, y))+geom_line()+geom_point()+
  geom_text(aes(label = round(y,2)),vjust="inward",hjust="inward",family="serif",size=4)+
scale_x_continuous(breaks=data1$x, labels = data1$x)+
labs(x="year",y="average family income",title="Zhejiang Province") + 
theme_bw() + theme(panel.grid=element_blank())+
theme(plot.title = element_text(size=12,hjust=0.5,family = "serif"))+
theme(axis.text = element_text(size=12,family = "serif"))+
theme(axis.title = element_text(size=12,family = "serif"))+
theme(text = element_text(size=12,family = "serif"))

x<-c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020)
y<-c(77.39,146.35,205.77,224.45,264.85,268.10,318.05,357.45,387.49,406.88)
data1<-cbind.data.frame(x,y)
library(ggplot2)
ggplot(data1, aes(x, y))+geom_line()+
  geom_text(aes(label = round(y,2)),vjust=0.5,family="serif",size=4)+
  scale_x_continuous(breaks=data1$x, labels = data1$x)+
  labs(x="year",y="value",title="DFI of Zhejiang province") + 
  theme_bw() + theme(panel.grid=element_blank())+
  theme(plot.title = element_text(size=12,hjust=0.5,family = "serif"))+
  theme(axis.text = element_text(size=12,family = "serif"))+
  theme(axis.title = element_text(size=12,family = "serif"))+
  theme(text = element_text(size=12,family = "serif"))

x<-c(2012,2014,2016,2018,2020)
y1<-c(199.6090,267.3136,271.4355,277.7756,286.4364)
y2<-c(124.4978,179.6767,182.4696,186.7357,193.5186)
y3<-c(-78.1607,-39.2771,-44.8909,-48.5816,-50.6124)
data1<-cbind.data.frame(x,y1,y2,y3)

options(repr.plot.width =8, repr.plot.height =6)

plot1<-ggplot(data1, aes(x, y1)) +
  geom_line()+geom_text(aes(label = round(y1,2)),vjust="inward",hjust="inward",family="serif",size=5)+
  scale_x_continuous(breaks=data1$x, labels = data1$x)+
  labs(x="year",y="factor",title="25% quantile Factor") + 
  theme_bw() + theme(panel.grid=element_blank())+
  theme(plot.title = element_text(size=16,hjust=0.5,family = "serif"))+
  theme(axis.text = element_text(size=16,family = "serif"))+
  theme(axis.title = element_text(size=16,family = "serif"))+
  theme(text = element_text(size=16,family = "serif"))

plot2<-ggplot(data1, aes(x, y2)) +
  geom_line()+geom_text(aes(label = round(y2,2)),vjust="inward",hjust="inward",family="serif",size=5)+
  scale_x_continuous(breaks=data1$x, labels = data1$x)+
  labs(x="year",y="factor",title="50% quantile Factor") + 
  theme_bw() + theme(panel.grid=element_blank())+
  theme(plot.title = element_text(size=16,hjust=0.5,family = "serif"))+
  theme(axis.text = element_text(size=16,family = "serif"))+
  theme(axis.title = element_text(size=16,family = "serif"))+
  theme(text = element_text(size=16,family = "serif"))

plot3<-ggplot(data1, aes(x, y3))+
  geom_line()+geom_text(aes(label = round(y3,2)),vjust="inward",hjust="inward",family="serif",size=5)+
  scale_x_continuous(breaks=data1$x, labels = data1$x)+
  labs(x="year",y="factor",title="75% quantile Factor") + 
  theme_bw() + theme(panel.grid=element_blank())+
  theme(plot.title = element_text(size=16,hjust=0.5,family = "serif"))+
  theme(axis.text = element_text(size=16,family = "serif"))+
  theme(axis.title = element_text(size=16,family = "serif"))+
  theme(text = element_text(size=16,family = "serif"))

grid.arrange(plot1, plot2, plot3, ncol=3,widths=c(12,12,12),heights=6)


#####generate w-lag#####
data_res<-read.csv("matlabdatanew1.CSV",header=T,fileEncoding='gbk')
famincnet_lag<-rep(0,27552)
for (i in 1:27552){
  if (i%%6!=1){
    famincnet_lag[i]<-log(data_res$faminc_net[i-1])
  } 
}
data_res<-cbind.data.frame(data_res,famincnet_lag)

famincnet_wlag<-rep(0,27552)
for (i in 1:6){
  dist_matrix <- read.csv(sprintf("matrix_eco%d.CSV",i))
  vector <- as.matrix(dist_matrix,nrow=4592) %*% as.matrix(log(data_res$faminc_net[data_res$year==i]),nrow=4592)
  for (j in 1:27552){
    if (j%%6==i){
      famincnet_wlag[j]<- vector[(j-i)%/%6+1]
    } 
  }
}
data_res<-cbind.data.frame(data_res,famincnet_wlag)

famincnet_wtlag<-rep(0,27552)
for (i in 1:6){
  dist_matrix <- read.csv(sprintf("matrix_eco%d.CSV",i))
  vector <- as.matrix(dist_matrix,nrow=4592) %*% as.matrix(data_res$famincnet_lag[data_res$year==i],nrow=4592)
  for (j in 1:27552){
    if (j%%6==i){
      famincnet_wtlag[j]<- vector[(j-i)%/%6+1]
    } 
  }
}
data_res<-cbind.data.frame(data_res,famincnet_wtlag)

sum_famincnet_lag<-rep(0,27552)
for (i in 1:27552){
  if (i%%6!=1){
    sum_famincnet_lag[i]<-log(data_res$sum_famincnet[i-1])
  } 
}
data_res<-cbind.data.frame(data_res,sum_famincnet_lag)

sum_famincnet_wlag<-rep(0,27552)
for (i in 1:6){
  dist_matrix <- read.csv(sprintf("matrix_eco%d.CSV",i))
  vector <- as.matrix(dist_matrix,nrow=4592) %*% as.matrix(log(data_res$sum_famincnet[data_res$year==i]),nrow=4592)
  for (j in 1:27552){
    if (j%%6==i){
      sum_famincnet_wlag[j]<- vector[(j-i)%/%6+1]
    } 
  }
}
data_res<-cbind.data.frame(data_res,sum_famincnet_wlag)

sum_famincnet_wtlag<-rep(0,27552)
for (i in 1:6){
  dist_matrix <- read.csv(sprintf("matrix_eco%d.CSV",i))
  vector <- as.matrix(dist_matrix,nrow=4592) %*% as.matrix(data_res$sum_famincnet_lag[data_res$year==i],nrow=4592)
  for (j in 1:27552){
    if (j%%6==i){
      sum_famincnet_wtlag[j]<- vector[(j-i)%/%6+1]
    } 
  }
}
data_res<-cbind.data.frame(data_res,sum_famincnet_wtlag)
######finc#####
finc_lag<-rep(0,27552)
for (i in 1:27552){
  if (i%%6!=1){
    finc_lag[i]<-log(data_res$finc[i-1]+1)
  } 
}
data_res<-cbind.data.frame(data_res,finc_lag)

finc_wlag<-rep(0,27552)
for (i in 1:6){
  dist_matrix <- read.csv(sprintf("matrix_eco%d.CSV",i))
  vector <- as.matrix(dist_matrix,nrow=4592) %*% as.matrix(log(data_res$finc[data_res$year==i]+1),nrow=4592)
  for (j in 1:27552){
    if (j%%6==i){
      finc_wlag[j]<- vector[(j-i)%/%6+1]
    } 
  }
}
data_res<-cbind.data.frame(data_res,finc_wlag)

finc_wtlag<-rep(0,27552)
for (i in 1:6){
  dist_matrix <- read.csv(sprintf("matrix_eco%d.CSV",i))
  vector <- as.matrix(dist_matrix,nrow=4592) %*% as.matrix(data_res$finc_lag[data_res$year==i],nrow=4592)
  for (j in 1:27552){
    if (j%%6==i){
      finc_wtlag[j]<- vector[(j-i)%/%6+1]
    } 
  }
}
data_res<-cbind.data.frame(data_res,finc_wtlag)

sum_finc_lag<-rep(0,27552)
for (i in 1:27552){
  if (i%%6!=1){
    sum_finc_lag[i]<-log(data_res$sum_finc[i-1])
  } 
}
data_res<-cbind.data.frame(data_res,sum_finc_lag)

sum_finc_wlag<-rep(0,27552)
for (i in 1:6){
  dist_matrix <- read.csv(sprintf("matrix_eco%d.CSV",i))
  vector <- as.matrix(dist_matrix,nrow=4592) %*% as.matrix(log(data_res$sum_finc[data_res$year==i]+1),nrow=4592)
  for (j in 1:27552){
    if (j%%6==i){
      sum_finc_wlag[j]<- vector[(j-i)%/%6+1]
    } 
  }
}
data_res<-cbind.data.frame(data_res,sum_finc_wlag)

sum_finc_wtlag<-rep(0,27552)
for (i in 1:6){
  dist_matrix <- read.csv(sprintf("matrix_eco%d.CSV",i))
  vector <- as.matrix(dist_matrix,nrow=4592) %*% as.matrix(data_res$sum_finc_lag[data_res$year==i],nrow=4592)
  for (j in 1:27552){
    if (j%%6==i){
      sum_finc_wtlag[j]<- vector[(j-i)%/%6+1]
    } 
  }
}
data_res<-cbind.data.frame(data_res,sum_finc_wtlag)
#####fproperty######
fproperty_lag<-rep(0,27552)
for (i in 1:27552){
  if (i%%6!=1){
    fproperty_lag[i]<-log(data_res$fproperty[i-1]+1)
  } 
}
data_res<-cbind.data.frame(data_res,fproperty_lag)

fproperty_wlag<-rep(0,27552)
for (i in 1:6){
  dist_matrix <- read.csv(sprintf("matrix_eco%d.CSV",i))
  vector <- as.matrix(dist_matrix,nrow=4592) %*% as.matrix(log(data_res$fproperty[data_res$year==i]+1),nrow=4592)
  for (j in 1:27552){
    if (j%%6==i){
      fproperty_wlag[j]<- vector[(j-i)%/%6+1]
    } 
  }
}
data_res<-cbind.data.frame(data_res,fproperty_wlag)

fproperty_wtlag<-rep(0,27552)
for (i in 1:6){
  dist_matrix <- read.csv(sprintf("matrix_eco%d.CSV",i))
  vector <- as.matrix(dist_matrix,nrow=4592) %*% as.matrix(data_res$fproperty_lag[data_res$year==i],nrow=4592)
  for (j in 1:27552){
    if (j%%6==i){
      fproperty_wtlag[j]<- vector[(j-i)%/%6+1]
    } 
  }
}
data_res<-cbind.data.frame(data_res,fproperty_wtlag)

sum_fproperty_lag<-rep(0,27552)
for (i in 1:27552){
  if (i%%6!=1){
    sum_fproperty_lag[i]<-data_res$sum_fproperty[i-1]
  } 
}
data_res<-cbind.data.frame(data_res,sum_fproperty_lag)

#####rqpd#####
library('rqpd')
data_res<-read.csv("matlabdatanew1.CSV",header=T,fileEncoding='gbk')

data_restore <- data_res
#data_res <- data_restore
data_res <- data_res[which(data_res$year>1),]
s<-as.factor(rep(1:4592,rep(5,4592)))
fit1<-rqpd(log(data_res$faminc_net)~log(data_res$index_aggregate)+log(data_res$trco+1)+
             data_res$familysize+data_res$company+log(data_res$bank_debit+1)+
             log(data_res$saving+1)+log(data_res$marketvalue+1)+data_res$job+data_res$phone+data_res$party+
             data_res$child_ratio+data_res$old_ratio+
             data_res$edu1+data_res$edu2+data_res$edu3+data_res$edu4+data_res$edu5+data_res$edu6+
             data_res$hea1+data_res$hea2+data_res$hea3+data_res$hea4+data_res$hea5+
             data_res$gender+data_res$age+data_res$marry+data_res$network+
             data_res$sum_famincnet+data_res$sum_famincnet_lag+
             log(data_res$famincnet_lag+1)+data_res$famincnet_wlag+data_res$famincnet_wtlag| s,
           panel(taus = c(0.9),tauw = rep(1,1)))
summary.rqpd(fit1)

fit1<-rqpd(log(data_res$faminc_net)~log(data_res$coverage_breadth)+log(data_res$trco+1)+
             data_res$familysize+data_res$company+log(data_res$bank_debit+1)+
             log(data_res$saving+1)+log(data_res$marketvalue+1)+data_res$job+data_res$phone+data_res$party+
             data_res$child_ratio+data_res$old_ratio+
             data_res$edu1+data_res$edu2+data_res$edu3+data_res$edu4+data_res$edu5+data_res$edu6+
             data_res$hea1+data_res$hea2+data_res$hea3+data_res$hea4+data_res$hea5+
             data_res$gender+data_res$age+data_res$marry+data_res$network+
             data_res$sum_famincnet+data_res$sum_famincnet_lag+
             log(data_res$famincnet_lag+1)+data_res$famincnet_wlag+data_res$famincnet_wtlag| s,
           panel(taus = c(0.9),tauw = rep(1,1)))
summary.rqpd(fit1)

fit1<-rqpd(log(data_res$faminc_net)~log(data_res$usage_depth)+log(data_res$trco+1)+
             data_res$familysize+data_res$company+log(data_res$bank_debit+1)+
             log(data_res$saving+1)+log(data_res$marketvalue+1)+data_res$job+data_res$phone+data_res$party+
             data_res$child_ratio+data_res$old_ratio+
             data_res$edu1+data_res$edu2+data_res$edu3+data_res$edu4+data_res$edu5+data_res$edu6+
             data_res$hea1+data_res$hea2+data_res$hea3+data_res$hea4+data_res$hea5+
             data_res$gender+data_res$age+data_res$marry+data_res$network+
             data_res$sum_famincnet+data_res$sum_famincnet_lag+
             log(data_res$famincnet_lag+1)+data_res$famincnet_wlag+data_res$famincnet_wtlag| s,
           panel(taus = c(0.9),tauw = rep(1,1)))
summary.rqpd(fit1)

fit1<-rqpd(log(data_res$faminc_net)~log(data_res$digitization_level)+log(data_res$trco+1)+
             data_res$familysize+data_res$company+log(data_res$bank_debit+1)+
             log(data_res$saving+1)+log(data_res$marketvalue+1)+data_res$job+data_res$phone+data_res$party+
             data_res$child_ratio+data_res$old_ratio+
             data_res$edu1+data_res$edu2+data_res$edu3+data_res$edu4+data_res$edu5+data_res$edu6+
             data_res$hea1+data_res$hea2+data_res$hea3+data_res$hea4+data_res$hea5+
             data_res$gender+data_res$age+data_res$marry+data_res$network+
             data_res$sum_famincnet+data_res$sum_famincnet_lag+
             log(data_res$famincnet_lag+1)+data_res$famincnet_wlag+data_res$famincnet_wtlag| s,
           panel(taus = c(0.9),tauw = rep(1,1)))
summary.rqpd(fit1)


######data3areas#####
library(dplyr)
data_res<-read.csv("matlabdatanew1.CSV",header=T,fileEncoding='gbk')
data_finance<-read.table("data_finance.csv",sep=',',header=T,fileEncoding = "gbk")

vec_east<-c(11,12,13,31,32,33,35,37,44,46)
data_plot<-subset.data.frame(data_finance, provcd %in% vec_east,select = c("index_aggregate","coverage_breadth","usage_depth","digitization_level","prov_name_eng","provcd","year"))
colnames(data_plot)<-c("index_aggregate","coverage_breadth","usage_depth","digitization_level","prov_name","provcd","year")
library(ggplot2)
ggplot(data_plot, aes(x = year, y = index_aggregate, group = prov_name))+
  geom_line(aes(lty=prov_name))+
  geom_point(aes(shape=prov_name))+
  scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10))+
  scale_x_continuous(breaks=data_plot$year, labels = data_plot$year)+
  labs(x="year",y="value",title="DFI of eastern area") + 
  theme_bw() + theme(panel.grid=element_blank())+
  theme(plot.title = element_text(size=18,hjust=0.5,family = "serif"))+
  theme(axis.text = element_text(size=15,family = "serif"))+
  theme(axis.title = element_text(size=18,family = "serif"))+
  theme(text = element_text(size=15,family = "serif"))

vec_east<-c(1,2,3,9,10,11,13,15,19,21)
data_east<-subset.data.frame(data_res, provcd %in% vec_east,select = c('fid','cid','provcd','countyid',
                                                                       'faminc_net','indinc_net','finc','fproperty',
                                                                       'trco','company','familysize',
                                                                       'bank_debit','fam_debit','peo_debit','debit_other',
                                                                       'saving','marketvalue','children','old',
                                                                       'code','gender','age','marry','edulevel','eduyear','network','health',
                                                                       'job','phone','party','year','dist_hz','child_ratio','old_ratio',
                                                                       'index_aggregate','coverage_breadth','usage_depth',
                                                                       'digitization_level','insurance','credit','payment',
                                                                       'sum_famincnet','sum_finc','sum_fproperty',
                                                                       'sum_famincnet_lag','sum_finc_lag','sum_fproperty_lag',
                                                                       'famincnet_lag','finc_lag','fproperty_lag'))
l1<-count(data_east,fid)
l2 <- l1[(l1$n==6),]
vec_fid<-l2$fid
data_east<-subset.data.frame(data_east, fid %in% vec_fid,select = c('fid','cid','provcd','countyid',
                                                                    'faminc_net','indinc_net','finc','fproperty',
                                                                    'trco','company','familysize',
                                                                    'bank_debit','fam_debit','peo_debit','debit_other',
                                                                    'saving','marketvalue','children','old',
                                                                    'code','gender','age','marry','edulevel','eduyear','network','health',
                                                                    'job','phone','party','year','dist_hz','child_ratio','old_ratio',
                                                                    'index_aggregate','coverage_breadth','usage_depth',
                                                                    'digitization_level','insurance','credit','payment',
                                                                    'sum_famincnet','sum_finc','sum_fproperty',
                                                                    'sum_famincnet_lag','sum_finc_lag','sum_fproperty_lag',
                                                                    'famincnet_lag','finc_lag','fproperty_lag'))
write.csv(data_east,"eastdata.CSV",row.names=F,fileEncoding='gbk')

data_matrix_eco<-read.csv("国内人均GDP.CSV",header=FALSE)
data_east<-read.csv("eastdata.CSV",header=TRUE,fileEncoding='gbk')
num <- nrow(data_east)/6
for (t in 2:6){
  data_east1<-data_east[which(data_east$year==t),]
  dist_east_eco<-matrix(0,nrow = num,ncol = num)
  for (i in 1:(num-1)){
    for (j in (i+1):num){
      if (data_east1$provcd[i]!=data_east1$provcd[j]){
        dist_east_eco[i,j]=1/abs(data_matrix_eco[t+1,(data_east1$provcd[i]+1)]-data_matrix_eco[t+1,(data_east1$provcd[j]+1)])
      }
      else{
        dist_east_eco[i,j]=0
      }
    }
  }
  dist_east_eco<-dist_east_eco+t(dist_east_eco)
  write.csv(dist_east_eco,sprintf("matrixeast_eco%d.CSV",t),row.names=F)
}

vec_medium<-c(14,34,36,41,42,43,15,45,50,51,52,53,54,61,62,63,64,65)
data_plot<-subset.data.frame(data_finance, provcd %in% vec_medium,select = c("index_aggregate","coverage_breadth","usage_depth","digitization_level","prov_name_eng","provcd","year"))
colnames(data_plot)<-c("index_aggregate","coverage_breadth","usage_depth","digitization_level","prov_name","provcd","year")
library(ggplot2)
ggplot(data_plot, aes(x = year, y = index_aggregate, group = prov_name))+
  geom_line(aes(lty=prov_name))+
  geom_point(aes(shape=prov_name))+
  scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18))+
  scale_x_continuous(breaks=data_plot$year, labels = data_plot$year)+
  labs(x="year",y="value",title="DFI of central and western") + 
  theme_bw() + theme(panel.grid=element_blank())+
  theme(plot.title = element_text(size=18,hjust=0.5,family = "serif"))+
  theme(axis.text = element_text(size=15,family = "serif"))+
  theme(axis.title = element_text(size=18,family = "serif"))+
  theme(text = element_text(size=15,family = "serif"))

vec_medium<-c(4,12,16,14,17,18)
data_medium<-subset.data.frame(data_res, provcd %in% vec_medium,select = c('fid','cid','provcd','countyid',
                                                                           'faminc_net','indinc_net','finc','fproperty',
                                                                           'trco','company','familysize',
                                                                           'bank_debit','fam_debit','peo_debit','debit_other',
                                                                           'saving','marketvalue','children','old',
                                                                           'code','gender','age','marry','edulevel','eduyear','network','health',
                                                                           'job','phone','party','year','dist_hz','child_ratio','old_ratio',
                                                                           'index_aggregate','coverage_breadth','usage_depth',
                                                                           'digitization_level','insurance','credit','payment',
                                                                           'sum_famincnet','sum_finc','sum_fproperty',
                                                                           'sum_famincnet_lag','sum_finc_lag','sum_fproperty_lag',
                                                                           'famincnet_lag','finc_lag','fproperty_lag'))
l1<-count(data_medium,fid)
l2 <- l1[(l1$n==6),]
vec_fid<-l2$fid
data_medium<-subset.data.frame(data_medium, fid %in% vec_fid,select = c('fid','cid','provcd','countyid',
                                                                        'faminc_net','indinc_net','finc','fproperty',
                                                                        'trco','company','familysize',
                                                                        'bank_debit','fam_debit','peo_debit','debit_other',
                                                                        'saving','marketvalue','children','old',
                                                                        'code','gender','age','marry','edulevel','eduyear','network','health',
                                                                        'job','phone','party','year','dist_hz','child_ratio','old_ratio',
                                                                        'index_aggregate','coverage_breadth','usage_depth',
                                                                        'digitization_level','insurance','credit','payment',
                                                                        'sum_famincnet','sum_finc','sum_fproperty',
                                                                        'sum_famincnet_lag','sum_finc_lag','sum_fproperty_lag',
                                                                        'famincnet_lag','finc_lag','fproperty_lag'))
write.csv(data_medium,"mediumdata.CSV",row.names=F,fileEncoding='gbk')

data_matrix_eco<-read.csv("国内人均GDP.CSV",header=FALSE)
data_medium<-read.csv("mediumdata.CSV",header=TRUE,fileEncoding='gbk')
num <- nrow(data_medium)/6
for (t in 2:6){
  data_medium1<-data_medium[which(data_medium$year==t),]
  dist_medium_eco<-matrix(0,nrow = num,ncol = num)
  for (i in 1:(num-1)){
    for (j in (i+1):num){
      if (data_medium1$provcd[i]!=data_medium1$provcd[j]){
        dist_medium_eco[i,j]=1/abs(data_matrix_eco[t+1,(data_medium1$provcd[i]+1)]-data_matrix_eco[t+1,(data_medium1$provcd[j]+1)])
      }
      else{
        dist_medium_eco[i,j]=0
      }
    }
  }
  dist_medium_eco<-dist_medium_eco+t(dist_medium_eco)
  write.csv(dist_medium_eco,sprintf("matrixmedium_eco%d.CSV",t),row.names=F)
}

vec_west<-c(15,45,50,51,52,53,54,61,62,63,64,65)
data_plot<-subset.data.frame(data_finance, provcd %in% vec_west,select = c("index_aggregate","coverage_breadth","usage_depth","digitization_level","prov_name_eng","provcd","year"))
colnames(data_plot)<-c("index_aggregate","coverage_breadth","usage_depth","digitization_level","prov_name","provcd","year")
library(ggplot2)
ggplot(data_plot, aes(x = year, y = digitization_level, group = prov_name))+
  geom_line(aes(lty=prov_name))+
  geom_point(aes(shape=prov_name))+
  scale_shape_manual(values = c(1,2,3,4,5,6,7,8,9,10,11,12))+
  scale_x_continuous(breaks=data_plot$year, labels = data_plot$year)+
  labs(x="year",y="value",title="digitization_level sub-index of western area") + 
  theme_bw() + theme(panel.grid=element_blank())+
  theme(plot.title = element_text(size=18,hjust=0.5,family = "serif"))+
  theme(axis.text = element_text(size=15,family = "serif"))+
  theme(axis.title = element_text(size=18,family = "serif"))+
  theme(text = element_text(size=15,family = "serif"))

vec_west<-c(5,20,22,23,24,25,26,27,28,29,30,31)
data_west<-subset.data.frame(data_res, provcd %in% vec_west,select = c('fid','cid','provcd','countyid',
                                                                       'faminc_net','indinc_net','finc','fproperty',
                                                                       'trco','company','familysize',
                                                                       'bank_debit','fam_debit','peo_debit','debit_other',
                                                                       'saving','marketvalue','children','old',
                                                                       'code','gender','age','marry','edulevel','eduyear','network','health',
                                                                       'job','phone','party','year','dist_hz','child_ratio','old_ratio',
                                                                       'index_aggregate','coverage_breadth','usage_depth',
                                                                       'digitization_level','insurance','credit','payment',
                                                                       'sum_famincnet','sum_finc','sum_fproperty',
                                                                       'sum_famincnet_lag','sum_finc_lag','sum_fproperty_lag',
                                                                       'famincnet_lag','finc_lag','fproperty_lag'))
l1<-count(data_west,fid)
l2 <- l1[(l1$n==6),]
vec_fid<-l2$fid
data_west<-subset.data.frame(data_west, fid %in% vec_fid,select = c('fid','cid','provcd','countyid',
                                                                    'faminc_net','indinc_net','finc','fproperty',
                                                                    'trco','company','familysize',
                                                                    'bank_debit','fam_debit','peo_debit','debit_other',
                                                                    'saving','marketvalue','children','old',
                                                                    'code','gender','age','marry','edulevel','eduyear','network','health',
                                                                    'job','phone','party','year','dist_hz','child_ratio','old_ratio',
                                                                    'index_aggregate','coverage_breadth','usage_depth',
                                                                    'digitization_level','insurance','credit','payment',
                                                                    'sum_famincnet','sum_finc','sum_fproperty',
                                                                    'sum_famincnet_lag','sum_finc_lag','sum_fproperty_lag',
                                                                    'famincnet_lag','finc_lag','fproperty_lag'))
write.csv(data_west,"westdata.CSV",row.names=F,fileEncoding='gbk')

data_matrix_eco<-read.csv("国内人均GDP.CSV",header=FALSE)
data_west<-read.csv("mediumdata.CSV",header=TRUE,fileEncoding='gbk')
num <- nrow(data_west)/6
for (t in 2:6){
  data_west1<-data_west[which(data_west$year==t),]
  dist_west_eco<-matrix(0,nrow = num,ncol = num)
  for (i in 1:(num-1)){
    for (j in (i+1):num){
      if (data_west1$provcd[i]!=data_west1$provcd[j]){
        dist_west_eco[i,j]=1/abs(data_matrix_eco[t+1,(data_west1$provcd[i]+1)]-data_matrix_eco[t+1,(data_west1$provcd[j]+1)])
      }
      else{
        dist_west_eco[i,j]=0
      }
    }
  }
  dist_west_eco<-dist_west_eco+t(dist_west_eco)
  write.csv(dist_west_eco,sprintf("matrixwest_eco%d.CSV",t),row.names=F)
}