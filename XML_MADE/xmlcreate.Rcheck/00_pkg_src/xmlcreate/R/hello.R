# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

XML_Made <- function(num_n) {
  setwd("E:/CBD_2020 - 副本/")
  x<-"E:/CBD_2020 - 副本/"
  ###########
  basic_sbbm<-c(
    "510700000000010221",
    "510700000000010220",
    "510700000000010044",
    "510700000000090017",
    "510700000000090082",
    "510700000000010303",
    "510700000000090043",
    "510700000000010038",
    "510700000000010073",
    "510700000000010029",
    "510700000000010049",
    "510700000000010030",
    "510700000000010028",
    "510700000000010033",
    "510700000000010035",
    "510700000000010212",
    "510700000000010047",
    "510700000000010034",
    "510700000000010216",
    "510700000000090018",
    "510700000000010048",
    "510700000000010074",
    "510700000000090023",
    "510700000000010304",
    "510700000000090047",
    "510700000000010305",
    "510700000000020093",
    "510700000000010032",
    "510700000000090007",
    "510700000000010065",
    "510700000000010075",
    "510700000000090032",
    "510700000000010222",
    "510700000000010215",
    "510700000000010261",
    "510700000000090045",
    "510700000000010037",
    "510700000000010218",
    "510700000000010302",
    "510700000000010214",
    "510700000000010026",
    "510700000000010079",
    "510700000000010050",
    "510700000000010066",
    "510700000000010219",
    "510700000000090049",
    "510700000000010027",
    "510700000000010064",
    "510700000000010036",
    "510700000000010301",
    "510700000000010078",
    "510700000000010213",
    "510700000000090050",
    "510700000000010039",
    "510700000000010046",
    "510700000000010506",
    "510700000000010306",
    "510700000000090044",
    "510700000000010031",
    "510700000000010045",
    "510700000000010260"
  )#附件区
  SBBM_basic<-c(
    "510700000000090047",
    "510700000000010219",
    "510700000000010220",
    "510700000000010221",
    "510700000000010222",
    "510700000000010036",
    "510700000000010037",
    "510700000000010038",
    "510700000000010039",
    "510700000000010044",
    "510700000000010045",
    "510700000000010046",
    "510700000000010215",
    "510700000000010216",
    "510700000000090018",
    "510700000000010032",
    "510700000000010033",
    "510700000000010034",
    "510700000000010035",
    "510700000000010301",
    "510700000000010302",
    "510700000000010213",
    "510700000000010214",
    "510700000000010212",
    "510700000000090032",
    "510700000000090050",
    "510700000000020093",
    "510700000000010026",
    "510700000000010027",
    "510700000000010028",
    "510700000000010029",
    "510700000000010030",
    "510700000000010031",
    "510700000000090049",
    "510700000000010506",
    "510700000000010066",
    "510700000000010065",
    "510700000000010073",
    "510700000000010075",
    "510700000000010074",
    "510700000000010261",
    "510700000000090007",
    "510700000000090023",
    "510700000000090017",
    "510700000000010260",
    "510700000000090082",
    "510700000000010303",
    "510700000000010049",
    "510700000000010047",
    "510700000000010048",
    "510700000000010304",
    "510700000000010305",
    "510700000000010218",
    "510700000000010079",
    "510700000000010050",
    "510700000000010064",
    "510700000000010078",
    "510700000000010306"

  )
  ID_basic<-c(
    "-481010578",
    "347939400#2",
    "347939400#2",
    "347939389#3",
    "347939389#3",
    "481053987#5",
    "733268547#5",
    "481010589#3",
    "347936973#8",
    "481053989#2",
    "180158760#6",
    "-180158760#4",
    "180158731#0",
    "347939372",
    "301032551#1",
    "539280438#2",
    "347939389#3",
    "539280500#3",
    "347939389#3",
    "347939389#3",
    "347939389#3",
    "481010583#0",
    "539280438#5",
    "481010582#0",
    "306041292#1",
    "-220376884#4",
    "-301187289#3",
    "347939400#4",
    "347939400#4",
    "347939389#3",
    "347939389#3",
    "180158760#3",
    "-180158760#1",
    "-220376884#3",
    "492135840#4",
    "180158760#0",
    "481010600#3",
    "468314341#4",
    "180162609#2",
    "492135850#6",
    "481053989#2",
    "733268544#3",
    "347936973#8",
    "481053988#2",
    "156821042",
    "481010589#0",
    "-347936966#1",
    "481010589#8",
    "301187290#4",
    "-301187290#2",
    "-347936966#1",
    "-347936966#1",
    "180158731#0",
    "156819333#3",
    "347936973#2",
    "481010600#3",
    "539280492#6",
    "-347936966#1"
  )
  ####################
  yeartime<-function(x){
    #x<-11#月份
    dayN<-c(31,28,31,30,31,30,31,31,30,31,30,31)
    dayNum<-c(1,32,60,91,121,152,182,213,244,274,305,335)
    dayS<-c("2019-01-","2019-02-","2019-03-","2018-04-","2019-05-","2019-06-"
            ,"2019-07-","2019-08-","2019-09-","2018-10-","2019-11-","2019-12-")
    NUM<-c("01","02","03","04","05","06","07","08","09")
    year<-c()
    for(i in 1:12)
    {
      for(j in 1:dayN[i])
      {
        if(j<10)
        {
          j<-NUM[j]
        }
        day_1<-paste0(dayS[i],j)
        year<-c(year,day_1)
      }
    }
    year<-year[dayNum[x]:(dayNum[x+1]-1)]
    return(year)#输出为一个数组
    #样本：[1] "2019-11-01" "2019-11-02" "2019-11-03"
  }#时间函数构建文件名格式化提取
  TrajGenerate<-function(x,y,z,e,f,year){
    library(dplyr)
    library(tidyr)
    # x<-1#日期
    # y<-"08"#小时
    # z<-1800#分隔时间数
    # e<-35#分隔估计长度上限
    # f<-1#分隔估计长度下限
    # function(x,y,z,e,f)
    # load(paste0("2019_day/","2019-11-01",".rdata"))
    load(paste0("2019_day/",year[x],".rdata"))
    #输入dataframe格式:sbbm chr---ID chr---time chr---ymd chr---h chr
    #数据样本:510700000000090048 川YQ6388 2019-11-01 08:06:06 2019-11-01 08
    track3<-filter(track3,h ==y)
    cars<-track3
    basic_sbbm<-data.frame(basic_sbbm)
    names(basic_sbbm)<-c('sbbm')
    basic_sbbm$sbbm<-as.character(basic_sbbm$sbbm)
    basic_sbbm<-data.frame(unique(basic_sbbm))
    carss<-merge(basic_sbbm,cars,by= "sbbm", all = F)
    cars<-carss
    cars<-filter(cars,ID!="污损车牌")
    cars<-filter(cars,ID!="污损号牌")
    cars<-filter(cars,ID!="污损牌照")
    cars<-filter(cars,ID!="未识别")
    g<-group_by(cars,cars$ID)
    index<-group_indices(g,ID)
    df<-data.frame(index)
    cars<-cbind(cars,df)
    cars<-cars[order(cars$index,as.POSIXlt(cars$time) ),]
    diff_time<-diff(as.POSIXlt(cars$time))
    diff_times<-data.frame(diff_time)
    add<-data.frame(diff_time=c(0))
    diff_times<-rbind(diff_times,add)
    cars<-cbind(cars,diff_times)
    next_car<-data.frame(cars$ID)
    next_car<-data.frame(nextcar=next_car[-1,])
    lastcar<-data.frame(nextcar=c("00000"))
    next_car<-data.frame(rbind(next_car,lastcar))
    next_sbbm<-data.frame(cars$sbbm)
    next_sbbm<-data.frame(nextsbbm=next_sbbm[-1,])
    lastsbbm<-data.frame(nextsbbm=c("00000"))
    next_sbbm<-data.frame(rbind(next_sbbm,lastsbbm))
    cars<-cbind(cars,next_car,next_sbbm)
    cars$index <-as.character(cars$index )
    cars$ID <-as.character(cars$ID )
    cars$nextcar <-as.character(cars$nextcar )
    cars$nextsbbm <-as.character(cars$nextsbbm )
    cars <- cars[which( cars$sbbm!=cars$nextsbbm),]
    f1<-c(cars$diff_time)
    #参数z
    f1[which(f1<z)] <-0
    f1[which(f1>=z)] <-1
    f1<-data.frame(f1)
    segment<-f1[,c('f1')]
    segment<-as.numeric(segment)
    j<-0
    cc<-NULL
    c<-NULL
    for( i in 1: length(segment))
    {
      c<-c(c,j)
      if(segment[i]==1)
      {
        j<-j+1
      }
      if(i%%10000==0)
      {
        cc<-c(cc,c)
        c<-NULL
        # print(i)
      }

    }
    cc<-c(cc,c)
    cc<-data.frame(cc)
    cars<-cbind(cars,cc)
    cars<-unite(cars,"index_cc",c("index","cc"), sep="-", remove = F)
    g_car<-group_by(cars,index_cc)
    g_car<-summarise(g_car,count=n())
    g_car[1:100,]
    plot(sort(g_car$count))
    #参数e和f
    # g_car <- g_car[-which(g_car$count>e),]}
    g_car <- g_car[-which(g_car$count<=f),]
    track<-merge(cars,g_car,by = "index_cc", all = FALSE)
    track<-filter(track,diff_time!=0)
    track$abstime<-unclass(as.POSIXct(track$time))
    g_OD<-group_by(track,index_cc)
    g_OD_start<-summarise(g_OD,abstime=min(abstime))
    g_OD_end<-summarise(g_OD,abstime=max(abstime))
    g_OD_time<-rbind(g_OD_start,g_OD_end)
    OD_order_1<-g_OD_time[order(g_OD_time$index_cc,g_OD_time$abstime ),]
    OD_order_2<-g_OD_time[order(g_OD_time$index_cc,-g_OD_time$abstime ),]
    OD<-cbind(OD_order_1,OD_order_2)
    OD<-OD[,-c(3)]
    names(OD)<-c('index_cc','t1','t2')
    OD<-with(OD,OD[t2>t1,])
    OD_and_track<-merge(track,OD,by= 'index_cc')
    tmp<-filter(OD_and_track,abstime==t1 | abstime ==t2)
    tmp_s<-tmp[order(tmp$index_cc,tmp$abstime ),]
    tmp_e<-tmp[order(tmp$index_cc,-tmp$abstime ),]
    tmp_s<-tmp_s[,c("index_cc","sbbm","abstime"),]
    tmp_e<-tmp_e[,c("index_cc","sbbm","abstime"),]
    tmp<-cbind(tmp_s,tmp_e)
    names(tmp)<-c('index_s','sbbm_s','abstime_s','index_e','sbbm_e','abstime_e')
    tmp<-filter(tmp,abstime_e>abstime_s)
    tmp<-tmp[,c('index_s','sbbm_s','sbbm_e')]
    names(tmp)<-c('index_cc','sbbm_s','sbbm_e')
    OD_and_track<-merge(tmp,OD_and_track,by= 'index_cc', all = F)
    OD_and_track<-filter(OD_and_track,OD_and_track$sbbm_s!=OD_and_track$sbbm_e)
    OD_and_track$total_time<-OD_and_track$t2-OD_and_track$t1
    OD_and_track<-filter(OD_and_track,OD_and_track$total_time<6000 & OD_and_track$total_time>1000)
    test<-OD_and_track
    ######################
    test = test[,c(1,2,3,4,5,6)]
    colnames(test)[1] = 'index'
    colnames(test)[2] = 'SBBM_S'
    colnames(test)[3] = 'SBBM_E'
    colnames(test)[4] = 'SBBM'
    return(test)
    #输出dataframe格式:index chr---SBBM_S chr---SBBM_E chr---SBBM chr---ID chr---time chr
    #数据样本:100-13 510700000000010074 510700000000010036 510700000000010036 川A24BY5 2019-11-01 08:56:04
  }#first和second的轨迹提取办法 必要要先跑yeartime_2019函数
  new<-c()
  for(i in 1:num_n){#i=1
    test<-TrajGenerate(i,"08",1800,35,1,yeartime(11))
    track2 <- test[order(test$ID,test$time),]
    options(scipen=200)
    basic_sbbm<-data.frame(basic_sbbm)
    names(basic_sbbm)<-c('SBBM')
    basic_sbbm$SBBM<-as.character(basic_sbbm$SBBM)
    basic_sbbm<-data.frame(unique(basic_sbbm))
    PI<-data.frame(SBBM_basic)
    names(PI)<-c('SBBM')
    PI$SBBM<-as.character(PI$SBBM)
    PI<-data.frame(unique(PI))
    PI$ID<-as.character(ID_basic)
    ssss<-merge(PI,basic_sbbm,by="SBBM")
    names(ssss)[2]<-c('IDC')
    track2<-merge(track2,ssss,by="SBBM")
    names(ssss)[2]<-c('IDC_S')
    names(ssss)[1]<-c('SBBM_S')
    track3<-merge(track2,ssss,by="SBBM_S")
    names(ssss)[2]<-c('IDC_E')
    names(ssss)[1]<-c('SBBM_E')
    track3<-merge(track3,ssss,by="SBBM_E")
    track3$time <- as.numeric(as.POSIXct(track3$time))
    track3 <- track3[order(track3$index,track3$time),]
    time<-as.numeric(as.POSIXct("2019-11-01 08:00:00"))
    track4<-group_by(track3,index)
    track4<-summarise(track4,count=n())
    track2<-merge(track3,track4,by="index")
    track2 <- track2[,c(1,5,6,7,8,9,10)]
    track3 <- group_by(track2,index,time)
    track3 <- summarise(track3,count=n())
    track3 <-track3[order(track3$time),]
    track3<-track3[!duplicated(track3[,c(1)]),]
    track3$num<-1:nrow(track3)
    track3 <- track3[,c(1,4)]
    track3 <-merge(track3,track2,by="index")
    track3 <-track3[order(track3$num),]
    new<-rbind(new,track3)
  }
  ta<-group_by(new,index)
  ta<-summarise(ta,count=n())


  total<-c()
  total<-paste('<?xml version="1.0" encoding="ISO-8859-1"?>
  <!-- generated on 2020-04-08 20:16:22.227196 by randomTrips.py v1_4_0+0000-d7c9afb42e options: -n mianyang.net.xml -p 1000 -->
  <routes xsi:noNamespaceSchemaLocation="http://sumo.dlr.de/xsd/routes_file.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">
  ',total)

  j<-1
  track3<-new
  for(i in 1:nrow(ta)){
    # i<-1
    b<-track3$time[j]-time
    c<-track3$IDC_S[j]
    d<-track3$IDC_E[j]
    # indexn<-track2$index[j]
    # if(indexs == indexn){
    count=track3$count[j]-1
    # k<-1
    e<-c
    for( k in 1:count){
      e<-c(e,track3$IDC[j+k])
    }
    j=j+k+1
    # t<-paste("<trip id= ",i," depart=",b,"from= ",c," to=",d," via=",e," />",sep="'",collapse = ",")
    t<-paste('<trip id= ',i,' depart=',b,' from= ',c,' to=',d,' via="',sep='"')
    for(i in 1:length(e))
    {
      t <- paste(t,e[i])
    }
    t <- paste(t,'" />')

    total<-c(total,t,sep = "\n")
  }
  # total<-paste(total,"</routes>")
  total<-c(total,"</routes>")
  cat(total,file="viaxml.trips.txt")

}
# XML_Made(1)
