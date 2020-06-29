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

made_son<- function(num_n) {
  setwd("E:/CBD_2020 - 副本/")
  # install.packages("sf")
  library("sf")
  library("dplyr")
  library("tidyr")
  library("plyr")
  library(stringr)
  #读取xml格式数据并解析
  num_n<-1
  xmlfile=xmlToList(xmlParse(file="mianyang.rou.xml"))
  xml=xmlParse(file="mianyang.rou.xml",encoding="UTF-8")
  xmltop = xmlRoot(xml)
  num_total<-xmlSize(xmltop)
  d<-data.frame(a=1:num_total)

  for(i in 1:num_total){
    # i<-1
    x<-c(xmlfile[i]$vehicle$route)
    x<-strsplit(x, " ", fixed=TRUE)
    y<-x$edges
    s<-as.data.frame(y)
    s$pipeb<-as.character(s$y)
    load(file="biao/pipei.rdata")
    s<-merge(s,pipe,by ="pipeb")
    ss<-nrow(s)
    if(ss !=1){
      e<-s$num[1]
      st<-st_point(c(e,0))
      num_x<-nrow(s)
      if(num_x != 1){
        for(j in 2:(num_x-1)){
          e<-s$num[j]
          t<-st_point(c(e,0))
          st<-rbind(st,t)
        }
      }
      ls <- st_linestring(st)
      d$time[i]<-as.numeric(xmlfile[i]$vehicle$.attrs[2])
      d$ODs[i]<-st[1,1]
      d$ODe[i]<-st[(length(st)/2),1]
      d$geom[i]<-st_sfc(ls)
    }
  }
  zk = st_as_sf(d)
  zk<-unite(zk,"OD",c("ODs","ODe"), sep="-", remove = F)
  testd<-group_by(zk,OD,time)
  testd<-summarise(testd,count=n())
  t<-filter(testd,count!=1)
  if(nrow(t)!=0){
    for (i in 1:nrow(t)) {
      zk<-filter(zk,OD!=t$OD[i])
    }
  }
  return(zk)
}
# made_son(1)
# library(Madeson)
