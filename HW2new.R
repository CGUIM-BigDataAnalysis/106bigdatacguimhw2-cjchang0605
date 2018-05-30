library(readr)
library(ggplot2)
library(xlsx)
library(readxl)
library(dplyr)
library(readODS)
library(ggplot2)

come103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_C.csv")
school103<-read_csv("http://stats.moe.gov.tw/files/detail/103/103_ab103_S.csv")
come104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_C.csv")
school104<-read_csv("http://stats.moe.gov.tw/files/detail/104/104_ab104_S.csv")
come105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_C.csv")
school105<-read_csv("http://stats.moe.gov.tw/files/detail/105/105_ab105_S.csv")
come106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_C.csv")
school106<-read_csv("http://stats.moe.gov.tw/files/detail/106/106_ab105_S.csv")
Ex<-read_excel("C:/Users/user/Desktop/Student_RPT_07.xlsx",skip = 1)
aboard<-read_csv("https://ws.moe.edu.tw/Download.ashx?u=C099358C81D4876CC7586B178A6BD6D5062C39FB76BDE7EC7685C1A3C0846BCDD2B4F4C2FE907C3E7E96F97D24487065577A728C59D4D9A4ECDFF432EA5A114C8B01E4AFECC637696DE4DAECA03BB417&n=4E402A02CE6F0B6C1B3C7E89FDA1FAD0B5DDFA6F3DA74E2DA06AE927F09433CFBC07A1910C169A1845D8EB78BD7D60D7414F74617F2A6B71DC86D17C9DA3781394EF5794EEA7363C&icon=..csv")


ForeignC<-merge(come103,come104,by="國別",all=T)
ForeignC2<-merge(come105,come106,by="國別",all=T)
ForeignT<-merge(ForeignC,ForeignC2,by="國別",all=T)
ForeignT<-ForeignT[,!grepl("洲",ForeignT)]
ForeignT$total<-rowSums(ForeignT[,-1],na.rm = T)

head(ForeignT[order(ForeignT$total,decreasing = T),c(1,38)],10)

ggplot(data=ForeignT,
       aes(x=國別,y=total))+
  theme_bw(base_size = 8) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  geom_bar(stat="identity") 



ForeignSchool<-merge(school103[,c(-1,-2)],school104[,c(-1,-2)],by="學校名稱",all=T)
ForeignSchool2<-merge(school105[,c(-1,-2)],school106[,c(-1,-2)],by="學校名稱",all=T)
TS<-merge(ForeignSchool,ForeignSchool2,by="學校名稱",all=T)
for (i in 2:37){
  for (j in 1:165) {
    if (grepl("…",TS[j,i])){
      TS[j,i]<-""
      TS[j,i]<-as.numeric(TS[j,i])
    }else{
      TS[j,i]<-as.numeric(TS[j,i])
    }
  }
}
TS<-TS[,c(-8,-17)]
TS$total<-rowSums(TS[,-1],na.rm = T)
head(TS[order(TS$total,decreasing = T),c(1,36)],10)


gE<-group_by(Ex,`對方學校(機構)國別(地區)`)%>%
  summarise(export=sum(小計))%>%
  arrange(desc(export))
  head(gE,10)

gS<-group_by(Ex,學校名稱)%>%
  summarise(total=sum(小計))%>%
  arrange(desc(total))
  
head(gS,10)

ggplot(data=gE,
       aes(x=`對方學校(機構)國別(地區)`,y=export))+
  theme_bw(base_size = 8) +
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  geom_bar(stat="identity") 




SA<-aboard[,c(1:3)]%>%
  arrange(desc(總人數))
  head(SA,10)







