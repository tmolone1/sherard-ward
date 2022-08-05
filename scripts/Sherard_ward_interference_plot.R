library(tidyverse)
source('./scripts/well_fxn_script.R')
tshort<-seq(5,365,5)
tprime<-95
b<-60
T<-b*89.14
Q<-2000*192.5
r<-670
S<-0.175
new_vec<-vector(mode="numeric", length=length(tshort))
j<-1

#### one year, one distance
    #s<-(Q/(4*pi*T)) * (Wu - Wu2)
for (i in tshort) {
  if (i <= tprime) {
    u<-(r^2*S)/(4*T*i)
    new_vec[j] <- well_fxn(u,200)
    j<-j+1
}
        else {
          u1<-(r^2*S)/(4*T*i)
          u2<-(r^2*S)/(4*T*(i-tprime))
          Wu<-well_fxn(u1,200)
          Wu2<-well_fxn(u2,200)
          new_vec[j] <- Wu-Wu2
          j=j+1
        }
}

df<-tibble(tshort, new_vec) %>% mutate(s = (Q/(4*pi*T)) * new_vec, saturated_production_interval=b-((Q/(4*pi*T)) * new_vec))
names(df)<-c("time","Wu", 's', 'saturated_production_interval')   

#one year drawdown plot
ggplot(df, aes(x=time,y=saturated_production_interval)) + geom_point() + geom_line()


# multiple distances/times
r<-c(seq(100,500,100),670,1000,seq(1320,5280,1320))
r<-c(-r,0,r) %>% sort()
Qs<-c(180*192.5,550*192.5) # first ward, then sherard
tbl_times<-rep(tshort,length(r)*2) %>% sort()
tbl<-tibble(r=rep(r,length(unique(tshort))*2),s=999,i=tbl_times,Q=c(rep(Qs[1],length(tbl_times)/2),rep(Qs[2],length(tbl_times)/2)))

#i<-200


for (Q in Qs) {
for (i in tshort) {
  if (i <= tprime) {
    u<-(r^2*S)/(4*T*i)
    s = (Q/(4*pi*T)) * (list_well_fxn(u,200))
    result<-tibble(r,s,i,Q)
    tbl<-rows_upsert(tbl,result,by=c("r","i","Q"))
  }
  else {
    u1<-(r^2*S)/(4*T*i)
    u2<-(r^2*S)/(4*T*(i-tprime))
    Wu<-list_well_fxn(u1,200)
    Wu2<-list_well_fxn(u2,200)
    s = (Q/(4*pi*T)) * (Wu-Wu2)
    result<-tibble(r,s,i,Q)
    tbl<-rows_upsert(tbl,result,by=c("r","i","Q"))
  }
}
}

tbl<-tbl%>% filter(i==tprime)
Ward<-tbl%>% filter(Q==Qs[1])
Sherard<-tbl%>% filter(Q==Qs[2])
Ward$r<-Ward$r-670
Ward$well<-"ward"
Sherard$well<-"sherard"
plotme<-tibble(rbind(Sherard,Ward))


p<-ggplot(plotme, aes(x=r,y=-s, shape=well)) + geom_point() + geom_line(aes(color=well)) +
  ggtitle(paste0("Cones of depression after 95 days")) + 
  labs(subtitle=paste0("Bill Ward No. 2 Pumping at 180 GPM (31% overlap in cones of depression)"),
       caption=paste0("Sherard No. 6 Pumping at permitted flow rate")) + 
  xlab("Distance from Sherard No. 6 well (ft)") +
  ylab("Drawdown from static water level (ft)") +
  ylim(-12,0) + 
  theme_classic()
#  scale_x_continuous(trans = 'log')
p <- p + labs(color="Days since pumping began") 
p<- p + guides(color = guide_colorbar(reverse=TRUE))
p

png(filename = './outputs/output.png',width=800, height=400, units='px')
p
dev.off()
