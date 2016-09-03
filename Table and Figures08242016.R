library(dlnm)
library(splines)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggthemes)
library(faraway)
library(gridExtra)
library(lubridate)
library(baidumap)
library(ggmap)
library(knitr)
library(reshape2)

##Writing a function to calculate the percent increase 
pi<-function(x){
  y<-round(100*(x-1),2)
  return(y)
}

data<-read.csv("0912bj.csv",head=T)
data$date<-as.Date(with(data,date,"%Y/%m/%d"))
data$o3[data$o3==-9999] <- NA 
data$o3[data$o3=="#DIV/0!"] <- NA
data$o3<-as.numeric(as.character(data$o3))##The factor class could not change to numeric directly,must use this expression.
class(data$o3)
summary(data$o3)


###generate the New predicted PM value
# Observed PM from Beijing (Non-embassy) monitor
data$pm_bj <- data$pm25hdbl
# Observed PM from Embassy monitor
data$pm_us <- data$pm25ussg

# predict pm_bj with pm_us
mod_bj <- lm(pm_bj ~ pm_us, na.action = na.exclude, data = data)
data$pre_bj <- predict(mod_bj, newdata = data)
data$pm_bj <- ifelse(is.na(data$pm_bj), data$pre_bj, data$pm_bj)
summary(data$pm_bj)
# After replacing NA with predicted values, it has only 15 NA.
summary(data$pm25hdbl)
# The observed data from non-Embassy monitor has 156 NA.

# predict pm_us with pm_bj
mod_us <- lm(pm_us ~ pm_bj, data = data)
data$pre_us <- predict(mod_us, newdata = data)
data$pm_us <- ifelse(is.na(data$pm_us), data$pre_us, data$pm_us)
summary(data$pm_us)
# After replacing NA with predicted values, it has only 15 NA.
summary(data$pm25ussg)
# The observed data from US Embassy has 254 NA.

# Get the average value of PM2.5 
data$ave_pm <- (data$pm_bj + data$pm_us)/2
summary(data$ave_pm)


# generate stratum,for year, month and dow
data$month <- as.factor(months(data$date))
data$year <- as.factor(format(data$date, format="%Y"))
data$dow <- as.factor(weekdays(data$date))
data$stratum <- as.factor(data$year:data$month:data$dow)
head(data)

# pm25ussg is used, based on the literature, the lag was selected of 5
cb1.pm25ussg <- crossbasis(data$ave_pm, lag=2, argvar=list(fun="lin",cen=0),
                           arglag=list(fun="ns",df=2))
cb1.tmean <- crossbasis(data$tmean, lag=7, argvar=list(fun="ns",df=5,cen=25),
                        arglag=list(fun="ns",df=3))


# select causes with over 500 deaths in total (some cause can't converge)
data_1 <- subset(data[,2:88])
cause.to.model <- subset(data_1)[colSums(data_1)>500]
names(cause.to.model)



##Descriptive Health Table
##groupde and specificed table
cause.table<-gather(cause.to.model,cause,count)
##changing the character "to" to "-", and all to a00toz99
cause.table$cause<-gsub("to","-",cause.table$cause)
cause.table$cause<-gsub("all","A00-Z99",cause.table$cause)
cause.table$cause<-gsub("I20I21I22I24","I20-I22,I24",cause.table$cause)
head(cause.table)
tail(cause.table)

###divided the ICD to two groups, gouped and specific
head(cause.table)
cause.table$type<-1
cause.table$type[cause.table$cause %in% c("A00-Z99","A00-R99","N00-N99","F00-F99","I00-I99","J00-J99","G00-G99","K00-K93","X60-X84")]<-0

cause.table$type<-factor(cause.table$type,levels=c(0,1),
                         labels=c("Grouped","Specific"))
head(cause.table)
health.table<-group_by(cause.table,type,cause)%>% 
  summarize(total = prettyNum(sum(count), big.mark = ","),
            mean_count = paste0(prettyNum(round(mean(count)),big.mark = ","),
                                " (", min(count), ", ",
                                max(count), ")"))

head(cause.table)

health.table<-group_by(cause.table,type,cause)%>% 
  summarize(total = prettyNum(sum(count), big.mark = ","),
            mean_count = paste0(prettyNum(round(mean(count)),big.mark = ","),
                                " (", min(count), ", ",
                                max(count), ")"))
health.table



health.table1<-group_by(cause.table,type,cause)%>% 
  summarize(tot=sum(count))

health.table1

health.table.new<-cbind(health.table,health.table1$tot)

colnames(health.table.new)<-c("type","cause","total","mean_count","tot")

health.table.new$tot<-as.numeric(health.table.new$tot)

mode(health.table.new$tot)

health.table.final<- group_by(health.table.new,type) %>%
  arrange(desc(tot))%>%
  ungroup()

health.table.final

health.table.final<-select(health.table.final,-tot)

kable(health.table.final,'markdown')
write.csv(health.table.final,file="health.table.csv")

##Plot of selected outcomes,Figures of health time series
names(data)
data.healthplot<-select(data,all,A00toR99,I00toI99,J00toJ99, K00toK93, G00toG99, N00toN99, X60toX84, F00toF99,date)
head(data.healthplot)
colnames(data.healthplot)<-c("A00-Z99","A00-R99","I00-I99","J00-J99", "K00-K93", "G00-G99", "N00-N99", "X60-X84", "F00-F99","date")
head(data.healthplot)



###using the facetting to do the plot
head(data.healthplot)
data.healthplot$day <- yday(data.healthplot$date) 
head(data.healthplot)
data.long<-gather(data.healthplot,cause,count,-date,-day)
head(data.long)

##plotting the counts~date
ggplot(data.long, aes(x=date, y=count))+
  geom_point(size=1)+
  ylab("Number of Deaths") +
  xlab("Date")+
  facet_wrap(~cause,nrow=9,scales="free_y")+
  theme_few()



##plotting the counts~day
ggplot(data.long, aes(x=day, y=count))+
  geom_point(size=1)+
  ylab("Number of Deaths") +
  xlab("Day")+
  facet_wrap(~cause,nrow=9,scales="free_y")+
  theme_few()



##Descriptive PM and Temperature table 
names(data)
environment<-select(data,pm25ussg,pm25hdbl,ave_pm,tmean,rh)
head(environment)
nrow(data)
environment.summary<-gather(environment,item,value)
head(environment.summary)

environment.table<-group_by(environment.summary,item) %>%
  summarize(median=round(median(value,na.rm=TRUE),1),
            mean = round(mean(value, na.rm = TRUE),1),
            range= paste0(round(min(value,na.rm=TRUE),1),",",round(max(value,na.rm=TRUE),1)),
            irange=paste0(round(quantile(value,probs=0.25,na.rm=TRUE),1),",",round(quantile(value,probs=0.75,na.rm=TRUE),1)),
            pmising=round(sum(is.na(value))/length(value)*100,1))


kable(environment.table,'markdown')
write.csv(environment.table,file="environment.table.csv")


##mapping
apstation1<-getCoordinate("海淀宝联公园",formatted=T)
apstation2<-getCoordinate("北京美国大使馆",formatted=T)
mestation<-getCoordinate("北京大兴区气象台",formatted=T)

map<-rbind(apstation1,apstation2,mestation)
map<-as.data.frame(map)
map$Station<-c("Air Pollution-Baolian","Air Pollution-US Embassy","Meteorology-Nanjiao")
head(map)

bjmap<-get_map("beijing",zoom=10,maptype="toner")

###making map,need to be more work 
ggmap(bjmap, extent = "device")+
  geom_point(aes(x=longtitude,y=latitude,color=Station),data=map,size=6)

##calculte the distance 
mapdist(apstation1,apstation2)
mapdist(apstation1,mestation)
mapdist(apstation2,mestation)

##correlation and scatter plot the PM2.5 between two air pollution station
cor(data$pm25ussg,data$pm25hdbl,use="complete.obs")

##scatter plot 
ggplot(data, aes(x = pm25hdbl, y = pm25ussg))+
  geom_point()+
  ylab(expression(paste(PM[2.5],"measured at the \n US Embassy monitor")))+
  xlab(expression(paste(PM[2.5],"measured at the \n Baolian monitor")))+
  xlim(0,600)+
  ylim(0,600)+
  theme_few()



##plot of environmental Table, time series of environmental variables 

e1<-ggplot(data, aes(x = date, y = pm25ussg))+
  geom_line()+
  ylab("PM2.5-US") + 
  theme_few()

e2<-ggplot(data, aes(x = date, y = pm25hdbl))+
  geom_line()+
  ylab("PM2.5-CHN") +
  theme_few()

e3<-ggplot(data,aes(x=date,y=ave_pm))+
  geom_line()+
  ylab("PM2.5-Model") +
  theme_few()


e4<-ggplot(data, aes(x = date, y = o3))+
  geom_line()+
  ylab("o3") +
  theme_few()


e5<-ggplot(data, aes(x = date, y = tmean))+
  geom_line()+
  ylab("Mean Temperature") +
  theme_few()

e6<-ggplot(data, aes(x = date, y = rh))+
  geom_line()+
  ylab("humidity") +
  xlab("Date")+
  theme_few()

grid.arrange(e1,e2,e3,e4,e5,e6,nrow = 6)###this plots is better for paper 



to_plot_pm <- data %>%
  select(date, pm25ussg, pm25hdbl, ave_pm)%>%
  gather(monitor, pm25, -date)%>%
  mutate(monitor = factor(monitor, levels = c("pm25ussg", "pm25hdbl", "ave_pm"), labels = c("US Embassy", "Baolian", "Average")))

e1 <- ggplot(to_plot_pm, aes(x = date, y = pm25, color = monitor)) +
  geom_line(show.legend = TRUE) +
  ylab("PM2.5 concentration") +
  theme_few()+
  theme(legend.position=c(0.1,0.81))

e2<-ggplot(data, aes(x = date, y = tmean))+
  geom_line()+
  ylab("Mean Temperature") +
  theme_few()

e3<-ggplot(data, aes(x = date, y = rh))+
  geom_line()+
  ylab("humidity") +
  xlab("Date")+
  theme_few()

grid.arrange(e1, e2, e3, ncol = 1, heights = c(0.6, 0.2, 0.2))

# set up a data frame for allRR results,and lists for matRR and cumRR results
ussg_all <- as.data.frame(matrix(NA, nrow = length(names(cause.to.model)), ncol = 3))
colnames(ussg_all) <- c("allRR","low","high")
rownames(ussg_all) <- names(cause.to.model)

ussg_mat <- vector("list",length(cause.to.model))
names(ussg_mat) <- names(cause.to.model)

ussg_cum <- vector("list",length(cause.to.model))
names(ussg_cum) <- names(cause.to.model)



##PM2.5 Main model, lag=7
# generate stratum,for year, month and dow
data$month <- as.factor(months(data$date))
data$year <- as.factor(format(data$date, format="%Y"))
data$dow <- as.factor(weekdays(data$date))
data$stratum <- as.factor(data$year:data$month:data$dow)
head(data)

# pm25ussg is used, based on the literature, the lag was selected of 7
cb1.pm25ussg <- crossbasis(data$ave_pm, lag=7, argvar=list(fun="lin",cen=0),
                           arglag=list(fun="ns",df=3))
cb1.tmean <- crossbasis(data$tmean, lag=7, argvar=list(fun="ns",df=5,cen=25),
                        arglag=list(fun="ns",df=3))


# pm25ussg is used,main model
# case-crossover analysis
for(i in 1:nrow(ussg_all)){
  m1 <- glm(data[,names(cause.to.model)[i]]~
              cb1.pm25ussg + cb1.tmean+ns(rh, 3)  + stratum, 
            family=quasipoisson(), data) 
  pred1 <- crosspred(cb1.pm25ussg, m1, at=10, cumul=TRUE)
  ussg_all[i,1:3] <- with(pred1,c(allRRfit,allRRlow,allRRhigh)) 
  
  mat_u <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
  colnames(mat_u) <- c("RR","low","high")
  ussg_mat[[names(cause.to.model[i])]] <- mat_u
  
  cum_u <- with(pred1,t(rbind(cumRRfit,cumRRlow,cumRRhigh)))
  colnames(cum_u) <- c("RR","low","high")
  ussg_cum[[names(cause.to.model[i])]] <- cum_u
}

ussg_all
ussg_mat
ussg_cum




#####Plot for cumlative lag effect of each day using the dataframe ussg_cum
head(ussg_cum)


###putting all lag day effects to one dataframe. the new data frame named ussg_brooke


for(i in 1:nrow(ussg_all)){
  ussg_cum_a <- as.data.frame(ussg_cum[[names(cause.to.model[i])]])
  ussg_cum_a$cause <- names(ussg_cum)[i]
  if(i == 1){
    ussg_brooke <- ussg_cum_a
  } else {
    ussg_brooke <- rbind(ussg_brooke, ussg_cum_a)
  }
}

##creat the new column to represent the lag informaiton from 0 to 5
head(ussg_brooke, 20)
ussg_brooke$lag <- rep(0:7)
ussg_brooke
head(ussg_brooke,100)

##Add a column named "tn" for ussg_brooke which is the total N, this is for arranging the plot later
long.cause.to.model<-gather(cause.to.model,cause,count)
head(long.cause.to.model)
total<-summarize(group_by(long.cause.to.model,cause),tn=sum(count))
total
ussg_brooke<-merge(ussg_brooke,total,by="cause")
head(ussg_brooke)



##using fucntion to change the value to percent increase 
ussg_brooke[c("RR","low","high")]<-apply(ussg_brooke[c("RR","low","high")],MARGIN=2,FUN=pi)

head(ussg_brooke)

##changing the character "to" to "-", and all to a00toz99
ussg_brooke$cause<-gsub("to","-",ussg_brooke$cause)
ussg_brooke$cause<-gsub("all","A00-Z99",ussg_brooke$cause)
ussg_brooke$cause<-gsub("I20I21I22I24","I20-I22,I24",ussg_brooke$cause)



###divided the ICD to two groups, gouped and specific
head(ussg_brooke)
ussg_lagplot<-ussg_brooke
ussg_lagplot$type<-1
ussg_lagplot$type[ussg_lagplot$cause %in% c("A00-Z99","A00-R99","N00-N99","F00-F99","I00-I99","J00-J99","G00-G99","K00-K93","X60-X84")]<-0

ussg_lagplot$type<-factor(ussg_lagplot$type,levels=c(0,1),
                          labels=c("Grouped","Specific"))
ussg_lagplot
ussg_lagplot$cause<-as.factor(ussg_lagplot$cause)
ussg_lagplot$lag<-factor(ussg_lagplot$lag,levels=c(0,1,2,3,4,5,6,7),labels=c("Lag 0","Lag 0-1","Lag 0-2","Lag 0-3","Lag 0-4","Lag 0-5","Lag 0-6","Lag 0-7"))
head(ussg_lagplot)

ussg_lagplot$sig<-0
ussg_lagplot$sig[ussg_lagplot$low > 0]<-1
ussg_lagplot$sig
ussg_lagplot$sig<-as.factor(ussg_lagplot$sig)

head(ussg_lagplot,20)



##creat two dataframe for making the grouped and specific plots
###and also order the grouped and specific 
ussg_lagplot_specific<-subset(ussg_lagplot,type=="Specific")
ussg_lagplot_specific$zimu<-substr(ussg_lagplot_specific$cause,start=1,stop=1)
order<-as.data.frame(matrix(NA,nrow=6),ncol=2)
head(order)
order[,1]<-c("I","J","K","G","N","F")
order[,2]<-c(1:6)
head(order)
names(order)<-c("zimu","xuhao")
head(order)
ussg_lagplot_specific<-merge(ussg_lagplot_specific,order,by="zimu")
ussg_lagplot_specific<-arrange(ussg_lagplot_specific,desc(xuhao),tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))
head(ussg_lagplot_specific)


ussg_lagplot_grouped<-subset(ussg_lagplot,type=="Grouped")%>%
  arrange(tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

head(ussg_lagplot_grouped)

ussg_lagplot_grouped$cause<-sprintf("%10s", ussg_lagplot_grouped$cause)
ussg_lagplot_grouped<-arrange(ussg_lagplot_grouped,tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

##grouped and specific on the same page of cumlative effect 
#grouped
a<-ggplot(ussg_lagplot_grouped, aes(x = RR, y = cause,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(x = low, xend = high,
                   yend = cause)) + 
  ylab("") +
  xlab("")+
  xlim(-6.5,6.5)+ 
  facet_wrap(~lag,nrow=1)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_vline(xintercept = 0, linetype = 2)


##Specific
b<-ggplot(ussg_lagplot_specific, aes(x = RR, y = cause,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(x = low, xend = high,
                   yend = cause)) + 
  ylab("") + 
  xlim(-6.5,6.5)+ 
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~lag,nrow=1)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_vline(xintercept = 0, linetype = 2)

grid.arrange(a,b,nrow = 2,heights=c(0.7,1.5))###this plots is better for paper 

###画lag的图


ussg_lagplot_grouped<-arrange(ussg_lagplot_grouped,-tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))


head(ussg_lagplot_grouped)

ggplot(ussg_lagplot_grouped, aes(x = lag, y = RR,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(y = low, yend = high,xend = lag)) + 
  ylab("") +
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~cause,ncol=3)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0, linetype = 2)

ussg_lagplot_specific<-arrange(ussg_lagplot_specific,xuhao,-tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

head(ussg_lagplot_specific)

ggplot(ussg_lagplot_specific, aes(x = lag, y = RR,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(y = low, yend = high,xend = lag)) + 
  ylab("") +
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~cause,ncol=3)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0, linetype = 2)






####做一个大表，用ussg_lagplot_grouped和ussg_lagplot_specific两个数据框做。
library(reshape2)
head(ussg_lagplot_grouped)
head(ussg_lagplot_specific)


grouped.new<-select(ussg_lagplot_grouped,-tn,-type,-sig)
options(digits=2)
grouped.new$rrwide<-paste(grouped.new$RR," (", grouped.new$low, ", ",grouped.new$high, ")")
head(grouped.new)
grouped.new.table<-dcast(grouped.new,cause~lag,value.var="rrwide")

specific.new<-select(ussg_lagplot_specific,-tn,-type,-sig)
options(digits=2)
specific.new$rrwide<-paste(specific.new$RR," (", specific.new$low, ", ",specific.new$high, ")")
head(specific.new)
specific.new.table<-dcast(specific.new,cause~lag,value.var="rrwide")

main.table<-rbind(grouped.new.table,specific.new.table)

write.csv(main.table,file="main.table.csv")

write.csv(specific.new.table,file="specific.new.table.csv")


###再细化敏感性分析
##1. sensitivity analysis to change knots (pm2.5从ns 变logknot). 
# generate stratum,for year, month and dow
data$month <- as.factor(months(data$date))
data$year <- as.factor(format(data$date, format="%Y"))
data$dow <- as.factor(weekdays(data$date))
data$stratum <- as.factor(data$year:data$month:data$dow)
head(data)

# pm25ussg is used, based on the literature, the lag was selected of 7
# 原模型 
###cb1.pm25ussg <- crossbasis(data$ave_pm, lag=7, argvar=list(fun="lin",cen=0),
                           ###arglag=list(fun="ns",df=3))
###cb1.tmean <- crossbasis(data$tmean, lag=7, argvar=list(fun="ns",df=5,cen=25),
                        ###arglag=list(fun="ns",df=3))



lagknots <- logknots(7, 3)
cb1.pm25ussg <- crossbasis(data$ave_pm, lag=7, argvar=list(fun="lin",cen=0),
                           arglag=list(knots=lagknots))
cb1.tmean <- crossbasis(data$tmean, lag=7, argvar=list(fun="ns",df=5,cen=25),
                        arglag=list(fun="ns",df=3))



# pm25ussg is used,main model lag=7
# case-crossover analysis
for(i in 1:nrow(ussg_all)){
  m1 <- glm(data[,names(cause.to.model)[i]]~
              cb1.pm25ussg + cb1.tmean+ns(rh, 3)  + stratum, 
            family=quasipoisson(), data) 
  pred1 <- crosspred(cb1.pm25ussg, m1, at=10, cumul=TRUE)
  ussg_all[i,1:3] <- with(pred1,c(allRRfit,allRRlow,allRRhigh)) 
  
  mat_u <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
  colnames(mat_u) <- c("RR","low","high")
  ussg_mat[[names(cause.to.model[i])]] <- mat_u
  
  cum_u <- with(pred1,t(rbind(cumRRfit,cumRRlow,cumRRhigh)))
  colnames(cum_u) <- c("RR","low","high")
  ussg_cum[[names(cause.to.model[i])]] <- cum_u
}

ussg_all
ussg_mat
ussg_cum




#####Plot for cumlative lag effect of each day using the dataframe ussg_cum
head(ussg_cum)


###putting all lag day effects to one dataframe. the new data frame named ussg_brooke


for(i in 1:nrow(ussg_all)){
  ussg_cum_a <- as.data.frame(ussg_cum[[names(cause.to.model[i])]])
  ussg_cum_a$cause <- names(ussg_cum)[i]
  if(i == 1){
    ussg_brooke <- ussg_cum_a
  } else {
    ussg_brooke <- rbind(ussg_brooke, ussg_cum_a)
  }
}

##creat the new column to represent the lag informaiton from 0 to 7
head(ussg_brooke, 20)
ussg_brooke$lag <- rep(0:7)
ussg_brooke
head(ussg_brooke,100)

##Add a column named "tn" for ussg_brooke which is the total N, this is for arranging the plot later
long.cause.to.model<-gather(cause.to.model,cause,count)
head(long.cause.to.model)
total<-summarize(group_by(long.cause.to.model,cause),tn=sum(count))
total
ussg_brooke<-merge(ussg_brooke,total,by="cause")
head(ussg_brooke)



##using fucntion to change the value to percent increase 
ussg_brooke[c("RR","low","high")]<-apply(ussg_brooke[c("RR","low","high")],MARGIN=2,FUN=pi)

head(ussg_brooke)

##changing the character "to" to "-", and all to a00toz99
ussg_brooke$cause<-gsub("to","-",ussg_brooke$cause)
ussg_brooke$cause<-gsub("all","A00-Z99",ussg_brooke$cause)
ussg_brooke$cause<-gsub("I20I21I22I24","I20-I22,I24",ussg_brooke$cause)



###divided the ICD to two groups, gouped and specific
head(ussg_brooke)
ussg_lagplot<-ussg_brooke
ussg_lagplot$type<-1
ussg_lagplot$type[ussg_lagplot$cause %in% c("A00-Z99","A00-R99","N00-N99","F00-F99","I00-I99","J00-J99","G00-G99","K00-K93","X60-X84")]<-0

ussg_lagplot$type<-factor(ussg_lagplot$type,levels=c(0,1),
                          labels=c("Grouped","Specific"))
ussg_lagplot
ussg_lagplot$cause<-as.factor(ussg_lagplot$cause)
ussg_lagplot$lag<-factor(ussg_lagplot$lag,levels=c(0,1,2,3,4,5,6,7),labels=c("Lag 0","Lag 0-1","Lag 0-2","Lag 0-3","Lag 0-4","Lag 0-5","Lag 0-6","Lag 0-7"))
head(ussg_lagplot)

ussg_lagplot$sig<-0
ussg_lagplot$sig[ussg_lagplot$low > 0]<-1
ussg_lagplot$sig
ussg_lagplot$sig<-as.factor(ussg_lagplot$sig)

head(ussg_lagplot,20)



##creat two dataframe for making the grouped and specific plots
###and also order the grouped and specific 
ussg_lagplot_specific<-subset(ussg_lagplot,type=="Specific")
ussg_lagplot_specific$zimu<-substr(ussg_lagplot_specific$cause,start=1,stop=1)
order<-as.data.frame(matrix(NA,nrow=6),ncol=2)
head(order)
order[,1]<-c("I","J","K","G","N","F")
order[,2]<-c(1:6)
head(order)
names(order)<-c("zimu","xuhao")
head(order)
ussg_lagplot_specific<-merge(ussg_lagplot_specific,order,by="zimu")
ussg_lagplot_specific<-arrange(ussg_lagplot_specific,desc(xuhao),tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))
head(ussg_lagplot_specific)


ussg_lagplot_grouped<-subset(ussg_lagplot,type=="Grouped")%>%
  arrange(tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

head(ussg_lagplot_grouped)

ussg_lagplot_grouped$cause<-sprintf("%10s", ussg_lagplot_grouped$cause)
ussg_lagplot_grouped<-arrange(ussg_lagplot_grouped,tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

##grouped and specific on the same page of cumlative effect 
#grouped
a<-ggplot(ussg_lagplot_grouped, aes(x = RR, y = cause,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(x = low, xend = high,
                   yend = cause)) + 
  ylab("") +
  xlab("")+
  xlim(-6.5,6.5)+ 
  facet_wrap(~lag,nrow=1)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_vline(xintercept = 0, linetype = 2)


##Specific
b<-ggplot(ussg_lagplot_specific, aes(x = RR, y = cause,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(x = low, xend = high,
                   yend = cause)) + 
  ylab("") + 
  xlim(-6.5,6.5)+ 
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~lag,nrow=1)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_vline(xintercept = 0, linetype = 2)

grid.arrange(a,b,nrow = 2,heights=c(0.7,1.5))###this plots is better for paper 

###画lag的图


ussg_lagplot_grouped<-arrange(ussg_lagplot_grouped,-tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))


head(ussg_lagplot_grouped)

ggplot(ussg_lagplot_grouped, aes(x = lag, y = RR,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(y = low, yend = high,xend = lag)) + 
  ylab("") +
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~cause,ncol=3)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0, linetype = 2)

ussg_lagplot_specific<-arrange(ussg_lagplot_specific,xuhao,-tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

head(ussg_lagplot_specific)

ggplot(ussg_lagplot_specific, aes(x = lag, y = RR,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(y = low, yend = high,xend = lag)) + 
  ylab("") +
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~cause,ncol=3)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0, linetype = 2)

####做一个大表，用ussg_lagplot_grouped和ussg_lagplot_specific两个数据框做。
library(reshape2)
head(ussg_lagplot_grouped)
head(ussg_lagplot_specific)


grouped.new<-select(ussg_lagplot_grouped,-tn,-type,-sig)
options(digits=2)
grouped.new$rrwide<-paste(grouped.new$RR," (", grouped.new$low, ", ",grouped.new$high, ")")
head(grouped.new)
grouped.new.table<-dcast(grouped.new,cause~lag,value.var="rrwide")

specific.new<-select(ussg_lagplot_specific,-tn,-type,-sig)
options(digits=2)
specific.new$rrwide<-paste(specific.new$RR," (", specific.new$low, ", ",specific.new$high, ")")
head(specific.new)
specific.new.table<-dcast(specific.new,cause~lag,value.var="rrwide")

s1.table<-rbind(grouped.new.table,specific.new.table)

write.csv(s1.table,file="s1.table.csv")





##2. sensitivity analysis to change knots (pm2.5从ns 变logknot,lagknots从3变2). 
# generate stratum,for year, month and dow
data$month <- as.factor(months(data$date))
data$year <- as.factor(format(data$date, format="%Y"))
data$dow <- as.factor(weekdays(data$date))
data$stratum <- as.factor(data$year:data$month:data$dow)
head(data)

# pm25ussg is used, based on the literature, the lag was selected of 7
# 原模型 
###cb1.pm25ussg <- crossbasis(data$ave_pm, lag=7, argvar=list(fun="lin",cen=0),
###arglag=list(fun="ns",df=3))
###cb1.tmean <- crossbasis(data$tmean, lag=7, argvar=list(fun="ns",df=5,cen=25),
###arglag=list(fun="ns",df=3))


lagknots <- logknots(7, 2)
cb1.pm25ussg <- crossbasis(data$ave_pm, lag=7, argvar=list(fun="lin",cen=0),
                           arglag=list(knots=lagknots))
cb1.tmean <- crossbasis(data$tmean, lag=7, argvar=list(fun="ns",df=5,cen=25),
                        arglag=list(fun="ns",df=3))



# pm25ussg is used,main model lag=7
# case-crossover analysis
for(i in 1:nrow(ussg_all)){
  m1 <- glm(data[,names(cause.to.model)[i]]~
              cb1.pm25ussg + cb1.tmean+ns(rh, 3)  + stratum, 
            family=quasipoisson(), data) 
  pred1 <- crosspred(cb1.pm25ussg, m1, at=10, cumul=TRUE)
  ussg_all[i,1:3] <- with(pred1,c(allRRfit,allRRlow,allRRhigh)) 
  
  mat_u <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
  colnames(mat_u) <- c("RR","low","high")
  ussg_mat[[names(cause.to.model[i])]] <- mat_u
  
  cum_u <- with(pred1,t(rbind(cumRRfit,cumRRlow,cumRRhigh)))
  colnames(cum_u) <- c("RR","low","high")
  ussg_cum[[names(cause.to.model[i])]] <- cum_u
}

ussg_all
ussg_mat
ussg_cum




#####Plot for cumlative lag effect of each day using the dataframe ussg_cum
head(ussg_cum)


###putting all lag day effects to one dataframe. the new data frame named ussg_brooke


for(i in 1:nrow(ussg_all)){
  ussg_cum_a <- as.data.frame(ussg_cum[[names(cause.to.model[i])]])
  ussg_cum_a$cause <- names(ussg_cum)[i]
  if(i == 1){
    ussg_brooke <- ussg_cum_a
  } else {
    ussg_brooke <- rbind(ussg_brooke, ussg_cum_a)
  }
}

##creat the new column to represent the lag informaiton from 0 to 7
head(ussg_brooke, 20)
ussg_brooke$lag <- rep(0:7)
ussg_brooke
head(ussg_brooke,100)

##Add a column named "tn" for ussg_brooke which is the total N, this is for arranging the plot later
long.cause.to.model<-gather(cause.to.model,cause,count)
head(long.cause.to.model)
total<-summarize(group_by(long.cause.to.model,cause),tn=sum(count))
total
ussg_brooke<-merge(ussg_brooke,total,by="cause")
head(ussg_brooke)



##using fucntion to change the value to percent increase 
ussg_brooke[c("RR","low","high")]<-apply(ussg_brooke[c("RR","low","high")],MARGIN=2,FUN=pi)

head(ussg_brooke)

##changing the character "to" to "-", and all to a00toz99
ussg_brooke$cause<-gsub("to","-",ussg_brooke$cause)
ussg_brooke$cause<-gsub("all","A00-Z99",ussg_brooke$cause)
ussg_brooke$cause<-gsub("I20I21I22I24","I20-I22,I24",ussg_brooke$cause)



###divided the ICD to two groups, gouped and specific
head(ussg_brooke)
ussg_lagplot<-ussg_brooke
ussg_lagplot$type<-1
ussg_lagplot$type[ussg_lagplot$cause %in% c("A00-Z99","A00-R99","N00-N99","F00-F99","I00-I99","J00-J99","G00-G99","K00-K93","X60-X84")]<-0

ussg_lagplot$type<-factor(ussg_lagplot$type,levels=c(0,1),
                          labels=c("Grouped","Specific"))
ussg_lagplot
ussg_lagplot$cause<-as.factor(ussg_lagplot$cause)
ussg_lagplot$lag<-factor(ussg_lagplot$lag,levels=c(0,1,2,3,4,5,6,7),labels=c("Lag 0","Lag 0-1","Lag 0-2","Lag 0-3","Lag 0-4","Lag 0-5","Lag 0-6","Lag 0-7"))
head(ussg_lagplot)

ussg_lagplot$sig<-0
ussg_lagplot$sig[ussg_lagplot$low > 0]<-1
ussg_lagplot$sig
ussg_lagplot$sig<-as.factor(ussg_lagplot$sig)

head(ussg_lagplot,20)



##creat two dataframe for making the grouped and specific plots
###and also order the grouped and specific 
ussg_lagplot_specific<-subset(ussg_lagplot,type=="Specific")
ussg_lagplot_specific$zimu<-substr(ussg_lagplot_specific$cause,start=1,stop=1)
order<-as.data.frame(matrix(NA,nrow=6),ncol=2)
head(order)
order[,1]<-c("I","J","K","G","N","F")
order[,2]<-c(1:6)
head(order)
names(order)<-c("zimu","xuhao")
head(order)
ussg_lagplot_specific<-merge(ussg_lagplot_specific,order,by="zimu")
ussg_lagplot_specific<-arrange(ussg_lagplot_specific,desc(xuhao),tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))
head(ussg_lagplot_specific)


ussg_lagplot_grouped<-subset(ussg_lagplot,type=="Grouped")%>%
  arrange(tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

head(ussg_lagplot_grouped)

ussg_lagplot_grouped$cause<-sprintf("%10s", ussg_lagplot_grouped$cause)
ussg_lagplot_grouped<-arrange(ussg_lagplot_grouped,tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

##grouped and specific on the same page of cumlative effect 
#grouped
a<-ggplot(ussg_lagplot_grouped, aes(x = RR, y = cause,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(x = low, xend = high,
                   yend = cause)) + 
  ylab("") +
  xlab("")+
  xlim(-6.5,6.5)+ 
  facet_wrap(~lag,nrow=1)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_vline(xintercept = 0, linetype = 2)


##Specific
b<-ggplot(ussg_lagplot_specific, aes(x = RR, y = cause,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(x = low, xend = high,
                   yend = cause)) + 
  ylab("") + 
  xlim(-6.5,6.5)+ 
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~lag,nrow=1)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_vline(xintercept = 0, linetype = 2)

grid.arrange(a,b,nrow = 2,heights=c(0.7,1.5))###this plots is better for paper 

###画lag的图


ussg_lagplot_grouped<-arrange(ussg_lagplot_grouped,-tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))


head(ussg_lagplot_grouped)

ggplot(ussg_lagplot_grouped, aes(x = lag, y = RR,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(y = low, yend = high,xend = lag)) + 
  ylab("") +
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~cause,ncol=3)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0, linetype = 2)

ussg_lagplot_specific<-arrange(ussg_lagplot_specific,xuhao,-tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

head(ussg_lagplot_specific)

ggplot(ussg_lagplot_specific, aes(x = lag, y = RR,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(y = low, yend = high,xend = lag)) + 
  ylab("") +
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~cause,ncol=3)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0, linetype = 2)

####做一个大表，用ussg_lagplot_grouped和ussg_lagplot_specific两个数据框做。
library(reshape2)
head(ussg_lagplot_grouped)
head(ussg_lagplot_specific)


grouped.new<-select(ussg_lagplot_grouped,-tn,-type,-sig)
options(digits=2)
grouped.new$rrwide<-paste(grouped.new$RR," (", grouped.new$low, ", ",grouped.new$high, ")")
head(grouped.new)
grouped.new.table<-dcast(grouped.new,cause~lag,value.var="rrwide")

specific.new<-select(ussg_lagplot_specific,-tn,-type,-sig)
options(digits=2)
specific.new$rrwide<-paste(specific.new$RR," (", specific.new$low, ", ",specific.new$high, ")")
head(specific.new)
specific.new.table<-dcast(specific.new,cause~lag,value.var="rrwide")

s2.table<-rbind(grouped.new.table,specific.new.table)

write.csv(s2.table,file="s2.table.csv")




##3. sensitivity analysis  (+ozone). 
# generate stratum,for year, month and dow
data$month <- as.factor(months(data$date))
data$year <- as.factor(format(data$date, format="%Y"))
data$dow <- as.factor(weekdays(data$date))
data$stratum <- as.factor(data$year:data$month:data$dow)
head(data)

# pm25ussg is used, based on the literature, the lag was selected of 7
# 原模型 

cb1.pm25ussg <- crossbasis(data$ave_pm, lag=7, argvar=list(fun="lin",cen=0),
               arglag=list(fun="ns",df=3))
cb1.tmean <- crossbasis(data$tmean, lag=7, argvar=list(fun="ns",df=5,cen=25),
                     arglag=list(fun="ns",df=3))


# pm25ussg is used,main model lag=7
# case-crossover analysis
for(i in 1:nrow(ussg_all)){
  m1 <- glm(data[,names(cause.to.model)[i]]~
              cb1.pm25ussg + cb1.tmean+ns(rh, 3)  + o3+stratum, 
            family=quasipoisson(), data) 
  pred1 <- crosspred(cb1.pm25ussg, m1, at=10, cumul=TRUE)
  ussg_all[i,1:3] <- with(pred1,c(allRRfit,allRRlow,allRRhigh)) 
  
  mat_u <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
  colnames(mat_u) <- c("RR","low","high")
  ussg_mat[[names(cause.to.model[i])]] <- mat_u
  
  cum_u <- with(pred1,t(rbind(cumRRfit,cumRRlow,cumRRhigh)))
  colnames(cum_u) <- c("RR","low","high")
  ussg_cum[[names(cause.to.model[i])]] <- cum_u
}

ussg_all
ussg_mat
ussg_cum




#####Plot for cumlative lag effect of each day using the dataframe ussg_cum
head(ussg_cum)


###putting all lag day effects to one dataframe. the new data frame named ussg_brooke


for(i in 1:nrow(ussg_all)){
  ussg_cum_a <- as.data.frame(ussg_cum[[names(cause.to.model[i])]])
  ussg_cum_a$cause <- names(ussg_cum)[i]
  if(i == 1){
    ussg_brooke <- ussg_cum_a
  } else {
    ussg_brooke <- rbind(ussg_brooke, ussg_cum_a)
  }
}

##creat the new column to represent the lag informaiton from 0 to 7
head(ussg_brooke, 20)
ussg_brooke$lag <- rep(0:7)
ussg_brooke
head(ussg_brooke,100)

##Add a column named "tn" for ussg_brooke which is the total N, this is for arranging the plot later
long.cause.to.model<-gather(cause.to.model,cause,count)
head(long.cause.to.model)
total<-summarize(group_by(long.cause.to.model,cause),tn=sum(count))
total
ussg_brooke<-merge(ussg_brooke,total,by="cause")
head(ussg_brooke)



##using fucntion to change the value to percent increase 
ussg_brooke[c("RR","low","high")]<-apply(ussg_brooke[c("RR","low","high")],MARGIN=2,FUN=pi)

head(ussg_brooke)

##changing the character "to" to "-", and all to a00toz99
ussg_brooke$cause<-gsub("to","-",ussg_brooke$cause)
ussg_brooke$cause<-gsub("all","A00-Z99",ussg_brooke$cause)
ussg_brooke$cause<-gsub("I20I21I22I24","I20-I22,I24",ussg_brooke$cause)



###divided the ICD to two groups, gouped and specific
head(ussg_brooke)
ussg_lagplot<-ussg_brooke
ussg_lagplot$type<-1
ussg_lagplot$type[ussg_lagplot$cause %in% c("A00-Z99","A00-R99","N00-N99","F00-F99","I00-I99","J00-J99","G00-G99","K00-K93","X60-X84")]<-0

ussg_lagplot$type<-factor(ussg_lagplot$type,levels=c(0,1),
                          labels=c("Grouped","Specific"))
ussg_lagplot
ussg_lagplot$cause<-as.factor(ussg_lagplot$cause)
ussg_lagplot$lag<-factor(ussg_lagplot$lag,levels=c(0,1,2,3,4,5,6,7),labels=c("Lag 0","Lag 0-1","Lag 0-2","Lag 0-3","Lag 0-4","Lag 0-5","Lag 0-6","Lag 0-7"))
head(ussg_lagplot)

ussg_lagplot$sig<-0
ussg_lagplot$sig[ussg_lagplot$low > 0]<-1
ussg_lagplot$sig
ussg_lagplot$sig<-as.factor(ussg_lagplot$sig)

head(ussg_lagplot,20)



##creat two dataframe for making the grouped and specific plots
###and also order the grouped and specific 
ussg_lagplot_specific<-subset(ussg_lagplot,type=="Specific")
ussg_lagplot_specific$zimu<-substr(ussg_lagplot_specific$cause,start=1,stop=1)
order<-as.data.frame(matrix(NA,nrow=6),ncol=2)
head(order)
order[,1]<-c("I","J","K","G","N","F")
order[,2]<-c(1:6)
head(order)
names(order)<-c("zimu","xuhao")
head(order)
ussg_lagplot_specific<-merge(ussg_lagplot_specific,order,by="zimu")
ussg_lagplot_specific<-arrange(ussg_lagplot_specific,desc(xuhao),tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))
head(ussg_lagplot_specific)


ussg_lagplot_grouped<-subset(ussg_lagplot,type=="Grouped")%>%
  arrange(tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

head(ussg_lagplot_grouped)

ussg_lagplot_grouped$cause<-sprintf("%10s", ussg_lagplot_grouped$cause)
ussg_lagplot_grouped<-arrange(ussg_lagplot_grouped,tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

##grouped and specific on the same page of cumlative effect 
#grouped
a<-ggplot(ussg_lagplot_grouped, aes(x = RR, y = cause,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(x = low, xend = high,
                   yend = cause)) + 
  ylab("") +
  xlab("")+
  xlim(-7,7)+ 
  facet_wrap(~lag,nrow=1)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_vline(xintercept = 0, linetype = 2)


##Specific
b<-ggplot(ussg_lagplot_specific, aes(x = RR, y = cause,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(x = low, xend = high,
                   yend = cause)) + 
  ylab("") + 
  xlim(-7,7)+ 
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~lag,nrow=1)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_vline(xintercept = 0, linetype = 2)

grid.arrange(a,b,nrow = 2,heights=c(0.7,1.5))###this plots is better for paper 

###画lag的图


ussg_lagplot_grouped<-arrange(ussg_lagplot_grouped,-tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))


head(ussg_lagplot_grouped)

ggplot(ussg_lagplot_grouped, aes(x = lag, y = RR,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(y = low, yend = high,xend = lag)) + 
  ylab("") +
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~cause,ncol=3)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0, linetype = 2)

ussg_lagplot_specific<-arrange(ussg_lagplot_specific,xuhao,-tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

head(ussg_lagplot_specific)

ggplot(ussg_lagplot_specific, aes(x = lag, y = RR,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(y = low, yend = high,xend = lag)) + 
  ylab("") +
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~cause,ncol=3)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0, linetype = 2)

####做一个大表，用ussg_lagplot_grouped和ussg_lagplot_specific两个数据框做。
library(reshape2)
head(ussg_lagplot_grouped)
head(ussg_lagplot_specific)


grouped.new<-select(ussg_lagplot_grouped,-tn,-type,-sig)
options(digits=2)
grouped.new$rrwide<-paste(grouped.new$RR," (", grouped.new$low, ", ",grouped.new$high, ")")
head(grouped.new)
grouped.new.table<-dcast(grouped.new,cause~lag,value.var="rrwide")

specific.new<-select(ussg_lagplot_specific,-tn,-type,-sig)
options(digits=2)
specific.new$rrwide<-paste(specific.new$RR," (", specific.new$low, ", ",specific.new$high, ")")
head(specific.new)
specific.new.table<-dcast(specific.new,cause~lag,value.var="rrwide")

s3.table<-rbind(grouped.new.table,specific.new.table)

write.csv(s3.table,file="s3.table.csv")



####4.sensitivity analysis 变化PM2.5暴露数据，用美国大使馆数据

data<-read.csv("0912bj.csv",head=T)
data$date<-as.Date(with(data,date,"%Y/%m/%d"))
data$o3[data$o3==-9999] <- NA 
data$o3[data$o3=="#DIV/0!"] <- NA
data$o3<-as.numeric(as.character(data$o3))##The factor class could not change to numeric directly,must use this expression.
class(data$o3)
summary(data$o3)

# generate stratum,for year, month and dow
data$month <- as.factor(months(data$date))
data$year <- as.factor(format(data$date, format="%Y"))
data$dow <- as.factor(weekdays(data$date))
data$stratum <- as.factor(data$year:data$month:data$dow)
head(data)

# pm25ussg is used, based on the literature, the lag was selected of 7
# 原模型 

cb1.pm25ussg <- crossbasis(data$pm25ussg, lag=7, argvar=list(fun="lin",cen=0),
                           arglag=list(fun="ns",df=3))
cb1.tmean <- crossbasis(data$tmean, lag=7, argvar=list(fun="ns",df=5,cen=25),
                        arglag=list(fun="ns",df=3))


# pm25ussg is used,main model lag=7
# case-crossover analysis
for(i in 1:nrow(ussg_all)){
  m1 <- glm(data[,names(cause.to.model)[i]]~
              cb1.pm25ussg + cb1.tmean+ns(rh, 3)  +stratum, 
            family=quasipoisson(), data) 
  pred1 <- crosspred(cb1.pm25ussg, m1, at=10, cumul=TRUE)
  ussg_all[i,1:3] <- with(pred1,c(allRRfit,allRRlow,allRRhigh)) 
  
  mat_u <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
  colnames(mat_u) <- c("RR","low","high")
  ussg_mat[[names(cause.to.model[i])]] <- mat_u
  
  cum_u <- with(pred1,t(rbind(cumRRfit,cumRRlow,cumRRhigh)))
  colnames(cum_u) <- c("RR","low","high")
  ussg_cum[[names(cause.to.model[i])]] <- cum_u
}

ussg_all
ussg_mat
ussg_cum




#####Plot for cumlative lag effect of each day using the dataframe ussg_cum
head(ussg_cum)


###putting all lag day effects to one dataframe. the new data frame named ussg_brooke


for(i in 1:nrow(ussg_all)){
  ussg_cum_a <- as.data.frame(ussg_cum[[names(cause.to.model[i])]])
  ussg_cum_a$cause <- names(ussg_cum)[i]
  if(i == 1){
    ussg_brooke <- ussg_cum_a
  } else {
    ussg_brooke <- rbind(ussg_brooke, ussg_cum_a)
  }
}

##creat the new column to represent the lag informaiton from 0 to 7
head(ussg_brooke, 20)
ussg_brooke$lag <- rep(0:7)
ussg_brooke
head(ussg_brooke,100)

##Add a column named "tn" for ussg_brooke which is the total N, this is for arranging the plot later
long.cause.to.model<-gather(cause.to.model,cause,count)
head(long.cause.to.model)
total<-summarize(group_by(long.cause.to.model,cause),tn=sum(count))
total
ussg_brooke<-merge(ussg_brooke,total,by="cause")
head(ussg_brooke)



##using fucntion to change the value to percent increase 
ussg_brooke[c("RR","low","high")]<-apply(ussg_brooke[c("RR","low","high")],MARGIN=2,FUN=pi)

head(ussg_brooke)

##changing the character "to" to "-", and all to a00toz99
ussg_brooke$cause<-gsub("to","-",ussg_brooke$cause)
ussg_brooke$cause<-gsub("all","A00-Z99",ussg_brooke$cause)
ussg_brooke$cause<-gsub("I20I21I22I24","I20-I22,I24",ussg_brooke$cause)



###divided the ICD to two groups, gouped and specific
head(ussg_brooke)
ussg_lagplot<-ussg_brooke
ussg_lagplot$type<-1
ussg_lagplot$type[ussg_lagplot$cause %in% c("A00-Z99","A00-R99","N00-N99","F00-F99","I00-I99","J00-J99","G00-G99","K00-K93","X60-X84")]<-0

ussg_lagplot$type<-factor(ussg_lagplot$type,levels=c(0,1),
                          labels=c("Grouped","Specific"))
ussg_lagplot
ussg_lagplot$cause<-as.factor(ussg_lagplot$cause)
ussg_lagplot$lag<-factor(ussg_lagplot$lag,levels=c(0,1,2,3,4,5,6,7),labels=c("Lag 0","Lag 0-1","Lag 0-2","Lag 0-3","Lag 0-4","Lag 0-5","Lag 0-6","Lag 0-7"))
head(ussg_lagplot)

ussg_lagplot$sig<-0
ussg_lagplot$sig[ussg_lagplot$low > 0]<-1
ussg_lagplot$sig
ussg_lagplot$sig<-as.factor(ussg_lagplot$sig)

head(ussg_lagplot,20)



##creat two dataframe for making the grouped and specific plots
###and also order the grouped and specific 
ussg_lagplot_specific<-subset(ussg_lagplot,type=="Specific")
ussg_lagplot_specific$zimu<-substr(ussg_lagplot_specific$cause,start=1,stop=1)
order<-as.data.frame(matrix(NA,nrow=6),ncol=2)
head(order)
order[,1]<-c("I","J","K","G","N","F")
order[,2]<-c(1:6)
head(order)
names(order)<-c("zimu","xuhao")
head(order)
ussg_lagplot_specific<-merge(ussg_lagplot_specific,order,by="zimu")
ussg_lagplot_specific<-arrange(ussg_lagplot_specific,desc(xuhao),tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))
head(ussg_lagplot_specific)


ussg_lagplot_grouped<-subset(ussg_lagplot,type=="Grouped")%>%
  arrange(tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

head(ussg_lagplot_grouped)

ussg_lagplot_grouped$cause<-sprintf("%10s", ussg_lagplot_grouped$cause)
ussg_lagplot_grouped<-arrange(ussg_lagplot_grouped,tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

##grouped and specific on the same page of cumlative effect 
#grouped
a<-ggplot(ussg_lagplot_grouped, aes(x = RR, y = cause,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(x = low, xend = high,
                   yend = cause)) + 
  ylab("") +
  xlab("")+
  xlim(-7,7)+ 
  facet_wrap(~lag,nrow=1)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_vline(xintercept = 0, linetype = 2)


##Specific
b<-ggplot(ussg_lagplot_specific, aes(x = RR, y = cause,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(x = low, xend = high,
                   yend = cause)) + 
  ylab("") + 
  xlim(-7,7)+ 
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~lag,nrow=1)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_vline(xintercept = 0, linetype = 2)

grid.arrange(a,b,nrow = 2,heights=c(0.7,1.5))###this plots is better for paper 

###画lag的图


ussg_lagplot_grouped<-arrange(ussg_lagplot_grouped,-tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))


head(ussg_lagplot_grouped)

ggplot(ussg_lagplot_grouped, aes(x = lag, y = RR,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(y = low, yend = high,xend = lag)) + 
  ylab("") +
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~cause,ncol=3)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0, linetype = 2)

ussg_lagplot_specific<-arrange(ussg_lagplot_specific,xuhao,-tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

head(ussg_lagplot_specific)

ggplot(ussg_lagplot_specific, aes(x = lag, y = RR,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(y = low, yend = high,xend = lag)) + 
  ylab("") +
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~cause,ncol=3)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0, linetype = 2)

####做一个大表，用ussg_lagplot_grouped和ussg_lagplot_specific两个数据框做。
library(reshape2)
head(ussg_lagplot_grouped)
head(ussg_lagplot_specific)


grouped.new<-select(ussg_lagplot_grouped,-tn,-type,-sig)
options(digits=2)
grouped.new$rrwide<-paste(grouped.new$RR," (", grouped.new$low, ", ",grouped.new$high, ")")
head(grouped.new)
grouped.new.table<-dcast(grouped.new,cause~lag,value.var="rrwide")

specific.new<-select(ussg_lagplot_specific,-tn,-type,-sig)
options(digits=2)
specific.new$rrwide<-paste(specific.new$RR," (", specific.new$low, ", ",specific.new$high, ")")
head(specific.new)
specific.new.table<-dcast(specific.new,cause~lag,value.var="rrwide")

s4.table<-rbind(grouped.new.table,specific.new.table)

write.csv(s4.table,file="s4.table.csv")



####5.sensitivity analysis 变化PM2.5暴露数据，用海淀宝联站

data<-read.csv("0912bj.csv",head=T)
data$date<-as.Date(with(data,date,"%Y/%m/%d"))
data$o3[data$o3==-9999] <- NA 
data$o3[data$o3=="#DIV/0!"] <- NA
data$o3<-as.numeric(as.character(data$o3))##The factor class could not change to numeric directly,must use this expression.
class(data$o3)
summary(data$o3)

# generate stratum,for year, month and dow
data$month <- as.factor(months(data$date))
data$year <- as.factor(format(data$date, format="%Y"))
data$dow <- as.factor(weekdays(data$date))
data$stratum <- as.factor(data$year:data$month:data$dow)
head(data)

# pm25ussg is used, based on the literature, the lag was selected of 7
# 原模型 

cb1.pm25ussg <- crossbasis(data$pm25hdbl, lag=7, argvar=list(fun="lin",cen=0),
                           arglag=list(fun="ns",df=3))
cb1.tmean <- crossbasis(data$tmean, lag=7, argvar=list(fun="ns",df=5,cen=25),
                        arglag=list(fun="ns",df=3))


# pm25ussg is used,main model lag=7
# case-crossover analysis
for(i in 1:nrow(ussg_all)){
  m1 <- glm(data[,names(cause.to.model)[i]]~
              cb1.pm25ussg + cb1.tmean+ns(rh, 3)  +stratum, 
            family=quasipoisson(), data) 
  pred1 <- crosspred(cb1.pm25ussg, m1, at=10, cumul=TRUE)
  ussg_all[i,1:3] <- with(pred1,c(allRRfit,allRRlow,allRRhigh)) 
  
  mat_u <- with(pred1,t(rbind(matRRfit,matRRlow,matRRhigh)))
  colnames(mat_u) <- c("RR","low","high")
  ussg_mat[[names(cause.to.model[i])]] <- mat_u
  
  cum_u <- with(pred1,t(rbind(cumRRfit,cumRRlow,cumRRhigh)))
  colnames(cum_u) <- c("RR","low","high")
  ussg_cum[[names(cause.to.model[i])]] <- cum_u
}

ussg_all
ussg_mat
ussg_cum




#####Plot for cumlative lag effect of each day using the dataframe ussg_cum
head(ussg_cum)


###putting all lag day effects to one dataframe. the new data frame named ussg_brooke


for(i in 1:nrow(ussg_all)){
  ussg_cum_a <- as.data.frame(ussg_cum[[names(cause.to.model[i])]])
  ussg_cum_a$cause <- names(ussg_cum)[i]
  if(i == 1){
    ussg_brooke <- ussg_cum_a
  } else {
    ussg_brooke <- rbind(ussg_brooke, ussg_cum_a)
  }
}

##creat the new column to represent the lag informaiton from 0 to 7
head(ussg_brooke, 20)
ussg_brooke$lag <- rep(0:7)
ussg_brooke
head(ussg_brooke,100)

##Add a column named "tn" for ussg_brooke which is the total N, this is for arranging the plot later
long.cause.to.model<-gather(cause.to.model,cause,count)
head(long.cause.to.model)
total<-summarize(group_by(long.cause.to.model,cause),tn=sum(count))
total
ussg_brooke<-merge(ussg_brooke,total,by="cause")
head(ussg_brooke)



##using fucntion to change the value to percent increase 
ussg_brooke[c("RR","low","high")]<-apply(ussg_brooke[c("RR","low","high")],MARGIN=2,FUN=pi)

head(ussg_brooke)

##changing the character "to" to "-", and all to a00toz99
ussg_brooke$cause<-gsub("to","-",ussg_brooke$cause)
ussg_brooke$cause<-gsub("all","A00-Z99",ussg_brooke$cause)
ussg_brooke$cause<-gsub("I20I21I22I24","I20-I22,I24",ussg_brooke$cause)



###divided the ICD to two groups, gouped and specific
head(ussg_brooke)
ussg_lagplot<-ussg_brooke
ussg_lagplot$type<-1
ussg_lagplot$type[ussg_lagplot$cause %in% c("A00-Z99","A00-R99","N00-N99","F00-F99","I00-I99","J00-J99","G00-G99","K00-K93","X60-X84")]<-0

ussg_lagplot$type<-factor(ussg_lagplot$type,levels=c(0,1),
                          labels=c("Grouped","Specific"))
ussg_lagplot
ussg_lagplot$cause<-as.factor(ussg_lagplot$cause)
ussg_lagplot$lag<-factor(ussg_lagplot$lag,levels=c(0,1,2,3,4,5,6,7),labels=c("Lag 0","Lag 0-1","Lag 0-2","Lag 0-3","Lag 0-4","Lag 0-5","Lag 0-6","Lag 0-7"))
head(ussg_lagplot)

ussg_lagplot$sig<-0
ussg_lagplot$sig[ussg_lagplot$low > 0]<-1
ussg_lagplot$sig
ussg_lagplot$sig<-as.factor(ussg_lagplot$sig)

head(ussg_lagplot,20)



##creat two dataframe for making the grouped and specific plots
###and also order the grouped and specific 
ussg_lagplot_specific<-subset(ussg_lagplot,type=="Specific")
ussg_lagplot_specific$zimu<-substr(ussg_lagplot_specific$cause,start=1,stop=1)
order<-as.data.frame(matrix(NA,nrow=6),ncol=2)
head(order)
order[,1]<-c("I","J","K","G","N","F")
order[,2]<-c(1:6)
head(order)
names(order)<-c("zimu","xuhao")
head(order)
ussg_lagplot_specific<-merge(ussg_lagplot_specific,order,by="zimu")
ussg_lagplot_specific<-arrange(ussg_lagplot_specific,desc(xuhao),tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))
head(ussg_lagplot_specific)


ussg_lagplot_grouped<-subset(ussg_lagplot,type=="Grouped")%>%
  arrange(tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

head(ussg_lagplot_grouped)

ussg_lagplot_grouped$cause<-sprintf("%10s", ussg_lagplot_grouped$cause)
ussg_lagplot_grouped<-arrange(ussg_lagplot_grouped,tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

##grouped and specific on the same page of cumlative effect 
#grouped
a<-ggplot(ussg_lagplot_grouped, aes(x = RR, y = cause,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(x = low, xend = high,
                   yend = cause)) + 
  ylab("") +
  xlab("")+
  xlim(-7,7)+ 
  facet_wrap(~lag,nrow=1)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_vline(xintercept = 0, linetype = 2)


##Specific
b<-ggplot(ussg_lagplot_specific, aes(x = RR, y = cause,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(x = low, xend = high,
                   yend = cause)) + 
  ylab("") + 
  xlim(-7,7)+ 
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~lag,nrow=1)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_vline(xintercept = 0, linetype = 2)

grid.arrange(a,b,nrow = 2,heights=c(0.7,1.5))###this plots is better for paper 

###画lag的图


ussg_lagplot_grouped<-arrange(ussg_lagplot_grouped,-tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))


head(ussg_lagplot_grouped)

ggplot(ussg_lagplot_grouped, aes(x = lag, y = RR,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(y = low, yend = high,xend = lag)) + 
  ylab("") +
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~cause,ncol=3)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0, linetype = 2)

ussg_lagplot_specific<-arrange(ussg_lagplot_specific,xuhao,-tn)%>%
  mutate(cause = factor(cause,levels=unique(cause)))

head(ussg_lagplot_specific)

ggplot(ussg_lagplot_specific, aes(x = lag, y = RR,color=sig))+
  geom_point(size=2)+
  geom_segment(aes(y = low, yend = high,xend = lag)) + 
  ylab("") +
  xlab("Percent increase per 10-units increase PM2.5")+
  facet_wrap(~cause,ncol=3)+
  scale_color_manual(values=c("black","red"))+
  theme_few()+
  theme(legend.position="none")+
  geom_hline(yintercept = 0, linetype = 2)

####做一个大表，用ussg_lagplot_grouped和ussg_lagplot_specific两个数据框做。
library(reshape2)
head(ussg_lagplot_grouped)
head(ussg_lagplot_specific)


grouped.new<-select(ussg_lagplot_grouped,-tn,-type,-sig)
options(digits=2)
grouped.new$rrwide<-paste(grouped.new$RR," (", grouped.new$low, ", ",grouped.new$high, ")")
head(grouped.new)
grouped.new.table<-dcast(grouped.new,cause~lag,value.var="rrwide")

specific.new<-select(ussg_lagplot_specific,-tn,-type,-sig)
options(digits=2)
specific.new$rrwide<-paste(specific.new$RR," (", specific.new$low, ", ",specific.new$high, ")")
head(specific.new)
specific.new.table<-dcast(specific.new,cause~lag,value.var="rrwide")

s5.table<-rbind(grouped.new.table,specific.new.table)

write.csv(s5.table,file="s5.table.csv")
