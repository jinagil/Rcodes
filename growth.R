rm(list=ls())
library(SDSRegressionR)
setwd("C:/Users/Jina Gil/Documents/SNU/Spring 2020/Int. Development/PPT")
jb <- read.csv("growth.csv", stringsAsFactors=FALSE, 
                check.names=FALSE, encoding="UTF-8")
gr_jb <- gather(jb, year, growth, '1960':'2019', factor_key=TRUE)
write.csv(gr_jb, "growth_l.csv", row.names = TRUE)
japan <- gr_jb[gr_jb$ccode %in% c("JPN"),]
brazil <- gr_jb[gr_jb$ccode %in% c("BRA"),]
ma11 <- function(x){
  rollmean(x, 11)
}
library(plyr)
library(zoo)
library(tidyverse)
library(lubridate)
library(fpp2) 
jp.r <- ddply(japan, .(ccode), numcolwise(ma11))
colnames(jp.r)[2] <- "rolm"
br.r <- ddply(brazil, .(ccode), numcolwise(ma11))
colnames(br.r)[2] <- "rolm"
write.csv(jp.r,"jpr.csv", row.names = TRUE)
write.csv(br.r, "brr.csv", row.names = TRUE)
jpr.m <- read.csv("jprm.csv", stringsAsFactors=FALSE, 
                  check.names=FALSE, encoding="UTF-8")
brr.m <- read.csv("brrm.csv", stringsAsFactors=FALSE, 
                  check.names=FALSE, encoding="UTF-8")
j <- ggplot(jpr.m, aes(x=year, y=rolm, group=ccode, color="Rolling Mean"))+geom_line()+theme_bw() +
  labs(title="GDP per Capita Growth, Japan", subtitle="GDP Growth + 11-year Rolling Mean",
      x="Year", y="Growth (%)")
j <- j + geom_line(aes(y=growth, color="GDP Growth"))
j + labs(colour = "factor")

b <- ggplot(brr.m, aes(x=year, y=rolm, group=ccode, color="Rolling Mean"))+geom_line()+theme_bw() +
  labs(title="GDP per Capita Growth, Brazil", subtitle="GDP Growth + 11-year Rolling Mean",
       x="Year", y="Growth (%)")
b <- b + geom_line(aes(y=growth, color="GDP Growth"))
b + labs(colour = "factor")