rm(list=ls())
library(SDSRegressionR)
setwd("C:/Users/Jina Gil/Documents/SNU/Spring 2020/Latin America/Assignments")
library(readxl)
ctr <- read.csv("data/comtrade2.csv", stringsAsFactors = FALSE)
library(dplyr)
ctr %>% arrange(country)

#Argentina
arg <- ctr[ctr$country %in% c("Argentina"),]
arg.ex <- arg[arg$tflowcode %in% c(2),]
arg.im <- arg[arg$tflowcode %in% c(1),]
ga.ex <- ggplot(arg.ex, aes(x=Period, y=trade.value, group=comcode, color=comdesc))+geom_line()+theme_bw() + 
  labs(title="Exports", subtitle="Argentina",
       x="Year", y="Values", fill="Types of Exported Goods")
ga.ex
ga.im <-ggplot(arg.im, aes(x=Period, y=trade.value, group=comcode, color=comdesc))+geom_line()+theme_bw() + 
  labs(title="Imports", subtitle="Argentina",
       x="Year", y="Values", fill="Types of Imported Goods")
ga.im

#Brazil
brz <- ctr[ctr$country %in% c("Brazil"),]
brz.ex <- brz[brz$tflowcode %in% c(2),]
brz.im <- brz[brz$tflowcode %in% c(1),]
gb.ex <- ggplot(brz.ex, aes(x=Period, y=trade.value, group=comcode, color=comdesc))+geom_line()+theme_bw() + 
  labs(title="Exports", subtitle="Brazil",
       x="Year", y="Values", fill="Types of Exported Goods")
gb.ex
gb.im <-ggplot(brz.im, aes(x=Period, y=trade.value, group=comcode, color=comdesc))+geom_line()+theme_bw() + 
  labs(title="Imports", subtitle="Brazil",
       x="Year", y="Values", fill="Types of Imported Goods")
gb.im
#Chile
chile <- ctr[ctr$country %in% c("Chile"),]
chile.ex <- chile[chile$tflowcode %in% c(2),]
chile.im <- chile[chile$tflowcode %in% c(1),]
gc.ex <- ggplot(chile.ex, aes(x=Period, y=trade.value, group=comcode, color=comdesc))+geom_line()+theme_bw() + 
  labs(title="Exports", subtitle="Chile",
       x="Year", y="Values", fill="Types of Exported Goods")
gc.ex
gc.im <-ggplot(chile.im, aes(x=Period, y=trade.value, group=comcode, color=comdesc))+geom_line()+theme_bw() + 
  labs(title="Imports", subtitle="Chile",
       x="Year", y="Values", fill="Types of Imported Goods")
gc.im

library(readstata13)
pwt <- read.dta13("C:/Users/Jina Gil/Documents/SNU/Spring 2020/Econ. Growth/data/pwt91.dta")
pwt$gcap <- pwt$rgdpna / pwt$pop
pwt.g = pwt %>%
  arrange(countrycode) %>%
  mutate(dif.year = year - lag(year),
         dif.growth = gcap -lag(gcap),
         grate = (dif.growth / dif.year)/gcap*100)
sum(pwt.g$gcap)
argen <- pwt.g[pwt.g$countrycode %in% c("ARG"),]
chi <- pwt.g[pwt.g$countrycode %in% c("CHL"),]
brazil <- pwt.g[pwt.g$countrycode %in% c("BRA"),]
scl=20
  
## First, Argentina
g_a <- ggplot(argen, aes(x=year, y=grate, color=countrycode))+
  geom_line()+theme_bw()+ geom_line(aes(y = ctfp*scl, colour = "TFP")) +
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Index 2011=1"))+
  labs(title="GDP per Capita Growth Rate", subtitle="Argentina",
       x="Year", y="Growth (%)")
g_a + labs(colour = "factor")

## Second, Brazil 
g_bra <- ggplot(brazil, aes(x=year, y=grate, color=countrycode))+
  geom_line()+theme_bw()+ geom_line(aes(y = ctfp*scl, colour = "TFP")) +
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Index 2011=1"))+
  labs(title="GDP per Capita Growth Rate", subtitle="Brazil",
       x="Year", y="Growth (%)")
g_bra + labs(colour = "factor")

## Third, Chile
g_c <- ggplot(chi, aes(x=year, y=grate, color=countrycode))+
  geom_line()+theme_bw()+ geom_line(aes(y = ctfp*scl, colour = "TFP")) +
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Index 2011=1"))+
  labs(title="GDP per Capita Growth Rate", subtitle="Chile",
       x="Year", y="Growth (%)")
g_c + labs(colour = "factor")

# Regression
arg_m <- arg[arg$comcode %in% c(6),]
arg_m <- merge(x = arg_m, y = argen[ , c("year", "grate")], by = "year", all.x=TRUE)
library(plm)
arg_me <- arg_m[arg_m$tflowcode %in% c(2),]
arg.r <- plm(grate ~ log1p(trade.value), data=arg_me, model="within")