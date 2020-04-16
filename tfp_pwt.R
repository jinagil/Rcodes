
# Thesis: Latin America's affect of TFP on GDP is lower? Because of Commodity? 

rm(list=ls())
library(SDSRegressionR)
setwd("s")
library(foreign)
library(readstata13)
pwt <- read.dta13("data/pwt91.dta")

##GDP per Capita
pwt$gcap <- pwt$rgdpna / pwt$pop

##per Capita Growth Rate
pwt.g = pwt %>%
  arrange(countrycode) %>%
  mutate(dif.year = year - lag(year),
         dif.growth = gcap -lag(gcap),
         grate = (dif.growth / dif.year)/gcap*100)
library(plm)
chile <- pwt.g[pwt.g$countrycode %in% c("CHL"),]
brazil <- pwt.g[pwt.g$countrycode %in% c("BRA"),]
la <- pwt.g[pwt.g$countrycode %in% c("ARG", "BRA", "CHL", "COL", "GTM", "PER", "URY",
                                 "MEX", "PRY", "VEN"),] 

# simple graphs
library(ggthemes)
g_all_growth <- ggplot(la, aes(x=year, y=rgdpna, group=countrycode, color=countrycode))+
  geom_line()+theme_bw()
g_all_growth
g_rate <- ggplot(la, aes(x=year, y=grate, group=countrycode, color=countrycode))+geom_line()+theme_bw()
g_rate
g_tfp <- ggplot(la, aes(x=year, y=ctfp, group=countrycode, color=countrycode))+
  geom_line()+theme_bw() + 
  labs(title="TFP", subtitle="10 States of Latin America",
              x="Year", y="Growth (%)")
g_tfp
g_capital <- ggplot(la, aes(x=year, y=rnna, group=countrycode, color=countrycode))+
                       geom_line()+theme_bw() + 
                       labs(title="Capital Stock", subtitle="10 States of Latin America",
                            x="Year", y="Growth (%)")
g_capital
g_lab <- ggplot(la, aes(x=labsh, y=ctfp, group=countrycode, color=countrycode))+
  geom_line()+theme_bw() + 
  labs(title="Labor Compensation share in GDP", subtitle="10 States of Latin America",
       x="Year", y="Growth (%)")
g_lab
g_hc <- ggplot(la, aes(x=year, y=hc, group=countrycode, color=countrycode))+
  geom_line()+theme_bw() + 
  labs(title="Human Capital Index", subtitle="10 States of Latin America",
       x="Year", y="Growth (%)")
g_hc

# Pick some Commodity-related nations
## 1. Saudi Arabia (Oil) & Chile (Copper)
saudi <- pwt.g[pwt.g$countrycode %in% c("SAU"),]
scl = 20
g_c <- ggplot(chile, aes(x=year, y=grate, color=countrycode))+
  geom_line()+theme_bw()+ geom_line(aes(y = ctfp*scl, colour = "TFP")) +
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Index 2011=1"))+
  labs(title="GDP per Capita Growth Rate", subtitle="Chile",
       x="Year", y="Growth (%)")
g_c + labs(colour = "factor")
g_saudi <- ggplot(saudi, aes(x=year, y=grate, color=countrycode))+
  geom_line()+theme_bw()+ geom_line(aes(y = ctfp*scl, colour = "TFP")) +
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Index 2011=1"))+
  labs(title="GDP per Capita Growth Rate", subtitle="Saudi Arabia",
       x="Year", y="Growth (%)")
g_saudi + labs(colour = "factor")
##2. Brazil (Agriculture)
g_bra <- ggplot(brazil, aes(x=year, y=grate, color=countrycode))+
  geom_line()+theme_bw()+ geom_line(aes(y = ctfp*scl, colour = "TFP")) +
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Index 2011=1"))+
  labs(title="GDP per Capita Growth Rate", subtitle="Brazil",
       x="Year", y="Growth (%)")
g_bra + labs(colour = "factor")
##3. Korea (Tech)
korea <- pwt.g[pwt.g$countrycode %in% c("KOR"),]
g_kor <- ggplot(korea, aes(x=year, y=grate, color=countrycode))+
  geom_line()+theme_bw()+ geom_line(aes(y = ctfp*scl, colour = "TFP")) +
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Index 2011=1"))+
  labs(title="GDP per Capita Growth Rate", subtitle="Republic of Korea",
       x="Year", y="Growth (%)")
g_kor + labs(colour = "factor")

