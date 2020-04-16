rm(list=ls())
library(SDSRegressionR)
setwd("C:/Users/Jina Gil/Documents/SNU/Spring 2020/Econ. Growth")
inc <- read.csv("data/netincome_world.csv", stringsAsFactors=FALSE, 
                check.names=FALSE, encoding="UTF-8")
inc_la <- inc[which(inc$region=='Latin America & Caribbean'),]
library(tidyr)
inc_lal <- gather(inc_la, year, growth, '1960':'2019', factor_key=TRUE)
"%not in%" <- Negate("%in%")
inc.h <- inc_lal[inc_lal$ccode %not in% c("ABW", "ATG", "BHS", "BRB", "CRI", "CUB", "CUW", "CYM", 
"GRD", "HTI", "KNA", "LCA", "MAF", "PRI", "SXM", "TCA","VGB", "VIR", "TTO", "VCT", "JAM"),]
inc.major <- inc.h[inc.h$ccode %in% c("ARG", "BRA", "CHL", "COL", "GTM", "PER", "URY",
                                      "MEX", "PRY", "VEN"),]
inc.mf <- inc.major[inc.major$year %not in% c("1960", "1961", "1962", "1963",
                                              "1964", "1965", "1966", "1967", 
                                              "1968", "1969", "2018", "2019"),]
library(ggplot2)
library(dplyr)
library(ggthemes)
ggplot(inc.mf, aes(x=year, y=growth, group=ccode, color=ccode))+geom_line()+theme_bw()

#11-year centered ma creation using plyr
library(plyr)
library(zoo)
library(tidyverse)
library(lubridate)
library(fpp2) 
ma11 <- function(x){
  rollmean(x, 11)
}
inc_ma <- ddply(inc.mf, .(ccode), numcolwise(ma11))
colnames(inc_ma)[2] <- "rolm"
write.csv(inc.mf,"data/inc_mf.csv", row.names = TRUE)
write.csv(inc_ma,"data/inc_ma.csv", row.names = TRUE)
incf <- read.csv("data/incf.csv", stringsAsFactors=FALSE, 
                check.names=FALSE, encoding="UTF-8")
incf_1975<-incf[incf$year %not in% c("1970", "1971", "1972", "1973", "1974"),]
ggplot(incf_1975, aes(x=year, y=rolm, group=ccode, color=ccode))+geom_line()+theme_bw()