rm(list=ls())
library(SDSRegressionR)
setwd("C:/Users/Jina Gil/Documents/SNU/Spring 2020/Econ. Growth")
library(readxl)
cap2012 <- read_excel("data/usgdp2012.xlsx")
cap1997 <- read_excel("data/usgdp1997.xlsx")
cap19c <- read_excel("data/usgcap19c.xlsx")
cap12.g = cap2012 %>%
  arrange(name) %>%
  mutate(dif.year = year - lag(year),
         dif.growth = gcap -lag(gcap),
         grate = (dif.growth / dif.year)/gcap*100)

cap97.g= cap1997 %>%
  arrange(name) %>%
  mutate(dif.year = year - lag(year),
         dif.growth = gcap -lag(gcap),
         grate = (dif.growth / dif.year)/gcap*100)
cap19c.g <- cap19c %>%
  arrange(name) %>%
  mutate(dif.year = year - lag(year),
         dif.growth = gcap -lag(gcap),
         grate = (dif.growth / dif.year)/gcap*100)
g.cap19 <- ggplot(cap19c.g, aes(x=year, y=grate, group=name, color=name))+geom_line()+theme_bw() + 
  labs(title="GDP per Capita Growth (%)", subtitle="U.S. growths",
       x="Year", y="GDP per Capita Growth (%)", fill="Regions")
g.cap19
onlyus97<- cap97.g[cap97.g$region %in% c("none"),]
onlyus19 <- cap12.g[cap12.g$region %in% c("none"),]
g.us <- ggplot(onlyus97, aes(x=year, y=grate, group=name, color=name))+geom_line()+theme_bw() + 
  labs(title="GDP per Capita Growth (%)", subtitle="U.S. growths",
       x="Year", y="GDP per Capita Growth (%)", fill="Regions")
g.us

## Research question: gdp per capita growth, driven by tech or natural resource?
#States
farwest97 <- cap97.g[cap97.g$region %in% c("farwest"),]
farwest19 <- cap12.g[cap12.g$region %in% c("farwest"),]
regions97 <- cap97.g[cap97.g$area %in% c("region"),]
regions19 <- cap12.g[cap12.g$area %in% c("region"),]
g.r97 <- ggplot(regions97, aes(x=year, y=grate, group=name, color=name))+geom_line()+theme_bw() + 
  labs(title="GDP per Capita Growth (%)", subtitle="U.S. State Regions",
       x="Year", y="GDP per Capita Growth (%)", fill="Regions")
g.r97
g.r19 <- ggplot(regions19, aes(x=year, y=grate, group=name, color=name))+geom_line()+theme_bw() + 
  labs(title="GDP per Capita Growth (%)", subtitle="U.S. State Regions",
       x="Year", y="GDP per Capita Growth (%)", fill="Regions")
g.r19
g.r97c <- ggplot(regions97, aes(x=year, y=gcap, group=name, color=name))+geom_line()+theme_bw() + 
  labs(title="GDP per Capita Growth (%)", subtitle="U.S. State Regions",
       x="Year", y="$ (1997)", fill="Regions")
g.r97c
g.far97 <- ggplot(farwest97, aes(x=year, y=grate, group=name, color=name))+geom_line()+theme_bw() + 
  labs(title="GDP per Capita Growth (%)", subtitle="U.S. States",
       x="Year", y="GDP per Capita Growth (%)", fill="Regions")
g.far97
g.far19 <- ggplot(farwest19, aes(x=year, y=grate, group=name, color=name))+geom_line()+theme_bw() + 
  labs(title="GDP per Capita Growth (%)", subtitle="U.S. States",
       x="Year", y="GDP per Capita Growth (%)", fill="Regions")
g.far19
g.far97.cap <- ggplot(farwest97, aes(x=year, y=gcap, group=name, color=name))+geom_line()+theme_bw() + 
  labs(title="GDP per Capita", subtitle="U.S. States",
       x="Year", y="$ (1997)", fill="Regions")
g.far97.cap
g.far19.cap <- ggplot(farwest19, aes(x=year, y=gcap, group=name, color=name))+geom_line()+theme_bw() + 
  labs(title="GDP per Capita", subtitle="U.S. States",
       x="Year", y="$ (1997)", fill="Regions")
g.far19.cap

south97 <- cap97.g[cap97.g$region %in% c("southeast", "southwest"),]
south19 <- cap12.g[cap12.g$region %in% c("southeast", "southwest"),]
g.south97 <- ggplot(south97, aes(x=year, y=gcap, group=name, color=name))+geom_line()+theme_bw() + 
  labs(title="GDP per Capita", subtitle="U.S. States",
       x="Year", y="$ (1997)", fill="Regions")
g.south97
g.south97.g <- ggplot(south97, aes(x=year, y=grate, group=name, color=name))+geom_line()+theme_bw() + 
  labs(title="GDP per Capita Growth (%)", subtitle="U.S. States",
       x="Year", y="GDP per Capita growth (%)", fill="Regions")
g.south97.g
g.south19 <- ggplot(south19, aes(x=year, y=gcap, group=name, color=name))+geom_line()+theme_bw() + 
  labs(title="GDP per Capita", subtitle="U.S. States",
       x="Year", y="$ (2012)", fill="Regions")
g.south19

##Industries?
ind2010 <- read_excel("data/usind2010.xlsx")
"%not in%" <- Negate("%in%")
ind.group <- ind2010[ind2010$type %in% c(1, 2, 3),]
ind.big <-ind2010[ind2010$type %in% c("goods", "service", "info"),]
g.ind <-ggplot(ind.group, aes(x=year, y=percent, group=name, color=name))+geom_line()+theme_bw() + 
  labs(title="Percentage of Industry for GDP", subtitle="Percentage %",
       x="Year", y="%", fill="Regions")
g.ind
g.big <- ggplot(ind.big, aes(x=year, y=percent, group=name, color=name))+geom_line()+theme_bw() + 
  labs(title="Percentage of Industry for GDP", subtitle="Percentage %",
       x="Year", y="%", fill="Regions")
g.big