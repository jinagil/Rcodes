rm(list=ls())
library(SDSRegressionR)
setwd("C:/Users/Jina Gil/Documents/SNU/Spring 2020/Econ. Growth")
chl <- read.csv("data/chile.csv", stringsAsFactors=FALSE, 
                check.names=FALSE, encoding="UTF-8")
library(ggplot2)
library(dplyr)
library(ggthemes)
scl = 10
g1 <- ggplot(chl, aes(x = year)) + 
  geom_line(aes(y = growth, colour = "GDP per Capita Growth")) + 
  geom_line(aes(y = TFP*scl, colour = "TFP")) +
  scale_y_continuous(sec.axis = sec_axis(~./scl, name = "Index 2011=1"))+
  ggtitle("Chile's GDP per Capita Growth and TFP") +
  labs(x="Year", y="Growth (%)") + theme_light()
g1