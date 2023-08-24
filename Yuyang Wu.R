rm(list=ls())
setwd("D:/Imperial College/final project")

library(tidyverse)
library(ggplot2)
library(dismo)
library(raster)
library(rstatix)
library(ggpubr)
library(ggtext)

RBmarking<-read.csv("EXP1.1.csv")
concentration<-as.factor(c(0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.4, 0.4, 0.4, 0.8, 0.8, 0.8, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.4, 0.4, 0.4, 0.8, 0.8, 0.8, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.4, 0.4, 0.4, 0.8, 0.8, 0.8, 0.1, 0.1, 0.1, 0.2, 0.2, 0.2, 0.4, 0.4, 0.4, 0.8, 0.8, 0.8))
feeding_days<-as.factor(c(1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,4))
RB<-mutate(RBmarking, concentration, feeding_days)
RB<-RB%>%rename(RB_concentration=concentration)

##FG1
ggplot(RB, aes(x=feeding_days, y=Rate..BM., fill=RB_concentration)) +
  stat_boxplot(geom = "errorbar",
               width=0.3,position = position_dodge(0.5)) +
  geom_boxplot(width=0.3,position = position_dodge(0.5))+
  labs(y="Percent of Males with Visual RB Staining (%)")
##FG2
ggplot(RB, aes(x=feeding_days,RB$Rate..MS., fill=RB_concentration)) +
  stat_boxplot(geom = "errorbar",
               width=0.3,position = position_dodge(0.5)) +
  geom_boxplot(width=0.3,position = position_dodge(0.5))+
  labs(y="Percent of Fertilized Females obtaining Sperm that Positive for RB (%)")

#### Anova test
RB1<-subset(RB,feeding_days=="1", select = c(Rate..BM.,Rate..MS., RB_concentration))
SIG1_RBcon <- RB1[c("RB_concentration","Rate..BM.")] 
SIG2_RBcon <- RB1[c("RB_concentration","Rate..MS.")]

RB2<-subset(RB,feeding_days=="2", select = c(Rate..BM.,Rate..MS., RB_concentration))
SIG3_RBcon <- RB2[c("RB_concentration","Rate..BM.")] 
SIG4_RBcon <- RB2[c("RB_concentration","Rate..MS.")]

RB3<-subset(RB,feeding_days=="3", select = c(Rate..BM.,Rate..MS., RB_concentration))
SIG5_RBcon <- RB3[c("RB_concentration","Rate..BM.")] 
SIG6_RBcon <- RB3[c("RB_concentration","Rate..MS.")]

RB4<-subset(RB,feeding_days=="4", select = c(Rate..BM.,Rate..MS., RB_concentration))
SIG7_RBcon <- RB4[c("RB_concentration","Rate..BM.")] 
SIG8_RBcon <- RB4[c("RB_concentration","Rate..MS.")]

RB5<-subset(RB,RB_concentration=="0.1", select = c(Rate..BM.,Rate..MS., feeding_days))
SIG9_RBcon <- RB5[c("feeding_days","Rate..BM.")] 
SIG10_RBcon <- RB5[c("feeding_days","Rate..MS.")]

RB6<-subset(RB,RB_concentration=="0.2", select = c(Rate..BM.,Rate..MS., feeding_days))
SIG11_RBcon <- RB6[c("feeding_days","Rate..BM.")] 
SIG12_RBcon <- RB6[c("feeding_days","Rate..MS.")]

RB7<-subset(RB,RB_concentration=="0.4", select = c(Rate..BM.,Rate..MS., feeding_days))
SIG13_RBcon <- RB7[c("feeding_days","Rate..BM.")] 
SIG14_RBcon <- RB7[c("feeding_days","Rate..MS.")]

RB8<-subset(RB,RB_concentration=="0.8", select = c(Rate..BM.,Rate..MS., feeding_days))
SIG15_RBcon <- RB8[c("feeding_days","Rate..BM.")] 
SIG16_RBcon <- RB8[c("feeding_days","Rate..MS.")]

library(multcompView)
library(ggsci)

df_SIG1<- SIG1_RBcon %>% as_tibble()
anova_SIG1 <- aov(Rate..BM.~RB_concentration,data=SIG1_RBcon)
summary(anova_SIG1)
tukey_SIG1 <- TukeyHSD(anova_SIG1)
tukey_SIG1

df_SIG2 <- SIG2_RBcon %>% as_tibble()
anova_SIG2 <- aov(Rate..MS.~ RB_concentration,data=SIG2_RBcon)
summary(anova_SIG2)
tukey_SIG2 <- TukeyHSD(anova_SIG2)
tukey_SIG2

df_SIG3<- SIG3_RBcon %>% as_tibble()
anova_SIG3 <- aov(Rate..BM.~RB_concentration,data=SIG3_RBcon)
summary(anova_SIG3)
tukey_SIG3 <- TukeyHSD(anova_SIG1)
tukey_SIG3

df_SIG4 <- SIG4_RBcon %>% as_tibble()
anova_SIG4 <- aov(Rate..MS.~ RB_concentration,data=SIG4_RBcon)
summary(anova_SIG4)
tukey_SIG4 <- TukeyHSD(anova_SIG4)
tukey_SIG4

df_SIG5<- SIG5_RBcon %>% as_tibble()
anova_SIG5 <- aov(Rate..BM.~RB_concentration,data=SIG5_RBcon)
summary(anova_SIG5)
tukey_SIG5 <- TukeyHSD(anova_SIG5)
tukey_SIG5

df_SIG6 <- SIG6_RBcon %>% as_tibble()
anova_SIG6 <- aov(Rate..MS.~ RB_concentration,data=SIG6_RBcon)
summary(anova_SIG6)
tukey_SIG6 <- TukeyHSD(anova_SIG2)
tukey_SIG6

df_SIG7<- SIG7_RBcon %>% as_tibble()
anova_SIG7 <- aov(Rate..BM.~RB_concentration,data=SIG7_RBcon)
summary(anova_SIG7)
tukey_SIG7 <- TukeyHSD(anova_SIG7)
tukey_SIG7

df_SIG8 <- SIG8_RBcon %>% as_tibble()
anova_SIG8 <- aov(Rate..MS.~ RB_concentration,data=SIG8_RBcon)
summary(anova_SIG8)
tukey_SIG8 <- TukeyHSD(anova_SIG8)
tukey_SIG8

df_SIG9<- SIG9_RBcon %>% as_tibble()
anova_SIG9 <- aov(Rate..BM.~feeding_days,data=SIG9_RBcon)
summary(anova_SIG9)
tukey_SIG9 <- TukeyHSD(anova_SIG9)
tukey_SIG9

df_SIG10<- SIG10_RBcon %>% as_tibble()
anova_SIG10 <- aov(Rate..MS.~feeding_days,data=SIG10_RBcon)
summary(anova_SIG10)
tukey_SIG10 <- TukeyHSD(anova_SIG10)
tukey_SIG10

df_SIG11<- SIG11_RBcon %>% as_tibble()
anova_SIG11 <- aov(Rate..BM.~feeding_days,data=SIG11_RBcon)
summary(anova_SIG11)
tukey_SIG11 <- TukeyHSD(anova_SIG11)
tukey_SIG11

df_SIG12<- SIG12_RBcon %>% as_tibble()
anova_SIG12 <- aov(Rate..MS.~feeding_days,data=SIG12_RBcon)
summary(anova_SIG12)
tukey_SIG12 <- TukeyHSD(anova_SIG12)
tukey_SIG12

df_SIG13<- SIG13_RBcon %>% as_tibble()
anova_SIG13 <- aov(Rate..BM.~feeding_days,data=SIG13_RBcon)
summary(anova_SIG13)
tukey_SIG13 <- TukeyHSD(anova_SIG13)
tukey_SIG13

df_SIG14<- SIG14_RBcon %>% as_tibble()
anova_SIG14 <- aov(Rate..MS.~feeding_days,data=SIG14_RBcon)
summary(anova_SIG14)
tukey_SIG14 <- TukeyHSD(anova_SIG14)
tukey_SIG14


df_SIG15<- SIG15_RBcon %>% as_tibble()
anova_SIG15 <- aov(Rate..BM.~feeding_days,data=SIG15_RBcon)
summary(anova_SIG15)
tukey_SIG15 <- TukeyHSD(anova_SIG15)
tukey_SIG15

df_SIG16<- SIG16_RBcon %>% as_tibble()
anova_SIG16 <- aov(Rate..MS.~feeding_days,data=SIG16_RBcon)
summary(anova_SIG16)
tukey_SIG16 <- TukeyHSD(anova_SIG16)
tukey_SIG16

#### overall CONCENTRATION
df1 <- RB %>% as_tibble()
anova1 <- aov(df1$Rate..BM.~df1$RB_concentration,data=df1)
summary(anova1)
tukey1 <- TukeyHSD(anova1)
tukey1

df2 <- RB %>% as_tibble()
anova2 <- aov(df2$Rate..MS.~df2$RB_concentration,data=df2)
summary(anova2)
tukey2 <- TukeyHSD(anova2)
tukey2

df3 <- RB %>% as_tibble()
anova3 <- aov(df3$Rate..BM.~df3$feeding_days,data=df3)
summary(anova3)
tukey3 <- TukeyHSD(anova3)
tukey3

df4 <- RB %>% as_tibble()
anova4 <- aov(df4$Rate..MS.~df4$feeding_days,data=df4)
summary(anova4)
tukey4 <- TukeyHSD(anova4)
tukey4
