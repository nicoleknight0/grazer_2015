#This is an analysis of the data collected by Carolyn Prentice (feeding trials,
#epiphyte data) and Ross Whippo (community composition data) in 2012.
#The goal is to estimate differences in feeding rates of epifaunal grazers in 
#eelgrass communities, as well as differences in their natural abundances

#this is the package we'll use to make plots
library(ggplot2)

#download feeding trial, epiphyte, and community composition data
#trial_data.24 only includes data from the 24 h trials

epi_data<-read.csv("epi_data.csv")
trial_data.24<-read.csv("feeding_trials_24h.csv")
trial_data<-read.csv("feeding_trials.csv")

#header meanings for trial data: replicate- unique code assigned to each
#replicate as XX-YY-#, where XX is grazer species, YY is epiphyte type, and # is
#the replicate number; dmass is final - initial mass over the trial period;
#dmass_per24 is dmass standardized to a 24 h period; dmass_minctrl is dmass
#minus the average dmass value for the corresponding control treatment
#dmass_stnd is change in mass, minus the effect of control, divided
#by grazer mass

#remove controls from feeding trial data (already accounted for in dmass_minctrl)

trial_data<-subset(trial_data,trial_type=="EXPT")
trial_data$time_hrs<-as.factor(trial_data$time_hrs)
trial_data$dmass_stnd<-as.numeric(trial_data$dmass_stnd)
trial_data$grazer<-droplevels(trial_data$grazer)

trial_data.24<-subset(trial_data.24,trial_type=="EXPT")
trial_data.24$time_hrs<-as.factor(trial_data.24$time_hrs)
trial_data.24$grazer<-droplevels(trial_data.24$grazer)
trial_data.24$dmass_stnd<-as.numeric(trial_data.24$dmass_stnd)


#shiiiiiiiiit very significant effect of time in lm.1, grazer effect
#still significant but not sure that means anything.  All of the
#p taylori treatments are 36 hrs, the iso/amphipod treatments are 11,
#21, 24, or 48 hours

lm.1<-aov(dmass_minctrl~grazer+epiphyte+time_hrs,data=trial_data)

#run this command to see effect of trial time across all treatments

#plot(trial_data$time_hrs,trial_data$dmass_stnd)

#try with only data from 24 h
lm.2<-aov(dmass_minctrl~grazer+epiphyte+grazer*epiphyte,data=trial_data.24)

#try standardized to weight

lm.3<-aov(dmass_stnd~grazer+epiphyte+time_hrs,data=trial_data)

lm.4<-aov(dmass_stnd~grazer+epiphyte+grazer*epiphyte,data=trial_data.24)