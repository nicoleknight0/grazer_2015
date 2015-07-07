#This is an analysis of the data collected by Carolyn Prentice (feeding trials,
#epiphyte data) and Ross Whippo (community composition data) in 2012.
#The goal is to estimate differences in feeding rates of epifaunal grazers in 
#eelgrass communities, as well as differences in their natural abundances

#this is the package we'll use to make plots
library(ggplot2)

#download feeding trial, epiphyte, and community composition data

epi_data<-read.csv("epi_data.csv")
trial_data<-read.csv("feeding_trials.csv")

#header meanings for trial data: replicate- unique code assigned to each
#replicate as XX-YY-#, where XX is grazer species, YY is epiphyte type, and # is
#the replicate number; dmass is final - initial mass over the trial period;
#dmass_per24 is dmass standardized to a 24 h period; dmass_minctrl is dmass
#minus the average dmass value for the corresponding control treatment

#remove controls from feeding trial data (already accounted for in dmass_minctrl)

trial_data<-subset(trial_data,trial_type=="EXPT")
trial_data$time_hrs<-as.factor(trial_data$time_hrs)
trial_data$grazer<-droplevels(trial_data$grazer)

#shiiiiiiiiit very significant effect of time

new<-aov(dmass_minctrl~grazer+epiphyte+time_hrs+grazer*epiphyte,data=trial_data)