#################################
# Course Project 2              #
# Exploratory Data Analysis     #
# Author: FVB                   #
# Date: feb/2017                #
#################################

#####################################################################
# Question 3.
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad)
# variable, which of these four sources have seen decreases in emissions from 1999-2008
# for Baltimore City? Which have seen increases in emissions from 1999-2008?
# Use the ggplot2 plotting system to make a plot answer this question. 
#####################################################################

#####################################################################
# For details in reading data see Plot1.R
# set working dir
    setwd("C:/R/proyectos/Data_Science_Coursera/04_EXPLORAR_Y_ANALIZAR_DATOS/EJERCICIOS/Course_Project_2/")
#Read data into dataframes
    nei <- readRDS("./data/summarySCC_PM25.rds")
    scc <- readRDS("./data/Source_Classification_Code.rds")
#####################################################################

library(ggplot2)

# select maryland baltimor data
BaltCity<- nei[nei$fips == "24510",c("Emissions", "year","type")]
str(BaltCity)
# other option use of subset---- maryland <- subset (nei, fips == "24510")
# num of observation
NROW(BaltCity)

# It is not necesary for the plot but if wanted the data table
# here are 3 ways of calculate the cros data table
# 1. xtabs
    ##xta<-xtabs(Emissions~year + type,BaltCity)
    # to test the results take a look at some
    # sum(BaltCity[BaltCity$year == 1999 & BaltCity$type == "POINT",1])
# 2. aggregatee
     xta<-aggregate(Emissions ~ year + type, BaltCity, sum)
# 3. library(dplyr)
    ## xta <- nei %>%
    ##    filter(fips == 24510) %>%
    ##    group_by(year, type) %>%
    ##    summarize(total.emissions = sum(Emissions))

# Create the plot3
plot3<-qplot(year,Emissions,data=xta, geom=c("point","line"),size=I(2),
     color = type, ylab = "Total emission (Tons)", xlab="year",
     main = "PM2.5 Emissions in Baltimore City")

# legend and axis format
plot3<- plot3 + theme(axis.title = element_text(face="bold.italic",
     size="12", color="brown"), legend.position="right") 

# save to png device 
png('plot3.png',height = 500, width=600, units = "px")
    plot3 
dev.off() # close the connection

## other good graph option

plot3.1<-qplot(as.factor(year), data = BaltCity, geom = "bar", weight = Emissions,
             fill = type, ylab = "Total emission (Tons)", xlab="year",
             main = "PM2.5 Emissions in Baltimore City")
png('plot3_1.png',height = 500, width=600, units = "px")
    plot3.1 
dev.off() # close the connection


