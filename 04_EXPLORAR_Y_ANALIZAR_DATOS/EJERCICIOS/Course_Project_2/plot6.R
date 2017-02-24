#################################
# Course Project 2              #
# Exploratory Data Analysis     #
# Author: FVB                   #
# Date: feb/2017                #
#################################

#####################################################################
# Question 6.
# Compare emissions from motor vehicle sources in Baltimore City with 
# emissions from motor vehicle sources in Los Angeles County, 
# California (fips == "06037").
# Which city has seen greater changes over time in motor vehicle emissions?
#    plot6.R & plot6.png - Graphs the change in PM2.5 emission from motor 
#                           vehicles for Baltimore and Los Angeles County 
#####################################################################

#####################################################################
# For details in reading data see Plot1.R
    # set working dir
    setwd("C:/R/proyectos/Data_Science_Coursera/04_EXPLORAR_Y_ANALIZAR_DATOS/EJERCICIOS/Course_Project_2/")
    #Read data into dataframes
    nei <- readRDS("./data/summarySCC_PM25.rds")
    scc <- readRDS("./data/Source_Classification_Code.rds")
#####################################################################
# 
# 1. Look in  scc datatable for vehicle 
# 2. Look in  nei datatable for Baltimore
# 3. Match this 2 tables 
# 3. Plot sum emission data by year.

# STEP 1... same as in Question 5
# Search each column for fields containing the word "vehicle" in scc dataframe
    rm(a)
    a<-1
    for (i in seq(scc)) {
        if (i==1) {
            a<-NROW(unique(grep("Vehicle",scc[,i],value=T,ignore.case=T)));
        }
        else    {
            a <- c(a, NROW(unique(grep("Vehicle",scc[,i],value=T,ignore.case=T))));
        }
    }
    a
# There are 5 col or varibles for further clasification vehicle
# col[8] = SCC.Level.Two seems to be a good way for a city 
    uni_val<-unique(grep("Vehicle",scc[,8],value=T,ignore.case=T))
# select de subset with this values
    sub_scc<- as.vector(scc[scc$SCC.Level.Two %in% uni_val,c("SCC","SCC.Level.Two")])

# Baltimore City, fips == "24510" --Los Angeles County fips == "06037"
# subset data for the two places:
    comp_df<- nei[nei$fips == "24510" | nei$fips == "06037",c("Emissions", "year","SCC","fips")]
# subset vehicle data from scc    
    comp_df<-comp_df[comp_df$SCC %in% sub_scc$SCC,]
   str(comp_df)

# Join tables for a good graph legend and clasification
    library(plyr)
    df<-join(comp_df,sub_scc)
# siplify table with aggregate
    df<-aggregate(Emissions ~ year+fips+SCC.Level.Two, df, sum)
# change flip num for name
    df$fips[df$fips == "24510" ]<-"Baltimore"
    df$fips[df$fips == "06037" ]<-"Los Angeles"

# Plot comparative graph by year
library(ggplot2)

plot6<-qplot(as.factor(year), data = df, geom = "bar", weight = Emissions, facets = . ~ fips,
             fill=SCC.Level.Two, ylab = "Emission in Tons", xlab="year",
             main = "Total PM2.5 emissions vehicles Baltimore vs LA")
plot6 <-plot6 + 
    theme(legend.position="top") +
    theme(legend.text = element_text(colour="blue", size=6)) +
    theme(legend.title = element_blank())

# plot to png 
png('plot6.png',height = 500, width=600, units = "px")
    plot6
dev.off() # close the connection








# Result table
#aggregate(Emissions ~ year + SCC.Level.Two, df, sum)
xtabs(Emissions~year,df)

ggplot(df, aes(x=as.factor(year),y=Emissions,fill=SCC.Level.Two)) +
    geom_bar(stat="identity") +
    #geom_text(aes(label=Emissions), vjust=1.5, colour="white",size=3) +
    facet_grid(. ~ fips ,margins = FALSE) 
