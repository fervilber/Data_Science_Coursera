#################################
# Course Project 2              #
# Exploratory Data Analysis     #
# Author: FVB                   #
# Date: feb/2017                #
#################################

#####################################################################
# Question 5.
# How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
#    plot5.R & plot5.png - 
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

# STEP 1.
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
    sub_scc<- as.vector(scc[scc$SCC.Level.Two %in% uni_val,c("SCC")])
    
    sub_scc<- as.vector(scc[scc$SCC.Level.Two %in% uni_val,c("SCC","SCC.Level.Two")])
    
# Baltimore City, fips == "24510"
# select maryland baltimor data
    BaltCity<- nei[nei$fips == "24510",c("Emissions", "year","SCC")]
    BaltCity<-BaltCity[BaltCity$SCC %in% sub_scc$SCC,]
    
    str(BaltCity)
# Join tables for a good graph legend
    library(plyr)
    df<-join(BaltCity,sub_scc)

# STEP 3. Plot summary result by year
    library(ggplot2)

    plot5<-qplot(as.factor(year), data = df, geom = "bar", weight = Emissions,
                 fill=SCC.Level.Two, ylab = "Emission in Tons", xlab="year",
                 main = "Total dust emissions vehicles in Baltimore")
    plot5 <-plot5 + 
            theme(legend.position="top") +
            theme(legend.text = element_text(colour="blue", size=6)) +
            theme(legend.title = element_blank())
              
# plot to png 
    png('plot5.png',height = 500, width=600, units = "px")
        plot5 
    dev.off() # close the connection
    
# Result table
    #aggregate(Emissions ~ year + SCC.Level.Two, df, sum)
    xtabs(Emissions~year,df)
    
