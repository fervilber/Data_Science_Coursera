#################################
# Course Project 2              #
# Exploratory Data Analysis     #
# Author: FVB                   #
# Date: feb/2017                #
#################################

#####################################################################
# Question 4.
# Across the United States, how have emissions from coal combustion-related
# sources changed from 1999-2008?
#    plot4.R & plot4.png - Graphs PM2.5 emissions from coal combustion sources for the United States 
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
# 1. Look in  scc datatable for coal combustion-related sources
# 2. Match this output with nei dataset, with SCC-column as Index
# 3. Plot sum emission data by year.

# STEP 1.
# Search each column for fields containing the word "coal" in scc dataframe
# look for the number of unique "coal" in each col of scc
rm(a)
a<-1
for (i in seq(scc)) {
    if (i==1) {
        a<-NROW(unique(grep("coal",scc[,i],value=T,ignore.case=T)));
    }
    else    {
        a <- c(a, NROW(unique(grep("coal",scc[,i],value=T,ignore.case=T))));
    }
}
a
# columns 3, 4, 9, 10 has coal in values but
# col 4= "EI.Sector" is the better clasification with 3 unique values of combustion and coal.
# other col has no specific names or use coal as general partition
# save a vector with the 
    uni_val<-unique(grep("coal",scc[,4],value=T,ignore.case=T))

# so we choose this field=col[4]=$EI.Sector for subset coal combustion-related sources
# subset the rows in SCC
    sub_scc<- scc[scc$EI.Sector %in% uni_val,"SCC"]
    # or  subset(scc, EI.Sector %in% uni_val, select=scc)

# STEP 2.Match with nei dataset
# subset the rows in nei datatable that contain sub_scc names. Select 3 cols of nei
    sub_nei<- nei[nei$SCC %in% sub_scc,c("Emissions", "year", "type")]
    # convert tons to billion (10^6) tons for better graph scale output
    sub_nei$Emissions<-sub_nei$Emissions/1000000
    
# STEP 3. Plot summary result by year
    library(ggplot2)
    plot4<-qplot(as.factor(year), data = sub_nei, geom = "bar", weight = Emissions,
               fill=type, ylab = "Emission in billion Tons", xlab="year",
               main = "Total annual Emission (Tons) from coal combustion USA")
# plot to png 
png('plot4.png',height = 500, width=600, units = "px")
    plot4 
dev.off() # close the connection
