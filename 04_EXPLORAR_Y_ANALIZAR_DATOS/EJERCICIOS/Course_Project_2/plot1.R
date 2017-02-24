#################################
# Course Project 2              #
# Exploratory Data Analysis     #
# Author: FVB                   #
# Date: feb/2017                #
#################################

#####################################################################
# Question 1.
# Reading data and plot total sum of PM2.5 tons 
# the processe is : 
#   1. read data
#   2. clean data
#   3. plot
#####################################################################

# set working dir
    setwd("C:/R/proyectos/Data_Science_Coursera/04_EXPLORAR_Y_ANALIZAR_DATOS/EJERCICIOS/Course_Project_2/")

#####################################################################
# 1. Read data from web to R
    # url data source
    data.url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
    destfile.name <- 'neidata.zip'
    download.file(data.url, destfile.name, method = 'auto')# en widows wininet, apple curl

# extract data in data dir
    dir.create("data") # create data dir
    unzip(destfile.name,exdir="data") # unzip files in data directory
    # check if data is in data dir
    dir("./data")

# Read data into R variable
    nei <- readRDS("./data/summarySCC_PM25.rds")
    scc <- readRDS("./data/Source_Classification_Code.rds")

# Show raw data
    head(nei, n=3); tail(nei, n=3)
    str(nei)
    #summary(nei)
    #range(nei$Emissions)

#####################################################################
# 2. Cleaning data.. data seem to be clean 

# Complete observations
    nei <- nei[complete.cases(nei), ]
        # equivalt to : nei<-na.omit(nei) # na.omit returns the object with incomplete cases removed
    # there is no incomplete data 
    # pues el numero de observaciones de str(nei) es igual a str(na.omit(nei)) =str(nei[complete.cases(nei), ])

# NA ... there are no NA values
    ## nei <- nei[!is.na(nei$Emissions), ]
    ## nei <- nei[!is.infinite(nei$Emissions), ] # puede ser?¿

# negative values... there are no negative values
    print("num negative values= "); NROW(nei[nei$Emissions <0,"Emissions"]) # zero
    print("num cero values= "); NROW(nei[nei$Emissions ==0,"Emissions"]) #  328494
    print("num high values> 100000= "); NROW(nei[nei$Emissions >100000,"Emissions"]) # a few 2
    # nei <- nei[nei$Emissions > 0, ] 

#####################################################################
# 3. Plot Question 1..
# make a plot showing the total PM2.5 emission 
# from all sources for each of the years 1999, 2002, 2005, and 2008
#----

# sum Emissions by year
anualsum <- split(nei[, c("Emissions")],as.factor(nei$year))
anualsum <- sapply(anualsum,sum)
str(anualsum)
# Result: vectors x-y 
y<-as.vector(anualsum)
x<-as.vector(names(anualsum))


# Create plot as a png file
png(filename = 'plot1.png',height = 500, width=600, units = "px")
# graph margins
par(mar = c(5,5,2,5)) 

# 1.paint background graph with barplot
    c3 <- rainbow(4, v=0.5,alpha=0.1) # soft colors
    # barplot in bakground
    barplot(y,col=c3,border = c3,space=0.3,axes=F)
    axis(side=4,col = "lightgray", col.axis = "lightgray", lwd = 1)
    mtext(side = 4, line = 3, expression((italic("PM2.5 Tons"))),col = "lightgray")
# 2. line plot
    par(new = T) # new graph in the same device
    plot(x = names(anualsum), y = as.vector(anualsum)/1000000,
         type = 'b',
         main = "Total PM2.5 dust emissions by Year",
         xlab = 'Year',
         ylab = 'PM2.5 Emissions (billion Tons)',
         lwd = 2,
         col="blue", col.lab="blue",
         col.axis = "blue", las=1) # las = axis label horizontal

    # Add label to points
    text(names(anualsum),as.vector(anualsum)/1000000,
        labels=paste(round(as.vector(anualsum)/1000000,digits=1),
        "\n", x),pos=1, cex= 0.7,col = "blue")
#
dev.off()

