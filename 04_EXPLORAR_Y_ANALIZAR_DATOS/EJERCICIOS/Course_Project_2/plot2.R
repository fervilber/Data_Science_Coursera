#################################
# Course Project 2              #
# Exploratory Data Analysis     #
# Author: FVB                   #
# Date: feb/2017                #
#################################

#####################################################################
# Question 2.
# Have total emissions from PM2.5 decreased in the Baltimore City,
# Maryland (`fips == "24510"`) from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.
#####################################################################

#####################################################################
# for reading data see Plot1.R

#####################################################################
# select maryland baltimor data
    maryland<- nei[nei$fips == "24510",c("Emissions", "year")]
    str(maryland)
    # other option use of subset---- maryland <- subset (nei, fips == "24510")
    # num of observation
    NROW(maryland)

# split data by year
    maryland<- split(maryland[, c("Emissions")],as.factor(maryland$year))
    maryland_sum <- sapply(maryland,sum)
    str(maryland)
    # other option use of dplyr --- aggregate(Emissions ~ year, maryland, sum)

# Result xy for plotting
    y<-as.vector(maryland_sum)
    x<-as.vector(names(maryland_sum))

# Create Plot2 as a png file
    png(filename = 'plot2.png',height = 500, width=600, units = "px")
    ## paint graph
    par(mar = c(5,5,5,2)) # margin(down,left,up,right) 
    # barplot for background
    c3 <- rainbow(4, v=0.5,alpha=0.1)
    barplot(y,col=c3,border = c3,space=0.3,axes=F)
    axis(side=4,col = "lightgray", col.axis = "lightgray", lwd = 1)
    #mtext(side = 4, line = 3, expression((italic("PM2.5 Tons"))),col = "lightgray")
    
    ## line plot foreground
    par(new = T)
    plot(x = x, y = y,
         type = 'b',pch = 16,
         main = "Total PM2.5 dust emissions by year \n Baltimore City" ,
         xlab = 'Year',
         ylab = 'PM2.5 Emissions (Tons)',
         lwd = 2, lty = 2,
         col="blue", col.lab="blue",
         col.axis = "black", las=1) # las = axis label horizontal
    ## label points
    text(x,y,labels=paste(round(y,digits=1),"\n", x),pos=1, cex= 0.7,col = "blue")
# close dev
dev.off()
