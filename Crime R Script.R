#Data on the Rocks
#March 2017
#Crime

#REQUIRED PACKAGES
library(xlsx) #package to import xls files directly
library(pastecs) #summary stats function stat.desc
library(ggplot2)
library(scales) #for percent axis
library(plyr) #data wrangling
library(dplyr) #data wrangling 2
library(tidyr) #data wrangling 3

#CUSTOM FUNCTIONS
Sp.Desc <- function(data)
{
  data.num <- data.frame(Dummy=rep(NA,dim(data)[1])) #Make a dummy data.frame
  data.cat <- data.frame(Dummy=rep(NA,dim(data)[1]))
  #separate categorical from numerical data
  for(i in 1:dim(data)[2]){
    if(!is.na(stat.desc(data[i])["mean",])){#if R can compute a mean then add to data.num
      data.num <- cbind(data.num, data[i])
    }
    else{
      data.cat <- cbind(data.cat, data[i])
    }
  }
  #Delete dummy variable
  data.num$Dummy <- NULL
  data.cat$Dummy <- NULL
  
  #Print Numerical results
  if(dim(data.num)[2]>0) {
    print(t(stat.desc(data.num))[,-c(2,3,6,7,11,14)])
    cat(noquote(""), sep="\n\n")
  }
  
  #Print categorical results
  if(dim(data.cat)[2]>0) {
    for(j in 1:dim(data.cat)[2]){
      cat(noquote(names(data.cat[j])))
      print(table(data.cat[j]))
    }
  }
}

DoR.Theme <- function(axis.text.size=16, axis.title.size=16, title.size=20, legend.position="none")
{
  theme(panel.grid.major = element_line(colour="grey90"), panel.grid.minor = element_blank(), panel.background = element_blank(),
        axis.line.x=element_line(colour="black"), axis.line.y=element_line(colour="black"),
        axis.title.x=element_text(colour = "black", size=axis.title.size), axis.title.y=element_text(colour = "black", size=axis.title.size),
        axis.text.x=element_text(colour = "black", size=axis.text.size), axis.text.y=element_text(colour = "black", size=axis.text.size),
        plot.title=element_text(colour = "black",size=title.size, face="bold", hjust=.5),
        axis.ticks=element_line(colour="black"), legend.position = legend.position, legend.key=element_blank())
}


#DATA IMPORT----
Crime <- read.xlsx("C:/Users/sbuja/Documents/Data on the Rocks/Crime/Crime Rates Raw.xlsx", sheetName="Sheet1")
View(Crime)
Sp.Desc(Crime)

#Computing change scores
Crime$Murder.C <- (Crime$Murder.R - Crime$Murder.R[1]) / Crime$Murder.R[1]
Crime$Rape.C <- (Crime$Rape.R - Crime$Rape.R[1]) / Crime$Rape.R[1]
Crime$Robbery.C <- (Crime$Robbery.R - Crime$Robbery.R[1]) / Crime$Robbery.R[1]
Crime$Assault.C <- (Crime$Assault.R - Crime$Assault.R[1]) / Crime$Assault.R[1]
Crime$Violent.C <- (Crime$Violent.R - Crime$Violent.R[1]) / Crime$Violent.R[1]
Crime$Property.C <- (Crime$Property.R - Crime$Property.R[1]) / Crime$Property.R[1]

#Finding the maximums
MaxData <- data.frame(Outcome=c("Murder.C", "Violent.C", "Property.C"), Max=rep(NA,3), Max.Year=rep(NA,3))
MaxData$Max[1] <- max(Crime$Murder.C)
MaxData$Max.Year[1] <- Crime$Year[which.max(Crime$Murder.C)]
MaxData$Max[2] <- max(Crime$Violent.C)
MaxData$Max.Year[2] <- Crime$Year[which.max(Crime$Violent.C)]
MaxData$Max[3] <- max(Crime$Property.C)
MaxData$Max.Year[3] <- Crime$Year[which.max(Crime$Property.C)]

#MaxData2 -- add extra points for graphing maximums
MaxData2 <- data.frame(Outcome=MaxData$Outcome)
MaxData2$Max <- MaxData$Max
MaxData2$Max.Year <- 2015
MaxData2 <- rbind(MaxData, MaxData2)
MaxData2$Outcome <- factor(MaxData2$Outcome, levels=c("Murder.C", "Violent.C", "Property.C"))
MaxData2

#GRAPHING DATA----
colours <- c("#bf0d0d", "#090ea3", "#089126")#Murder-bloodred, Violent-deep blue, property-green
Crime.plot <- ggplot(MaxData2, aes(x=Max.Year, y=Max, colour=Outcome)) + 
  geom_hline(yintercept=0, linetype="3232", colour="black", size=1) +
  geom_line(size=2, alpha=.2) +
  geom_point(data=subset(MaxData2, Max.Year==2015), aes(x=Max.Year, y=Max, colour=Outcome), size=3) +
  scale_colour_manual(values=colours) +
  #All Property Crime
  geom_line(data=Crime, aes(x=Year, y=Property.C), size=2, colour=colours[3]) + 
  annotate("text", label="All Property Crime", x=2014.5, y=.55, colour=colours[3], size=6, fontface="bold", hjust=1, vjust=0) + 
  #All Violent Crime
  geom_line(data=Crime, aes(x=Year, y=Violent.C), size=2, colour=colours[2]) + 
  annotate("text", label="All Violent Crime", x=2014.5, y=1.15, colour=colours[2], size=6, fontface="bold", hjust=1, vjust=0) + 
  #Murder
  geom_line(data=Crime, aes(x=Year, y=Murder.C), size=2, colour=colours[1]) + 
  annotate("text", label="Murder", x=2014.5, y=.35, colour=colours[1], size=6, fontface="bold", hjust=1, vjust=0) + 
  scale_x_continuous("Year", limits=c(1970,2017), breaks=c(seq(1970,2015,10),2015), expand = c(0,0)) +
  scale_y_continuous("Change in Crime Rate since 1970", labels=percent, limits=c(-.6,1.3), breaks=seq(-.5,1,0.5), expand = c(0,0)) +
  ggtitle("Crime in America") +
  DoR.Theme()
Crime.plot

ggsave(Crime.plot, filename="Crime.plot.png", width = 8, height=7, dpi=500)

#Data for Simile
MaxData.R <- data.frame(Outcome=c("Murder.R", "Violent.R", "Property.R"), Max=rep(NA,3), Max.Year=rep(NA,3))
#Historical
MaxData.R$Max[1] <- max(Crime$Murder.R)
MaxData.R$Max.Year[1] <- Crime$Year[which.max(Crime$Murder.R)]
MaxData.R$Max[2] <- max(Crime$Violent.R)
MaxData.R$Max.Year[2] <- Crime$Year[which.max(Crime$Violent.R)]
MaxData.R$Max[3] <- max(Crime$Property.R)
MaxData.R$Max.Year[3] <- Crime$Year[which.max(Crime$Property.R)]
#Current
MaxData.R$Current[1] <- Crime$Murder.R[46]
MaxData.R$Current[2] <- Crime$Violent.R[46]
MaxData.R$Current[3] <- Crime$Property.R[46]

MaxData.R$Fraction <- MaxData.R$Current / MaxData.R$Max

MaxData.R

#geom_curve(aes(x=2016, xend=2016, y=MaxData2[3,2], yend=Crime$Property.C[46]), colour=colours[3], size=1, curvature=-.3) +


#Data to export
write.csv(Crime, file="Crime.csv", row.names=F)
