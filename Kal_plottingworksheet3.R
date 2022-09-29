####### Import libraries #######
library (dplyr)
library(ggplot2)
setwd("C:/Users/KvBrown/Documents/Classes/SURC 2022")
setwd("~/Classes/SURC 2022")

####### Import datasets #######
Advantageous <- read.csv("Kal_dataplot.csv")

####### Summary SE load - needed for error bars#######
#Provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=TRUE, conf.interval=.95) {
  library(doBy)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- "sd"
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- "N"
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
} 

####### Plots #######
#advantageous choices over time, grouped by bias-consistent vs. -inconsistent trials
summarystats <- summarySE(Advantageous, measurevar="Advantage", groupvars=c("Trial", "Bias"))
advant <- ggplot(summarystats, aes(x=Trial, y=Advantage, color=Bias, fill=Bias))+
  geom_line()+
  geom_point()
  #geom_errorbar(aes(ymin=Advantage-se, ymax=Advantage+se), width=.2)

ggsave("Advantageous-Trial-Bias.png", width = 12, height = 5) 
ggplot(Advantageous)
advant
advant + theme_linedraw()
advant + theme(panel.background = element_rect(fill = "coral1",
                                                colour = "coral1",
                                                size = 0.5, linetype = "solid"))
colors()
install.packages("ggthemes")
library(ggthemes)

####### This is the line graph with on line Below that I selected. May change it though.####
AD <- advant + theme_classic (base_size = 20) + scale_color_calc() + #ggtitle("Advantageous Choices Over Time Depending on Bias")
  theme (legend.position = "top", legend.title = element_blank()) +
  scale_y_continuous(name="Proportion of Advantageous Choices", 
                    breaks=seq(0,1,.2), limits=c(0,1)) +
  scale_x_continuous(breaks=seq(0,72,9), limits=c(0,72))
  ggsave("Advantageous-Trial-Bias-stata.png", width = 12, height = 7)
#AD + theme(legend.text = element_text(size=14))
#AD + theme(axis.title.x=element_text(size=14),
#           axis.title.y=element_text(size=14),
#           legend.text = element_text(size=14))



####### Transform data #######
i <- 1; ii<- 1 #this is our index value that increases by 1 each loop
id <- numeric (dim(Advantageous)[1]/72)
fb <- character (dim(Advantageous)[1]/72)
bi.avg <- numeric (dim(Advantageous)[1]/72)


for (i in 1:dim(Advantageous)[1]) {
  
  if (Advantageous$Trial[i] == 1){
    if (!is.na(Advantageous$Advantage[i])){
      if(Advantageous$Bias[i] == 'Bias-Inconsistent'){
        sum <- Advantageous$Advantage[i]
        total <- 1
      }
      else{
        sum <- 0
        total <- 0
      }
    }
  }
  else{
    if (!is.na(Advantageous$Advantage[i])){
      if(Advantageous$Bias[i] == 'Bias-Inconsistent'){
        sum <- sum + Advantageous$Advantage[i]
        total <- total + 1
      }
      else{
        sum <- sum
        total <- total
      }
    }
  }
  
  if (Advantageous$Trial[i] == 72){
    advant.avg <- sum/total
    id[ii] <- Advantageous$Subject[i]
    fb[ii] <- Advantageous$Feedback[i]
    bi.avg[ii] <- advant.avg
    ii <- ii + 1
  }
}

new <- cbind(id,fb,bi.avg)
write.csv(new, file = "Cumulative.Advantageous.csv")
CuAd <- read.csv("Cumulative.Advantageous.csv")
mysplit <- split(CuAd,fb)
mysplit$Full

install.packages("janitor")
library(janitor)
ggplot(CuAd, aes(x=bi.avg, color=fb))+ geom_histogram()
ggplot(CuAd, aes(x=bi.avg, color=fb))+ geom_histogram(binwidth = 1, color="black", fill="white") 
ggplot(CuAd, aes(x=bi.avg, color=fb))+ geom_histogram(aes(y=..density..), colour="black", fill="white")+ geom_density(alpha=.2, fill="#146F25")
CG <- ggplot(CuAd, aes(x=bi.avg, color=fb)) + geom_histogram(fill=fb)      
CG +scale_color_brewer("Dark2")
ggplot(CuAd, aes(x=bi.avg, color=fb)) + geom_histogram(fill="white")  
CG <- ggplot(CuAd, aes(x=bi.avg, color=fb)) + geom_histogram(fill="white")
CG +scale_color_manual(values=c("#2B3FAD", "#AD1D20", "#146F25"))
ggsave("Cumulative.Advantageous2.png", width = 17, height= 12)
CAF<- ggplot(mysplit[["Full"]], aes(x=bi.avg,)) + geom_histogram(fill="white", color="black")
ggsave("CuADF.png", width = 17, height = 12)
CAP <- ggplot(mysplit[["Partial"]], aes(x=bi.avg,)) + geom_histogram(fill="white", color="black")
ggsave("CuADP.png", width = 17, height = 12)
CAM <- ggplot(mysplit[["Minimal"]], aes(x=bi.avg,)) + geom_histogram(fill="white", color="black")
ggsave("CuADM.png", width = 17, height = 12)
c1 <- rgb(240,19,19, max = 255, alpha = 80, names = "lt.red")
c2 <- rgb(255,222,3, max = 255, alpha = 100, names = "lt.yell")
c3 <- rgb (1,88,188, max = 255, alpha = 80, names = "lt.blue")
c4 <- rgb(1,188,57, max = 255, alpha = 80, names = "lt.green")
c6 <- rgb(255,135,43, max = 255, alpha = 100, names = "lt.orange")
c5 <- rgb(168,110,255, max = 255, alpha = 80, names = "lt.purple")


############Density Plots#########
ggplot(CuAd, aes(x=bi.avg, fill= fb)) + geom_density(alpha = 0.4) 
library(plyr)
Mg <- ddply(CuAd, "fb", summarise, grp.mean=mean(bi.avg))
ggplot(CuAd, aes(x=bi.avg, fill= fb)) + geom_density(alpha = 0.4) + 
  scale_fill_manual(values = c("Full" = "#F0131350",
                               "Partial"= "#FF872B80",
                               "Minimal" = "#FFDE0350")) + 
  geom_vline(data=Mg, aes(xintercept=grp.mean, color=fb),
             linetype = "dashed", size = 1)
############# HERE #######################
#This code here is the current density plot. 
DPCUAD <- ggplot(CuAd, aes(x=bi.avg, fill= fb)) + geom_density(alpha = 0.5) + 
  geom_vline(data=Mg, aes(xintercept=grp.mean, color=fb),
             linetype = "dashed", size = 1) + 
  scale_fill_manual(values = c("Full" = "#A86EFF50",
                               "Partial"= "#01BC3950",
                               "Minimal" = "#0158BC50")) + 
  scale_color_manual(values = c("Full" = "#402F4B",
                                "Partial"= "#FFFFFF",
                                "Minimal" = "#AAAAAA"))+ 
  labs (#title= "Distributions of Advantageous Choices by Feedback", 
        #x="Proportion of Bias-Inconsistent Advantageous Choices", 
        y="Density") + theme_classic(base_size = 25) +
  scale_x_continuous(name="Proportion of Bias-Inconsistent Advantageous Choices", 
                     breaks=seq(0,1,.2), limits=c(0,1)) +
  guides(fill=guide_legend(title="Feedback\nCondition"), 
         color=guide_legend(title="Feedback\nCondition"))
ggsave("DPCUAD.png", width = 12, height = 7)
