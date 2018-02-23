###README ----
## SexRatioAnalyses.R
## Purpose: Analyze Valeriana edulis population sex ratio change over elevation (2000-4000m) and time (1978-2011)
## Author:## William K. Petry (ETH Zürich, william.petry@usys.ethz.ch)
## Recycled for learning purposses by Pawel: pwaryszak@tulane.edu
## 
# This script is divided into the following sections:
#
# 1. Preliminaries
#   1.1. Load required packages & functions
#   1.2. Set working directory & data options
#   1.3. Import & clean data
# 2. Sex ratio change over elevation
# 3. Sex ratio change over time
# 4. Calculate pace of sex ratio change (m/decade)

## 1. Preliminaries ----
## 1.1. Load required packages & functions ====
library(gmm)
library(propagate)
library(tidyverse)
library(boot)

# Define function to calculate the standard error of the mean
 SE <- function(x){sd(x, na.rm=TRUE)/sqrt(length(x[!is.na(x)]))}

## 1.2. Set working directory & data options ====
getwd()

## 1.3. Import & clean data ====
sr.data.raw <- read.csv ("https://raw.githubusercontent.com/PWaryszak/TURFscripts/master/SexRatios.csv")
#OR:
sr.data.raw <- read.csv ("SexRatios.csv") # if downloaded to your working directory 
 
str(sr.data.raw)#data.frame':	31 obs. of  8 variables:
sr.data.raw$RANK1<-cut(sr.data.raw$sr.2011, c(0.2,0.3,0.4, 0.5))
sr.data.raw #see how RANK works

#Ratios were transformed for analysis as per:
sr.data <-  sr.data.raw  %>% mutate_each(funs(logit), Lsr.1978=sr.1978, Lsr.2011=sr.2011, contains("sr.")) # logit transform ratio data
str(sr.data )#31 obs. of  10 variables:
names(sr.data) # "population"   "elev.m"       "latitude.dd"  "longitude.dd" "sr.1978"      "n.1978"      
# "sr.2011"      "n.2011"       "Lsr.1978"     "Lsr.2011"

## 2. Sex ratio change over elevation ----
# Fit regression model
sr.elev.mod <- lm(Lsr.2011~elev.m, data=sr.data)
summary(sr.elev.mod)
# Calculate pace of sex ratio change per 100 m
(osr.shift.space <- inv.logit(sr.elev.mod$coefficients["elev.m"]*100))
osr.shift.space <- inv.logit(sr.elev.mod$coefficients["elev.m"]*100)
osr.shift.space

#FIGURE 1 in Paper:
#grpah sex ration change along the elevation:
library(tidyverse)
ggplot(data = sr.data, aes(x = sr.2011, y = elev.m ))+
   geom_point(aes(size=sr.2011, color = sr.2011))+
   scale_colour_gradient(low = "red", high = "blue")+
   geom_line(stat="smooth",method = "lm",size=.8)+
   labs(x ="Sex Ratio % Male" , y= "Elevation (m)")+
   ggtitle("Sex-specific responses to climate change... Petry et al (2016)")+
   theme_light()+ theme(plot.title = element_text(face = "italic",size=15, hjust= 0.5),
                        legend.position = "none",
                        axis.title.y=element_text(size=14),
                        axis.title.x=element_text(size=14))
 
 ggsave(filename="SexRatioFigure1d.jpg", width=6.5, height=3)

 
#Fun Frick on a plot wth magick:
install.packages("magick")# Install only once if not installed on your comp yet
library(magick)#
fig <- image_graph(width = 750, height = 400, res = 96)
ggplot(data = sr.data, aes(x = sr.2011, y = elev.m ))+
  geom_point(aes(size=sr.2011, color = sr.2011))+
  scale_colour_gradient(low = "red", high = "blue")+
  geom_line(stat="smooth",method = "lm",size=.8)+
  labs(x ="Sex Ratio % Male" , y= "Elevation (m)")+
  ggtitle("Sex-specific responses to climate change... Petry et al (2016)")+
  theme_light()+ theme(plot.title = element_text(face = "italic",size=15, hjust= 0.5),
                       legend.position = "none",
                       axis.title.y=element_text(size=14),
                       axis.title.x=element_text(size=14))

dev.off()
frink <- image_read("https://jeroen.github.io/images/frink.png")
out <- image_composite(fig, frink, offset = "+560+30")
print(out)
image_scale(frink, "300")#width
image_scale(frink, "x300") # height: 300px

#Pawel, Does Sex Ratio change across elevation?======
#ratio data is tricky as it is a range. Other, more intuitive way to look at ratio data:
#is to create contingency table:
#where we can campare sex ratio frequencies against elevation ranks:
sr.data.raw <- read.csv ("https://raw.githubusercontent.com/PWaryszak/TURFscripts/master/SexRatios.csv")
df<-sr.data.raw
range(df$sr.2011) #0.2276923 0.5000000
range(df$sr.1978, na.rm =T) #0.2566 0.4882
sum(is.na(df$sr.1978))/ length(df$sr.1978) * 100 #61% of 1978 data comprise NA-s!!!!
range(df$elev.m)# 2040 3790m

#Ranking Sex ratios in 2011:
df$Rank2011<-cut(df$sr.2011, seq(0.2, 0.5, 0.1), labels = c(1:3), right=FALSE) 
summary(df$Rank2011)
#1    2    3 NA's 
#6   19    5    1 

#Ranking Sex ratios in 1978:
df$Rank1978<-cut(df$sr.1978, right=FALSE, seq(0.2, 0.5, 0.1), labels = c(1:3)) 
summary(df$Rank1978)
#1    2    3 NA's 
#3    7    2   19 

#Ranking elvations:
df$RankElev<-cut(df$elev.m, right=FALSE, seq(2000, 4000, 500), labels = c(1:4)) 
summary(df$RankElev)
#1  2  3  4 
#2 14 11  4

#Chi Square Stats(http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence)
library(MASS)
tbl <- table(df$Rank2011,df$RankElev)
tbl #the contingency table 
chisq.test(tbl)
#Pearson's Chi-squared test
#data:  tbl
#X-squared = 8.4658, df = 6, p-value = 0.2059

#CONCLUSION2011 = Sex Ratio is independent of elevation!! contrary to what paper says!!!
#As the p-value  0.2059 is greater than the .05 significance level, we do not reject 
#the null hypothesis that the sex ratio is independent of the elevation in 2011.

tbl2 <- table(df$Rank1978,df$RankElev)
tbl2 #the contingency table 
chisq.test(tbl2)
#Pearson's Chi-squared test
#data:  tbl2
#X-squared = NaN, df = 6, p-value = NA

#CONCLUSION1978 = Sex Ratio was measured only at two elevation ranks!!! 
#How can you  compare it to 2011?

## 3. Sex ratio change over time ----
# Subset data to past-present population pairs and remove populations where
# sex ratio change over time is confounded with heavy grazing
sub.sr.data <- sr.data %>%
  filter(!is.na(sr.1978)) %>%
  filter(!population %in% c("Brush Creek Cow Camp", "Brush Creek Pasture"))
# Paired t-test for change in sex ratio over time
sr.time.mod <- with(sub.sr.data, t.test(Lsr.2011, Lsr.1978, paired=T))
sr.time.mod
# Calculate pace of sex ratio change per decade
((100*inv.logit(mean(sub.sr.data$Lsr.2011, na.rm=T))-100*inv.logit(mean(sub.sr.data$Lsr.1978, na.rm=T)))/(2011-1978))*10

#Pawel's ranking way on cleaned sub.sr.data:=====
#Ranking Sex ratios in 2011:
sub.sr.data$Rank2011<-cut(sub.sr.data$sr.2011, seq(0.2, 0.5, 0.1), labels = c(1:3), right=FALSE) 
summary(sub.sr.data$Rank2011)
#1 2 3  
#0 7 3 

#Ranking Sex ratios in 1978:
sub.sr.data$Rank1978<-cut(sub.sr.data$sr.1978, right=FALSE, seq(0.2, 0.5, 0.1), labels = c(1:3)) 
summary(sub.sr.data$Rank1978)
#1 2 3
#3 7 0 

#Ranking elvations:
sub.sr.data$RankElev<-cut(sub.sr.data$elev.m, right=FALSE, seq(2000, 4000, 500), labels = c(1:4)) 
summary(sub.sr.data$RankElev)
#1 2 3 4 
#0 7 3 0 

#Chi Square Stats(http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence)
library(MASS)
tbl.sub1 <- table(sub.sr.data$Rank2011,sub.sr.data$RankElev)
tbl.sub1 #the contingency table 
chisq.test(tbl.sub1)
#Pearson's Chi-squared test
#data:  tbl
#X-squared = NaN, df = 6, p-value = NA


tbl.sub2 <- table(sub.sr.data$Rank2011,sub.sr.data$RankElev)
tbl.sub2 #the contingency table 
chisq.test(tbl.sub2)
#Pearson's Chi-squared test
#data:  tbl2
#X-squared = NaN, df = 6, p-value = NA




## 4. Calculate pace of sex ratio change (m/decade) ----
# Gather model coefficients and standard errors for error propagation analysis
sr.elev.mod.summary <- summary(sr.elev.mod)
pace.data <- data.frame(elev_int=sr.elev.mod.summary$coefficients["(Intercept)", 1:2], 
                        elev_b=sr.elev.mod.summary$coefficients["elev.m", 1:2], 
                        contemp=c(mean(sub.sr.data$Lsr.2011, na.rm=T), SE(sub.sr.data$Lsr.2011)), 
                        histor=c(mean(sub.sr.data$Lsr.1978, na.rm=T), SE(sub.sr.data$Lsr.1978)))
pace.expr <- expression(((histor-elev_int)/elev_b)-((contemp-elev_int)/elev_b))
set.seed(914) # set random number generating seed to replicate output
pace.res <- propagate(pace.expr, pace.data, nsim=1e6)

# Calculate mean pace of sex ratio change
(sr.pace <- pace.res$sim["Mean"]/((2011-1978)/10))
# Calculate upper and lower standard errors
(sr.pace.use <- (pace.res$sim["Mean"]+((pace.res$sim["97.5%"]-pace.res$sim["Mean"])/qnorm(0.975)))/
  ((2011-1978)/10)) # Mean + USE
(sr.pace.lse <- (pace.res$sim["Mean"]-((pace.res$sim["2.5%"]-pace.res$sim["Mean"])/qnorm(0.025)))/
  ((2011-1978)/10)) # Mean - LSE
