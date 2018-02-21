#Exercise: Draw TURF_Boxplot=======
# = means this line is not executed. It is used for comments.

#The following vector data are based on responses from TURF members (where we had 50% turnout!!!)
#Let us create three vectors with names, time spent and experience with R:
#[A vector is a sequence of data elements of the same basic type.]

N<-c("Anonymous","Fabiola", "Sarah","Emily","Stephen","Renata", "Steve","Christina")
T <- c(0.38, 5.34, 0.50, 2.41, 1.07, 0.52, 5.56, 1.00)
E<- c(">5" , "1to5","1to5",">5","<1", "<1","1to5",">5")

#Use these three vectors to create a data frame that holds all together:
TURF_Survey<- data.frame(Name = N, Time_Spent = T, R_Experience_Years = E)
str(TURF_Survey)#str stands for structure, see what is it?

install.packages("tidyverse")#this package contains ggplot. Install only once.
library(tidyverse)#Once installed you need to call it up to the console.

#Let us draw boxplot of how much time each TRUFian spent on suvey:
#GGplot stands for Grammar of Graphics and means that you can paint multple layers:
#Always start from setting your canvas = what is you x, what is your y:

TURF_Boxplot<- ggplot(data = TURF_Survey, aes(x= "Tulane R Fans", y=Time_Spent) )+ geom_boxplot()
TURF_Boxplot

#Add layers to you liking to beatify the plot:
t1<- TURF_Boxplot + geom_jitter(position=position_jitter(width=0.3),aes(color=Name, size=Time_Spent*2)) 
t1#run to see
t2<-t1+theme_bw() +ggtitle("Time spent on Survey (minutes)") +ylab("")
t2#run to see
t3 <- t2 + theme(?axis.text.y=element_text(size=22),
                  axis.title.x=element_blank(),
                  axis.text.x=element_text(size=20),
                  axis.title.y=element_text(size=24),
                  legend.position = "right",
                  legend.text = element_text(size = 16),
                  legend.title = element_text(size = 18, face="bold"),
                  plot.title = element_text(lineheight=1.2, face="bold",hjust = 0.5))
t3 #run tosee
ggsave(filename="p.png", dpi=600, width=140, height=140, unit= "mm") # way to save quality figures

#You can also merge all the layers together:
ggplot(TURF_Survey, aes(x= Survey, y=Time_Spent) )+ 
   geom_boxplot()+
   geom_jitter(position=position_jitter(),aes(color=Name, size = Name))+
   theme_bw() +ggtitle("TURF_Survey_2017") +ylab("Minutes")+ xlab("")+
   theme(?axis.text.y=element_text(size=22),
           axis.title.x=element_text(size=22),
           axis.text.x=element_blank(),
           axis.title.y=element_text(size=24),
           legend.position = "right",
           legend.text = element_text(size = 16),
           legend.title = element_text(size = 18, face="bold"),
           plot.title = element_text(lineheight=1.2, face="bold",hjust = 0.5))
ggsave(filename = paste0(here("/"), last_plot()$labels$title, ".png"),
       width = 5, height = 4, dpi = 300)



#EX next: Draw a TURF experiance with R PIE CHART =========
#Pie presents how experienced in R TURF is:
library(tidyverse)
str(TURF_Survey) #double check data structure

#Compute count of 3 types of responses on R_experience in entire data
#It is also called piping
Count_Experience <- TURF_Survey%>% #Create Count_Experience object
  group_by(R_Experience_Years) %>% #that is grouped by levels of factor R_Experience_Years
  summarise(R_Experience = length(R_Experience_Years)) #summarize by counting length of each of three vectors


Count_Experience#Order of R_Experience_Years factors is mixed up. Let us get its levels more orderly
Count_Experience$R_Experience_Years<-factor(Count_Experience$R_Experience_Years,
                                            levels = c("<1", "1to5",">5"))

BarPlot<- ggplot(data = Count_Experience, aes(x="Time With R", y = R_Experience, fill = R_Experience_Years))+
  geom_bar(width = 1, stat = "identity")
BarPlot

#Paint barplot as pie using coord_polar:
pie <- BarPlot + coord_polar("y", start=0)+ggtitle("Our experience with R at TURF")
pie#Run to see

#Ways to beatify it:
pie2<- pie + theme(? axis.text.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks = element_blank(),
              panel.grid  = element_blank(),
              plot.title = element_text(hjust = 0.5, face="bold"),
              legend.position = "bottom")
pie2
pie3<- pie2 +ylab("")+xlab("")
pie3
