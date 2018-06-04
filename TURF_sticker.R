#WORK in progress on TURF sticker
#To prooduce TURF sticker I followed hints from this web:
#WEB: https://github.com/GuangchuangYu/hexSticker
#Image Magick software needs to pre-installed as well:
#WEB: https://www.imagemagick.org/script/download.php
#install.packages("hexSticker")
library("hexSticker")
library("tidyverse")

#The following vector data are based on responses from the first TURF members:
#More INFO: https://github.com/PWaryszak/TURFscripts/blob/master/Ex1_TURF_boxplot_pie.R#L26
N<-c("Anonymous","Fabiola", "Sarah","Emily","Stephen","Renata", "Steve","Christina")
T <- c(0.38, 5.34, 0.50, 2.41, 1.07, 0.52, 5.56, 1.00)

TURF_Survey<- data.frame(Name = N, Time_Spent = T)
str(TURF_Survey)#8 obs. of  2 variables:

#DRAW TURF sticker graph:
#WEB:https://www.r-graph-gallery.com/301-custom-lollipop-chart/
p1 <- ggplot(TURF_Survey, aes(x=Name, y=Time_Spent)) +
      geom_segment( aes(x=Name, xend= Name, y=0, yend=Time_Spent), col = "green")+
      geom_point(color="darkgreen", fill=alpha("darkgreen", 0.3),
              alpha=0.7, shape=21) +
      scale_x_discrete(label=abbreviate,1)+
      scale_y_continuous(limits = c(0,6))+
      theme_bw() +
                theme(axis.text.y=element_blank(),
                      axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.title.y=element_blank(),
                      legend.position = "none")

p1
 
#DRAW A STICKER:
 sticker(p1, package="TURF",p_x = 1, p_y = 1.6, p_size=16, s_x=1, s_y=.95, s_width=1.3, s_height=1,
         h_fill = "white", h_color = "darkgreen", p_color = "darkgreen",
         filename="TURF_Sticker1.png")
 