#START SIMPLE:
#Can you go further in a manual car as compared to automatic.
#How much further you can go?
#Try to run Anova, lm  and t.test at the same time to compare the output:
data(cars)
unique(mtcars$am)

go1 <- aov(mpg~as.factor(am), data=mtcars) 
go1 
TukeyHSD(go1)

go2<-t.test(mpg~as.factor(am), data=mtcars)
go2

go3 <- lm(mpg~as.factor(am), data=mtcars) 
summary(go3)

str(summary(go3))

#Draw intercept + slope. How to interpret that?
INTERCEPT <- summary(go3)$ coefficients[1,1] #  INTERCEPT
INTERCEPT
SLOPE <- summary(go3) $ coefficients[2,1] #SLOPE
SLOPE

par(mfrow=c(1,1))#sets the grid for drawing simple plots.
curve(INTERCEPT + SLOPE*x, xlim=c(0,1), ylim=c(0,40), col="red", bty= "n")
#Assumption is that values of INTERCEPT and SLOPE are normally distributed:
par(mfrow=c(1,2))
hist((mtcars$mpg)[mtcars$am==1])
hist((mtcars$mpg)[mtcars$am==0])

#What if x is continuous?========
attach(cars) #Run it all:
n=2
X= cars$speed 
Y=cars$dist
df=data.frame(X,Y)
vX=seq(min(X)-2,max(X)+2,length=n)
vY=seq(min(Y)-15,max(Y)+15,length=n)
mat=persp(vX,vY,matrix(0,n,n),zlim=c(0,.1),theta=-30,ticktype ="detailed", box = FALSE)
reggig=glm(Y~X,data=df,family=gaussian(link="identity"))
x=seq(min(X),max(X),length=501)
C=trans3d(x,predict(reggig,newdata=data.frame(X=x),type="response"),rep(0,length(x)),mat)
lines(C,lwd=2)
sdgig=sqrt(summary(reggig)$dispersion)
x=seq(min(X),max(X),length=501)
y1=qnorm(.95,predict(reggig,newdata=data.frame(X=x),type="response"), sdgig)
C=trans3d(x,y1,rep(0,length(x)),mat)
lines(C,lty=2)
y2=qnorm(.05,predict(reggig,newdata=data.frame(X=x),type="response"), sdgig)
C=trans3d(x,y2,rep(0,length(x)),mat)
lines(C,lty=2)
C=trans3d(c(x,rev(x)),c(y1,rev(y2)),rep(0,2*length(x)),mat)
polygon(C,border=NA,col="yellow", dpi=600)
C=trans3d(X,Y,rep(0,length(X)),mat)
points(C,pch=19,col="red")
n=8
vX=seq(min(X),max(X),length=n)
mgig=predict(reggig,newdata=data.frame(X=vX))
sdgig=sqrt(summary(reggig)$dispersion)
for(j in n:1){
  stp=251
  x=rep(vX[j],stp)
  y=seq(min(min(Y)-15,qnorm(.05,predict(reggig,newdata=data.frame(X=vX[j]),type="response"), sdgig)),max(Y)+15,length=stp)
  z0=rep(0,stp)
  z=dnorm(y, mgig[j], sdgig)
  C=trans3d(c(x,x),c(y,rev(y)),c(z,z0),mat)
  polygon(C,border=NA,col="light blue",density=40)
  C=trans3d(x,y,z0,mat)
  lines(C,lty=2)
  C=trans3d(x,y,z,mat)
  lines(C,col="blue")}


#Everytime we add variable we add extra dimension:
library(scatterplot3d) 
attach(mtcars) 
scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE,
              main="3D Scatterplot for 3 Variables") 

s3d <-scatterplot3d(wt,disp,mpg, pch=16, highlight.3d=TRUE,
                    type="h", main="3D Scatterplot")

fit.lm <- lm(mpg ~ wt+disp) 
s3d$plane3d(fit.lm)


#multivariate chaos:======
library(vegan)
library (vegan3d)
par(mfrow=c(1,1))#setting the grid for plotting.

#DATA off WEB:https://www.davidzeleny.net/anadat-r/doku.php/en:ordiagrams_examples
veg.data <- read.csv ("veg.data.csv")
str(veg.data) #data.frame':	97 obs. of  274 variables:
write.csv(veg.data, file = "veg.data.csv", row.names = F)
env.data <- read.csv("env.data.csv", row.names = F)  
str(env.data) #data.frame':	97 obs. of  28 variables

#Way to deal with multivariate chaos 
#is to compute centroids and how the relate one to another:

DCA <- decorana (veg = log1p (veg.data))
ordirgl (DCA) #this can be viewed as one row of data for 97 species
dim((DCA)$ rproj) #should be 97 as n = 97 in veg.data


### Default 'ordiplot3d'
data(dune)
data(dune.env)
ord <- cca(dune ~ A1 + Moisture, dune.env)
ordiplot3d(ord)
### A boxed 'pin' version
ordiplot3d(ord, type = "h")
### More user control
pl <- ordiplot3d(ord, scaling = 3, angle=15, type="n")
points(pl, "points", pch=16, col="red", cex = 0.7)
### identify(pl, "arrows", col="blue") would put labels in better positions
text(pl, "arrows", col="blue", pos=3)
text(pl, "centroids", col="blue", pos=1, cex = 1)
### Add species using xyz.convert function returned by ordiplot3d
sp <- scores(ord, choices=1:3, display="species", scaling=3)
text(pl$xyz.convert(sp), rownames(sp), cex=0.7, xpd=TRUE)
### Two ways of adding fitted variables to ordination plots
ord <- cca(dune)
ef <- envfit(ord ~ Moisture + A1, dune.env, choices = 1:3)
### 1. use argument 'envfit'
ordiplot3d(ord, envfit = ef)
### 2. use returned envfit.convert function for better user control
pl3 <- ordiplot3d(ord)
plot(pl3$envfit.convert(ef), at = pl3$origin)
### envfit.convert() also handles different 'choices' of axes
pl3 <- ordiplot3d(ord, choices = c(1,3,2))
plot(pl3$envfit.convert(ef), at = pl3$origin)
### ordirgl
ordirgl(ord, display = "species", type = "t")
rgl.quit()




#4 TURF meeting on Multviariates (Mar 09, 2018):
#To SEE HOW "VegEnvDataNew2018.csv" was produced
#check out 2 following files:#"HydrologicDataECF.R" and VegECF.R"
#More info: Pawel (pwaryszak@tulane.edu).

#"Freshwater" RDA========
VegEnvData <- read.csv("VegEnvDataNew2018.csv")
freshOnly <- VegEnvData[ VegEnvData$Community=="Freshwater",]
dim(freshOnly)#541 442
sum(is.na(freshOnly$MeanSalinity)) # rows with NA-s that need removing
sum(is.na(freshOnly$Phraaust)) #0 rows with NA - yay!
FreshNArows<- is.na(freshOnly$MeanSalinity) #object = NA rows in freshOnly
freshOnly_Cover <- freshOnly[ ! FreshNArows,]#remove NA rows
dim(freshOnly_Cover)#504 442

freshVeg_Cover<-freshOnly_Cover[,9:437] #Freshwater veg cover data only
names(freshVeg_Cover)# from "Acerrubr" to "ZiziMill" = check if true
#freshVeg_0and1<-ifelse ( freshVeg_Cover == 0 , 0 , 1)##turning cover to presence/absence data if needed.
freshEnv<-freshOnly_Cover [ , c(1:8, 287, 439:442)] # Env data for Freshwater Community +"Phraust"
dim(freshEnv)#504  13

##BRAY Vegetation distance matrix construction [Cover Values]:
?vegdist # = The function computes dissimilarity indices
BRAYfresh <- vegdist(freshVeg_Cover, distance = "bray")

df.response1 <- decostand( BRAYfresh, method = 'hellinger' )#?decostand# standardization method  for community data
?decostand
fresh_rda <- rda(df.response1 ~ MeanSalinity + Phraaust, freshEnv)
#ANOVA:
Fresh_Anova_Cover<-anova.cca(fresh_rda, by = "margin")
Fresh_Anova_Cover
#STATS OUTPUT:
#Model: rda(formula = df.response1 ~ MeanSalinity + Phraaust, data = freshEnv)
##############Df  Variance       F Pr(>F)    
#MeanSalinity   1 0.0002679 16.3952  0.001 ***
#Phraaust       1 0.0001383  8.4615  0.001 ***
#Residual     501 0.0081877  

# Compute % Variance explained by RDA1 and RDA2 ==
#== adj.r.squared * Cum. constr. eigenvalues prop. explained
a <- summary(fresh_rda)
a$concont$importance[2,1]#Cum. constr. eigenvalues prop. explained for RDA1
a$concont$importance[3,2]#Cum. constr. eigenvalues prop. explained for RDA2
#######################RDA1      RDA2
#Cumulative Proportion 0.6610200 1.0000000

#RDAs explained = Proportion explained *R2 *100%
RDA1_fresh <- round(100 * RsquareAdj(fresh_rda)$adj.r.squared * summary(fresh_rda)$concont$importance[2,1], digits = 1)
RDA1_fresh #2.9%
RDA2_fresh <- round(100 * RsquareAdj(fresh_rda)$adj.r.squared * summary(fresh_rda)$concont$importance[3,2], digits = 1)
RDA2_fresh #4.3%

#R2:
R2 <- RsquareAdj(fresh_rda)$adj.r.squared # R2 of the variance explained
R2#0.04338864

# PLOTS (quick ones to see)
plot(fresh_rda, display=c("lc","cn"), main="Lousiana Freshwhater Communities")
# GRAPHING Freshwater RDA with ggplot:
# Use the "scores" function, then use the elements of it, casting them to data frames:
df.sites <- as.data.frame( scores(fresh_rda)$sites )
# The environment variables are in another element: $CCA$biplot gives the biplot coords for the env variables 
df.env <- as.data.frame( fresh_rda$CCA$biplot[, 1:2] )
df.env$var <- rownames( fresh_rda$CCA$biplot )
df.env$xOrg <- 0 #for plotting arrows
df.env$yOrg <- 0 #for plotting arrows
FreshPlot <- ggplot(data=df.sites, aes(x=RDA1, y=RDA2 ) ) +
  xlab('RDA1 (2.9 % of variation)') + 
  ylab('RDA2 (4.3 % of variation)') +
  geom_hline(yintercept=0, colour="black", linetype="dotted" ) +
  geom_vline(xintercept=0, colour="black", linetype="dotted" ) +
  geom_segment(data=df.env, aes(x=xOrg, y=yOrg, xend=RDA1, yend=RDA2), size=3,
               colour="red", arrow=arrow(length=unit(10,"point") ) ) + geom_point()+
  annotate("text", x = 0.75, y = -0.18, label = c("Mean Salinity ***"), size=8, color="darkgreen") +
  annotate("text", x = 0.3, y = 1.07, label = c("Phragmites Cover ***"), size=8, color="darkblue") + theme_bw()
FreshPlot+ theme(axis.text.x = element_text(size=22,hjust=.5,vjust=.5,face="plain"),
                 axis.text.y = element_text(size=22,hjust=1,vjust=0,face="plain"),  
                 axis.title.x = element_text(size=22,hjust=.5,vjust=0,face="plain"),
                 axis.title.y = element_text(size=22),
                 legend.title = element_text(size=22),
                 plot.title = element_text(size=22, lineheight=1.8, face="bold", hjust = 0.5)) +
  ggtitle("Lousiana Freshwater Plant Communities (2007 - 2016)")
#ggsave('2018Freshwater_RDA_Plot4b.jpeg', dpi=300, height=5, width=9)


#EXTRA Mvariates fun:
#CENTROIDS to display in 2d plot:
data(varespec, varechem)
ord <- cca(varespec ~ Al + P + K, varechem)
plot(ord, display = c("species","sites"))
mod <- cca(dune ~ Management, dune.env)
plot(mod, type = "n") 
pl <- with(dune.env, ordihull(mod, Management, scaling = "symmetric", col = 1:4, draw="polygon", label =TRUE))
with(dune.env, ordiellipse(mod, Management, scaling = "symmetric", kind = "ehull", col = 1:4, lwd=3)) 

