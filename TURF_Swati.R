#### Model 1: Geometric Growth ======
### Written by Swati Patel, April 7, 2018

# initialize population
N_initial=100
finalT=10
times=c(0, seq(1,finalT))
Nt=matrix(0,1, length(times))
Nt[1]=N_initial

# parameters
r=.9 

# run population growth
for(i in 1:(length(times)-1)){
	Nt[i+1]=r*Nt[i]
}

plot(times, Nt, type="l", main="Population Growth", col = "red", lwd = 3)

#### Model 2: Random Geometric Growth ======
# initialize population
N_initial=10
finalT=100
times=c(0, seq(1,finalT))
Nt=matrix(0,1, length(times))
Nt[1]=N_initial

plot(0,0, xlim=c(0,finalT), ylim=c(0,100), xlab="time", ylab="population density", main="Population Growth", col="white")

for(j in 1:10){
  # run population growth
  for(i in 1:(length(times)-1)){
    r=runif(1, .5, 1.5)
    Nt[i+1]=r*Nt[i]
  }
  
  points(times, Nt, type="l", main="Population Growth", ylim=c(0,100), col=j, lwd=5)
  
}

#### Model 3: Logistic discrete growth======
#parameter 
K=100

plot(0,0, xlim=c(1,4), ylim=c(0,K), xlab="r", ylab="population density", col="white")

rvec=seq(1,4, by=0.01)
plottimes=80:100

for(j in 1:length(rvec)){
  
  # initialize population
  N_initial=1
  finalT=100
  times=c(0, seq(1,finalT))
  Nt=matrix(0,1, length(times))
  Nt[1]=N_initial
  
  r=rvec[j]
  
  # run population growth
  for(i in 1:(length(times)-1)){
    Nt[i+1]=r*Nt[i]*(1-Nt[i]/K)
  }
  
  rrep=rep(r, length(plottimes))
  points(rrep, Nt[plottimes])
  
}