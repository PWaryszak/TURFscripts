## The FINAL "TURF-starts" Challenge:

#The challenge is described in the rmd file ("Ex2_TURF_in_MARKDOWN_Final")
#I intend to sent that file to TURF group to practice basic R:

#This "Ex2" R-file is a shortcut for those who want to find a solution quicker or struggle with the challenge.

#Set a seed for the random number generator
set.seed(101) # so that we all get the same result - don't change!

# Generate 1,000 normally distributed values, mean=50 sd=10 and assign to a variable 'myVars'
?rnorm
myVars <- rnorm(1000, 50, 10)

# What is the median value in myVars?
median(myVars)

# What are the maximum and minimum values of myVars?
range(myVars)
max(myVars)
min(myVars)

# Save the minimum and maximum values to two new variables (myMin and myMax)
myMin <- min(myVars)
myMax <- max(myVars)

# Round down myMin to an integer and re-assign to the same variable name
myMin <- floor(myMin) 
round(myMin)
# Round up myMax to an integer and re-assign to the same variable name
myMax <- ceiling(myMax)
myMax
# Multiply 14 by 4903 and assign to a new variable 
myVar <- 14 * 4903
myVar
# Multiply myMin by myMax and add the variable you created in the previous step

# The Final result is (time to claim R-Monkey prize):
myMin * myMax + myVar
sum(prod(myMin, myMax), myVar)
?prod #prod = multiplicatioin. R has tens of ways to do the same thing.







