# Basic Hypothesis Testing in R for Engineers and Geoscientists New to R
# Michael Pyrcz, University of Texas at Austin, Twitter @GeostatsGuy

# This will be used in my Introduction to Geostatistics Undergraduate Class 
# It is assumed that students have no previous R experience. We use a simple data set with porosity from 2 wells, X1 and X2
# These exmaples are worked out by-hand in the accompanying spreadsheet called 'Lecture7_CI_Hypoth_eg_R.xlsx'. 

# Load the required libraries, you may have to first go to "Tools/Install Packages..." to install these first using base classes
# none needed

# Declare functions
# no functions required in this demonstration

# Set the working directory, I always like to do this so I don't lose files and to simplify subsequent read and writes
setwd("C:/PGE337")                                        # choose your local working directory

# Read the data table from a comma delimited file - data on GitHub/GeostatsGuy/GeoDataSets
mydata = read.csv("PorositySample2Units.csv")             # read in comma delimited data file

# Let's visualize the first several rows of our data so we can make sure we successfully loaded it
head(mydata)                                              # show the first several rows of a data table in the console

# Check out the summary statistics for each column
summary(mydata)                                # summary statistics for the multivariate data file

# There are so many options in R for summary statistics

# Binned data with frequency and proportions in each bin 
install.packages("Hmisc")
library(Hmisc)
describe(mydata)

# Almost every univariate statistic in one command 
install.packages("pastecs")
library(pastecs)
stat.desc(mydata) 
# Did you notice that we got the confidence interval of the mean at a 95% confidence level

# Let's demonstrate calculating a confidence interval by hand 
alpha = 0.05
s1 = sd(mydata$X1)
m1 = mean(mydata$X1)
n = nrow(mydata)

SE = s1/sqrt(n)
t_score = qt(c(0.025,0.975),df=n-1) # we get both P025 and P975 at the same time in an array
CI_95 = m1 + t_score*SE

# Here's an hypothesis test for H0: mu1 = 0.0 that includes the confidence intervals 
t.test_result = t.test(mydata$X1,conf.level = .95)
CI_95_check = t.test_result$conf.int

# Let's try the t-test for difference in means assuming pooled variance
t.test(mydata$X1,mydata$X2,var.equal = TRUE)

# Let's try the t-test for difference in means assuming unequal variances
t.test(mydata$X1,mydata$X2)
# notes: p-value is the symetric probability to be outside the t-statistic (both tails)

# Let's f-test the difference in variances
var.test(mydata$X2,mydata$X1,alternative="greater")
# note we have set the first to have the greater variance and test if this statistically significant larger

# Hope this was helpful,

# Michael
#
# Michael J. Pyrcz, P.Eng., Ph.D.
# Associate Professor, the University of Texas at Austin (@GeostatsGuy)
# mpyrcz@austin.utexas.edu


