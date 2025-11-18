###############################################################################
####################### Descriptive Statistics ################################

###############################################################################
####################### Author: Christos Dalamarinis ##########################

###############################################################################
############# Purpose: Generate Comprehensive Summary Statistics ##############



#Load required libraries
library(psych)
library(pastecs)


# Load Data
data <- read.csv("sample_data.csv") #insert your data file

# ============ Check Data Are Read Correctly ==========
head(data)        #to see the head (first lines) of your data file
tail(data)        #to see the tail (last lines) of your data file
dim(data)         #to see the dimensions (number of rows and columns) of your data file
names(data)       #to see the names of the variables In your data file
summary(data)     #to get a summary of the variables In your data file
View(data)        #to see the data as a spread sheet

# ============ Examine Preliminary Descriptive Stats ==
describe(data)
describe(data$score) #this allows you to inspect a specific variable in your dataset, the "$" allows to index variables in your dataset
psych::describe(data$score) #same as above just different syntax

# ============ Checking for Conditions (optional) ====
table(data$tstatus)
describe.by(data$score, group = data$group) #this allows you to look at a variable within a group, here we group score values


#============= Additional =============================
#The describe() function does not show the standard error of skew/kurtosis, "se" is the standard error of the mean
#The function below calculates the SEs
skewKurtSE <- function(x) {
  n <- length(na.omit(x))
  skew <- psych::describe(x)$skew
  kurt <- psych::describe(x)$kurtosis
  skew_se <- sqrt( ( 6 * n * (n-1)) / ( (n-2) * (n+1) * (n+3) ) )
  kurt_se <- sqrt( (4 * (n^2 - 1) * skew_se^2) / ( (n-3) * (n+5)) )
  return(data.frame(Skew=skew, Skew_SE=skew_se, Kurtosis=kurt, Kurtosis_SE=kurt_se))
}

#The code above creates a function, and we can use it to calculate the SE for our variables
Standard_error_score_varaible <- skewKurtSE(data$score) #this creates an additional item in our Environment
Standard_error_score_varaible #this prints out in the Console, however we can also see the results in the Environment


# ===================== Outliers ======================
table(data$score) #this produces a table and we can assess potential outliers numerically

# =================== Visualizations ==================
#Except the numerical inspection, we can create Histograms to assess Outliers
hist(data$score) 

#We can also create a Box plot that illustrates Outliers
boxplot(data$score)

#In case the box plot illustrates an Outlier, we can extract information from the plot
score_boxplot <- boxplot(data$score)
score_boxplot$out # this will tells us the numeric value of the outlier data point, then we can index it:
data[which(data$score %in% score_boxplot$out), 'subject']

# =================== Normal Distribution =============
#We might also want to statistically test normal distribution, as graphs might be difficult to interpret.
#We can run a Shapiro-Wilk to test for normality
shapiro.test(data$score)

#Below there is another way to assess normality using the stat.desc() function from the library(pastecs)
stat.desc(data$score, norm=T)

# =====================================================