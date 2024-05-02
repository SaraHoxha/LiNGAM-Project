######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 07: Data access ##########

# explict call of functions in packages
x = c(0, 1, 5, 8, 9)
median(x)
# median is from package stats
stats::median(x)

# install packages
#install.packages("ISwR") # just once
library(ISwR) # attach (load) package
search() # attached packages (in RStudio/environment)

# visible data frames
data()
# load dataset from package ISwR
data(energy)
# view/edit/modifying data frame
View(energy)
energy2 = edit(energy) # edit - return modifications
fix(energy) # edit - modifications on the same data frame
xnew = edit(data.frame()) # new data frame from scratch

# a list of R datasets
# http://vincentarelbundock.github.io/Rdatasets/

# Set working directory for I/O
setwd("C:/Users/Salvatore Ruggieri/Desktop/tmp")
# working directory (where file are saved/loaded)
getwd() # get current working dir

# I/O - see docs: R-data.pdf or Help, including connection to RDBMS
write.csv(energy, "file.csv", row.names=FALSE)
energy.copy = read.csv("file.csv", encoding="UTF-8", sep=",", header=T) 
save(energy, file="file.RData") # R binary format
rm(energy) # remove energy dataset
load("file.RData") # reload energy dataset
save(list = ls(all = TRUE), file="file.RData") # R binary format
save.image("file.RData") # shortcut of previous line
rm(list=ls()) # clean all
load("file.RData") # reload workspace

# load dataframe from the web, e.g., UCI Machine Learning or Kaggle
iris.uci = read.csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"), header=FALSE)
colnames(iris.uci) = c("sepal.length","sepal.width","petal.length","petal.width","species")
View(iris.uci)

# attach datasets
energy$expend
attach(energy) 
expend # instead of energy$expend
detach('energy')
detach() # all datasets *and* packages
library(ISwR) # re-attach ISwR

# data summaries
attach(energy) 
mean(expend)
mean(c(1, 3, NA, 5), na.rm=T)
median(expend)
quantile(expend)
quantile(expend, seq(0, 1, 0.1))
var(expend) # empirical variance
sd(expend) # empiricial standard deviation
summary(energy)

# more on dataframes

# assignment is copying! (different from Python dataframes)
energy.other = energy
energy.other[1, 1] = 0
energy.other[1,]
energy[1,]

# sub-dataframe
energy[energy$expend < 10,] 
subset(energy, expend<10) 

# groups
s = split(expend, stature) 
s
median(s$lean)
median(s$obese)
# implicit loops
tapply(expend, stature, median)  # apply median on stature for each group of expend
tapply(stature, stature, length)  # or 
table(stature) # contingency table - one dimension

# contingency tables
bins = cut(expend, c(5, 8, 11, 14)) # discretization in factors
bins
table(stature, bins) # contingency table - two dimensions

# another dataset from ISwR
thuesen 
# implicit loops
lapply(thuesen, mean, na.rm=T) # list apply - apply median for each element of the list
sapply(thuesen, mean, na.rm=T) # simplify to a vector or matrix

sapply(split(expend, stature), mean) # this is tapply
# implicit loops on matrices
m = matrix(1:12, c(3,4))
m
apply(m, 2, median) # second parameter 2 = cols, 1 = rows

# sorting, ranking and unique values
sort(expend, decreasing=T)
o = order(expend)
o
expend[o]
energy[o,] # rows in order, all columns
rank(expend)
rank(c(1,2,2,4), ties.method="first") # see help(rank) for methods 
stature
unique(stature)
unique(c(1,2,2,4))

# plotting options
weight = c(60, 72, 57, 90, 95, 72)
height = c(1.75, 1.80, 1.65, 1.90, 1.74, 1.91)
plot(height, weight, pch=2, main="A sample plot", sub="about BMI", xlab="Height (m)", ylab="Weight (Kg)")
# lines and curves
hh = c(1.65, 1.70, 1.75, 1.80, 1.85, 1.90)
lines(hh, 22.5 * hh^2) # connecting points
curve(22.5 * x^2, add=T, col=2) # curve given function

# histograms
hist(expend) # how many bins?
hist(expend, breaks=4)
hist(expend, freq=F)

# discrete sampling - uniform
sample(c('a', 'b', 'c', 'd'), 5, replace=T)
sample(1:50, 5, replace=T)
hist(sample(1:50,10000,replace=T),freq=F) # 1000 -> 1000000

# discrete sampling - with given probabilities
s = sample( c("H","T"), 1000, replace=T, prob=c(0.9, 0.1))
plot(as.factor(s)) # hist require numeric vector

# train-test splitting of energy
training_positions = sample(1:nrow(energy), nrow(energy)*.6)
training = energy[training_positions,] 
training
test = energy[-training_positions,]
test

# exercise: max of a vector

# SOLUTION A: sort descending then take first element
maxvA = function(x) {
  x1 = sort(x, decreasing=T)
  x1[1]
}
# sample use
maxvA(expend)
vs = c('hello', 'world')
maxvA(vs)

# SOLUTION B: iterate and keep maximum value so far
maxvB = function(x) {
  m = -Inf # are we sorting numbers?
  for(v in x)
    if(v > m)
      m = v
  return(m)
}
# sample use
maxvB(expend)
maxvB(vs)
maxvB(c('-A', '-B'))

# SOLUTION C: iterate from 2 on and keep maximum value so far
maxvC = function(x) {
  l = length(x)
  if(l==0)
    return(-Inf)
  m = x[1]
  for(v in x[-1]) # x[2:length(x)] wrong!
    if(v > m)
      m = v
  return(m)
}
# sample use
maxvC(expend)
maxvC(vs)
maxv(c('-A', '-B'))

# pay attention!
c(1)[2:1]
5:1 # n:m for n>m is c(n, n-1, ..., m)

# compare maxvC and max (built-in)
# install.packages("microbenchmark")
library(microbenchmark) # attach (load) package
microbenchmark( maxvC(rgeom(10000, 0.01)), times=1e3) 
microbenchmark( max(rgeom(10000, 0.01)), times=1e3) # built-in's are faster

# multiple assignment / unpacking of Python: NO!
#x1,y1 = 2,3 # error
x1=2; y1=3

# implicit loops
replicate(10, mean(rgeom(5, 0.1))) # for 10 times: generate 5 random numbers and take the mean

# all and any
r = rgeom(100, 0.1)
r
all(r>10)
any(r>10)

# set working dir to current source file
# install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Reticulate: calling Python from R
# https://rstudio.github.io/reticulate/
# > install.packages("reticulate")
# if needed also
#   > reticulate::install_miniconda('C:/Miniconda')
library(reticulate)
# if needed set path to miniconda directory
Sys.setenv(RETICULATE_MINICONDA_PATH = 'C:/Miniconda')
source_python('sds07.py') # this may require to run RStudio in administrator-mode for installing required Python packages
add(5, 10)

# Reading Excel files
library(readxl)
# load table
dataxls = read_excel("sds07.xlsx", na = "n.a.")

# making factors
dataxls$Geography = as.factor(dataxls$Geography)
dataxls$State = as.factor(dataxls$State)
View(dataxls)

# pivoting
data_pivoted = data.frame()
for(i in 2010:2016) {
  # select a year
  data_year = dataxls[c("Geography", "State", paste("Year ", i, sep=""))]
  # rename year
  names(data_year)[3] = "Population"
  # add current year column
  data_year$Year = i
  data_year = data_year[, c(1,2,4,3)]
  # remove rows with all measures equal to NA
  data_year = data_year[ !is.na( data_year$Population ), ]
  # append
  data_pivoted = rbind(data_pivoted, data_year)
}

View(data_pivoted)

# Exercise at home: slides 38-39 of sds02.pdf
