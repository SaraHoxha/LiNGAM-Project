######################################
# Statistics for data science: R 
# 
# http://didawiki.di.unipi.it/doku.php/mds/sds/
######################################

# [CTRL-Enter] to execute a line

###### Script Lesson 17: Data preprocessing in R ##########

par(mar=c(4,4,1,1))

# Data preparation
# see https://web.archive.org/web/20180630231707/http://scg.sdsu.edu/dataset-adult_r/

# set working directory
# Opt 1: if current file is run - require package rstudioapi
# namespace "::" if package is not loaded
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 
# Opt 2: if current file is sourced
#setwd(getSrcDirectory()[1]) 

# execute script in a separate file
source("dataprep.R", echo=T) 
# and save in R binary format
save(data, file="sds17.Rdata") 

# or load previously saved data
#load(file="sds17.Rdata") 

census = data$train

# barplots (of factors data types)
plot(census$income)

# histogram and density: equal width
hist(census$age, freq=FALSE, breaks=10)
lines(density(census$age, bw=3), col=2, lwd=2)
# histogram and density: equal freq
hist(census$age, freq=FALSE, breaks=quantile(census$age, seq(0, 1, .10)))
lines(density(census$age, bw=3), col=2, lwd=2)

# box plot
plot(census$age ~ census$income)

# scatter plot and correlation
plot(census$age, census$hr_per_week)
cor(census$age, census$hr_per_week, method="spearman")

# normalized mutual information, adapted from sds11.R
nmi = function(x, y) {
  px = table(x)/length(x)
  py = table(y)/length(y)
  pxy = table(x, y)/length(y)
  pxy = pxy[pxy>0] # avoid 0*log(0)
  hx = -sum(px*log(px))
  hy = -sum(py*log(py))
  hxy = -sum(pxy*log(pxy))
  sim = (hx+hy-hxy)/( (hx+ hy)/2 ) 
  asim = (hx+hy-hxy)/hx
  c(sim, asim)
}

# contingency table
table(census$income, census$occupation)
# Theil's U
nmi(census$income, census$occupation)

# Descriptive statistics package
library(DescTools)
# nominal-nominal
UncertCoef(census$income, census$occupation) # Theil's U sym
UncertCoef(census$income, census$occupation, direction ="row") # Theil's U asym
Desc(census$occupation)
Desc(census$income)
Desc(census$occupation ~ census$income)

Phi(census$occupation, census$income)
Lambda(census$occupation, census$income)

# continuous-nominal
PlotFdist(census$age)
Desc(census$age ~ census$income)

# continuous-continuous
Desc(census$age ~ census$hr_per_week)
PlotMarDens(census$age, census$hr_per_week, census$income)

# data.frame info
Abstract(census)
PlotMiss(census)

# more at  https://www.rdocumentation.org/packages/DescTools/versions/0.99.44

# The Landscape of R Packages for Automated Exploratory Data Analysis
# https://journal.r-project.org/archive/2019/RJ-2019-033/index.html

library(DataExplorer)
create_report(census)

