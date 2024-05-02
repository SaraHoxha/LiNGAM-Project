# Solution of exercise at home: slides 38-39 of sds02.pdf

# 1.  Install (if needed) the MASS package and load it
cat("1.  Install (if needed) the MASS package and load it\n")
#install.packages("MASS")
library(MASS)
cat("\n\n")

# 2.  Load the "Animals" data set
cat("2.  Load the \"Animals\" data set\n")
data(Animals)
ls(Animals)
cat("\n\n")

# 3.  Compute the ratio between animals' brain size and their body size, adding the result
#     as a new column called "proportions" to the Animals data frame
cat("3.  Compute the ratio between animals' brain size and their body size, adding the result
    as a new column called \"proportions\" to the Animals data frame\n")
Animals$proportions = Animals$brain/Animals$body
Animals
# print two empty lines for separating outputs
cat("\n\n")

# 4.  Compute average and standard deviation of the "proportions"
cat("4.  Compute average and standard deviation of the \"proportions\"\n")
m = mean(Animals$proportions)
s = sd(Animals$proportions)
cat("Proportions: mean=", m, ", sd=", s, "\n\n", sep = "")

# 5.  Remove the column "proportions" from the data frame
cat("5.  Remove the column \"proportions\" from the data frame\n")
Animals$proportions = NULL
Animals
cat("\n\n")

# 6.  Select animals with body size > 100
cat("6.  Select animals with body size > 100\n")
Animals[Animals$body>100,]
cat("\n\n")

# 7.  Get a list of animals' names with body size > 100 and brain size > 100
cat("7.  Get a list of animals' names with body size > 100 and brain size > 100\n")
BigBodyBigBrain = Animals[Animals$body>100 & Animals$brain>100,]
rownames(BigBodyBigBrain)
cat("\n\n")

# 8.  Find the average body and brain size for the first 10 animals in the dataset
cat("8.  Find the average body and brain size for the first 10 animals in the dataset\n")
First10 = Animals[1:10,]
First10
means = list(m_body=mean(First10$body), m_brain=mean(First10$brain))
cat("Mean body=", means$m_body, ", Mean brain=", means$m_brain, "\n\n", sep = "")


# 9.  Write a function that returns a list of two elements containing the mean value and
#     the standard deviation of a vector of elements
#     Apply this to the body and brain sizes of Animals
cat("9.  Write a function that returns a list of two elements containing the mean value and
    the standard deviation of a vector of elements
    Apply this to the body and brain sizes of Animals\n")
mean.std = function(x) {
    m = mean(x)
    s = sd(x)
    ret = list(mean = m, sd = s)
    return(ret)
}
body.stats = mean.std(Animals$body)
brain.stats = mean.std(Animals$brain)
cat("Body stats: mean=", body.stats$mean, ", sd=", body.stats$sd, "\n", sep = "")
cat("Brain stats: mean=", brain.stats$mean, ", sd=", brain.stats$sd, "\n\n", sep = "")

# 10. Create a vector called body_norm with 100 samples from a Normal random variable
#     with average and standard deviation equal to those of body sizes in the Animals dataset
#     print the summary of the generated dataset
#     compare the summary with another dataset of 100 samples with same average and sd = 1
cat("10. Create a vector called body_norm with 100 samples from a Normal random variable
    with average and standard deviation equal to those of body sizes in the Animals dataset
    print the summary of the generated dataset
    compare the summary with another dataset of 100 samples with same average and sd = 1\n")
body_norm = rnorm(100, mean=body.stats$mean, sd=body.stats$sd)
body_norm1 = rnorm(100, mean=body.stats$mean, sd=1)
summary(body_norm)
summary(body_norm1)
cat("\n\n")

# SET WORKING DIR FOR I/O
wdir = "C:/Users/ruggi/Desktop/tmp"
setwd(wdir)

# 11. Save the Animals data frame to a file named "animals_a.txt" omitting row and column names
cat("11. Save the Animals data frame to a file named \"animals_a.txt\" omitting row and column names\n")
write.table(Animals, "animals_a.txt", row.names = T, col.names = T, sep = '\t')
cat("\n\n")

# 12. Create a copy of the file named "animals_b.txt", then
#     modify some data in it
#     read the file into a new data frame, Animals_b
#     write a function that returns two data frames with the set of different rows in the two datasets
cat("12. Create a copy of the file named \"animals_b.txt\", then
    modify some data in it
    read the file into a new data frame, Animals_b
    write a function that returns two data frames with the set of different rows in the two datasets\n")
df.diff = function(a, b){
    diff_vector = (a$body != b$body) | (a$brain != b$brain)
    a.diff = a[diff_vector,]
    b.diff = b[diff_vector,]
    ret = list(a_diff = a.diff, b_diff = b.diff)
    return (ret)
}
Animals_b = read.table("animals_b.txt", header = T, sep='\t')
diff = df.diff(Animals, Animals_b)
diff$a_diff
diff$b_diff
cat("\n\n")

# 13. Save the workspace to a file, clean the workspace, restore the workspace from the file
cat("13. Save the workspace to a file, clean the workspace, restore the workspace from the file\n")
save.image("R_intro_exercise.RData")
rm(list=c("diff","df.diff","Animals_b","body_norm","body_norm1","body.stats","brain.stats",
          "mean.std","means","First10","BigBodyBigBrain","m","s"))
load("R_intro_exercise.RData")
