x = getwd()
setwd(x)
clopidogrel = readxl::read_excel("clopidogrel.xlsx")
inputvar <- clopidogrel[c(1:8)]
outputvar <- clopidogrel[c(9, 10)]

summary(clopidogrel)
hist(clopidogrel$clopidogrel)
hist(clopidogrel$Mortality_Score)
hist(clopidogrel$Diabetes)
hist(clopidogrel$Aspirin_Use)
hist(clopidogrel$Age)
hist(clopidogrel$Height)
hist(clopidogrel$Weight)
hist(clopidogrel$BSA)
hist(clopidogrel$EBL24)
hist(clopidogrel$logEBL24)

# We find Mortality Score, Age, Height, Weight and BSA to be continuous variables
# Both the output variables are continuous as well
# Clopidogrel use, Asipirin use and Diabetes are the discontinuous variables
#####################################

#Code for Covariance matrices of input and output variables
cov(inputvar)
cov(outputvar)

#####################################

# Code for computing mean, median and standard deviations of each variables
names(clopidogrel)
str(clopidogrel)

apply(clopidogrel, 2, mean, na.rm = TRUE)
apply(clopidogrel, 2, median, na.rm = TRUE)
apply(clopidogrel, 2, sd, na.rm = TRUE)

pca1 = prcomp(inputvar, scale = TRUE)

names(pca1)
pca1$rotation
pca1$sdev
pca1$x
pca1$scale
biplot(pca1, scale = 0)

pca1_var = pca1$sdev^2
pca1_var

pca1_explained = pca1_var/sum(pca1_var)
pca1_explained

plot(pca1_explained, xlab = "Principal Component", ylab =  "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
plot(cumsum(pca1_explained), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = 'b')



# We find that the mean, median and standard deviations of the variables are varying widely
# Therefore we must normalize the means
# If we perform PCA without standardizing then Height would've been the most critical factor
# Therefore we standardize to make mean 0 and standard deviation 1
 

########################################

# Principal Component Analysis of Age, Weight and Mortality Score
pcadata <- clopidogrel[c(2, 5, 7)]
pca2 = prcomp(x = pcadata, scale = TRUE)
names(pca2)
pca2$rotation
pca2$sdev
pca2$x
pca2$scale
biplot(pca2, scale = 0)

pca2_var = pca2$sdev^2
pca2_var

pca2_explained = pca2_var/sum(pca2_var)
pca2_explained

plot(pca2_explained, xlab = "Principal Component", ylab =  "Proportion of Variance Explained", ylim = c(0,1), type = 'b')
plot(cumsum(pca2_explained), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", ylim = c(0,1), type = 'b')


# As you can see there are 3 distinct principal components of the data
# The rotation matrix gives the coordinates of the data in the rotated system
# The coordinates are the principal component scores
#########################################

# SVD Analysis of Age, Weight and Mortality
#install.packages("ripa")
#install.packages("rafalib")
#install.packages("rARPACK")
#library(ripa)
#library(rARPACK)
#library(rafalib)

svddata <- clopidogrel[c(2,5,7)]
svddata.svd <- svd(svddata)
u <- svddata.svd$u
v <- svddata.svd$v
s <- diag(svddata.svd$d)
dim(u)
dim(v)
dim(s)

########################################
