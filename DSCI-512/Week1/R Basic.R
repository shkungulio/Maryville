#R cases sensitive
x=2018
X =2019
#print out variables
x
X
#generate numerical vectors
v = c(1,2,3,4)
#print out the vector
v
print(v)

#get the length of the vector
length(v)
#charact vector
cv = c("a", "b", "c")
print(cv)
length(cv)
#generate matrix
m = matrix(c(1,2,3,4,5,6), nrow =3, ncol =2)
print(m)
#categorical variable: factor
ft = c("MO", "IL", "MI")
ft
#convert character to a factor
ft = factor(ft)
levels(ft)
#Data frames: each column has the same data type
df = data.frame(age = c(25,50,75), policyYear = c(1,2,5), gender = c("F", "M", "F"))
df
#get the dimesnion
dim(df)
#get the number of rows
nrow(df)
#get the number of columns
ncol(df)
#random normal distribution with mean 0 and standard deviation = 1
x= rnorm(100)
x
#speciy mean and standard deviation
y= rnorm(100, mean =10, sd =2)
y
#compute the mean
mean(y)
#compute the variance
var(y)
#compute the correlation coefficients
cor(x,y)
#set the seed for reproducible
set.seed(2018)
x= rnorm(1000)

