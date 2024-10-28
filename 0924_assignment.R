
#### Dataset################################
library (datasets) # load package
?iris
data(iris) # import dataset
head(iris) # visualize 'head' dataset
tail(iris)
summary(iris) #  object summary
str(iris) # examine the structure of the object
class(iris) # look charactor
fix(iris) # spreadsheet, can directive fix datd with window, but not useful

#### Selection################################

students<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/students.txt',header=T, sep="\t", dec='.') # read data set from url
students
str(students) #Comment on the structure of this dataset
#int (integer) save number
#chr (character) save word

#select culoum
#1.1
students$height
#1.2
students[ ,1]
students[2,4] #[no. row(__), no. column(|)].

### Subset################################

#1.1
students$gender=="female" #condition
f<-students$gender=="female" # filter, save data to "f"
females<-students[f,] # selection
females
#1.2 shorter
females <- students$gender=="female"
females

#add name
f<-students$gender=="female"
females<-students[f,]
rownames(females) <-c('Vanessa', 'Vicky', 'Michelle', 'Joyce', 'Victoria')
females

### Practice 2.1################################
#Using the iris data set, create three smaller data sets corresponding to the three species designated in the column Species.
library(datasets)
data(iris)
iris

setosa <- iris$Species == "setosa"
iris_setosa <- iris[setosa,]
iris_setosa

versicolor <- iris$Species == "versicolor"
iris_versicolor <- iris[versicolor,]
iris_versicolor

virginica <- iris$Species == "virginica"
iris_virginica <- iris[virginica,]
iris_virginica

### Sample################################

sample(data, size, replace = FALSE, prob = NULL)
# check in detail source code 
# View(sample) OR getAnywhere(sample()) 

students<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/students.txt',header=T, sep="\t", dec='.') # read data set from url
f<-students$gender=="female"
females<-students[f,]
rownames(females) <-c('Vanessa', 'Vicky', 'Michelle', 'Joyce', 'Victoria')
females

#select from filter
1:nrow(females)
sf<-sample(1:nrow(females), 2) # filter with two randomly selected female students
sf # the selection
females[sf,]


### Sorting################################

#1.1
students<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/students.txt',header=T, sep="\t", dec='.') # read data set from url
students_order<-order(students$height) # create a vector with order
students [students_order,] # sorting data set with  with filter
#1.2 short
students[order(students$height), ]

#2.1 use [-]
ind2<-order(-students$height) 
#2.2 use [decreasing]
ind3<-order(students$height,decreasing =T) 

### Recoding ################################

# If `condition` then give 'blue'. If not then gives 'red'.
students<-read.table('https://www.dipintothereef.com/uploads/3/7/3/5/37359245/students.txt',header=T, sep="\t", dec='.') # read data set from url

#1.1
colors<-ifelse(students$gender=='male', 'blue','red') 
colors
students$colors<-ifelse(students$gender=='male','blue','red') # create a new column
students
#1.2 short
students$gender<-ifelse(students$gender=='male', 'blue','red') # replace an existing column
students

# [==] equal to
# [>=] equal to or greater than
# [<=] equal to or less than
# [!=] not equal to
# [&] and
# [|] or

### Practice 2.2 ################################

#Using the iris dataset create a new variable with flower colors. 
#Iris setosa is purple.Iris versicolor is blue. Iris virginica is pink. 
#Sort individuals by decreasing Sepal.Width. 
#What can you hypothesize on the size of sepal for these three species. 
#Get back the data set for the species having the smallest sepal width. 
#Delete the variable color in this subset*

library(datasets)
data(iris)
iris

#Iris setosa is purple.Iris versicolor is blue. Iris virginica is pink.
color <- ifelse(iris$Species == "setosa", 'purple',ifelse(iris$Species == "versicolor", 'blue', 'pink'))
color
iris$colors <- ifelse(iris$Species == "setosa", 'purple',ifelse(iris$Species == "versicolor", 'blue', 'pink'))
iris

#Sort individuals by decreasing Sepal.Width.
iris_order <- order(-iris$Sepal.Width)
iris [iris_order,]

#What can you hypothesize on the size of sepal for these three species.

setosa <- iris$Species == "setosa"
iris_setosa <- iris[setosa, ]
iris_setosa
mean_se_len <- mean(iris_setosa$Sepal.Length)
mean_se_width <- mean(iris_setosa$Sepal.Width)
mean_se_len
mean_se_width

versicolor <- iris$Species == "versicolor"
iris_versicolor <- iris[versicolor, ]
iris_versicolor
mean_ve_len <- mean(iris_versicolor$Sepal.Length)
mean_ve_width <- mean(iris_versicolor$Sepal.Width)
mean_ve_len
mean_ve_width

virginica <- iris$Species == "virginica"
iris_virginica <- iris[virginica, ]
iris_virginica
mean_vi_len <- mean(iris_virginica$Sepal.Length)
mean_vi_width <- mean(iris_virginica$Sepal.Width)
mean_vi_len
mean_vi_width

#Get back the data set for the species having the smallest sepal width.
mean_se_width
mean_ve_width
mean_vi_width

versicolor <- iris$Species == "versicolor"
iris_versicolor <- iris[versicolor, ]
iris_versicolor
mean_ve_width

#Delete the variable color in this subset*
iris_versicolor$colors<-NULL
iris_versicolor
