library(datasets)
# import data
data(iris)

iris2 <- datasets::iris

library(RCurl)
iris3 <- read.csv(text = getURL("iris.csv") )

### summary
head(iris)
head(iris,4)
tail(iris,4)

summary(iris)
summary(iris$Sepal.Width)

### missing data
is.na(iris) # returns True for missing data
sum(is.na(iris)) # returns na values
 
#### skimr() - expands on summary() by providing 
#### larger set of statistics
install.packages("skimr")
library(skimr)
skim(iris)



##### Group data by column

iris %>% 
  dplyr::group_by(Species) %>% 
  skim()
#####

#############################
# Quick data visualization
#
# R base plot()
#############################


# Panel plots
plot(iris)
plot(iris, col = "red")

# Scatter plot
plot(iris$Sepal.Width, iris$Sepal.Length)

plot(iris$Sepal.Width, iris$Sepal.Length, col = "red")     # Makes red circles

plot(iris$Sepal.Width, iris$Sepal.Length, col = "red",     # Makes red circles + Adds x and y axis labels
     xlab = "Sepal width", ylab = "Sepal length")

# Histogram
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, col = "red")   # Makes red bars

# Feature plots

library(caret)
featurePlot(x = iris[,1:4], 
            y = iris$Species, 
            plot = "box",
            strip=strip.custom(par.strip.text=list(cex=.7)),
            scales = list(x = list(relation="free"), 
                          y = list(relation="free")))
