##### PCA ######################
install.packages("factoextra")

library("factoextra")
library(psych)
library("FactoMineR")


setwd("C:\\Users\\Rohan\\Desktop\\DV")
happy <- read.csv("happy.csv", stringsAsFactors = FALSE)
head(happy)

#### Scatter Plot & Correlations ####

pairs(happy[,],
      gap = 1,
      pch=50)

pairs.panels(happy,
             gap=1, pch = 20)


#########   PCA  #####################
happy_pca <- prcomp(happy, 
                    scale. = TRUE)




attributes(happy_pca)
happy_pca$center

happy_pca$scale
print(happy_pca)
summary(happy_pca)

############  eigen values ######
get_eig(happy_pca)

#### scree plot ######
fviz_screeplot(happy_pca, addlabels = TRUE, ylim = c(0, 55), 
               barfill="sky blue", 
               barcolor="sky blue", 
               linecolor="black")

###################### orthogonality of PCs ########

pairs.panels(happy_pca$x,
             gap=1, 
             pch = 20, 
             digits = 2,
             hist.col="cyan",stars=TRUE)




##########
var <- get_pca_var(happy_pca)
var


###### Coordinates of variables
head(var$coord)
head(var$contrib)
var$contrib
var$cor
###### Graph of variables: default plot

fviz_pca_var(happy_pca, col.var = "blue")


# Control variable colors using their contributions
fviz_pca_var(happy_pca, col.var="contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE# Avoid text overlapping
)



# Contributions of variables to PC1
fviz_contrib(happy_pca, choice = "var", axes = 1, top = 10)

# Contributions of variables to PC2
fviz_contrib(happy_pca, choice = "var", axes = 2, top = 10)


#### bi plot
dev.new(width=60, height=20)


biplot(happy_pca, pc.biplot = TRUE, cex=0.5, add=TRUE,xaxt="n", yaxt="n",
       main="Happiness Data", xpd=NA, 
       xlab=paste0("PC1, ", round(100*happy_pca$sdev[1]^2/sum(happy_pca$sdev^2),2), "% variance"),
       ylab=paste0("PC2, ", round(100*happy_pca$sdev[2]^2/sum(happy_pca$sdev^2),2), "% variance"))

