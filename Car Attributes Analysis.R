

car <- read.csv("~/car.txt", sep="")
head(car)
car1<- car[, 3:16]
head(car1)

######################################################################################################
######  Cleaning the Data  ###########################################################################
######################################################################################################


# First, I label all the attributes into 14 variables

x1<- car$wheel.base
x2<- car$length
x3<- car$width
x4<- car$height
x5<- car$curb.weight
x6<- car$engine.size
x7<- car$bore
x8<- car$stroke
x9<- car$compression.ratio
x10<- car$horsepower
x11<- car$peak.rpm
x12<- car$city.mpg
x13<- car$highway.mpg
x14<- car$price

######################################################################################################
###########  Descriptive Analysis  ###################################################################
######################################################################################################

# In order to find the potential relationship between 14 variables, I calculated the coveriance and correlation matrices among 14 vaiables

# Converiance Matrix
round(cov(car1),1)

# Correlation Matrix
round(cor(car1),1)


#pairs(car)

######################################################################################################
#####  Simple Linear Regression  #####################################################################
######################################################################################################

#  By combining city.mpg and highway.mpg two variables together, I am trying to apply a simple linear regression to predict the 
#  the city.mpg and highway.mpg


Model_1<- lm(cbind(x12,x13)~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x14)
summary(Model_1)

summary(manova(Model_1))
# x4, x6, x7, x8, x10, x11, and x14 are not significant in predicting the dependent variable

mod12<- lm(cbind(x12,x13)~ x1+x2+x3+x5+x9)
summary(mod12)
# For X12, city mpg, x1, x2, x5, x9 seems to be significant in affecting the dependent variable
# For X13, X5, and X9 seem to be significant

summary(manova(mod12))

######################################################################################################
#######  Reducing Dimensions of object data using PCA Methodology ####################################
######################################################################################################


CAR.pca<- princomp(car1,cor  = TRUE)  
summary(CAR.pca, loadings= TRUE)
# By using PCA, we could use 4 principal components to reduce the dimensions, as cumulatively, four principal components could represent 87% of the variance. 
# Based on the first principal component, it is clear that cars with smaller sizes, lighter in weights, and higher city or highway mpg could contribute to a higher PC1 component value. 

######################################################################################################
####   Data Visualization  ###########################################################################
######################################################################################################

screeplot(CAR.pca, type = "line")
abline(h= 1,col= "red")

# From the screeplot, it appears that first four principal components can reflect greater variances of the dataset.
# Therefore, the dimension can be reduced by these four principal components.


biplot(CAR.pca)
screeplot(CAR.pca)


# Also from the biplot, we can see that high way mpg, city mpg, peak rpm, and compression ration is positively correlated in the first principal component. Horsepower, engine size, length width, and other variables are negatively affecting the first principal component value. 
# For the first principal component, it more likely reflect the car’s fuel efficiency factor, as cars with higher city or highway mpg, smaller in sizes could result higher principal component values. 
# For example, the 7th car in the bi plot, has a higher PC1 value, which corresponding to Chevrolet, that has the highest city and highway mpg and relatively small car sizes, in the car dataset.

# The second principal component more likely reflect the engine performance factor, as cars with higher horsepower, engine size, price will have a high PC2 value, similarly, these cars are negatively related the car sizes, such as length, width, and weight.
# For example, the 33rd car, which has the highest PC2 value, happens to be a Porsche, which has a higher price, greater horsepower, and engine size compare to other cars.  


CAR<- dist(as.matrix(car1), method = "euclidean")
CAR.cluster<- hclust(CAR, method = "single")
plot(CAR.cluster,labels = car[,2])

hc111<-hclust(CAR, "single");
hc222<-hclust(CAR, "complete") 
hc333<-hclust(CAR, "median"); 
hc444<-hclust(CAR, "ward")

opar <- par(mfrow = c(2, 2))    # plot four graphs in one page

plot(hc111,hang=-1,labels = car[,2], main = "Clustering Using Single Method"); re1<-rect.hclust(hc111,k=3,border="red")
plot(hc222,hang=-1,labels = car[,2], main = "Clustering Using Complete Method");re2<-rect.hclust(hc222,k=3,border="red")
plot(hc333,hang=-1,labels = car[,2], main = "Clustering Using Median Method"); re3<-rect.hclust(hc333,k=3,border="red")
plot(hc444,hang=-1,labels = car[,2], main = "Clustering Using Ward Method");re4<-rect.hclust(hc444,k=3,border="red")


######################################################################################################
#### Factor Analysis using Exploratory Factor Analysis (EFA)     #####################################
######################################################################################################


CAR.EFA <- factanal(car1, 3, rotation="varimax", scores="regression")
names(CAR.EFA)
print(CAR.EFA)
# anything above or below .2 is recognized as a loading
print(CAR.EFA, digits=2, cutoff=.3, sort=TRUE)  # Notice difference 
# only show above or below .3

CAR.EFA1 <- factanal(car1, 4, rotation="varimax", scores="regression")
print(CAR.EFA1, digits=2, cutoff=.6, sort=TRUE)  
# The overlap reduced

# By using Exploratory Factor Analysis, we could come up with three factors. 
# From the loadings of the factors, the factor 1 reflect the car fuel efficiency, for which cars that are more fuel efficient will have lower factor value in factor 1.
# The factor 2 reflect the height factor of the cars, as cars with greater height, greater wheel base will result higher value in factor 2.
# The factor 3 reflect the car engine performance. A higher compression ratio with lower engine peak rpm indicates a more efficient and powerful engine, therefore, the factor 3 value will be higher as well.
# Accordingly, the car dataset can be divided into three latent variables, fuel efficiency, height, and engine performance.


CAR.EFA1$loadings
CAR.EFA.loadings <- CAR.EFA1$loadings[,1:2] 
plot(CAR.EFA.loadings,type="n") # set up plot 
text(CAR.EFA.loadings,labels=names(car),cex=.7) # add variable names


# Factor plot between factor 1 and factor 2
CAR.EFA.scores <- CAR.EFA1$scores[,1:2] 
plot(CAR.EFA.scores,type="n") # set up plot 
text(CAR.EFA1$scores[,1],CAR.EFA1$scores[,2]) # add variable names

# As we already come up with three factors in the previous question. Therefore, we are able to better classify the data in the car dataset.
# The dimensions have been reduced. In the score plot, I only used the two factors’ scores in explaining the data. 
# As the factor 1 reflects the fuel efficiency, therefore, the lowest score in factor 1 will indicate the most fuel efficiency car in the dataset, which is the 7th car, the Chevrolet. This is same result we can get from the PCA analysis for the PC1. In addition, those 33rd, 22nd, 17th cars (Porsche, Mercedes, and jaguar2) who has the higher factor 1 scores, will indicate the least fuel efficiency and correspondently have higher prices.
# For the Factor 2, the 23rd car Mercedes, and 30th car peugot1 who have a relatively higher factor 2 scores, will indicates these two cars seem to higher than other cars.


# Factor plot between factor 1 and factor 3



CAR.EFA.scores1 <- CAR.EFA1$scores[,c(1,3)] 
plot(CAR.EFA.scores1,type="n") # set up plot 
text(CAR.EFA1$scores[,1],CAR.EFA1$scores[,2]) # add variable names



# Factor plot between factor 2 and factor 3
CAR.EFA.scores2 <- CAR.EFA1$scores[,c(2,3)] 
plot(CAR.EFA.scores2,type="n") # set up plot 
text(CAR.EFA1$scores[,1],CAR.EFA1$scores[,2]) # add variable names

CAR.ev <- eigen(cor(car1)) # get eigenvalues
CAR.ap <- parallel(subject=nrow(car1),var=ncol(car1),
                  rep=100,cent=.05)
CAR.nS <- nScree(x=CAR.ev$values, aparallel=CAR.ap$eigen$qevpea)
CAR.nS
plotnScree(CAR.nS)

