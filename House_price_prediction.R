#Importing Libraries
library(ggplot2)
library(ggcorrplot)
library(e1071)
options(repr.plot.width = 14, repr.plot.height = 10)

#Importing dataset
hd <- read_csv("Dataset/data.csv")

#Data exploration

head(hd) # checking the first rows in dataset

tail(hd) #checking the last rows in dataset


print(paste("Number of rows: ", nrow(hd))) #number of rows
print(paste("Number of columns: ", ncol(hd))) #number of columns 

summary(hd) #returns the minimum, maximum, mean, median, and 1st and 3rd quartiles for a numerical vector

str(hd) #displays structures of dataset

colnames(hd) #columns name of dataset

unique(hd$city) # Unique cities

#Data selection

maind <- hd[,c("price","bedrooms","sqft_living","floors",
               "sqft_lot", "condition", "view", "yr_built")]
head(maind)

#Checking Null Values 
sum(is.na(maind))

#Figure out house age
maind$oldbuilt <- as.integer(format(Sys.Date(), "%Y")) - maind$yr_built

drops <- c("yr_built")
maind = maind[ , !(names(maind) %in% drops)]

#Plot Correlation matrix
cor(maind)

corr <- round(cor(maind), 1)

# Plot
# creating Correlogram
ggcorrplot(corr,
           type = "lower",
           lab = TRUE, 
           lab_size = 5,  
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlogram of Housing Dataset", 
           ggtheme=theme_gray)


#Plot scatterplot matrix
pairs(~bedrooms + sqft_living + floors + condition, data = maind,
      main = "Scatterplot Matrix",col = "lightsteelblue3")

#Plot boxplot for checking outliers
par(mfrow=c(2, 3))  # divide graph area in 2 columns
boxplot(maind$bedrooms, main="Bedrooms",col = "lightsalmon1")
boxplot(maind$sqft_living, main="sqft_living", col = "lightsalmon1")
boxplot(maind$floors, main="floors", col = "lightsalmon1")
boxplot(maind$condition, main="condition", col = "lightsalmon1")
boxplot(maind$view, main="view",col = "lightsalmon1")
boxplot(maind$oldbuilt, main="oldbuilt",col = "lightsalmon1")


#Bedrooms vs Floors
ggplot(maind, aes(x = bedrooms, y = floors)) +
  geom_count(,col = "lightsalmon1", show.legend = FALSE) +
  labs(
    y = "floors",
    x = "bedrooms",
    title = "Bedrooms vs Floors"
  )+
  theme(plot.title = element_text(size = 30),
  )

plot(x = maind$sqft_living, y = maind$sqft_lot,
     xlab = "sqft_living",
     ylab = "sqft_lot",
     xlim = c(0, 3000), 
     ylim = c(0, 20000),
     main = "sqft_living vs sqft_lot",
     col = "lightsteelblue3"
)

#Plot density plot to check normality
par(mfrow=c(2, 3)) 

plot(density(maind$bedrooms), main="Density Plot: Bedrooms", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maind$bedrooms), 2)))  
polygon(density(maind$bedrooms), col="antiquewhite3")

plot(density(maind$sqft_living), main="Density Plot: sqft_living", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maind$sqft_living), 2)))  
polygon(density(maind$sqft_living), col="darkolivegreen1")

plot(density(maind$sqft_lot), main="Density Plot: sqft_lot", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maind$sqft_lot), 2)))  
polygon(density(maind$sqft_lot), col="antiquewhite3")

plot(density(maind$condition), main="Density Plot: condition", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maind$condition), 2)))  
polygon(density(maind$condition), col="darkolivegreen1")

plot(density(maind$floors), main="Density Plot: floors", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maind$floors), 2)))  
polygon(density(maind$floors), col="antiquewhite3")

plot(density(maind$oldbuilt), main="Density Plot: oldbuilt", ylab="Frequency",
     sub=paste("Skewness:", round(e1071::skewness(maind$oldbuilt), 2)))  
polygon(density(maind$oldbuilt), col="darkolivegreen1")
