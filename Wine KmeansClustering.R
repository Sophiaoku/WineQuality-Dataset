white <- read.csv('winequality-white.csv', sep = ';')
red <- read.csv('winequality-red.csv', sep = ';')

#adding a new labeled column 
white$label <- 'white'
red$label <- 'red'

head(white)
head(red)

wine <- merge(red,white, all = TRUE)
head(wine)
str(wine)

#Exploratory data analysis

hist(wine$residual.sugar)

library(ggplot2)

graph1 <- ggplot(wine,aes(x = residual.sugar, fill = label)) + geom_histogram( bins = 50, color = 'black') + theme_bw() + 
   scale_fill_manual(values = c('#ae4554','#faf7ea'))
graph1

graph2 <- ggplot(wine,aes(x=citric.acid, fill = label)) + geom_histogram(bins = 50, color = 'black') +
  theme_bw() + scale_fill_manual(values = c('#ae4554','#faf7ea'))
graph2


graph3 <- ggplot(wine,aes(x=alcohol, fill = label)) + geom_histogram(bins = 50, color = 'black') +
  theme_bw() + scale_fill_manual(values = c('#ae4554','#faf7ea'))
graph3

graph4 <- ggplot(wine, aes(x = residual.sugar, y = citric.acid, color = label)) + geom_point(alpha=0.2) + 
  scale_color_manual(values = c('#ae4554','#faf7ea')) +theme_dark()
graph4

graph5 <- ggplot(wine,aes(x=volatile.acidity,y=residual.sugar)) + geom_point(aes(color=label),alpha=0.2) +
  scale_color_manual(values = c('#ae4554','#faf7ea')) +theme_dark()
graph5

#building the model 

clus.data <- wine[1:12]
head(clus.data)

wine.cluster <- kmeans(clus.data,2)
print(wine.cluster)

table(wine$label,wine.cluster$cluster)
