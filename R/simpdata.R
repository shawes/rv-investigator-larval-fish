larval.fish.data <- read.csv('reef_fish.csv')
                         
mydata <- larval.fish.data[-c(1:5)]
simpdata <- simp <- diversity(mydata, "simpson",MARGIN=1, base=exp(1))
simpdata <- cbind(larval.fish.data,simpdata)


mean.simp <- aggregate( simp~Depth+Feature, larval.fish.data, mean )
sd.dimp <- aggregate( simp~Depth+Feature, larval.fish.data, sd )
allsimp <- cbind(mean.simp,sd.dimp)
allsimp <- allsimp[-c(4,5)]
allsimp <- group_by(allsimp,Feature,Depth)
barplot(allsimp$simp)
c <- ggplot(allsimp, aes(factor(Feature)))

qplot(factor(Feature), data=allsimp, geom="bar", fill=factor(Feature))
# By default, uses stat="bin", which gives the count in each category
c + geom_bar()