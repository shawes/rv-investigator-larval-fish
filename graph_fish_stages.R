library("ggplot2")
library("dplyr")
library("reshape2")
library("RVAideMemoire")


## Calculate the proportions of each stage per depth
getProportions <- function(data){
  data$Sum[data$Stage == 'Preflexion'] = sum(data$Count[data$Stage == 'Preflexion'])
  data$Sum[data$Stage == 'Flexion'] = sum(data$Count[data$Stage == 'Flexion'])
  data$Sum[data$Stage == 'Postflexion'] = sum(data$Count[data$Stage == 'Postflexion'])
  data = mutate(data, Prop = Count / Sum)
  return(data)
}

## Build the graph
graphBuilder <- function(data){
  
  # Order the factors to print nicely on x-axis
  data$Depth <- factor(data$Depth,levels=c("Surface","Top","Bottom"))
  
  graph <- ggplot(data, 
                  aes(x = Depth, y = Prop, fill = Stage, group=Stage, shape=Stage, colour=Stage, ymax=1.0, ymin=0)) +
    geom_line(size=1.5) +
    geom_point(size=4) +
    scale_y_continuous(breaks = seq(0, 1.0, 0.1), 
                       limits = c(0, 1.0), 
                       expand = c(0, 0)) +
    scale_x_discrete(labels=c("0-3","3-50","50-100")) +
    labs(x = "Depth (m)", y = "Proportion") +
    scale_colour_brewer(palette="Dark2") +
    theme(text = element_text(size=20))
  return(graph)
}

printGraphForFish <- function(countdata) {
  prop <- getProportions(countdata)
  graphBuilder(prop)
}

performFishersTest <- function(df) {
  matrix <- acast(df,Depth ~ Stage,value.var="Count")
  ft <- fisher.test(matrix,alternative="two.sided")
  print(ft)
  if(ft$p.value < 0.05)  {
    mult <- fisher.multcomp(matrix,"bonferroni")
    print(mult)
  }
  return(ft)
}

## Read in the CSV
myData <- read.csv('flexion_data2.csv', colClasses = c("factor","factor","character","factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

## Group data by depth and then stage
by_stage <- group_by(myData, Depth, Stage)

labrids <- summarise(by_stage, Count = sum(Labridae))
result <- performFishersTest(labrids)
printGraphForFish(labrids)

scarids <- summarise(by_stage, Count = sum(Scaridae))
performFishersTest(scarids)
printGraphForFish(scarids)

bothids <- summarise(by_stage, Count = sum(Bothidae))
performFishersTest(bothids)
printGraphForFish(bothids)

scorpids <- summarise(by_stage, Count = sum(Scorpaenidae))
performFishersTest(scorpids)
printGraphForFish(scorpids)

serranids <- summarise(by_stage, Count = sum(Serranidae))
performFishersTest(serranids)
printGraphForFish(serranids)

synodontids <- summarise(by_stage, Count = sum(Synodontidae))
performFishersTest(synodontids)
printGraphForFish(synodontids)

mullids <- summarise(by_stage, Count = sum(Mullidae))
performFishersTest(mullids)
printGraphForFish(mullids)

pomacentrids <- summarise(by_stage, Count = sum(Pomacentridae))
performFishersTest(pomacentrids)
printGraphForFish(pomacentrids)








