library("ggplot2")
library("dplyr")
library("reshape2")
library("ca")
library("vcd")
library("RVAideMemoire")

ontogenydata <- read.csv('ovm_tidied.csv',skip = 2,nrows = 172) ## 172 is the first 16 sites
names(ontogenydata) <- tolower(names(ontogenydata))

getFishData <- function(df,pre.name,fle.name,post.name){
  fish <- subset(df,select = c("site","depth", pre.name, fle.name, post.name))
  names(fish) <- c("site","depth","preflexion","flexion","postflexion")
  fishmelt <- melt(fish, id.vars = c("site","depth"),
                          measure.vars = c("preflexion","flexion","postflexion"),
                          variable.name = "stage")
  fishmelt <- group_by(fishmelt, depth, stage)
  fishmelt
}

synodontids <- getFishData(ontogenydata,"pre_synodontidae","fle_synodontidae","pos_synodontidae")
serranids <- getFishData(ontogenydata,"pre_serranidae","fle_serranidae","pos_serranidae")
pomacentrids <- getFishData(ontogenydata,"pre_pomacentridae","fle_pomacentridae","pos_pomacentridae")
mullids <- getFishData(ontogenydata,"pre_mullidae","fle_mullidae","pos_mullidae")
labrids <- getFishData(ontogenydata,"pre_labridae","fle_labridae","pos_labridae")
scarids <- getFishData(ontogenydata,"pre_scaridae","fle_scaridae","pos_scaridae")
bothids <- getFishData(ontogenydata,"pre_bothidae","fle_bothidae","pos_bothidae")
triglids <- getFishData(ontogenydata,"pre_triglidae","fle_triglidae","pos_triglidae")
scorpids <- getFishData(ontogenydata,"pre_scorpaenidae","fle_scorpaenidae","pos_scorpaenidae")

## Calculate the proportions of each stage per depth
getProportions <- function(data){
  data$sum[data$stage == 'preflexion'] = sum(data$count[data$stage == 'preflexion'])
  data$sum[data$stage == 'flexion'] = sum(data$count[data$stage == 'flexion'])
  data$sum[data$stage == 'postflexion'] = sum(data$count[data$stage == 'postflexion'])
  data = mutate(data, prop = count / sum)
  return(data)
}

## Build the graph
graphBuilder <- function(data){
  
  # Order the factors to print nicely on x-axis
  data$depth <- factor(data$depth,levels=c("0","25","75"))
  
  graph <- ggplot(data, 
                  aes(x = depth, y = prop, fill = stage, group=stage, shape=stage, colour=stage, ymax=1.0, ymin=0)) +
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

printLineGraphForFish <- function(countdata) {
  prop <- getProportions(countdata)
  graphBuilder(prop)
}

printMosaicGraphForFish <- function(countdata) {
  matrix <- acast(countdata,depth ~ stage,value.var="count")
  print(matrix)
  plot(ca(matrix))
  mosaic(matrix,shade=T,legend=T)
  assoc(matrix,shade=T,legend=T)
}

performFishersTest <- function(countdata) {
  matrix <- acast(countdata,depth ~ stage,value.var="count")
  ft <- fisher.test(matrix,alternative="two.sided")
  print(ft)
  if(ft$p.value < 0.05)  {
    mult <- fisher.multcomp(matrix,"bonferroni")
    print(mult)
  }
  return(ft)
}

performChiSqTest <- function(countdata) {
  matrix <- acast(countdata,depth ~ stage,value.var="count")
  ft <- chisq.test(matrix)
  print(ft)
  return(ft)
}


labrids.sum <- summarise(labrids, count = sum(value))
performChiSqTest(labrids.sum)
printLineGraphForFish(labrids.sum)
printMosaicGraphForFish(labrids.sum)

scarids.sum <- summarise(scarids, count = sum(value))
performFishersTest(scarids.sum)
printLineGraphForFish(scarids.sum)
printMosaicGraphForFish(scarids.sum)

bothids.sum <- summarise(bothids, count = sum(value))
performFishersTest(bothids.sum)
printLineGraphForFish(bothids.sum)
printMosiacGraphForFish(bothids.sum)

scorpids.sum <- summarise(scorpids, count = sum(value))
performFishersTest(scorpids.sum)
printLineGraphForFish(scorpids.sum)
printMosaicGraphForFish(scorpids.sum)

serranids.sum <- summarise(serranids, count = sum(value))
performFishersTest(serranids.sum)
printLineGraphForFish(serranids.sum)
printMosaicGraphForFish(serranids.sum)

synodontids.sum <- summarise(synodontids, count = sum(value))
performFishersTest(synodontids.sum)
printLineGraphForFish(synodontids.sum)
printMosaicGraphForFish(synodontids.sum)

mullids.sum <- summarise(mullids, count = sum(value))
performFishersTest(mullids.sum)
printLineGraphForFish(mullids.sum)
printMosaicGraphForFish(mullids.sum )

pomacentrids.sum <- summarise(pomacentrids, count = sum(value))
performFishersTest(pomacentrids.sum)
printLineGraphForFish(pomacentrids.sum)
printMosaicGraphForFish(pomacentrids.sum)
