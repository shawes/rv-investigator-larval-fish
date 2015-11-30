library("ggplot2")
library("dplyr")
library("reshape2")
library("RVAideMemoire")

ontogenydata <- read.csv('ovm_tidied.csv',skip = 2,nrows = 172) ## 172 is the first 16 sites
names(ontogenydata) <- tolower(names(ontogenydata))

scarids <- subset(ontogenydata,select = c("site","depth","pre_scaridae","fle_scaridae","pos_scaridae"))
names(scarids) <- c("site","depth","preflexion","flexion","postflexion")
scaridmelt <- melt(scarids, id.vars = c("site","depth"),
                   measure.vars = c("preflexion","flexion","postflexion"),
                   variable.name = "stage")
scaridmelt <- group_by(scaridmelt, depth, stage)

poms <- subset(ontogenydata,select = c("site","depth","pre_pomacentridae","fle_pomacentridae","pos_pomacentridae"))
names(poms) <- c("site","depth","preflexion","flexion","postflexion")
pomsmelt <- melt(poms, id.vars = c("site","depth"),
                   measure.vars = c("preflexion","flexion","postflexion"),
                   variable.name = "stage")
pomsmelt <- group_by(pomsmelt, depth, stage)

labrids <- subset(ontogenydata,select = c("site","depth","pre_labridae","fle_labridae","pos_labridae"))
names(labrids) <- c("site","depth","preflexion","flexion","postflexion")
labridsmelt <- melt(labrids, id.vars = c("site","depth"),
                 measure.vars = c("preflexion","flexion","postflexion"),
                 variable.name = "stage")
labridsmelt <- group_by(labridsmelt, depth, stage)

serranids <- subset(ontogenydata,select = c("site","depth","pre_serranidae","fle_serranidae","pos_serranidae"))
names(serranids) <- c("site","depth","preflexion","flexion","postflexion")
serranidsmelt <- melt(serranids, id.vars = c("site","depth"),
                    measure.vars = c("preflexion","flexion","postflexion"),
                    variable.name = "stage")
serranidsmelt <- group_by(serranidsmelt, depth, stage)


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

printGraphForFish <- function(countdata) {
  prop <- getProportions(countdata)
  graphBuilder(prop)
}

performFishersTest <- function(df) {
  matrix <- acast(df,depth ~ stage,value.var="count")
  print(matrix)
  plot(ca(matrix))
  require(vcd)
  mosaic(matrix,shade=T,legend=T)
  assoc(matrix,shade=T,legend=T)
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

labrids <- summarise(labridsmelt, count = sum(value))
performFishersTest(labrids)
printGraphForFish(labrids)

scarids <- summarise(scaridmelt, count = sum(value))
performFishersTest(scarids)
printGraphForFish(scarids)

bothids <- summarise(by_stage, count = sum(value))
performFishersTest(bothids)
printGraphForFish(bothids)

scorpids <- summarise(by_stage, count = sum(value))
performFishersTest(scorpids)
printGraphForFish(scorpids)

serranids <- summarise(serranidsmelt, count = sum(value))
performFishersTest(serranids)
printGraphForFish(serranids)

synodontids <- summarise(by_stage, count = sum(value))
performFishersTest(synodontids)
printGraphForFish(synodontids)

mullids <- summarise(by_stage, count = sum(Mullidae))
performFishersTest(mullids)
printGraphForFish(mullids)

pomacentrids <- summarise(pomsmelt, count = sum(value))
performFishersTest(pomacentrids)
printGraphForFish(pomacentrids)








