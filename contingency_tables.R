flexion_df <- read.csv('flexion_data.csv')

bothids <- matrix(c(10,12,8, 4,4,6, 18,17,5), 3, 3,dimnames = list(depth = c("Surface", "Top", "Bottom"),
                                                                   stage = c("Preflexion", "Flexion", "Postflexion")))
fisher.test(bothids)

bothids2 <- matrix(c(12,8,4,6,17,5), 2, 3,dimnames = list(depth = c("Top", "Bottom"),
                                                                   stage = c("Preflexion", "Flexion", "Postflexion")))
fisher.test(bothids,simulate.p.value = TRUE, B = 1e5)

labrids <- matrix(c(38,68,61, 14,101,95, 5,14,2), 3, 3,dimnames = list(depth = c("Surface", "Top", "Bottom"),
                                                                   stage = c("Preflexion", "Flexion", "Postflexion")))
fisher.test(bothids)

fisher.multcomp(labrids, 
                p.method = "none")

test <- acast(flexion_df, Depth~Stage, sum)
