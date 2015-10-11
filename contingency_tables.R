flexion_df <- read.csv('flexion_data.csv', colClasses = c("factor","factor","character","factor","factor","numeric"))

bothids <- matrix(c(18,17,5, 10,12,8, 4,4,6), 3, 3,
                  dimnames = list(depth = c("Surface", "Top", "Bottom"),
                                  stage = c("Preflexion", "Flexion", "Postflexion")))

labrids <- matrix(c(5,14,2, 38,68,58, 14,101,95), 3, 3,
                  dimnames = list(depth = c("Surface", "Top", "Bottom"),
                                  stage = c("Preflexion", "Flexion", "Postflexion")))

mullids <- matrix(c(4,0,0, 16,5,0, 26,11,4), 3, 3,
                  dimnames = list(depth = c("Surface", "Top", "Bottom"),
                                  stage = c("Preflexion", "Flexion", "Postflexion")))

poms <- matrix(c(6,5,1, 5,16,4, 0,4,1), 3, 3,
                  dimnames = list(depth = c("Surface", "Top", "Bottom"),
                                  stage = c("Preflexion", "Flexion", "Postflexion")))

scarids <- matrix(c(1,1,2, 14,20,16, 12,12,15), 3, 3,
                  dimnames = list(depth = c("Surface", "Top", "Bottom"),
                                  stage = c("Preflexion", "Flexion", "Postflexion")))

scorps <- matrix(c(7,3,0, 9,15,7, 4,16,19), 3, 3,
                  dimnames = list(depth = c("Surface", "Top", "Bottom"),
                                  stage = c("Preflexion", "Flexion", "Postflexion")))

synods <- matrix(c(9,6,4, 7,16,8, 2,8,1), 3, 3,
                  dimnames = list(depth = c("Surface", "Top", "Bottom"),
                                  stage = c("Preflexion", "Flexion", "Postflexion")))

serranids <- matrix(c(8,5,6, 4,16,10, 0,4,1), 3, 3,
                  dimnames = list(depth = c("Surface", "Top", "Bottom"),
                                  stage = c("Preflexion", "Flexion", "Postflexion")))


fisher.test(bothids,alternative="two.sided")
fisher.test(labrids,alternative="two.sided")
fisher.test(scorps,alternative="two.sided")
fisher.test(serranids,alternative="two.sided")
fisher.test(synods,alternative="two.sided")
fisher.test(mullids,alternative="two.sided")
fisher.test(scarids,alternative="two.sided")
fisher.test(poms,alternative="two.sided")


fisher.multcomp(scorps,"bonferroni")
fisher.multcomp(labrids,"bonferroni")

