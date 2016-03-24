ogrInfo(".", "ALLpatches")
habitats.rg <- readOGR(".", "ALLpatches")
print(proj4string(habitats.rg))
plot(habitats.rg, axes=TRUE, border="gray")

ogrInfo(".", "larvae_paths")
larva <- readOGR(".", "larvae_paths")
print(proj4string(larva))
plot(larva, axes=TRUE, border="gray")