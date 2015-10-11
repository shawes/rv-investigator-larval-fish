Input_Scarid =(
  "Stage  Depth Count
  Preflexion   Surface    1
  Flexion  Surface  14
  Postflexion    Surface   12
  Preflexion   Top    1
  Flexion  Top  20
  Postflexion    Top   12
  Preflexion   Bottom    2
  Flexion  Bottom  16
  Postflexion    Bottom   15
  ")

fish_prop <- read.table(textConnection(Input_Scarid),header=TRUE)

fish_prop=
  mutate(fish_prop,
         Depth = factor(Depth,levels=c("Surface","Top","Bottom")),
         Stage = factor(Stage,levels=unique(Stage))
  )
fish_prop$ Sum[fish_prop$Stage == 'Preflexion'] =
  sum(fish_prop$ Count[fish_prop$Stage == 'Preflexion'])

fish_prop$ Sum[fish_prop$Stage == 'Flexion'] =
  sum(fish_prop$ Count[fish_prop$Stage == 'Flexion'])

fish_prop$ Sum[fish_prop$Stage == 'Postflexion'] =
  sum(fish_prop$ Count[fish_prop$Stage == 'Postflexion'])

fish_prop=
  mutate(fish_prop,
         prop = Count / Sum
  )

ggplot(fish_prop, 
       aes(x = Depth, y = prop, fill = Stage, group=Stage, shape=Stage, colour=Stage, ymax=1.0, ymin=0)) +
  geom_line(size=1.5) +
  geom_point(size=4) +
  scale_y_continuous(breaks = seq(0, 1.0, 0.1), 
                     limits = c(0, 1.0), 
                     expand = c(0, 0)) +
  scale_x_discrete(labels=c("0-3","3-50","50-100")) +
  labs(x = "Depth (m)", y = "Proportion") +
  scale_colour_brewer(palette="Dark2") +
  theme(text = element_text(size=20))