

## ---------------------------
##
## Script Purpose: Figures demonstrating microstructural indirect efffect sizes
##                 I.e how DTI microstructure appears to mediate the VRF-CF association
##                 (1) Rename variables from supplementary table to abbreviated form
##                 (2) Create a new column that specifies which hemisphere the tract is assigned to
##                 (3) Create a plot of tract associations (FA, MD) for both hemispheres
##
##
##

## Load packages -----
install.packages("tidyverse")
install.packages("ggplot2")

library("tidyverse")
library("ggplot2")

# Load supplementary table 11 & 12 from manuscript -----
brain_IDP_plot <- read.csv("supplementary_table11.csv")
CMR_plot <- read.csv("supplementary_table12.csv")

# Match to column names in supplementary table 11
names(CMR_plot)[names(CMR_plot) == "cluster"] <- "category"
names(CMR_plot)[names(CMR_plot) == "feature"] <- "mediator"

#Combine mediation analyses
Heart_Brain_mediation_plot <- rbind(CMR_plot,
                                    brain_IDP_plot)

# Rename some mediator column metrics for clarity (e.g. fa -> FA and md -> MD)
Heart_Brain_mediation_plot$category <- str_replace_all(Heart_Brain_mediation_plot$category, 
                                                       c("fa" = "FA", 
                                                         "md" = "MD",
                                                         "icvf" = "ICVF",
                                                         "isovf" = "ISOVF",
                                                         "od" = "OD",
                                                         "mo" = "MO",
                                                         "volume" = "Volume"))

mycolors <- paletteer_c("grDevices::Blue-Red",18)
mycolors <- paletteer_c("grDevices::Purple-Blue", 10)

mycolors <- (values = c("#023FA5FF",
                        "#455AA7FF",
                        "#3C5488B2",
                        "#6673B1FF",
                        "#828BBBFF",
                        "#9BA1C5FF",
                        # "#4DBBD5B2",
                        "#8796C2FF",
                        "#95AFD0FF",
                        "#A7C6DDFF",
                        "#7695f0",
                        
                        "#DED3D5FF",
                        "#C8969FFF",
                        "#BD7B88FF",
                        "#AF5E70FF",
                        "#A03C56FF",
                        "#8E063BFF"
))

Heart_Brain_mediation_plot  <- Heart_Brain_mediation_plot  %>%
  mutate(category = fct_relevel(category, 
                                "MO",
                                "ICVF",
                                "ISOVF",
                                "l2",
                                "MD",
                                "OD",
                                "l1",
                                "FA",
                                "l3",
                                "Volume",
                                "Global Variance",
                                "Local Uniformity",
                                "Shape",
                                "Size",
                                "Local Dimness",
                                "Global Intensity"))

# Rename some measure column metrics for clarity (e.g. de -> direct effect and ie -> indirect effect)
Heart_Brain_mediation_plot$measure <- str_replace_all(Heart_Brain_mediation_plot$measure, 
                                                      c("de" = "direct effect", 
                                                        "ie" = "indirect effect"))

Heart_Brain_mediation_plot$mediator <- as.factor(Heart_Brain_mediation_plot$mediator)

Heart_Brain_mediation_plot %>%
  ggplot(aes(x = coefficient, y = category)) +
  stat_boxplot(
    geom = "errorbar",
    position = pd,
    width = 0.3,
    # colour = "darkgrey",
    alpha = 0.9,
    size = 0.3
  ) +
  geom_boxplot(
    aes(fill = category),
    #alpha =0.4,
    position = pd,
    # colour = "darkgrey",
    #  alpha = 0.9,
    size = 0.1,
    outlier.shape = NA
  ) +
  #geom_jitter(aes(colour = category,
  #                #alpha = mediator)
  #                alpha = 0.7,
  #                width = 0.1, 
  #                height = 0.1)) +
  geom_point(aes(colour = category,
                 alpha = 0.6),
             size = 0.9, 
             position = position_jitter()) +
  # position = "jitter"
  # ) +
  theme(legend.position = "none") +
  scale_fill_manual(values = mycolors) +
  scale_colour_manual(values = mycolors) +
  theme_bw() +
  theme(legend.position = "none") +
  #guides(fill = FALSE) +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9), 
        axis.title.x = element_text(face = "bold", size = 11), 
        axis.title.y = element_text(face = "bold", size = 11)) +
  # coord_flip ()+
  # Write the axis
  xlab("") +
  ylab("") +
  facet_wrap(~measure)