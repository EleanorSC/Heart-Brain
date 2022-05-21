## ---------------------------
##
## Script Purpose: Figure demonstrating Brain IDP and CMR IDP indirect efffect sizes
##                 Boxplot demonstrating how different features  mediate the VRF-CF association
##
##                 (1) Combine both brain IDP SEM analyses and CMR IDP mediation results
##                 (2) Rename columns for clarity on plot
##                 (3) Assign boxplot colours for cardiac vs brain IDPs
##                 (4) Ensure boxplots are ordered in terms of effect size &
##                     grouped in terms of whether they are cardiac or brain IDPs
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

# Match to column names in supplementary table 11 -----
names(CMR_plot)[names(CMR_plot) == "cluster"] <- "category"
names(CMR_plot)[names(CMR_plot) == "feature"] <- "mediator"

# Combine mediation analyses -----
Heart_Brain_mediation_plot <- rbind(CMR_plot,
                                    brain_IDP_plot)

# Rename some mediator column metrics for clarity (e.g. fa -> FA and md -> MD) -----
Heart_Brain_mediation_plot$category <- stringr::str_replace_all(Heart_Brain_mediation_plot$category, 
                                                       c("fa" = "FA", 
                                                         "md" = "MD",
                                                         "icvf" = "ICVF",
                                                         "isovf" = "ISOVF",
                                                         "od" = "OD",
                                                         "mo" = "MO",
                                                         "volume" = "Volume"))

# NOTE that grDevices::Blue-Red colour palette works well for this figure 
# Here I have individually assigned hex colours to demonstrate which colour affects which boxplot
# mycolors <- paletteer::paletteer_c("grDevices::Blue-Red",18) 

mycolors <- (values = c( # BRAIN IDP COLOUR ASSIGNMENTS  -----
                         
                        "#023FA5FF", # MO
                        "#455AA7FF", # ICVF
                        "#3C5488B2", # ISOVF
                        "#6673B1FF", # l2
                        "#828BBBFF", # MD
                        "#9BA1C5FF", # OD
                        "#8796C2FF", # l1
                        "#95AFD0FF", # FA
                        "#A7C6DDFF", # L3
                        "#7695f0",   # Volume (note consider revising- too blue?)
                        
                        # CARDIAC MRI IDP COLOUR ASSIGNMENTS  -----
                        
                        "#DED3D5FF", # Global variance
                        "#C8969FFF", # Local uniformity
                        "#BD7B88FF", # Shape
                        "#AF5E70FF", # Size
                        "#A03C56FF", # Local Dimness
                        "#8E063BFF"  # Global intensity
))

# Manual reorder note that setting x = reorder(mediator,-coefficient) works just as well -----
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

# Rename some measure column metrics for clarity 
# (e.g. de -> direct effect and ie -> indirect effect) -----
Heart_Brain_mediation_plot$measure <- stringr::str_replace_all(Heart_Brain_mediation_plot$measure, 
                                                      c("de" = "direct effect", 
                                                        "ie" = "indirect effect"))

Heart_Brain_mediation_plot$mediator <- as.factor(Heart_Brain_mediation_plot$mediator)

#### Note consider revising still:
#### (1) gghighlight L thalamic radiation volume

# Boxplot -----
pd = position_dodge(width = 0.5)

Heart_Brain_mediation_plot %>%
  ggplot(aes(x = coefficient, y = category)) +
  stat_boxplot(
    geom = "errorbar",
    position = pd,
    width = 0.3,
   #  colour = "darkgrey",
    alpha = 0.9,
    size = 0.6
  ) +
  geom_boxplot(
    aes(fill = category),
    position = pd,
   #  colour = "darkgrey",
    #  alpha = 0.9,
    size = 0.1,
    outlier.shape = NA
  ) +
  geom_point(aes(colour = category,
                 alpha = 0.6),
             size = 0.9, 
             position = position_jitter()) +
  theme(legend.position = "none") +
  scale_fill_manual(values = mycolors) +
  scale_colour_manual(values = mycolors) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9), 
        axis.title.x = element_text(face = "bold", size = 11), 
        axis.title.y = element_text(face = "bold", size = 11)) +
  # coord_flip ()+
  xlab("") +
  ylab("") +
  facet_wrap(~measure)

### END