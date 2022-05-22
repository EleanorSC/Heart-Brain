## ---------------------------
##
## Script Purpose: Figure 3 demonstrating single mediation modelling 
##                 Geom_point plot comparing the estimated indirect effects of the mediators
##                 
##
##                 (1) Assign new column describing which latents are in which group (Heart v Brain)
##                 (2) Rename columns for clarity on plot
##                 (3) Assign point estimate colours for cardiac vs brain latent factors
##                 (4) Ensure point estimates are ordered in terms of effect size &
##                     grouped in terms of whether they are cardiac or brain latents
##                 (5) Set shape of points to indicate significance 
##
##
##

## Load packages -----
#install.packages("tidyverse")
#install.packages("ggplot2")
#
#library("tidyverse")
#library("ggplot2")

# Load supplementary table 10 from manuscript -----
plot <- read.csv("supplementary_table9.csv")

# Create column indicating whether heart or brain latent factor -----
plot  <-
  plot %>% mutate(
    type =
      case_when(
        grepl("heart", mediator) ~ "Heart",
        grepl("brain", mediator) ~ "Brain",
        TRUE ~ "All"
      )
  )

# Create column indicating which cluster -----
plot  <-
  plot %>% mutate(
    cluster_type =
      case_when(
        grepl("latent_pc", mediator) ~ "PCA",
        grepl("latent_cc", mediator) ~ "CCA",
        TRUE ~ "PCA"
      )
  )


# ### For tidy y-axis, remove underscores on mediator column -----
plot$mediator <- stringr::str_replace_all(plot$mediator,
                                          c("_" = " ",
                                            "pc" = "PC",
                                            "cc" = "CC" 
                                            ))

# Plot only direct & indirect effect -----
plot <- plot %>% filter(measure == "ie" | measure == "de")

# Rename some measure column metrics for clarity
# (e.g. de -> direct effect and ie -> indirect effect) -----
plot$measure <- stringr::str_replace_all(plot$measure,
                                         c("de" = "direct effect",
                                           "ie" = "indirect effect"))


## new order, group by type of dimensionality reduction method?
# Manual reorder note that setting x = reorder(mediator,-coefficient) works just as well -----
plot  <- plot  %>%
  mutate(mediator = fct_relevel(mediator, 
                                ## BRAIN LATENTS
                                "brain latent CC1",
                                "brain latent gMD",
                                "brain latent gFA",
                                "brain latent atrophy",
                                "brain latent PC2",
                                "brain latent PC3",
                                "brain latent CC3",
                                "brain latent PC1",
                                "brain latent grey",
                                "brain latent CC2",

                                ## BRAIN LATENTS
                                "heart latent CC1",
                                "heart latent PC1",
                                "heart latent PC3",
                                "heart latent CC3",
                                "heart latent PC2",
                                "heart latent CC2",
                                
                                
  ))


#### GEOM POINT PLOT -----
#### note: if you want the results displayed horizontally, #coord_flip, and include:
####        theme(axis.text.x = element_text(angle = 45, hjust = 1))
####        We have manually assigned an order to these point-estimates, but
####        x = reorder(mediator,-coefficient) works too

x <- ggplot(
  plot,
  aes(
    x = mediator,
    y = coefficient,
    colour = mediator,
    group = type,
    shape = Significant
  )
) +
  
  geom_point(position = position_dodge(width = 0.9),
             size = 2.2,
             stroke = 0.9) +
  
  geom_errorbar(
    aes(ymin = ci.lower,
        ymax = ci.upper),
    position = position_dodge(0.9),
    width = 0.3,
    colour = "darkgrey",
  # colour = "#414141",
    alpha = 0.9,
    size = 0.8
  ) +
  
  geom_hline(yintercept = 0, linetype = "dotted") +
  
  coord_flip() +
  theme_bw() +
  xlab("") +
  ylab("Indirect effect") +
  facet_wrap( ~ measure)

x +
  theme(legend.position="none",
        axis.title.x = element_text(
          size = 11,
          face = "bold",
          colour = "black"
        ),
        axis.title.y = element_text(
          size = 11,
          face = "bold",
          colour = "black"
        )
  ) +
  scale_shape_manual(values = c(1,
                                16)) +
  scale_colour_manual(values = mycolors)

###### 


