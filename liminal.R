library(tidyverse)
library(liminal)
library(Rtsne)
data(fake_trees)

set.seed(2020)
tsne <- Rtsne::Rtsne(dplyr::select(fake_trees, dplyr::starts_with("dim")))
tsne_df <- data.frame(tsneX = tsne$Y[, 1], tsneY = tsne$Y[, 2])
limn_tour_link(
  tsne_df,
  fake_trees,
  cols = dim1:dim10,
  color = branches
)

library(palmerpenguins)
penguins <- penguins %>%
  na.omit() # 11 observations out of 344 removed
# use only vars of interest, and standardise
# them for easier interpretation
penguins_sub <- penguins[,c(1, 3:6)] %>% 
  mutate(across(where(is.numeric),  ~ scale(.)[,1])) %>%
  rename(bl = bill_length_mm,
         bd = bill_depth_mm,
         fl = flipper_length_mm,
         bm = body_mass_g)

set.seed(2022)
p_tsne <- Rtsne::Rtsne(penguins_sub[,2:5])
p_tsne_df <- data.frame(tsneX = p_tsne$Y[, 1], tsneY = p_tsne$Y[, 2])
limn_tour_link(
  p_tsne_df,
  penguins_sub,
  cols = bl:bm,
  color = species
)

