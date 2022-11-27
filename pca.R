# Generate the 2D plane of PCA and plot with data

library(tidyverse)
library(palmerpenguins)
penguins <- penguins %>% filter(!is.na(bill_length_mm)) 
penguins_sub <- penguins[,c(1, 3:6)] %>% 
  mutate(across(where(is.numeric),  ~ scale(.)[,1])) %>%
  rename(bl = bill_length_mm,
         bd = bill_depth_mm,
         fl = flipper_length_mm,
         bm = body_mass_g)

# Calculate PCA
penguins_pca <- prcomp(penguins_sub[,2:5], scale=FALSE)
ggplot(as_tibble(penguins_pca$x), aes(x=PC1, y=PC2)) + 
  geom_point() +
  theme(aspect.ratio=1)

# Create 4 points on corners of plane on first two PCs
penguins_m <- penguins_sub %>%
  summarise_if(is.numeric, mean)
# corners: -1, -1
#          -1, 1
#           1, -1
#           1, 1
penguins_pc_plane <- bind_rows(penguins_m,
                               penguins_m,
                               penguins_m,
                               penguins_m)
penguins_pc_plane[1,] <- penguins_pc_plane[1,]-
  3*penguins_pca$rotation[,1]-3*penguins_pca$rotation[,2]
penguins_pc_plane[2,] <- penguins_pc_plane[2,]-
  3*penguins_pca$rotation[,1]+3*penguins_pca$rotation[,2]
penguins_pc_plane[3,] <- penguins_pc_plane[3,]+
  3*penguins_pca$rotation[,1]-3*penguins_pca$rotation[,2]
penguins_pc_plane[4,] <- penguins_pc_plane[4,]+
  3*penguins_pca$rotation[,1]+3*penguins_pca$rotation[,2]

penguins_pc_plane <- bind_rows(penguins_pc_plane, penguins_sub[,2:5])
penguins_pc_plane_edges <- as.matrix(data.frame(from = c(1, 1, 2, 3),
                                  to = c(2, 3, 4, 4)))
col <- c(rep("orange", 4), rep("black", nrow(penguins_sub)))
lcol <- c(rep("orange", 4), rep("black", nrow(penguins_sub)))
animate_xy(penguins_pc_plane, col=col, 
           edges=penguins_pc_plane_edges,
           edges.col=lcol, axes="bottomleft")

render_gif(penguins_pc_plane, grand_tour(), 
           display_xy(col=col, 
                      edges=penguins_pc_plane_edges,
                      edges.col=lcol, axes="bottomleft"), 
           "penguins_pca_plane.gif", apf=1/5, 
           frames=100, width=300, height=300)
