# Classification boundaries
library(tidyverse)
library(palmerpenguins)
library(RColorBrewer)

# Get data
penguins <- penguins %>% filter(!is.na(bill_length_mm)) 
penguins_sub <- penguins[,c(1, 3:6)] %>% 
  mutate(across(where(is.numeric),  ~ scale(.)[,1])) %>%
  rename(bl = bill_length_mm,
         bd = bill_depth_mm,
         fl = flipper_length_mm,
         bm = body_mass_g)


# Generate a tour path 
bases <- save_history(penguins_sub[,2:5], max = 5)
tour_path <- interpolate(bases, 0.1)

d <- dim(tour_path)
p_frames <- NULL
for (i in 1:d[3]) {
  cat(i, "\n")
  d1 <- as.matrix(penguins_sub[,2:5]) %*% matrix(tour_path[,,i], ncol=2)
  d1 <- data.frame(d1)
  d1$species <- penguins_sub$species
  d1$frame <- i
  p_frames <- rbind(p_frames, d1)
}

p <- ggplot(p_frames, aes(x=X1, y=X2, 
                          colour=species, 
                          frame=frame)) +
  geom_point() +
  scale_colour_brewer("", palette="Dark2")
pg <- ggplotly(p, width=450, height=450) %>% animation_opts(200, redraw = FALSE, easing = "linear", transition=0)
save_html(pg, file="penguins_plotly.html")

