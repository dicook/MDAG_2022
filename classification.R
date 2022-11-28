# Classification boundaries
library(tidyverse)
library(palmerpenguins)
library(randomForest)
library(MASS)
library(RColorBrewer)
library(tourr)

# Get data
penguins <- penguins %>% filter(!is.na(bill_length_mm)) 
penguins_sub <- penguins[,c(1, 3:6)] %>% 
  mutate(across(where(is.numeric),  ~ scale(.)[,1])) %>%
  rename(bl = bill_length_mm,
         bd = bill_depth_mm,
         fl = flipper_length_mm,
         bm = body_mass_g)

# Fit model
penguins_rf <- randomForest(species~., data=penguins_sub,  
                            ntree = 1000)
penguins_lda <- lda(species~., data=penguins_sub)

# Generate grid of values
set.seed(1124)
penguins_grid <- tibble(bl=runif(1000, -3, 3),
                        bd=runif(1000, -3, 3),
                        fl=runif(1000, -3, 3),
                        bm=runif(1000, -3, 3))
penguins_grid <- penguins_grid %>%
  mutate(rf_pred = predict(penguins_rf, penguins_grid),
         lda_pred = predict(penguins_lda, penguins_grid)$class)

# Generate a tour path to compare classifications
clbases <- save_history(penguins_sub[,2:5], max = 5)

# Show forest boundaries
library(RColorBrewer)
clrs <- brewer.pal(3, "Dark2")

rfcol <- clrs[
  as.numeric(
    penguins_grid$rf_pred)]
animate_slice(penguins_grid[,1:4], 
              planned_tour(clbases),
              col=rfcol, axes="bottomleft")
render_gif(penguins_grid[,1:4], 
           planned_tour(clbases),
           display_slice(col=rfcol, 
                        axes="bottomleft"),
           "penguins_rf.gif", apf=1/5, 
           frames=100, width=300, height=300)

# Show lda boundaries
ldacol <- clrs[
  as.numeric(
    penguins_grid$lda_pred)]
animate_slice(penguins_grid[,1:4], planned_tour(clbases),
              col=ldacol, axes="bottomleft")
render_gif(penguins_grid[,1:4], 
           planned_tour(clbases),
           display_slice(col=ldacol, 
                         axes="bottomleft"),
           "penguins_lda.gif", apf=1/5, 
           frames=100, width=300, height=300)

# Save both to html in order to replay together
cltour_path <- interpolate(clbases, 0.1)
d <- dim(cltour_path)
p_frames <- NULL
for (i in 1:d[3]) {
  cat(i, "\n")
  d1 <- as.matrix(penguins_grid[,1:4]) %*% matrix(cltour_path[,,i], ncol=2)
  d1 <- data.frame(d1)
  d1$rf_pred <- penguins_grid$rf_pred
  d1$lda_pred <- penguins_grid$lda_pred
  d1$frame <- i
  p_frames <- rbind(p_frames, d1)
}

p_rf <- ggplot(p_frames, aes(x=X1, y=X2, colour=rf_pred, frame=frame)) +
  geom_point() +
  scale_colour_brewer("", palette="Dark2")
pg_rf <- ggplotly(p_rf, width=450, height=450) %>% animation_opts(200, redraw = FALSE, easing = "linear", transition=0)
save_html(pg, file="penguins_rf.html")

p_lda <- ggplot(p_frames, aes(x=X1, y=X2, colour=lda_pred, frame=frame)) +
  geom_point() +
  scale_colour_brewer("", palette="Dark2")
pg_lda <- ggplotly(p_lda, width=450, height=450) %>% animation_opts(200, redraw = FALSE, easing = "linear", transition=0)
save_html(pg, file="penguins_lda.html")

