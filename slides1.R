
# Printing
options(digits=2, width=120)

# libraries
library(tidyverse)
library(ggthemes)

## ----eval=FALSE, echo=TRUE------------------------------
#> # Install from CRAN
#> install.packages(c("tourr",
#>                    "geozoo",
#>                    "spinifex",
#>                    "liminal",
#>                    "tidyverse",
#>                    "palmerpenguins",
#>                    "RColorBrewer"))
#> # Getting help
#> help(package="tourr")


## -------------------------------------------------------
sessionInfo()


## ----runthis1, echo=TRUE--------------------------------
library(tidyverse)
library(palmerpenguins)
penguins <- penguins %>% filter(!is.na(bill_length_mm)) 
penguins_sub <- penguins[,c(1, 3:6)] %>% 
  mutate(across(where(is.numeric),  ~ scale(.)[,1])) %>%
  rename(bl = bill_length_mm,
         bd = bill_depth_mm,
         fl = flipper_length_mm,
         bm = body_mass_g)
summary(penguins_sub)


## ----echo=TRUE, eval=FALSE------------------------------
#> ggplot(penguins_sub,
#>    aes(x=fl,
#>        y=bm,
#>        colour=species)) +
#>   geom_point(alpha=0.8,
#>              size=3) +
#>   scale_colour_brewer("",
#>     palette="Dark2") +
#>   theme(aspect.ratio=1,
#>   legend.position="bottom")


## ----runthis2, fig.width=5, fig.height=5, out.width="100%"----
ggplot(penguins_sub, 
   aes(x=fl, 
       y=bm, 
       colour=species)) +
  geom_point(alpha=0.8, 
             size=3) +
  scale_colour_brewer("",
    palette="Dark2") + 
  theme(aspect.ratio=1,
  legend.position="bottom")


## ----runthis3, echo=TRUE, eval=FALSE--------------------
library(RColorBrewer)
clrs <- brewer.pal(3, "Dark2")
col <- clrs[
   as.numeric(
     penguins_sub$species)]
 animate_xy(penguins_sub[,2:5],
            col=col,
            axes="off")


## ----eval=FALSE-----------------------------------------
#> library(RColorBrewer)
#> set.seed(20200622)
#> clrs <- brewer.pal(3, "Dark2")
#> col <- clrs[as.numeric(penguins_sub$species)]
#> render_gif(penguins_sub[,2:5], grand_tour(),
#>            display_xy(col=col, axes="bottomleft"),
#>            "penguins2d.gif", apf=1/5,
#>            frames=100, width=300, height=300)


## ----fig.width=4, fig.height=4, out.width="100%"--------
ggplot(penguins_sub, aes(x=bl, y=bd, colour=species)) +
  geom_point() +
  scale_colour_brewer("", palette="Dark2") +
  theme(aspect.ratio=1)


## ----fig.width=4, fig.height=4, out.width="100%"--------
ggplot(penguins_sub, aes(x=bd, y=bm, colour=species)) +
  geom_point() +
  scale_colour_brewer("", palette="Dark2") +
  theme(aspect.ratio=1)


## ----understanding, eval=FALSE--------------------------
#> # Generate a plotly animation to demonstrate
#> library(plotly)
#> library(htmltools)
#> 
#> # Generate sequence of bases
#> # set.seed(3)
#> set.seed(4)
#> random_start <- basis_random(4)
#> bases <- save_history(penguins_sub[,3:6], grand_tour(2),
#>     start=random_start, max = 5)
#> bases[,,1] <- random_start # something needs fixing
#> tour_path <- interpolate(bases, 0.1)
#> d <- dim(tour_path)
#> 
#> # Make really big data of all projections
#> penguins_d <- NULL; penguins_axes <- NULL
#> for (i in 1:d[3]) {
#>   fp <- as.matrix(penguins_sub[,2:5]) %*%
#>     matrix(tour_path[,,i], ncol=d[2])
#>   fp <- tourr::center(fp)
#>   colnames(fp) <- c("d1", "d2")
#>   penguins_d <- rbind(penguins_d, cbind(fp, rep(i+10, nrow(fp))))
#>   fa <- cbind(matrix(0, d[1], d[2]),
#>               matrix(tour_path[,,i], ncol=d[2]))
#>   colnames(fa) <- c("origin1", "origin2", "d1", "d2")
#>   penguins_axes <- rbind(penguins_axes,
#>                          cbind(fa, rep(i+10, nrow(fa))))
#> }
#> colnames(penguins_d)[3] <- "indx"
#> colnames(penguins_axes)[5] <- "indx"
#> 
#> df <- as_tibble(penguins_d) %>%
#>   mutate(species = rep(penguins_sub$species, d[3]))
#> dfaxes <- as_tibble(penguins_axes) %>%
#>   mutate(labels=rep(colnames(penguins_sub[,2:5]), d[3]))
#> dfaxes_mat <- dfaxes %>%
#>   mutate(xloc = rep(max(df$d1)+1, d[3]*d[1]),
#>          yloc=rep(seq(-1.2, 1.2, 0.8), d[3]),
#>          coef=paste(round(dfaxes$d1, 2), ", ",
#>                     round(dfaxes$d2, 2)))
#> p <- ggplot() +
#>        geom_segment(data=dfaxes,
#>                     aes(x=d1*2-5, xend=origin1-5,
#>                         y=d2*2, yend=origin2,
#>                         frame = indx), colour="grey70") +
#>        geom_text(data=dfaxes, aes(x=d1*2-5, y=d2*2, label=labels,
#>                                   frame = indx), colour="grey70") +
#>        geom_point(data = df, aes(x = d1, y = d2, colour=species,
#>                                  frame = indx), size=1) +
#>        scale_colour_brewer("", palette="Dark2") +
#>        geom_text(data=dfaxes_mat, aes(x=xloc, y=yloc,
#>                                   label=coef, frame = indx)) +
#>        theme_void() +
#>        coord_fixed() +
#>   theme(legend.position="none")
#> pg <- ggplotly(p, width=700, height=400) %>%
#>   animation_opts(200, redraw = FALSE,
#>                  easing = "linear", transition=0)
#> save_html(pg, file="penguins_manual_demo.html")


## ----runthis4, eval=FALSE-------------------------------
#> sphere1 <- sphere.hollow(p=4)$points %>% as_tibble()
#> animate_xy(sphere1, axes="bottomleft")


## ----eval=FALSE-----------------------------------------
#> render_gif(sphere1, grand_tour(),
#>            display_xy(axes="bottomleft"),
#>            "sphere4d_1.gif", frames=100, width=400, height=400)


## ----runthis5, eval=FALSE-------------------------------
#> sphere2 <- sphere.solid.random(p=4)$points %>% as_tibble()
#> animate_xy(sphere2, axes="bottomleft")


## ----eval=FALSE-----------------------------------------
#> render_gif(sphere2, grand_tour(),
#>            display_xy(axes="bottomleft"),
#>            "sphere4d_2.gif", frames=100, width=400, height=400)


## ----runthis6, eval=FALSE-------------------------------
#> cube1 <- cube.face(p=4)$points %>% as_tibble()
#> animate_xy(cube1, axes="bottomleft")


## ----eval=FALSE-----------------------------------------
#> render_gif(cube1, grand_tour(),
#>            display_xy(axes="bottomleft"),
#>            "cube4d_1.gif", frames=100, width=400, height=400)


## ----runthis7, eval=FALSE-------------------------------
#> cube2 <- cube.solid.random(p=4)$points %>% as_tibble()
#> animate_xy(cube2, axes="bottomleft")


## ----eval=FALSE-----------------------------------------
#> render_gif(cube2, grand_tour(),
#>            display_xy(axes="bottomleft"),
#>            "cube4d_2.gif", frames=100, width=400, height=400)
#> 


## ----runthis8, eval=FALSE-------------------------------
#> torus <- torus(p = 4, n = 5000, radius=c(8, 4, 1))$points %>% as_tibble()
#> animate_xy(torus, axes="bottomleft")


## ----eval=FALSE-----------------------------------------
#> render_gif(torus, grand_tour(),
#>            display_xy(axes="bottomleft"),
#>            "torus4d.gif", frames=100, width=400, height=400)


## ----runthis9, eval=FALSE-------------------------------
#> mobius <- mobius()$points %>% as_tibble()
#> animate_xy(mobius, axes="bottomleft")


## ----eval=FALSE-----------------------------------------
#> render_gif(mobius, grand_tour(),
#>            display_xy(axes="bottomleft"),
#>            "mobius.gif", frames=100, width=400, height=400)


## ----eval=FALSE-----------------------------------------
#> set.seed(1000)
#> sphere1 <- sphere.hollow(p=4)$points %>% as_tibble()
#> sphere1$group <- "sphere"
#> t2 <- save_history(penguins_sub[,2:5], grand_tour(1),
#>   max = 20)
#> t2_interp <- tourr::interpolate(t2)
#> t2_interp_m <- apply(t2_interp, c(3,1), c) %>% as_tibble()
#> t2_interp_m$group <- "path"
#> tour_path_1d <- bind_rows(sphere1, t2_interp_m)
#> t_pch <- c(rep(16, nrow(sphere1)), rep(16, nrow(t2_interp_m)))
#> t_col <- c(rep("grey70", nrow(sphere1)), rep("orange", nrow(t2_interp_m)))
#> t_cex <- c(rep(1, nrow(sphere1)), rep(2, nrow(t2_interp_m)))
#> render_gif(tour_path_1d[,1:4], grand_tour(), display_xy(pch=t_pch, col=t_col, cex=t_cex, axes="off", half_range=0.6), gif_file="tour_path.gif", apf=1/5)


## ----eval=FALSE-----------------------------------------
#> clrs <- brewer.pal(3, "Dark2")
#> col <- clrs[
#>   as.numeric(
#>     penguins_sub$species)]
#> set.seed(2022)
#> render_gif(penguins_sub[,2:5], guided_tour(lda_pp(penguins_sub$species)),
#>            display_xy(col=col, axes="bottomleft"),
#>            "penguins2d_guided.gif",
#>            frames=100, width=300, height=300, loop=FALSE)


## ----runthis10, eval=FALSE------------------------------
animate_xy(penguins_sub[,2:5], grand_tour(),
            axes = "bottomleft", col=col)
set.seed(2022)
pp <- animate_xy(penguins_sub[,2:5],
           guided_tour(lda_pp(penguins_sub$species)),
           axes = "bottomleft", col=col)
best_proj <- pp$basis[length(pp$basis)][[1]] # Save the final projection


## ----eval=FALSE-----------------------------------------
#> clrs <- brewer.pal(3, "Dark2")
#> col <- clrs[as.numeric(penguins_sub$species)]
#> render_gif(data=penguins_sub[,2:5],
#>            tour_path = radial_tour(as.matrix(best_proj), mvar = 2),
#>            display = display_xy(col = col),
#>            gif_file = "penguins_rt_bd.gif",
#>            apf = 1/20,
#>            frames = 100,
#>            width = 300, height = 300)
#> 
#> render_gif(data=penguins_sub[,2:5],
#>            tour_path = radial_tour(as.matrix(best_proj), mvar = 1),
#>            display = display_xy(col = col),
#>            gif_file = "penguins_rt_bl.gif",
#>            apf = 1/20,
#>            frames = 100,
#>            width = 300, height = 300)
#> 
#> render_gif(data=penguins_sub[,2:5],
#>            tour_path = radial_tour(as.matrix(best_proj), mvar = 3),
#>            display = display_xy(col = col),
#>            gif_file = "penguins_rt_fl.gif",
#>            apf = 1/20,
#>            frames = 100,
#>            width = 300, height = 300)
#> 
#> render_gif(data=penguins_sub[,2:5],
#>            tour_path = radial_tour(as.matrix(best_proj), mvar = 4),
#>            display = display_xy(col = col),
#>            gif_file = "penguins_rt_bm.gif",
#>            apf = 1/20,
#>            frames = 100,
#>            width = 300, height = 300)
#> 


## ----runthis11, eval=FALSE------------------------------
#> clrs <- brewer.pal(3, "Dark2")
#> col <- clrs[as.numeric(penguins_sub$species)]
#> # Check contribution of bl, change mvar to switch variables
animate_xy(penguins_sub[,2:5],
           radial_tour(as.matrix(best_proj), 
                       mvar = 3),
           col = col)


## ----eval=FALSE-----------------------------------------
#> render_gif(penguins_sub[,2:5], local_tour(start=best_proj, 0.9),
#>            display_xy(col=col, axes="bottomleft"),
#>            "penguins2d_local.gif",
#>            frames=200, width=300, height=300)


## ----runthis12, eval=FALSE------------------------------
#> animate_xy(penguins_sub[,2:5], local_tour(start=best_proj, 0.9),
#>            axes = "bottomleft", col=col)


## ----runthis13, eval=FALSE------------------------------
#> sphere2 <- sphere.solid.random(p=4)$points %>% as_tibble()
#> animate_slice(sphere2, axes="bottomleft")


## ----eval=FALSE-----------------------------------------
#> render_gif(sphere2, grand_tour(),
#>            display_slice(axes="bottomleft"),
#>            "sphere4d_solid_slice.gif", frames=100, width=400, height=400)
#> 


## ----runthis14, eval=FALSE------------------------------
#> sphere1 <- sphere.hollow(p=4)$points %>% as_tibble()
#> animate_slice(sphere1, axes="bottomleft", half_range=0.6)


## ----eval=FALSE-----------------------------------------
#> render_gif(sphere1, grand_tour(),
#>            display_slice(axes="bottomleft", half_range=0.6),
#>            "sphere4d_slice.gif", frames=100, width=400, height=400)


## ----runthis15, eval=FALSE------------------------------
#> torus <- torus(p = 4, n = 5000, radius=c(8, 4, 1))$points %>% as_tibble()
#> animate_slice(torus, axes="bottomleft", half_range=0.8)


## ----eval=FALSE-----------------------------------------
#> render_gif(torus, grand_tour(),
#>            display_slice(axes="bottomleft", half_range=0.8),
#>            "torus4d_slice.gif", frames=100, width=400, height=400)


## ----runthis16, eval=FALSE------------------------------
#> cube1 <- cube.face(p=4)$points %>% as_tibble()
#> # Slicing needs data to be on a standard scale
#> cube1_std <- cube1 %>%
#>   mutate(across(where(is.numeric),  ~ scale(.)[,1]))
#> animate_slice(cube1_std, axes="bottomleft")


## ----eval=FALSE-----------------------------------------
#> render_gif(cube1_std, grand_tour(),
#>            display_slice(axes="bottomleft"),
#>            "cube4d_slice.gif", frames=100, width=400, height=400)


## ----runthis17, eval=FALSE, echo=TRUE-------------------
#> penguins_pca <- prcomp(penguins_sub[,2:5], center = FALSE)
#> penguins_coefs <- penguins_pca$rotation[, 1:3]
#> penguins_scores <- penguins_pca$x[, 1:3]
#> clrs <- brewer.pal(3, "Dark2")
#> col <- clrs[
#>   as.numeric(
#>     penguins_sub$species)]
#> animate_pca(penguins_scores, pc_coefs = penguins_coefs, col=col)


## ----eval=FALSE-----------------------------------------
#> render_gif(penguins_scores, grand_tour(),
#>            display_pca(pc_coefs = penguins_coefs,
#>                        col=col, axes="bottomleft"),
#>            "penguins2d_pca.gif",
#>            frames=100, width=300, height=300)


## ----eval=FALSE-----------------------------------------
#> 
#> render_gif(data=penguins_sub[,2:5],
#>            tour_path = grand_tour(1),
#>            display = display_dist(half_range = 1.3),
#>            gif_file = "penguins1d.gif",
#>            apf = 1/20,
#>            frames = 100,
#>            width = 300, height = 300)
#> render_gif(penguins_sub[,2:5], grand_tour(),
#>            display_density2d(col=col, axes="bottomleft"),
#>            "penguins2d_dens.gif",
#>            frames=100, width=300, height=300)


## ----runthis18, eval=FALSE------------------------------
#> animate_dist_cl(penguins_sub[,2:5], half_range=1.3)
#> animate_density2d(penguins_sub[,2:5], col=col, axes="bottomleft")

