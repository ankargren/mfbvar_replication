source("plot_settings.R")
grid_vec <- seq(0, 1, length.out = 5)
dat <- expand.grid(x = grid_vec, y = grid_vec) %>%
  as_tibble()
grid_vec2x <- seq(grid_vec[2], grid_vec[4], length.out = 7)[-c(1, 7)]
grid_vec2y <- seq(grid_vec[3], grid_vec[5], length.out = 7)[-c(1, 7)]

grid_vec3x <- seq(grid_vec2x[3], grid_vec2x[5], length.out = 5)[-c(1, 5)]
grid_vec3y <- seq(grid_vec2y[3], grid_vec2y[5], length.out = 5)[-c(1, 5)]

dat2 <- expand.grid(x = grid_vec2x,
                    y = grid_vec2y)
dat3 <- expand.grid(x = grid_vec3x,
                    y = grid_vec3y)
point_size <- 3

grid1 <- ggplot(dat, aes(x = x, y = y)) + geom_point(size = point_size) + geom_point(aes(x = grid_vec2x[3], y = grid_vec2y[3]), color = "black", size = 5, shape = 22, fill = "red") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(y = "$\\lambda_1$",
       x = "$\\lambda_2$") +
  theme(axis.title.y = element_text(size = 3/2*theme_get()$axis.title.y$size),
        axis.title.x = element_text(size = 3/2*theme_get()$axis.title.x$size))

grid2 <- ggplot(dat, aes(x = x, y = y)) + geom_point(alpha = 0.5, size = point_size) + geom_point(data = dat2, size = point_size) + geom_point(aes(x = grid_vec3x[2], y = grid_vec3y[2]), color = "black", size = 5, shape = 22, fill = "red") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(y = "$\\lambda_1$",
       x = "$\\lambda_2$") +
  theme(axis.title.y = element_text(size = 3/2*theme_get()$axis.title.y$size),
        axis.title.x = element_text(size = 3/2*theme_get()$axis.title.x$size))


grid3 <- ggplot(dat, aes(x = x, y = y)) + geom_point(alpha = 0.25, size = point_size) + geom_point(data = dat2, alpha = 0.5, size = point_size) + geom_point(data = dat3, size = point_size) + geom_point(aes(x = grid_vec3x[3], y = grid_vec3y[3]), color = "black", size = 5, shape = 22, fill = "red") +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(y = "$\\lambda_1$",
       x = "$\\lambda_2$") +
  theme(axis.title.y = element_text(size = 3/2*theme_get()$axis.title.y$size),
        axis.title.x = element_text(size = 3/2*theme_get()$axis.title.x$size))

tikzDevice::tikz(file = sprintf("figure/grid%s.tex", 1), width = 6, height = 6)
grid1
dev.off()

tikzDevice::tikz(file = sprintf("figure/grid%s.tex", 2), width = 6, height = 6)
grid2
dev.off()

tikzDevice::tikz(file = sprintf("figure/grid%s.tex", 3), width = 6, height = 6)
grid3
dev.off()
