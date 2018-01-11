library(tidyverse)
library(knitr)
library(lubridate)
library(tikzDevice)
library(mfbvar)
scale_linetype_manual <- function(...) ggplot2::scale_linetype_manual(..., values = c("solid", "F3", "66", "22"))
theme_set(theme_minimal(base_size = 10))
theme_update(axis.text.y = element_text(size = 18, color = "black"),
             axis.text.x = element_text(size = 18, margin = margin(t = 5), color = "black"),
             axis.title.x = element_text(size = 20, margin = margin(t = 20, b = 1)),
             axis.title.y = element_text(size = 20),
             legend.key.width = unit(0.3, "inch"),
             legend.key.height = unit(0.25, "inch"),
             legend.box.margin = margin(6, 4, 6, 6),
             legend.text=element_text(size=18),
             legend.title=element_text(size=20),
             plot.title = element_text(size=18),
             panel.grid = element_blank(),
             axis.line = element_line(size = 0.5, colour = "black"),#colour = "grey80"),
             axis.ticks = element_line(size = 1, colour = "black"),#, colour = "grey80"),
             panel.spacing = unit(0.15, "inch"),
             strip.text = element_text(size = 10))
theme_update(legend.position = "right")


opts_chunk$set(fig.width=6,
               fig.asp=0.62,
               cache=TRUE,
               dev='tikz',
               external=TRUE,
               fig.align='center',
               echo=FALSE
)

full_size <-   theme(panel.grid.major = element_blank(),
                     axis.title.y = element_text(size = 0.5*theme_get()$axis.title.y$size),
                     axis.title.x = element_text(size = 0.5*theme_get()$axis.title.x$size),
                     axis.text.y = element_text(size = 0.5*theme_get()$axis.text.y$size),
                     axis.text.x = element_text(size = 0.5*theme_get()$axis.text.x$size),
                     legend.text = element_text(size = 0.5*theme_get()$legend.text$size),
                     legend.title = element_text(size = 0.5*theme_get()$legend.title$size),
                     legend.key.width = unit(0.4, "inch"),
                     legend.key.height = unit(0.15, "inch"))

facet_lines <- theme(panel.grid.major = element_line(size = 0.5, colour = "grey90"))
