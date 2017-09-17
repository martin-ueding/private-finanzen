library(ggplot2)
library(tidyr)
library(reshape2)

plotopts <- list(svg = list(width=6, height=4))

theme_set(theme_light())

my.ggsave <- function(filename, ...) {
    if (!dir.exists('output')) {
        dir.create('output')
    }
    ggsave(paste('output/', filename, '.svg', sep = ''), width = 5, height = 4, ...)
    ggsave(paste('output/', filename, '.png', sep = ''), width = 5, height = 4, dpi = 150, ...)
}

pdf(width = 7, height = 4)
