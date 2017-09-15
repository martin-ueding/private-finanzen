library(ggplot2)
library(tidyr)

plotopts <- list(svg = list(width=6, height=4))

theme_set(theme_light())

my.ggsave <- function(filename, ...) {
    ggsave(filename, width = 5, height = 4, ...)
}
