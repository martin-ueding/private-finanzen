library(ggplot2)
library(tidyr)

theme_set(theme_light())

get.grenzsteuersatz <- Vectorize(function(einkommen) {
    if (einkommen <= 8820) {
        return (0)
    }
    else if (einkommen <= 53665) {
        unten.e <- 8820
        oben.e <- 53665

        unten.s <- 0.14
        oben.s <- 0.42

        anteil <- (einkommen - unten.e) / (oben.e - unten.e)
        return (unten.s + anteil * (oben.s - unten.s))
    }
    else if (einkommen <= 254446) {
        return (0.42)
    }
    else {
        return (0.45)
    }
})

get.zulage <- Vectorize(function(einzahlung, bruttoeinkommen, kinder) {
    max.zulage <- 175 + kinder * 300

    vier.prozent <- bruttoeinkommen * 0.04

    if (einzahlung < 60) {
        zulage <- 0
    }
    else {
        zulage <- min(einzahlung / vier.prozent, 1) * max.zulage
    }

    return (zulage)
}, vectorize.args = c('einzahlung'))

get.steuererlass <- Vectorize(function(einzahlung, bruttoeinkommen, kinder) {
    maximal.absetzbar <- 2100
    zulage <- get.zulage(einzahlung, bruttoeinkommen, kinder) 
    erlass <- get.grenzsteuersatz(bruttoeinkommen) * min(einzahlung, maximal.absetzbar)
    max(erlass - zulage, 0)
}, vectorize.args = c('einzahlung'))

einkommen <- seq(0, 300000, 2000)
df.grenzsteuersatz = data.frame(einkommen = einkommen / 1000,
                                grenzsteuersatz = get.grenzsteuersatz(einkommen) * 100)

my.png <- list(suffix = 'png', options = list(width=600, height=400, res = 150))
my.svg <- list(suffix = 'svg', options = list(width=6, height=4))
my.pdf <- list(suffix = 'pdf', options = list(width=6, height=4))

my.dev <- function(name, bruttoeinkommen, kinder) {
    filename <- sprintf('plot-riester-%s-%d-%d.%s', name, bruttoeinkommen, kinder, my$suffix)
    do.call(my$suffix, c(filename, my$options))
}

my <- my.png

my.dev('grenzsteuersatz', 0, 0)
p <- ggplot(df.grenzsteuersatz, aes(x = einkommen, y = grenzsteuersatz)) +
    geom_line() +
    labs(x = 'Jahresbruttoeinkommen / 1000 EUR', y = 'Grenzsteuersatz / %',
         title = 'Grenzsteuersatz')
print(p)

makeplots <- function(bruttoeinkommen, kinder) {
    einzahlungen <- seq(0, 3000, 10)
    zulage <- get.zulage(einzahlungen, bruttoeinkommen, kinder)
    erlass <- get.steuererlass(einzahlungen, bruttoeinkommen, kinder)
    vorteil <- pmax(zulage, erlass)
    eigenanteil <- einzahlungen - vorteil

    df.eigenanteil = data.frame(einzahlungen = einzahlungen,
                                zulage = zulage,
                                erlass = erlass,
                                vorteil = vorteil,
                                eigenanteil = eigenanteil,
                                rendite = vorteil / eigenanteil)

    cat('Mein Grenzsteuersatz: ', get.grenzsteuersatz(bruttoeinkommen), '\n')
    cat('4% des Bruttoeinkommens: ', 0.04 * bruttoeinkommen, '\n')

    abline.4.prozent <- geom_vline(xintercept = 0.04 * bruttoeinkommen, color = 'gray')
    abline.2100 <- geom_vline(xintercept = 2100, color = 'gray')

    subtitle <- sprintf('Bruttoeinkommen %d EUR + %d Kinder', bruttoeinkommen, kinder)

    gathered = gather(df.eigenanteil,
                      value = 'value', key = 'type',
                      erlass, zulage)

    my.dev('vorteil', bruttoeinkommen, kinder)
    p <- ggplot(gathered, aes(x = eigenanteil, y = value, color = type)) +
        geom_line() +
        geom_line(aes(x = eigenanteil, y = vorteil), color = 'black', size = 2, alpha = 0.2) +
        abline.4.prozent + abline.2100 +
        labs(x = 'Einzahlungen / Jahr EUR', y = 'Vorteil / Jahr EUR',
             title = 'Zulage und Steuervorteil',
             subtitle = subtitle,
             type = 'Förderung')
    print(p)

    my.dev('effizienz', bruttoeinkommen, kinder)
    p <- ggplot(df.eigenanteil, aes(x = eigenanteil, y = rendite)) +
        geom_line() +
        abline.4.prozent + abline.2100 +
        labs(x = 'Einzahlungen / Jahr EUR', y = 'Vorteil / Eigenanteil',
             title = 'Rendite durch Förderung und Steuervorteil',
             subtitle = subtitle)
    print(p)
}

makeplots(22423, 0)
makeplots(55000, 0)
makeplots(0, 2)
