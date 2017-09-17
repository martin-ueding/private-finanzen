library(privatefinanzen)
source('plot-setup.R')

einkommen <- seq(0, 300000, 2000)
df.grenzsteuersatz = data.frame(einkommen = einkommen / 1000,
                                grenzsteuersatz = grenzsteuersatz(einkommen) * 100)

ggplot(df.grenzsteuersatz, aes(x = einkommen, y = grenzsteuersatz)) +
    geom_line() +
    labs(x = 'Jahresbruttoeinkommen / 1000 EUR', y = 'Grenzsteuersatz / %',
         title = 'Grenzsteuersatz')
my.ggsave('grenzsteuersatz')

makeplots <- function(bruttoeinkommen, kinder) {
    einzahlungen <- seq(0, 3000, 10)
    zulage <- riester.zulage(einzahlungen, bruttoeinkommen, kinder)
    erlass <- riester.steuererlass(einzahlungen, bruttoeinkommen, kinder)
    vorteil <- pmax(zulage, erlass)
    eigenanteil <- einzahlungen - vorteil

    df.eigenanteil = data.frame(einzahlungen = einzahlungen,
                                zulage = zulage,
                                erlass = erlass,
                                vorteil = vorteil,
                                eigenanteil = eigenanteil,
                                rendite = vorteil / eigenanteil)

    cat('Mein Grenzsteuersatz: ', grenzsteuersatz(bruttoeinkommen), '\n')
    cat('4% des Bruttoeinkommens: ', 0.04 * bruttoeinkommen, '\n')

    abline.4.prozent <- geom_vline(xintercept = 0.04 * bruttoeinkommen, color = 'gray')
    abline.2100 <- geom_vline(xintercept = 2100, color = 'gray')

    subtitle <- sprintf('Bruttoeinkommen %d EUR + %d Kinder', bruttoeinkommen, kinder)

    gathered = gather(df.eigenanteil,
                      value = 'value', key = 'type',
                      erlass, zulage)
    p <- ggplot(gathered, aes(x = einzahlungen, y = value, color = type)) +
        geom_line() +
        geom_line(aes(x = einzahlungen, y = vorteil), color = 'black', size = 2, alpha = 0.2) +
        abline.4.prozent + abline.2100 +
        labs(x = 'Einzahlungen / Jahr EUR', y = 'Vorteil / Jahr EUR',
             title = 'Zulage und Steuervorteil',
             subtitle = subtitle,
             type = 'Förderung')
    my.ggsave(sprintf('riester-vorteil-%d-%d', bruttoeinkommen, kinder))
    print(p)

    p <- ggplot(df.eigenanteil, aes(x = einzahlungen, y = rendite)) +
        geom_line() +
        abline.4.prozent + abline.2100 +
        labs(x = 'Einzahlungen / Jahr EUR', y = 'Vorteil / Eigenanteil',
             title = 'Rendite durch Förderung und Steuervorteil',
             subtitle = subtitle)
    my.ggsave(sprintf('riester-effizienz-%d-%d', bruttoeinkommen, kinder))
    print(p)
}

makeplots(22423, 0)
makeplots(55000, 0)
makeplots(0, 2)
