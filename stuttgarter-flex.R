library(privatefinanzen)
source('plot-setup.R')

pkosten.vertrieb <- 0.0244
kosten.vertrieb <- 20.46
kosten.sonst <- 32.46
pkosten.sonst <- 0.0048

jahr <- seq(2016, 2076, 1)

einzahlung <- c(rep(25 * 12, 41), rep(0, 20))

einzahlung <- einzahlung * 1.05^(1:length(einzahlung) - 1)

renditen <- c(0, 3, 6)
rendite.wort <- function(name, rendite) sprintf('%s.%i', name, rendite)

erzeuge.streuung <- function(name, len) {
    r <- list()

    for (rendite in renditen) {
        r[[rendite.wort(name, rendite)]] <- rep(0, len)
    }

    return (r)
}

guthaben <- erzeuge.streuung('guthaben', length(jahr))
kosten <- erzeuge.streuung('kosten', length(jahr))

beitragssumme <-  sum(einzahlung) * 12
cat('Beitragssumme: ', beitragssumme, '\n')

for (i in 1:length(einzahlung)) {
    for (rendite in renditen) {
        # Die bisherigen Beiträge ist die Summe aller Einzahlungen der Jahre,
        # die kleiner als das aktuelle sind.
        bisherige.beiträge <- sum(einzahlung[1:length(einzahlung) < i])
        # Die künftigen Beiträge ist der aktuelle Beitrag die restlichen Jahre.
        künftige.beiträge <- einzahlung[i] * max(sum(einzahlung > 0) - i + 1, 0)
        beitragssumme[i] <- bisherige.beiträge + künftige.beiträge

        altes.guthaben <- ifelse(i == 1, 0, guthaben[[rendite.wort('guthaben', rendite)]][i - 1])

        kosten[[rendite.wort('kosten', rendite)]][i] <-
            ifelse(i <= 5, pkosten.vertrieb * beitragssumme / 5, 0) +
            kosten.vertrieb +
            kosten.sonst +
            pkosten.sonst * altes.guthaben

        guthaben[[rendite.wort('guthaben', rendite)]][i] <-
            altes.guthaben * (1 + (rendite) / 100) +
            einzahlung[i] -
            kosten[[rendite.wort('kosten', rendite)]][i]
    }
}

eigenbeitrag.accum <- cumsum(einzahlung)

cols <- c(list(jahr = jahr,
               einzahlung = einzahlung * 12,
               eigenbeitrag.accum = eigenbeitrag.accum,
               beitragssumme = beitragssumme),
          kosten,
          guthaben)

df <- do.call('data.frame', cols)

print(df)
cat('\n')
print(df[41, ])

write.table(df, file='output/stuttgarter-flex.tsv')

df2 <- gather(df, value = 'value', key = 'type',
              einzahlung, kosten.0, kosten.3, kosten.6)
ggplot(df2, aes(x = jahr, y = value, color = type)) +
    geom_line() +
    labs(title = 'WWK Premium FörderRente protect',
         subtitle = 'Laufende Größen',
         type = 'Größe',
         x = 'Jahr',
         y = 'EUR')
my.ggsave('stuttgarter-flex-laufend')

df2 <- gather(df, value = 'value', key = 'type',
              guthaben.0, guthaben.3, guthaben.6, beitragssumme, eigenbeitrag.accum)
ggplot(df2, aes(x = jahr, y = value / 1000, color = type)) +
    geom_line() +
    labs(title = 'WWK Premium FörderRente protect',
         subtitle = 'Akkumulative Größen',
         type = 'Größe',
         x = 'Jahr',
         y = '1000 EUR')
my.ggsave('stuttgarter-flex-akkumuliert')