library(privatefinanzen)
source('plot-setup.R')

produkt <- 'ebase Depot'

jahr <- seq(2016, 2067, 1)

einzahlung <- c(rep(25 * 12, 41), rep(0, 11))
kosten <- 36

guthaben <- erzeuge.streuung('guthaben', length(jahr))

for (i in 1:length(einzahlung)) {
    for (rendite in renditen) {
        altes.guthaben <- ifelse(i == 1, 0, guthaben[[rendite.wort('guthaben', rendite)]][i - 1])

        guthaben[[rendite.wort('guthaben', rendite)]][i] <-
            altes.guthaben * (1 + rendite / 100) +
            einzahlung[i] -
            ifelse(altes.guthaben < 20000, kosten, 0.0)
    }
}

eigenbeitrag.accum <- cumsum(einzahlung)

cols <- c(list(jahr = jahr,
               einzahlung = einzahlung,
               eigenbeitrag.accum = eigenbeitrag.accum),
          guthaben)

df <- do.call('data.frame', cols)

data.2057 <- df[df$jahr == 2057, ]
print(data.2057)

steuersatz <- (0.25 * 1.05)
auszahlung.3 <- data.2057$guthaben.3 - (data.2057$guthaben.3 - data.2057$eigenbeitrag.accum) * steuersatz
auszahlung.6 <- data.2057$guthaben.6 - (data.2057$guthaben.6 - data.2057$eigenbeitrag.accum) * steuersatz
cat('Auszahlung im Jahr 2057 bei 3% Rendite: ', auszahlung.3, '\n')
cat('Auszahlung im Jahr 2057 bei 6% Rendite: ', auszahlung.6, '\n')

write.table(df, file='output/ebase-depot.tsv')

df2 <- gather(df, value = 'value', key = 'type',
              einzahlung)
ggplot(df2, aes(x = jahr, y = value, color = type)) +
    geom_line() +
    labs(title = produkt,
         subtitle = 'Laufende Größen',
         type = 'Größe',
         x = 'Jahr',
         y = 'EUR')
my.ggsave('ebase-depot-laufend')

df2 <- gather(df, value = 'value', key = 'type',
              guthaben.0, guthaben.3, guthaben.6, eigenbeitrag.accum)
ggplot(df2, aes(x = jahr, y = value / 1000, color = type)) +
    geom_line() +
    labs(title = produkt,
         subtitle = 'Akkumulative Größen',
         type = 'Größe',
         x = 'Jahr',
         y = '1000 EUR')
my.ggsave('ebase-depot-akkumuliert')
