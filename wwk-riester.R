library(privatefinanzen)
source('plot-setup.R')

# Für die Dauer der Grundphase werden für jede Zulage einmalige Abschluss- und
# Vertriebskosten in Höhe von 2.50 % der Zulage einbehalten.
pkosten.vertrieb <- 0.0250

# Auf Sonderzahlungen wird nochmal eine weitere Gebühr erhoben.
pkosten.sonderzahlung <- 0.0250

# Auf alle Eigenbeiträge kommt nochmal eine Gebühr drauf.
pkosten.eigen <- 0.0650

pkosten.verwaltung.eigen <- 0.0250

# Jährlich werden nochmal Kosten in Höhe der Beitragssumme erhoben. Die Frage
# ist nur, wie diese Beitragssumme errechnet wird, wenn sich die monatlichen
# Beiträge ändern.
pkosten.beitragssumme <- 0.0016

# Verwaltungskosten des Gesamtguthabens, das sollte recht einfach zu
# implementieren sein.
pkosten.verwaltung <- 0.0030

# Zur Absicherung der Mindestleistung werden nochmal Kosten auf das
# Gesamtguthaben erhoben. Dies ist variabel, sodass ich hier den aktuellen Wert
# von 0.8 % nehme. Man könnte auch den schlimmsten Wert nehmen, 2.50 %.
pkosten.absicherung <- 0.0080
#pkosten.absicherung <- 0.0250

# Ich habe den Vertrag Ende 2016 abgeschlossen. Hier nehme ich einfach an, dass
# ich das schon zu Anfang gemacht hätte, dann kann ich in ganzen Jahren
# rechnen.
jahr <- seq(2016, 2067, 1)

bruttogehalt <- c(rep(8000, 1),    # Ein Jahr nur 5 EUR/Monat
                  rep(22000, 3),   # Promotion
                  rep(50000, 37),  # Die restliche Zeit 50 k/Jahr angenommen
                  rep(0, 11))

einzahlung <- 0.04 * bruttogehalt

# TEST: Den Beitrag einfach konstant bei 5 EUR/Monat halten und schauen, ob die
# gleichen Zahlen wie im Versicherungsschein herauskommen.
einzahlung <- c(rep(60, 41), rep(0, length(jahr) - 41))

stopifnot(length(jahr) == length(einzahlung))

# Mögliche Renditen, die man erhalten kann.
renditen <- c(0, 3, 6)
rendite.wort <- function(name, rendite) sprintf('%s.%i', name, rendite)

erzeuge.streuung <- function(name, len) {
    r <- list()

    for (rendite in renditen) {
        r[[rendite.wort(name, rendite)]] <- rep(0, len)
    }

    return (r)
}

beitragssumme <- rep(0, length(jahr))
eigenbeitrag <- rep(0, length(jahr))
guthaben <- erzeuge.streuung('guthaben', length(jahr))
kosten <- erzeuge.streuung('kosten', length(jahr))
zulage <- rep(0, length(jahr))

for (i in 1:length(einzahlung)) {
    r.zulage <- riester.zulage(einzahlung[i], bruttogehalt[i], kinder = 0)
    r.erlass <- riester.steuererlass(einzahlung[i], bruttogehalt[i], kinder = 0)
    zulage[i] <- r.zulage + max(r.erlass, 0)
    eigenbeitrag[i] <- einzahlung[i] - zulage[i]

    for (rendite in renditen) {
        # Die bisherigen Beiträge ist die Summe aller Einzahlungen der Jahre,
        # die kleiner als das aktuelle sind.
        bisherige.beiträge <- sum(einzahlung[1:length(einzahlung) < i])
        # Die künftigen Beiträge ist der aktuelle Beitrag die restlichen Jahre.
        künftige.beiträge <- einzahlung[i] * max(sum(einzahlung > 0) - i + 1, 0)
        beitragssumme[i] <- bisherige.beiträge + künftige.beiträge

        altes.guthaben <- ifelse(i == 1, 0, guthaben[[rendite.wort('guthaben', rendite)]][i - 1])

        kosten[[rendite.wort('kosten', rendite)]][i] <-
            pkosten.vertrieb * einzahlung[i] +
            pkosten.eigen * eigenbeitrag[i] +
            pkosten.verwaltung.eigen * eigenbeitrag[i] +
            pkosten.beitragssumme * beitragssumme[i] +
            pkosten.verwaltung * altes.guthaben +
            pkosten.absicherung * altes.guthaben

        guthaben[[rendite.wort('guthaben', rendite)]][i] <-
            altes.guthaben * (1 + (rendite) / 100) +
            eigenbeitrag[i] + zulage[i] -
            kosten[[rendite.wort('kosten', rendite)]][i]
    }
}

eigenbeitrag.accum <- cumsum(eigenbeitrag)

cols <- c(list(jahr = jahr,
               bruttogehalt = bruttogehalt,
               einzahlung = einzahlung,
               zulage = zulage,
               eigenbeitrag = eigenbeitrag,
               eigenbeitrag.accum = eigenbeitrag.accum,
               beitragssumme = beitragssumme),
          kosten,
          guthaben)

df <- do.call('data.frame', cols)

print(df)
cat('\n')
print(df[41, ])

write.table(df, file='wwk-riester.tsv')

df2 <- gather(df, value = 'value', key = 'type',
              einzahlung, kosten.0, kosten.3, kosten.6, zulage, eigenbeitrag)
ggplot(df2, aes(x = jahr, y = value, color = type)) +
    geom_line() +
    labs(title = 'WWK Premium FörderRente protect',
         subtitle = 'Laufende Größen',
         type = 'Größe',
         x = 'Jahr',
         y = 'EUR')
my.ggsave('wwk-riester-laufend')

# https://gis.stackexchange.com/a/121225
zoomtheme <- theme(legend.position="none", axis.line = element_blank(),
                   axis.text.x = element_blank(),
                   axis.title.x = element_blank(), axis.title.y = element_blank(),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(), 
                   panel.background = element_rect(color = 'red', fill = "white"),
                   plot.margin = unit(c(0, 0, -6, -6), "mm"))

df2 <- gather(df, value = 'value', key = 'type',
              guthaben.0, guthaben.3, guthaben.6, beitragssumme, eigenbeitrag.accum)
p1 <- ggplot(df2, aes(x = jahr, y = value / 1000, color = type)) +
    geom_line()

y <- eigenbeitrag.accum[length(eigenbeitrag.accum)] / 1000
p2 <- p1 + coord_cartesian(xlim = c(2050, 2060), ylim = c(y - 5, y + 5)) + zoomtheme

vergrößerung <-  annotation_custom(grob = ggplotGrob(p2),
                                     xmin = 2020, xmax = 2030,
                                     ymin = 300, ymax = 450)

p1 + labs(title = 'WWK Premium FörderRente protect',
         subtitle = 'Akkumulative Größen',
         type = 'Größe',
         x = 'Jahr',
         y = '1000 EUR') +
    geom_hline(yintercept = 2.81956)
my.ggsave('wwk-riester-akkumulativ')
