library(privatefinanzen)
source('plot-setup.R')

# Für die Dauer der Grundphase werden für jede Zulage einmalige Abschluss- und
# Vertriebskosten in Höhe von 2.50 % der Zulage einbehalten.
pkosten.vertrieb <- 0.0250

# Auf Sonderzahlungen wird nochmal eine weitere Gebühr erhoben.
pkosten.sonderzahlung <- 0.0250

# Auf alle Eigenbeiträge kommt nochmal eine Gebühr drauf.
pkosten.eigen <- 0.0650

# Jährlich werden nochmal Kosten in Höhe der Beitragssumme erhoben. Die Frage
# ist nur, wie diese Beitragssumme errechnet wird, wenn sich die monatlichen
# Beiträge ändern.
pkosten.beitragssumme <- 0.0016

# Verwaltungskosten des Gesamtguthabens, das sollte recht einfach zu
# implementieren sein.
pkosten.verwaltung <- 0.0030

# Zur Absicherung der Mindestleistung werden nochmal Kosten auf das
# Gesamtguthaben erhoben. Dies ist variabel, sodass ich hier den aktuellen Wert
# nehme. Man könnte auch den schlimmsten Wert nehmen, 2.5 %.
pkosten.absicherung <- 0.0008

# Ich habe den Vertrag Ende 2016 abgeschlossen. Hier nehme ich einfach an, dass
# ich das schon zu Anfang gemacht hätte, dann kann ich in ganzen Jahren
# rechnen.
jahr <- seq(2016, 2057, 1)

bruttogehalt <- c(rep(1500, 1),    # Ein Jahr nur 5 EUR/Monat
                  rep(22000, 3),   # Promotion
                  rep(50000, 36),  # Die restliche Zeit 50 k/Jahr angenommen
                  rep(0, 2))

einzahlung <- 0.04 * bruttogehalt

stopifnot(length(jahr) >= length(einzahlung))

erzeuge.streuung <- function(name, len) {
    # Mögliche Renditen, die man erhalten kann.
    renditen <- c(0, 3, 6, 9)

    r <- list()

    for (rendite in renditen) {
        r[[sprintf('%s.%i', name, rendite)]] <- rep(0, len)
    }

    return (r)
}

guthaben <- rep(0, length(jahr))
kosten <- rep(0, length(jahr))
zulage <- rep(0, length(jahr))
eigenbeitrag <- rep(0, length(jahr))
beitragssumme <- rep(0, length(jahr))

for (i in 1:length(einzahlung)) {
    zulage[i] <- riester.zulage(einzahlung[i], bruttogehalt[i], kinder = 0)
    eigenbeitrag[i] <- einzahlung[i] - zulage[i]

    # Die bisherigen Beiträge ist die Summe aller Einzahlungen der Jahre, die
    # kleiner als das aktuelle sind.
    bisherige.beiträge <- sum(einzahlung[1:length(einzahlung) < i])
    # Die künftigen Beiträge ist der aktuelle Beitrag die restlichen Jahre.
    künftige.beiträge <- einzahlung[i] * max(length(einzahlung) - i - 1, 0)
    beitragssumme[i] <- bisherige.beiträge + künftige.beiträge

    kosten[i] <- pkosten.vertrieb * einzahlung[i] +
        pkosten.eigen * eigenbeitrag[i] +
        pkosten.beitragssumme

    altes.guthaben <- ifelse(i == 1, 0, guthaben[i - 1])
    guthaben[i] <- altes.guthaben + eigenbeitrag[i] + zulage[i] - kosten[i]
}

df <- data.frame(jahr = jahr,
                 bruttogehalt = bruttogehalt,
                 einzahlung = einzahlung,
                 guthaben = guthaben,
                 kosten = kosten,
                 zulage = zulage,
                 eigenbeitrag = eigenbeitrag,
                 beitragssumme = beitragssumme)

write.table(df, file='wwk-riester.tsv')

df2 <- gather(df, value = 'value', key = 'type',
              einzahlung, kosten, zulage, eigenbeitrag)
ggplot(df2, aes(x = jahr, y = value, color = type)) +
    geom_line()

df2 <- gather(df, value = 'value', key = 'type',
              guthaben, beitragssumme)
ggplot(df2, aes(x = jahr, y = value, color = type)) +
    geom_line()
