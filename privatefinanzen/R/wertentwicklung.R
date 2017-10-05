entwickle.einzahlungen <- function(einzahlungen, rendite) {
    jahre <- 1:length(einzahlungen)
    faktor <- 1 + rendite
    entwicklung <- faktor^rev(jahre)
    werte <- einzahlungen * entwicklung
    summe <- sum(werte)

    list(faktor = faktor,
         jahre = jahre,
         entwicklung = entwicklung,
         werte = werte,
         summe = summe)
}

renditen <- c(0, 3, 6)
rendite.wort <- function(name, rendite) sprintf('%s.%i', name, rendite)

erzeuge.streuung <- function(name, len) {
    r <- list()

    for (rendite in renditen) {
        r[[rendite.wort(name, rendite)]] <- rep(0, len)
    }

    return (r)
}
