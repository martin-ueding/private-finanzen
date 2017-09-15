grenzsteuersatz <- Vectorize(function(einkommen) {
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
