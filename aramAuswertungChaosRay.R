#
rm(list = ls())

# Am Ende noch anpassen
pakete <- c("EMT")

libList <- installed.packages()
for (paket in pakete) {
	if (!(paket %in% libList)) {install.packages(paket, dependencies = TRUE)}
	library(paket, character.only = TRUE)
}


###
# Daten laden
haeufigkeit <- list()
haeufigkeit$champBesitz <- c(26, 24, 24, 23, 22, 22, 21, 21, 20, 20, 20, 20, 19, 19, 19, 19, 18, 18, 18, 18, 18, 18, 17, 17, 16, 16, 16, 15, 15, 14, 14, 11, 10)
haeufigkeit$champBesitzNeu2018 <- c(20, 20, 19, 16, 12, 12, 11, 9)
haeufigkeit$champBesitzNeu2019 <- c(9,8,7,6,4)
haeufigkeit$champNichtBesitzt <- c(12, 11, 10, 10, 9, 9, 8, 8, 8, 8, 8, 8, 8, 8, 8, 7, 7, 7, 7, 7, 7, 7, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1)

anzahl <- lapply(haeufigkeit, length)

###
# Binomialtest für Besitz/Nicht-Besitz
anzahlChampBesitzGanzeZeit <- anzahl$champBesitz + anzahl$champBesitzNeu2019
anzahlChampBesitz <- anzahl$champBesitz

# Freie Rotation
champsFreieRotation <- 42 * (1 - anzahlChampBesitz / 140)
#champsFreieRotation <- 14 * 3

# Für eine Korrektur der wiederholten Champions in den letzten 3 Wochen: https://leagueoflegends.fandom.com/wiki/Champion_rotation_archive

# Anzahl ARAMs Champ im Besitz/Freie Rotation
anzahlARAMBesitz <- sum(haeufigkeit$champBesitz) + sum(haeufigkeit$champBesitzNeu2018)
anzahlARAMFreieRotation <- sum(haeufigkeit$champNichtBesitzt) + sum(haeufigkeit$champBesitzNeu2019)

##
# Binom Test
binomTestFreieRotationNurBesitz <- binom.test(sum(haeufigkeit$champBesitz), sum(haeufigkeit$champBesitz) + sum(haeufigkeit$champNichtBesitzt), p = anzahlChampBesitzGanzeZeit / (anzahlChampBesitzGanzeZeit + champsFreieRotation))

if (FALSE) {
	binomTestFreieRotationNurBesitz 

data:  sum(haeufigkeit$champBesitz) and sum(haeufigkeit$champBesitz) + sum(haeufigkeit$champNichtBesitzt)
number of successes = 608, number of trials = 1076, p-value = 0.1339
alternative hypothesis: true probability of success is not equal to 0.5420827
95 percent confidence interval:
 0.5348256 0.5949292
sample estimates:
probability of success 
             0.5650558 

}


###
# Multinomialtests
# Dauern alle viel zu lange/nicht genügend Speicher vorhanden (nur 192 GB RAM)
if (FALSE) {
	dauerMulti <- Sys.time()
	multinomialTestChampsBesitz <- multinomial.test(haeufigkeit$champBesitz, rep(1/anzahl$champBesitz, anzahl$champBesitz), useChisq = TRUE)
	print(Sys.time() - dauerMulti)

	dauerMulti <- Sys.time()
	multinomialTestChampsBesitzMonteCarlo <- multinomial.test(haeufigkeit$champBesitz, rep(1/anzahl$champBesitz, anzahl$champBesitz), MonteCarlo = TRUE, ntrial = 10^8, atOnce = 10^7)
	print(Sys.time() - dauerMulti)


	dauerMulti <- Sys.time()
	multinomialTestChampsNichtImBesitz <- multinomial.test(haeufigkeit$champNichtBesitzt, rep(1/anzahl$champNichtBesitzt, anzahl$champNichtBesitzt), useChisq = TRUE)
	print(Sys.time() - dauerMulti)
}


# Berechne den Likelihood Test Ratio nach https://en.wikipedia.org/wiki/Multinomial_test
multinomLikelihoodRatio <- function(obs) {
    # obs <- haeufigkeit$champBesitz[1:5]

    # Pi und nObs
    pi <- 1 / length(obs)
    nObs <- length(obs)
    nSamp <- sum(obs)

    piVec <- rep(pi, nObs)

    # Maximum likelihood Werte für p
    pML <- obs / nSamp

    # Teststatistik
    lnLR <- -2 * sum(obs * log(piVec / pML))

    # Korrektorterm
    q2 <- 1 + (nObs + 1) / (6 * nSamp) + nObs ^ 2 / (6 * nSamp ^ 2)

    lnLRKorr <- lnLR / q2

    # Chi-Squared Test
    pWert <- pchisq(lnLRKorr, nObs - 1, lower.tail = FALSE)

    return(pWert)
}


pChampionBesitzt <- multinomLikelihoodRatio(haeufigkeit$champBesitz) # 0.8976319
pChampionNichtBesitzt <- multinomLikelihoodRatio(haeufigkeit$champNichtBesitzt) # 0.1456617

















