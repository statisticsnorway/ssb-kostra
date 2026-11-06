<!-- badges: start -->
[![R-CMD-check](https://github.com/statisticsnorway/ssb-kostra/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/statisticsnorway/ssb-kostra/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->
  
# Kostra R methods

Denne R-pakken ble opprinnelig laget for bruk i KOSTRA/KOMPIS[^1] og inneholder «metodebiblioteket» som ble utviklet i prosjektet.
Begrepet «metodebiblioteket» brukes i dag i en annen betydning, se 
[SSBs metodebibliotek](https://statisticsnorway.github.io/ssb-metodebiblioteket/).

Funksjonene som ble programmert som en del av KOSTRA-metodebiblioteket følger en bestemt standard slik det er beskrevet i 
[Methods library of embedded R functions at Statistics Norway](https://www.revistadestatistica.ro/wp-content/uploads/2017/11/RRS-4_2017_tipar_A10.pdf).

I forbindelse med regresjonsmetoder finnes et par dokumenter som forklarer noen detaljer: 
[Residualer i lineære modeller](extra/Residualer.pdf) og 
[Details about regression in the R package Kostra](extra/LmImpute.pdf).

Pakken inneholder funksjonene `ProtectKostra()` og `RoundKostra()` for konfidensialitetsbeskyttelse. 
Disse funksjonene er imidlertid gamle, har begrenset funksjonalitet og et spesielt grensesnitt 
som er preget av både den tidligere standarden for KOSTRA/KOMPIS og standarden og mulighetene i R-pakken 
[sdcTable](https://cran.r-project.org/package=sdcTable).
I stedet anbefales funksjoner i R-pakkene 
[GaussSuppression](https://cran.r-project.org/package=GaussSuppression) og 
[SmallCountRounding](https://cran.r-project.org/package=SmallCountRounding).

Funksjonene `KostraRegnskap()` og `BalanseRegnskap()` for regnskapsberegninger, 
som fortsatt finnes i denne pakken av hensyn til KOMPIS, anses også som utdaterte. 
Funksjonene er flyttet til en egen pakke, 
[ssb-kostraregnskap](https://github.com/statisticsnorway/ssb-kostraregnskap/), 
der funksjonaliteten er videreutviklet.


[^1]: Brukerne av IT-systemet KOMPIS kunne produsere statistikk ved hjelp av 
      spesialspråket VTL og ved å velge metoder fra et metodebibliotek i en meny. 
      Disse brukerne forholdt seg ikke til programmeringsspråket bak metodene. 
      I KOSTRA-metodebiblioteket var alle metodene programmert i R.