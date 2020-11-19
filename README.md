# Kostra R methods

Denne r-pakken er i bruk i Kostra/KOMPIS og inneholder "metodebiblioteket" som ble utviklet i prosjektet. 

En del kode er publisert som eksterne CRAN-pakker (easySdcTable, SmallCountRounding, SSBtools) som Kostra-pakken avhenger av.

Se mer informasjon og forklaring av metodene på [KOSTRA-produksjon: Metode – Brukerdokumentasjon](https://wiki.ssb.no/display/KOSPRO/Metode+-+Brukerdokumentasjon).

Det er metodeseksjonen som forvalter denne r-koden slik det fremgår av [Sjekkliste for overlevering](https://wiki.ssb.no/pages/viewpage.action?pageId=66848286).

Funksjonene som ble programmert som en del av metodebiblioteket følger en bestemt standard slik det er beskrevet i  [R-funksjoner for metodebiblioteket](https://wiki.ssb.no/display/s880/R-funksjoner+for+metodebiblioteket+20.april+2017) og  [Methods library of embedded R functions at Statistics Norway](https://wiki.ssb.no/display/s880/Methods+library+of+embedded+R+functions+at+Statistics+Norway).

I forbindelse med regresjonsmetoder finnes et par dokumenter som forklarer noen detaljer: [Residualer i lineære modeller](https://wiki.ssb.no/display/s880/Residualer) og [Details about regression in the R package Kostra](https://wiki.ssb.no/display/s880/Details+about+regression+in+the+R+package+Kostra).

Konfidensialitet ved avrunding ble tatt inn i Kostra-prosjektet på et senere stadium enn de andre metodene. Litt bakgrunn for denne implementeringen finnes på [Hvordan implementere avrunding à la Heldal i KOMPIS/KOSTRA?](https://wiki.ssb.no/pages/viewpage.action?pageId=148571267)


Regnskapsberegninger (KostraRegnskap og BalanseRegnskap) ble tatt inn i denne r-pakken for å løse en krise i Kostra-prosjektet der førsøk på annen implementering hadde vist seg ikke å fungere. Teori om hvordan disse problemstillingene ble løst finnes på [Hierarkiske kombinasjoner ved matriseoperasjoner](https://wiki.ssb.no/display/s880/Hierarkiske+kombinasjoner+ved+matriseoperasjoner). De såkalte beregningstestene (...BeregningInput og ...BeregningHierarki) ble implementert etter at dette dokumentet ble skrevet. 
Litt forklaring av parameteren stjernetabell finns på [stjernetabell](https://wiki.ssb.no/display/s880/stjernetabell). Regnskapsfunksjonene som er integrert i systemet følger ikke metodebibliotekstandarden. Mange inputvariabler er av typen data.frame og det forventes faste variabelnavn. Underliggende og mer generiske funksjoner ligger i r-pakka SSBtools.



