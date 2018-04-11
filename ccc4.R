
# loesche alle alten objekte im file

rm(list=ls())

# library ggplot2
library(ggplot2)


# FUNCTION: simuliere_jahresergebnisse t+1 und t+2 (1 durchgang)
  # polizzenanzahl: 10000 (konstant)
  # jahrespraemie pro polizze: 300
  # bonus/malus-prozent: default 53% (i.e. falls schadenfall: verliert bonusstufe; 
  # falls kein schadenfall: gewinnt bonusstufe fuer folgejahr)

  # verteilungsfunktion von anzahl schadenfaelle pro polizze und jahr: poissonverteilt mit lambda=0.15
  # kosten pro schadenfall: exponentialverteilt mit erwartungswert 1000 euro; 
  # E(x)=1/lambda >> lambda=1/E(x)=1/1000

  # jahresergebnis = sum(jahrespraemien aller polizzen) - sum(kosten fuer schadenfaelle)

simuliere_jahresergebnisse <- function(polizzenanzahl=10000, jahrespraemie=300, bonus_malus_prozent=0.53){
  
  # SIMULIERE t (geschrieben als "t0" bei variablen)
    # schadenfaelle und kosten dafuer simulieren
    lambda_schadenverteilung <- 0.15
    schadenfaelle_proPolizze_t0 <- rpois(10000, lambda=lambda_schadenverteilung)
    anzahl_schadenfaelle_t0 <- sum(schadenfaelle_proPolizze_t0)
    
    erwartungswert_kostenProSchadenfall <- 1000 # in euro
    kosten_simul_t0 <- rexp(anzahl_schadenfaelle_t0, rate = 1/erwartungswert_kostenProSchadenfall)
    
    # einnahmen t berechnen (annahme: im ersten berechnungsjahr zahlen noch alle gleich viel)
    einnahmen_t0 <- polizzenanzahl * jahrespraemie
    
    # jahresergebnis t berechnen
    jahresergebnis_t0 <- einnahmen_t0 - sum(kosten_simul_t0) 
  
 # SIMULIERE t+1 (geschrieben als "t1" bei variablen)
    # schadenfaelle und kosten dafuer simulieren
    schadenfaelle_proPolizze_t1 <- rpois(10000, lambda=lambda_schadenverteilung)
    anzahl_schadenfaelle_t1 <- sum(schadenfaelle_proPolizze_t1)
    
    erwartungswert_kostenProSchadenfall <- 1000 # in euro
    kosten_simul_t1 <- rexp(anzahl_schadenfaelle_t1, rate = 1/erwartungswert_kostenProSchadenfall)
    
    # einnahmen t+1 berechnen (annahme: falls im ersten jahr kein schaden war, dann praemienbonus von x%)
    einnahmen_t1 <- sum(schadenfaelle_proPolizze_t0) * jahrespraemie +  # bei schaden >> zahle volle praemie 
                    (polizzenanzahl - sum(schadenfaelle_proPolizze_t0)) * jahrespraemie*(1-bonus_malus_prozent) # kein schaden >> bonus
    
    # jahresergebnis t+1 berechnen
    jahresergebnis_t1 <- einnahmen_t1 - sum(kosten_simul_t1) 
    
 # SIMULIERE t+2 (geschrieben als "t2" bei variablen)
    # schadenfaelle und kosten dafuer simulieren
    schadenfaelle_proPolizze_t2 <- rpois(10000, lambda=lambda_schadenverteilung)
    anzahl_schadenfaelle_t2 <- sum(schadenfaelle_proPolizze_t2)
    
    erwartungswert_kostenProSchadenfall <- 1000 # in euro
    kosten_simul_t2 <- rexp(anzahl_schadenfaelle_t2, rate = 1/erwartungswert_kostenProSchadenfall)
    
    # einnahmen t+1 berechnen (annahme: falls im ersten jahr kein schaden war, dann praemienbonus von x%)
    einnahmen_t2 <- sum(schadenfaelle_proPolizze_t1) * jahrespraemie +  # bei schaden >> zahle volle praemie 
      (polizzenanzahl - sum(schadenfaelle_proPolizze_t1)) * jahrespraemie*(1-bonus_malus_prozent) # kein schaden >> bonus
    
    # jahresergebnis t+1 berechnen
    jahresergebnis_t2 <- einnahmen_t2 - sum(kosten_simul_t2) 


 # liste mit t+1, t+2 werten und den parametern zurueckgeben (für den betrachteten simulationslauf)
    list(jahresergebnis_t1=jahresergebnis_t1, jahresergebnis_t2=jahresergebnis_t2 )
} 




# durchfuehrung von n simulationen fuer die jahresergebnisse t+1 und t+2 fuer die versicherung
# (annahme: unter gleichbleibenden bedingungen)

  # datenbasis setzen
  n <- 1500  
  polizzenanzahl <- 10000
  jahrespraemie <- 300
  bonus_malus_prozent <- 0.53
  jahresergebnis_df <- data.frame(t1=rep(0,n), t2=rep(0,n))
  liste_jahresergebnisse <- vector(mode="list")
  ergebnisvektor_t1 <- rep(0,n)
  ergebnisvektor_t2 <- rep(0,n)
  erwartungswert_kostenProSchadenfall <- 1000
  
 
  # n simulationen (unter gleichen bedingungen)
  for(i in 1:n){
    
    liste_jahresergebnisse[[i]] <- simuliere_jahresergebnisse(polizzenanzahl, jahrespraemie, bonus_malus_prozent)
    ergebnisvektor_t1[i] <- liste_jahresergebnisse[[i]]$jahresergebnis_t1
    ergebnisvektor_t2[i] <- liste_jahresergebnisse[[i]]$jahresergebnis_t2
  }

  
  # ergebnisvektoren fuer simulation der jahresergebnisse t+1 und t+2 in n durchlaeufen anzeigen
  ergebnisvektor_t1
  ergebnisvektor_t2
  
  

# wahrscheinlichkeit (p), dass bei gegebener prämien% x das jahr t+1 bzw. t+2 mit negativem ergebnis endet
# (basierend auf den n simulierten durchgaengen)
  p_negativesErgebnis_t1 <- sum(ergebnisvektor_t1 < 0) / length(ergebnisvektor_t1)
  p_negativesErgebnis_t2 <- sum(ergebnisvektor_t2 < 0) / length(ergebnisvektor_t2)   
  

# bei gegebenenem x prämien% ein 90% konfidenzintervall für jahresergebnis in t+1 und t+2
  
  alpha <- 0.10 
  
  # 90% KI fuer jahresergebnis t+1
    sd_t1 <- sd(ergebnisvektor_t1)
    mean_t1 <- mean(ergebnisvektor_t1)
    konfidenzintervall_90_t1 <- mean_t1 + c(-1,1) * qnorm(1-alpha/2) * sd_t1 / sqrt(n)   
 
  # 90% KI fuer jahresergebnis t+2
    sd_t2 <- sd(ergebnisvektor_t2)
    mean_t2 <- mean(ergebnisvektor_t2)
    konfidenzintervall_90_t2 <- mean_t2 + c(-1,1) * qnorm(1-alpha/2) * sd_t2 / sqrt(n)   
    

    
    
# visualisierung der simulationsergebnisse 
    
  # t+1
  max(ergebnisvektor_t1)
  min(ergebnisvektor_t1)
  sum(ergebnisvektor_t1<0)

    # histogramm der verteilung der jahresergebnisse t+1 (in basic R plot)
    hist(ergebnisvektor_t1, cex.axis=1, 
         ylim=c(0,n-900), xlim=c(min(ergebnisvektor_t1),max(ergebnisvektor_t1)), 
         col="bisque1",
         main="Histogramm Jahresergebnis t+1", 
         xlab="Jahresergebnis t+1", ylab="Häufigkeit"
    )
  
    # histogramm in ggplot2:  
    qplot(as.data.frame(ergebnisvektor_t1),   
          geom="histogram",
          bins=20,  
          main = "Histogramm Jahresergebnis t+1",
          xlab = "Jahresergebnis t+1",  
          ylab = "Häufigkeit",
          fill=I("lightsalmon2"), 
          colour=I("black"), 
          alpha=I(.4)
    ) + scale_y_continuous(breaks = seq(0, 600, by = 25)) + scale_x_continuous(breaks = seq(-100000, 300000, by = 50000)) +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"), 
            plot.title = element_text(size=14, face="bold"))  +
      geom_vline(xintercept = mean_t1, , colour="turquoise4", alpha=0.5, 
                 size = 2, lty="solid") +
      annotate("text", mean_t1-17000 , 6, vjust=2, 
               label = "mean", colour="turquoise4") + 

    
     
    
    # boxplot (in basic R plot)
    boxplot(ergebnisvektor_t1, ylab="Jahresergebnis t+1", main="Boxplot Jahresergebnis t+1")
    
    
    # boxplot in ggplot2:
    ggplot(as.data.frame(ergebnisvektor_t1), aes(x="", y=as.data.frame(ergebnisvektor_t1))) + 
      geom_boxplot(
        color="springgreen3",
        fill="springgreen3",
        alpha=0.4,
        outlier.colour="firebrick2",
        outlier.fill="firebrick2",
        outlier.size=3.5
      ) +
      scale_y_continuous(breaks = seq(-100000, 400000, by = 25000)) +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"), 
            plot.title = element_text(size=14, face="bold"))  +
      stat_boxplot(geom = "errorbar", width = 0.4) + 
      labs(title="Boxplot Jahresergebnis t+1", 
           x="", y="Jahresergebnis t+1")
   
  
    
    # t+2
    max(ergebnisvektor_t2)
    min(ergebnisvektor_t2)
    sum(ergebnisvektor_t2<0)
    
    # histogramm der verteilung der jahresergebnisse t+1 (in basic R plot)
    hist(ergebnisvektor_t2, cex.axis=1, 
         ylim=c(0,n-900), xlim=c(min(ergebnisvektor_t2),max(ergebnisvektor_t2)), 
         col="bisque1",
         main="Histogramm Jahresergebnis t+2", 
         xlab="Jahresergebnis t+2", ylab="Häufigkeit"
    )
    
    # histogramm in ggplot2:  
    qplot(as.data.frame(ergebnisvektor_t2),   
          geom="histogram",
          bins=20,  
          main = "Histogramm Jahresergebnis t+2",
          xlab = "Jahresergebnis t+2",  
          ylab = "Häufigkeit",
          fill=I("lightsalmon2"), 
          colour=I("black"), 
          alpha=I(.4)
    ) + scale_y_continuous(breaks = seq(0, 600, by = 25)) + scale_x_continuous(breaks = seq(-100000, 
        350000, by = 50000)) +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"), 
            plot.title = element_text(size=14, face="bold"))  
    
    
    
    # boxplot (in basic R plot)
    boxplot(ergebnisvektor_t2, ylab="Jahresergebnis t+2", main="Boxplot Jahresergebnis t+2")
    

    # boxplot in ggplot2:
    ggplot(as.data.frame(ergebnisvektor_t2), aes(x="", y=as.data.frame(ergebnisvektor_t2))) + 
      geom_boxplot(
        color="springgreen3",
        fill="springgreen3",
        alpha=0.4,
        outlier.colour="firebrick2",
        outlier.fill="firebrick2",
        outlier.size=3.5
      ) +
      scale_y_continuous(breaks = seq(-100000, 350000, by = 25000)) +
      theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"), 
            plot.title = element_text(size=14, face="bold"))  +
      stat_boxplot(geom = "errorbar", width = 0.4) + 
      labs(title="Boxplot Jahresergebnis t+2", 
           x="", y="Jahresergebnis t+2")  
    

           
     # konfidenzintervall fuer den mittelwert des jahresergebnis
           plot(ergebnisvektor_t1, ylab="Jahresergebnis t+1", 
                xlab="Anzahl der Jahresergebnis t+1 Simulationen")
           abline(h=mean_t1, lwd=1, col="green")
           abline(h=konfidenzintervall_90_t1[1], lwd=1, lty="dashed", col="red")
           abline(h=konfidenzintervall_90_t1[2], lwd=1, lty="dashed", col="red")
           legend(x=5, y=30000, legend=c("mean", "konfidenzintervallgrenzen"), 
                  pch=c(18,19), col=c("green","red"), cex=0.8)
           
           plot(ergebnisvektor_t2, ylab="Jahresergebnis t+2", 
                xlab="Anzahl der Jahresergebnis t+2 Simulationen")
           abline(h=mean_t2, lwd=1, col="green")
           abline(h=konfidenzintervall_90_t2[1], lwd=1, lty="dashed", col="red")
           abline(h=konfidenzintervall_90_t2[2], lwd=1, lty="dashed", col="red")
           legend(x=5, y=1000, legend=c("mean", "konfidenzintervallgrenzen"), 
                  pch=c(18,19), col=c("green","red"), cex=0.8)
           
           

           