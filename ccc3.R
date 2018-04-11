
# loesche alle alten objekte im file

rm(list=ls())

# verwende library ggplot2 fuer visualisierung

library(ggplot2)

# CHECK: rechner-performance
x <- NULL
system.time(for(i in 1:50000) x <- c(x,i))


# function zur simulation 
simulation_sticker <- function(stickeranzahlGesamt=682, paketpreis=0.90, 
                               bilderProPaket=5, bildMehrmalsProPaketMoeglich="nein"){
  
  # TEST data
  # stickeranzahlGesamt <- 682
  # paketpreis <- 0.90
  # bilderProPaket <- 5
  # bildMehrmalsProPaketMoeglich <- "nein"
  
  # basis: album als leeren vector mit n elementen erstellen (mit 0 codiert)
  album <- rep(0,stickeranzahlGesamt)
  
  # neue bilder kaufen und einpflegen, solange das album nicht voll ist
  anzahl_paketkaeufe <- 0
    while(0 %in% album){ # START WHILE
      
      # neues paket kaufen (zufallszahlen mit gleichverteilung generieren)
      if(bildMehrmalsProPaketMoeglich == "nein"){
        # ziehe 5 bilder ohne zuruecklegen
        paket_kaufen <- sample(1:stickeranzahlGesamt, bilderProPaket, replace=FALSE)
      
      }  else {
        # ziehe 5 bilder mit zuruecklegen
        paket_kaufen <- sample(1:stickeranzahlGesamt, bilderProPaket, replace=TRUE)
        
      }
      
      # anzahl paketkaeufe um 1 erhoehen
      anzahl_paketkaeufe <- anzahl_paketkaeufe + 1
      
      # neue bilder in album einpflegen
      for(i in 1:length(paket_kaufen)){
        
        if( !(paket_kaufen[i] %in% album) ){
          index <- paket_kaufen[i]
          album[index] <- paket_kaufen[i]
        }
        
      }
      
    } # END WHILE


  # RETURN: anzahl der paketkaeufe, die notwendig sind, bis das album voll ist
  anzahl_paketkaeufe 

}


# TEST aufruf der function: (1 simulationsdurchlauf)
  simulation_sticker()


  
  
# SIMULATION DER 2 FAELLE:
# (mit und ohne mehrmaliges vorkommen desselben bildes in einem paket)
  # n ... anzahl der durchlaeufe (default: 1000)
  
simulation_cases <- function(n=1000) {   
    # SIMULATION VON n SITUATIONEN (jeweils bis das album voll ist)
      # FALL 1: jedes bild kann nur 1mal pro paket vorkommen
      ergebnisse_fall1 <- rep(0,n)
      
     zeitmessung_fall1 <- system.time( # zeitmessung anfang
       
      for(i in 1:n){
        
        ergebnisse_fall1[i] <- simulation_sticker() 
        
      }
      
     ) # zeitmessung ende
    
    
    
    # SIMULATION VON n SITUATIONEN (jeweils bis das album voll ist)
     # FALL 2: jedes bild kann mehrmals pro paket vorkommen
     ergebnisse_fall2 <- rep(0,n)
     
     zeitmessung_fall2 <- system.time( # zeitmessung anfang
       
       for(i in 1:n){
         
         ergebnisse_fall2[i] <- simulation_sticker(stickeranzahlGesamt=682, 
                                                   paketpreis=0.90, 
                                                   bilderProPaket=5, 
                                                   bildMehrmalsProPaketMoeglich="ja") 
         
       }
       
     ) # zeitmessung ende
     
  
  # RETURN liste mit zeitmessungen und ergebnissen
     list(zeitmessung_fall1=zeitmessung_fall1, 
          zeitmessung_fall2=zeitmessung_fall2,
          ergebnisse_fall1=ergebnisse_fall1,
          ergebnisse_fall2=ergebnisse_fall2)

}
 

# AUFRUF DER FUNKTION simulation_cases:
  ergebnisliste <- simulation_cases(1000)
  ergebnisliste
 
 
# ------------------------------------------------------------------------------------------------  
  # DARSTELLUNG DER SIMULATIONSERGEBNISSE:
    # parameter:
    # ergebnisliste ... liste, die die zeitmessungen und simulationsergebnisse 
    #                   von fall 1 und 2 enthaelt
    # paketpreis ... preis pro stickerpaket in euro
  darstellen <- function(ergebnisliste, paketpreis=0.90){
    
    ###################################################
    # FALL 1: jedes bild kommt nur 1mal pro paket vor #
    ###################################################
    
    
    alpha <- 0.05   
    median1 <- median(ergebnisliste$ergebnisse_fall1)
    mean1 <- mean(ergebnisliste$ergebnisse_fall1)
    sd1 <- sd(ergebnisliste$ergebnisse_fall1)
    konfidenzintervall1 <- mean1 + c(-1,1) * qnorm(1-alpha/2) * sd1 / 
                           sqrt(length(ergebnisliste$ergebnisse_fall1))
    quantile1 <- quantile(ergebnisliste$ergebnisse_fall1)
    
    
      # basic plot der ergebnisse von fall 1
      plot(ergebnisliste$ergebnisse_fall1, 
           main="Anzahl der notwendigen Paketkäufe bis Album voll ist (Fall 1)",
           xlab="Simulationen", ylab="Anzahl notwendiger Paketkäufe")
      abline(h=mean(ergebnisliste$ergebnisse_fall1), 
             lty="dotted", col="red", lwd=3)
      mtext(text=paste("Mittelwert:",ceiling(mean1)), col="red")
      

      
      # histogramm der notwendigen paketkäufe darstellen
      # (inkl. mittelwert und grenzen des konfidenzintervalls)
      qplot(as.data.frame(ergebnisliste$ergebnisse_fall1),
            geom="histogram",
            bins=20,  
            main = "Histogramm der notwendigen Paketkäufe bis Album voll ist (Fall 1)",
            xlab = "Anzahl der notwendigen Paketkäufe",  
            ylab = "Häufigkeit",
            fill=I("green"), 
            colour=I("black"), 
            alpha=I(.3),
            ylim = c(0, 250)
      ) + 
        geom_vline(xintercept=mean1, colour="saddlebrown", alpha=0.3, 
                   size = 1, lty="solid") +
        annotate("text", label = paste("Mittelwert", ceiling(mean1)), x = mean1+60, y = 200, 
                 color = "saddlebrown") +
        scale_y_continuous(breaks = seq(0, 250, by = 25)) + 
        scale_x_continuous(breaks = seq(500, 2000, by = 250)) +
        theme(axis.text=element_text(size=10), axis.title=element_text(size=11,face="bold"), 
              plot.title = element_text(size=13, face="bold")) + 
        geom_vline(xintercept=konfidenzintervall1[1], colour="blue", alpha=0.3, 
                   size = 1, lty="solid") +
        annotate("text", label = "Untere Grenze KI", x = mean1-175, y = -5, 
                 color = "blue") + 
        geom_vline(xintercept=konfidenzintervall1[2], colour="blue", alpha=0.3, 
                   size = 1, lty="solid") +
        annotate("text", label = "Obere Grenze KI", x = mean1+165, y = -5, 
                 color = "blue") 

      
        # quantile
        ggplot(as.data.frame(ergebnisliste$ergebnisse_fall1), 
               aes(x="", y=as.data.frame(ergebnisliste$ergebnisse_fall1))) + 
          geom_boxplot(
            color="springgreen3",
            fill="springgreen3",
            alpha=0.4,
            outlier.colour="firebrick2",
            outlier.fill="firebrick2",
            outlier.size=3.5
          ) +
          scale_y_continuous(breaks = seq(0, 2000, by = 100)) +
          theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"), 
                plot.title = element_text(size=14, face="bold"))  +
          stat_boxplot(geom = "errorbar", width = 0.4) + 
          labs(title="Boxplot der Anzahl der notwendigen Paketkäufe (Fall 1)", 
               x="", y="Anzahl der notwendigen Paketkäufe bis Album voll ist")
      
       
  # FALL 1: BETRACHTUNG DER KOSTEN BIS ALBUM VOLL IST:
        mean1_kosten <- mean1*paketpreis
        median1_kosten <- median1*paketpreis
        sd1_kosten <- sd1*paketpreis
        konfidenzintervall1_kosten <- mean1_kosten + c(-1,1) * qnorm(1-alpha/2) * sd1_kosten / 
          sqrt(length(ergebnisliste$ergebnisse_fall1)) 
        quantile_kosten1 <- quantile(ergebnisliste$ergebnisse_fall1*paketpreis)
      
       
        # histogramm der notwendigen kosten darstellen
        # (inkl. mittelwert und grenzen des konfidenzintervalls)
        qplot(as.data.frame(ergebnisliste$ergebnisse_fall1 * paketpreis),
              geom="histogram", 
              bins=20,  
              main = "Histogramm der notwendigen Kosten bis Album voll ist (Fall 1)",
              xlab = "Kosten bis Album voll ist",  
              ylab = "Häufigkeit",
              fill=I("green"), 
              colour=I("black"), 
              alpha=I(.3),
              ylim = c(0, 250)
        ) + 
          geom_vline(xintercept=mean1_kosten, colour="saddlebrown", alpha=0.3, 
                     size = 1, lty="solid") +
          annotate("text", label = paste("Mittelwert", ceiling(mean1_kosten)), 
                   x = mean1_kosten+20, y = 170, color = "saddlebrown") +
          scale_y_continuous(breaks = seq(0, 250, by = 25)) + 
          scale_x_continuous(breaks = seq(500, 2000, by = 100)) +
          theme(axis.text=element_text(size=10), axis.title=element_text(size=11,face="bold"), 
                plot.title = element_text(size=14, face="bold")) + 
          geom_vline(xintercept=konfidenzintervall1_kosten[1], colour="blue", alpha=0.3, 
                     size = 1, lty="solid") +
          annotate("text", label = "Untere Grenze KI", x = konfidenzintervall1_kosten[1]-150, y = -5, 
                   color = "blue") + 
          geom_vline(xintercept=konfidenzintervall1_kosten[2], colour="blue", alpha=0.3, 
                     size = 1, lty="solid") +
          annotate("text", label = "Obere Grenze KI", x = konfidenzintervall1_kosten[2]+150, y = -5, 
                   color = "blue") 
        
        
        # quantile der kosten
        ggplot(as.data.frame(ergebnisliste$ergebnisse_fall1 * paketpreis), 
               aes(x="", y=as.data.frame(ergebnisliste$ergebnisse_fall1 * paketpreis))) + 
          geom_boxplot(
            color="springgreen3",
            fill="springgreen3",
            alpha=0.4,
            outlier.colour="firebrick2",
            outlier.fill="firebrick2",
            outlier.size=3.5
          ) +
          scale_y_continuous(breaks = seq(0, 2000, by = 100)) +
          theme(axis.text=element_text(size=11), axis.title=element_text(size=12,face="bold"), 
                plot.title = element_text(size=14, face="bold"))  +
          stat_boxplot(geom = "errorbar", width = 0.4) + 
          labs(title="Boxplot der notwendigen Kosten bis Album voll ist (Fall 1)", 
               x="", y="Kosten bis Album voll ist") 
        
        
    #############################################################
    # FALL 2: jedes bild kann mehr als 1mal pro paket vorkommen #
    #############################################################
  
        alpha <- 0.05   
        mean2 <- mean(ergebnisliste$ergebnisse_fall2)
        median2 <- median(ergebnisliste$ergebnisse_fall2)
        sd2 <- sd(ergebnisliste$ergebnisse_fall2)
        konfidenzintervall2 <- mean2 + c(-1,1) * qnorm(1-alpha/2) * sd2 / 
                               sqrt(length(ergebnisliste$ergebnisse_fall2))
        quantile2 <- quantile(ergebnisliste$ergebnisse_fall2)

        
        # basic plot der ergebnisse von fall 2
        plot(ergebnisliste$ergebnisse_fall2, 
             main="Anzahl der notwendigen Paketkäufe bis Album voll ist (Fall 2)",
             xlab="Simulationen", ylab="Anzahl notwendiger Paketkäufe")
        abline(h=mean(ergebnisliste$ergebnisse_fall2), 
               lty="dotted", col="red", lwd=3)
        mtext(text=paste("Mittelwert:",ceiling(mean2)), col="red")
        
        
        
        # histogramm der notwendigen paketkäufe darstellen
        # (inkl. mittelwert und grenzen des konfidenzintervalls)
        qplot(as.data.frame(ergebnisliste$ergebnisse_fall2),
              geom="histogram",
              bins=20,  
              main = "Histogramm der notwendigen Paketkäufe bis Album voll ist (Fall 2)",
              xlab = "Anzahl der notwendigen Paketkäufe",  
              ylab = "Häufigkeit",
              fill=I("green"), 
              colour=I("black"), 
              alpha=I(.3),
              ylim = c(0, 250)
        ) + 
          geom_vline(xintercept=mean2, colour="saddlebrown", alpha=0.3, 
                     size = 1, lty="solid") +
          annotate("text", label = paste("Mittelwert", ceiling(mean2)), x = mean2+60, y = 200, 
                   color = "saddlebrown") +
          scale_y_continuous(breaks = seq(0, 250, by = 25)) + 
          scale_x_continuous(breaks = seq(500, 2000, by = 250)) +
          theme(axis.text=element_text(size=10), axis.title=element_text(size=11,face="bold"), 
                plot.title = element_text(size=13, face="bold")) + 
          geom_vline(xintercept=konfidenzintervall2[1], colour="blue", alpha=0.3, 
                     size = 1, lty="solid") +
          annotate("text", label = "Untere Grenze KI", x = mean2-175, y = -5, 
                   color = "blue") + 
          geom_vline(xintercept=konfidenzintervall2[2], colour="blue", alpha=0.3, 
                     size = 1, lty="solid") +
          annotate("text", label = "Obere Grenze KI", x = mean2+165, y = -5, 
                   color = "blue") 
          
        
        
        # quantile
        ggplot(as.data.frame(ergebnisliste$ergebnisse_fall2), 
               aes(x="", y=as.data.frame(ergebnisliste$ergebnisse_fall2))) + 
          geom_boxplot(
            color="springgreen3",
            fill="springgreen3",
            alpha=0.4,
            outlier.colour="firebrick2",
            outlier.fill="firebrick2",
            outlier.size=3.5
          ) +
          scale_y_continuous(breaks = seq(0, 2000, by = 100)) +
          theme(axis.text=element_text(size=12), axis.title=element_text(size=12,face="bold"), 
                plot.title = element_text(size=14, face="bold"))  +
          stat_boxplot(geom = "errorbar", width = 0.4) + 
          labs(title="Boxplot der Anzahl der notwendigen Paketkäufe (Fall 2)", 
               x="", y="Anzahl der notwendigen Paketkäufe bis Album voll ist")
        
        
        # FALL 2: BETRACHTUNG DER KOSTEN BIS ALBUM VOLL IST:
        mean2_kosten <- mean2*paketpreis
        median2_kosten <- median2*paketpreis
        sd2_kosten <- sd2*paketpreis
        konfidenzintervall2_kosten <- mean2_kosten + c(-1,1) * qnorm(1-alpha/2) * sd2_kosten / 
          sqrt(length(ergebnisliste$ergebnisse_fall2)) 
        quantile_kosten2 <- quantile(ergebnisliste$ergebnisse_fall2*paketpreis)
        
        
        # histogramm der notwendigen kosten darstellen
        # (inkl. mittelwert und grenzen des konfidenzintervalls)
        qplot(as.data.frame(ergebnisliste$ergebnisse_fall2 * paketpreis),
              geom="histogram", 
              bins=20,  
              main = "Histogramm der notwendigen Kosten bis Album voll ist (Fall 2)",
              xlab = "Kosten bis Album voll ist",  
              ylab = "Häufigkeit",
              fill=I("green"), 
              colour=I("black"), 
              alpha=I(.3),
              ylim = c(0, 250)
        ) + 
          geom_vline(xintercept=mean2_kosten, colour="saddlebrown", alpha=0.3, 
                     size = 1, lty="solid") +
          annotate("text", label = paste("Mittelwert", ceiling(mean2_kosten)), x = mean2_kosten+20, y = 170, 
                   color = "saddlebrown") +
          scale_y_continuous(breaks = seq(0, 250, by = 25)) + 
          scale_x_continuous(breaks = seq(500, 2000, by = 100)) +
          theme(axis.text=element_text(size=10), axis.title=element_text(size=11,face="bold"), 
                plot.title = element_text(size=13, face="bold")) + 
          geom_vline(xintercept=konfidenzintervall2_kosten[1], colour="blue", alpha=0.3, 
                     size = 1, lty="solid") +
          annotate("text", label = "Untere Grenze KI", x = konfidenzintervall2_kosten[1]-150, y = -5, 
                   color = "blue") + 
          geom_vline(xintercept=konfidenzintervall2_kosten[2], colour="blue", alpha=0.3, 
                     size = 1, lty="solid") +
          annotate("text", label = "Obere Grenze KI", x = konfidenzintervall2_kosten[2]+150, y = -5, 
                   color = "blue") 
        
        
        # quantile der kosten
        ggplot(as.data.frame(ergebnisliste$ergebnisse_fall2 * paketpreis), 
               aes(x="", y=as.data.frame(ergebnisliste$ergebnisse_fall2 * paketpreis))) + 
          geom_boxplot(
            color="springgreen3",
            fill="springgreen3",
            alpha=0.4,
            outlier.colour="firebrick2",
            outlier.fill="firebrick2",
            outlier.size=3.5
          ) +
          scale_y_continuous(breaks = seq(0, 2000, by = 100)) +
          theme(axis.text=element_text(size=11), axis.title=element_text(size=12,face="bold"), 
                plot.title = element_text(size=14, face="bold"))  +
          stat_boxplot(geom = "errorbar", width = 0.4) + 
          labs(title="Boxplot der notwendigen Kosten bis Album voll ist (Fall 2)", 
               x="", y="Kosten bis Album voll ist") 
        
        
 }
  

  # funktion zur darstellung der simulationsergebnisse aufrufen
    
  paketpreis <- 0.90  # paketpreis vom user waehlbar

  darstellen(ergebnisliste, paketpreis)
  
  
  
  
  

  
  
  
  
  
  
  
  
  
  
  
  
  
  