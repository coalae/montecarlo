# alte objekte aus dem workspace loeschen

rm(list=ls())

# library ggplot2 verwenden

library(ggplot2)

# einfachen LCG-algorithmus auswaehlen:
  
  # X(n+1) = (a * X(n) + b) mod m, wobei a,b,m(sequenzlaenge) waehlbare parameter sind
  # gleichverteilte sequenz U(1),...,U(n) ~ iid Uni(0,1)
  # seedvalue ... initialer wert fuer runs
  # U1,...,Un ... 1. generator
  # V1,...,Vn ... 2. generator
  # wi ... U rank(Vi) ... kombinierter generator
  # vi verwenden, damit man "random" permutation von U1,...,Un erzeugen

    # function des LCG
    linearCongruentialGenerator <- function(n, a=114343, b=0, m=235199, seedValue=3){ 

      zahlen <- numeric(n)
      voriger <- seedValue # zuerst auf intialwert setzen, 
                           # dann immer vorigen wert von zahlen nehmen
      
      # zahlen nach dem oben angegebenen algorithmus generieren 
      for (i in 1:n){ 
        zahlen[i] <- (a*voriger+b) %% m
        voriger <- zahlen[i]
      }
      
      zahlen/m
    
    }

# bestimmte anzahl von zufallszahlen mit dem LCG-algorithmus generieren
    
  anzahl_zuGenerierendeZahlen <- 1500 # anzahl der zu generierenden zahlen frei waehlbar
  
  zufallszahlen <- linearCongruentialGenerator(anzahl_zuGenerierendeZahlen)

  
# anhand von histogramm visualisieren, ob zahlen gleichverteilt ausschauen
  
  # histogramm der generierten zufallszahlen
  qplot(as.data.frame(zufallszahlen),
        geom="histogram",
        bins=10,  
        main = "Histogramm der Zufallszahlen (eingeteilt in 10 Bereiche)",
        xlab = "Bereiche der Zufallszahlen",  
        ylab = "Haeufigkeit der Zufallszahlen",
        fill=I("green"), 
        colour=I("black"), 
        alpha=I(.3)
        ) + 
     geom_hline(yintercept=length(zufallszahlen)/10, colour="dodgerblue2", alpha=0.3, 
                size = 1, lty="dashed") +
     annotate("text", 0.17 , anzahl_zuGenerierendeZahlen/10, vjust=2, 
              label = "Häufigkeit bei Theor. Gleichverteilung", colour="dodgerblue2")
  
  
# kombiniere die beiden LCG
  
  # LCG1 (u)
  lcg_u <- linearCongruentialGenerator(n=1500)
  
  # LCG2 (v)
  lcg_v <- linearCongruentialGenerator(n=1500)
  
  # rang des LCG2 (vi)
  raenge <- order(lcg_v)
  bind_u_vrank <- cbind(lcg_u,raenge)
  lcg_w <- bind_u_vrank[order(raenge), ] # lcg_u zufallszahlen nach den 
                                         # raengen von lcg_v geordnet (aufsteigend)
  combined_generator_w <- lcg_w[,1] # kombinierter LCG w
  

# CHI-QUADRAT-ANPASSUNGSTEST:
  # testet auf gleichverteilung der zufallszahlen, falls nichts weiteres gegeben

  chisq.test(combined_generator_w) 
  
  # bzw. test auf rows einer matrix aus den zufallszahlen
  zufallsmatrix <- matrix(combined_generator_w, nrow=2)
  
  chisq.test(zufallsmatrix[1, ], zufallsmatrix[2, ])
  
  # INTERPRETATION: p-value > alpha, daher test nicht signifikant,
                 #  also ist die H0 akzeptiert (d.h. gleichverteilung)

  
# KOLMOGOROV-SMIRNOV-TEST
  # KS-test testet in H0, ob die empirische verteilungsfunktion aus den daten der
  # theoretischen verteilungsfunktion der gleichverteilung entspricht.
  # KS-test testet dies hier also fuer die ergebnisse des kombinierten LCG
  
  ks.test(combined_generator_w, punif) # punif ... ist die verteilungsfunktion der gleichverteilung, 
  #           mit der hier verglichen wird

  # INTERPRETATION:
  # hier ist der p-value nicht signifikant, und daher wird die nullhypothese der
  # gleichverteilung der zufallszahlen, die vom komibinierten generator erzeugt wurden, 
  # akzeptiert
  
    
# ILLUSTRATIVE GRAFIKEN ZU CHI-QUADRAT-ANPASSUNGSTEST und KOLMOGOROV-SMIRNOV-TEST:
  x11()
  hist(combined_generator_w, breaks=10,
       main="Histogramm der Zufallszahlen",
       xlab="Zufallszahlen", ylab="Häufigkeit",
       ylim=c(0,170),
       col="wheat1", alpha=0.7) # simple plot histogramm des kombinierten LCG w
  
  abline(h=length(combined_generator_w)/10, col="darkslateblue", lty="dashed")
  text(0.05, -10+length(combined_generator_w)/10, 
       "Häufikgeit bei Theoretischer Gleichverteilung", 
       col = "darkslateblue", adj = c(0, -.1), cex=0.8)
  
 
    
  # ggplot histogramm/qplot der mit LCG w generierten zufallszahlen
  qplot(as.data.frame(combined_generator_w),
        geom="histogram",
        bins=10,  
        main = "Histogramm der Zufallszahlen (eingeteilt in 10 Bereiche)",
        xlab = "Bereiche der Zufallszahlen",  
        ylab = "Haeufigkeit der Zufallszahlen",
        fill=I("darkturquoise"), 
        colour=I("black"), 
        alpha=I(.3)
  ) + 
    geom_hline(yintercept=length(combined_generator_w)/10, colour="maroon4", alpha=0.3, 
               size = 1, lty="dashed") +
    annotate("text", 0.17 , anzahl_zuGenerierendeZahlen/10, vjust=2, 
             label = "Häufigkeit bei Theor. Gleichverteilung", colour="maroon4")
  


# PLOT AUF SERIELLE KORRELATION
  # ob qualitaet von Wi besser als Ui ist

  # plot-funktionen fuer korrelationen aus "1_Random Numbers.r" entnommen 
  # (Quelle R Script von Prof. Hudec)  
  random2d.plot <- function(x)
  {
    xi <- x[-length(x)]
    yi <- x[-1]
    plot(xi, yi, axes=T, xlab="",ylab="", cex=0.6)
    
  }
  
  random3d.plot <- function(x)
  {
    xi <- x[1:(length(x)-2)]
    yi <- x[2:(length(x)-1)]
    zi <- x[3:length(x)]
    
    cloud(zi ~ xi * yi,xlab="",ylab="",zlab="", cex=0.6)
  }
  
  # anzeige der grafiken in 2 plots nebeneinander setzen
  par(mfrow=c(1,2))
   
  # 2d-plot fuer LCG u
  random2d.plot(lcg_u)
  
  # 2d-plot fuer kombinierten LCG w
  random2d.plot(combined_generator_w)


  
  # 3d-plot fuer LCG u
  random3d.plot(lcg_u)
  
  # 3d-plot fuer kombinierten LCG w
  random3d.plot(combined_generator_w)
  
  
  