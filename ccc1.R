
# loesche alle alten objekte im file

rm(list=ls())

# verwende library ggplot2 fuer visualisierung

library(ggplot2)

# generiere eine sequenz von n ausprägungen der zufallsvariablen S und T
# S und T sind gleichverteilt im intervall (-1,1]

    n <- 15000  # testwert für anzahl der simulationen setzen
    
    s_gleichverteilt <- runif(n, min=-1, max=1)  
    t_gleichverteilt <- runif(n, min=-1, max=1)  
    
    s_und_t <- cbind(s_gleichverteilt,t_gleichverteilt)
     

# bedingung, dass (x^2+y^2) < 1, ueberpruefen
# (also pruefen, ob punktkoordinaten im eingeschriebenen kreis liegen) 

   # platz zur speicherung der true/false ergebnisse
      pi <- rep(0,n)  

   # bedingung abpruefen: 
      # setze TRUE, falls bedingung erfuellt
      # setze FALSE, falls bedingung nicht erfuellt 
      for (i in 1:n) { 
        
        pi[i] <- ifelse( (sum(s_und_t[i,1]^2, s_und_t[i,2]^2) < 1), T, F)
      
      } 
    

# P(A) = |c|/|B| = PI/4:
# also PI = 4*|C|/|B| = 4*(punkte im kreis)/(punkte im rechteck)
      
     4*sum(pi)/sum(length(pi))
     
     
# PLOT 1: darstellung der simulationsergebnisse mit hilfe von ggplot2  

    x11()
    ggplot(as.data.frame(s_und_t), aes(x=s_gleichverteilt, y=t_gleichverteilt,col=pi+1)) + 
    geom_point(size=0.5) + 
    labs(title="Simulation von 2 unabhängigen, gleichverteilten Zufallsvariablen", 
         x="Ausprägung der Zufallsvariable S", y="Ausprägung der Zufallsvariable T") +
    theme(legend.position="none")     
     

# PLOT 2: darstellung des annaeherungsprozesses der simultation an PI
    
    # matrix anlegen fuer die auspraegungen der pi-werte
    pi_matrix_s_t <- matrix( ,nrow=n,ncol=2)
    
    # befuellen der matrix mit pi-werten
    for(i in 1:n){
      
      pi_matrix_s_t[i,] <- c(i, sum(pi[1:i])*4 / sum(length(1:i)) )
      
    }    

    
    # visualisierung mit ggplot2, dass sich die simulationsergebnisse an PI annaehern
    x11()
    ggplot(as.data.frame(pi_matrix_s_t), aes(x=pi_matrix_s_t[ ,1], y=pi_matrix_s_t[ ,2])) + 
      geom_hline(yintercept=3.1415, colour="blue", size = 1) +  
      geom_line(colour="red", size=1) +
      labs(title="Simulation nähert sich an die Zahl PI an", 
           x="Anzahl der Simulationen", y="Simulierte PI-Werte") +
      theme(legend.position="none") +
      annotate("text", n , 3.1415, vjust=2, label = "PI (3.14)", colour="blue")
    
    
    
    
    
    
    
        