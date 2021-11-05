source("teoriadecision_funciones_incertidumbre.R")

alfas = function(tablaX, precisionAlfa=0.05, favorable=TRUE){
  X = tablaX;
  Altmin = apply(X,MARGIN=1,min);
  Altmax = apply(X,MARGIN=1,max);
  valfa = seq(from=0,to=1,by=precisionAlfa);
  vHurwicz = rep(0,length(valfa));
  alternativa = rep(0,length(valfa));
  Alt_vHurwicz = rep(0,length(valfa));
  for (i in 1:length(valfa)) {
    alfab = valfa[i];
    if (favorable) {
      vAltH = alfab * Altmax + (1-alfab) * Altmin;
      vHurwicz[i] = max(vAltH)
      alternativa[i]=which.max(vAltH) #que alternativa lo cumple
    } else {
      vAltH = alfab * Altmin + (1-alfab) * Altmax;
      vHurwicz[i] = min(vAltH)
      alternativa[i]=which.min(vAltH) #que alternativa lo cumple
    }
  }
  
  altsinrep=unique(alternativa) #me quedo con las diferentes alternativas (quitando las repeticiones)
  alfasb = (which(!duplicated(alternativa)))*precisionAlfa - precisionAlfa
  #posiciones de cuándo aparecen las nuevas alternativas en el intervalo [0,1] (alfas)
  alfas=alfasb[-1] #quitamos la primera que siempre será 0)
  
  #salida que se nos pide:
  return(
    
    if (length(alfas)==1) {
      cat ("La mejor alternativa es",altsinrep[1],"para alfa perteneciente a [ 0 ,",
           alfas[1],"] y la mejor alternativa es",altsinrep[2],
           "para alfa perteneciente a [",alfas[1],", 1 ]")
    } else {
      if (length(alfas)==2) {
        cat ("La mejor alternativa es",altsinrep[1],"para alfa perteneciente a [ 0 ,",
             alfas[1],"], la mejor alternativa es",altsinrep[2],
             "para alfa perteneciente a [",alfas[1],",",alfas[2],
             "] y la mejor alternativa es", altsinrep[3], "si alfa pertenece a [",
             alfas[2],", 1 ]")
      } else {
        cat ("La mejor alternativa es",altsinrep[1],"para alfa perteneciente a [ 0 ,",
             alfas[1],"], ")
        for (i in 1:(length(alfas)-1)) {
          cat ("la mejor alternativa es",altsinrep[i],"para alfa perteneciente a [",
               alfas[i],",",alfas[i+1],"] ")
        }
        cat ("y la mejor alternativa es", altsinrep[length(altsinrep)], "si alfa pertenece a [",
             alfas[length(alfas)],", 1 ]")
      }
      
    })
}

tM = matrix(c(1500, 1500, 1500, 1500, 
              500, 3000, 3000, 3000, 
              -5000, 2000, 4500, 4500, 
              -1500, 1000, 3500, 6000), nrow = 4, byrow = T)
rownames(tM) <- c("Fabricar 1 ud", "Fabricar 2 uds", "Fabricar 3 uds", "Fabricar 4 uds")
colnames(tM) <- c("Demanda = 1", "Demanda = 2", "Demanda = 3", "Demanda = 4")
tM

criterio.Todos(tM, alfa=0.7,favorable = TRUE)
dibuja.criterio.Hurwicz(tM,favorable=TRUE)
alfas(tM,precisionAlfa = 0.02,favorable = TRUE)
alfas(tM,precisionAlfa = 0.10,favorable = TRUE)

