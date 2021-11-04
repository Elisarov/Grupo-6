
source("teoriadecision_funciones_incertidumbre.R")

## Código de la función pedida:

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
        cat ("la mejor alternativa es",altsinrep[i+1],"para alfa perteneciente a [",
             alfas[i],",",alfas[i+1],"] ") 
      }
      cat ("y la mejor alternativa es", altsinrep[length(altsinrep)], "si alfa pertenece a [", 
           alfas[length(alfas)],", 1 ]")
      }
    }
  )
  
} #fin de función creada



## EJEMPLO para comprobar si funciona:

# Una persona quiere tomar la decisión de qué medio de transporte usar para ir 
# diariamente a trabajar. Las alternativas son las siguientes: coche, autobús,
# patinete eléctrico y metro.
# El dinero que ahorraría el trabajador (expresado en euros) usando cada uno de
# los transportes varía según: 

# E1: va al trabajo en dicho transporte y vuelve andando
# E2: va al travajo andando y vuelve en dicho transporte
# E3: va y vuelve del trabajo en dicho transporte
# E4: va y vuelve al trabajo en dicho transporte repartiendo gastos (si es 
#     posible) con un compañero de trabajo.

# y se recoge en la siguiente tabla:

#            E1 E2 E3 E4
# Coche      24 27 10 16
# Autobús    16 16 16 16
# Patinete   13 23 21 15
# Metro      25 24 14 14

# ¿Qué transporte sería la mejor opción (con el que más ahorre) para ir a 
# trabajar?

# SOLUCIÓN:

# Objetivo: maximizar ahorro.
# 
# Planteamiento:
#   
# -Un decisor
# -Modelo favorable
# 
# Alternativas:
#   
# 1="Coche"
# 2="Autobús"
# 3="Patinete eléctrico"
# 4="Metro"
# 
# Estados de la naturaleza:
#   
# E1: va al trabajo en dicho transporte y vuelve andando
# E2: va al travajo andando y vuelve en dicho transporte
# E3: va y vuelve del trabajo en dicho transporte
# E4: va y vuelve al trabajo en dicho transporte repartiendo gastos (si es 
#     posible) con un compañero de trabajo.
# 
# Resolución:

t2=crea.tablaX(c(24,27,10,16,16,16,16,16,23,23,21,15,25,24,14,14),4,4)
criterio.Todos(t2, alfa=0.3,favorable = TRUE)

#Según el criterio de Wald la mejor alternativa es la 2 (Autobús), según el 
#criterio optimista la mejor alternativa es la 1 (coche) y según los demás 
#criterios la mejor alternativa es la 3 (Patinete Eléctrico).

#NOTA: se ha aplicado el criterio de Hurwicz para alfa=0.3, vamos a ver ahora 
#      como varía para los diferentes valores de alfa

dibuja.criterio.Hurwicz(t2,favorable=TRUE)

alfas(t2,precisionAlfa = 0.05,favorable = TRUE)
# La mejor alternativa es 2 para alfa perteneciente a [ 0 , 0.15 ], 
# la mejor alternativa es 3 para alfa perteneciente a [ 0.15 , 0.35 ] la mejor 
# alternativa es 4 para alfa perteneciente a [ 0.35 , 0.7 ] y la mejor 
# alternativa es 1 si alfa pertenece a [ 0.7 , 1 ]


# Hago lo mismo con una mayor precisión en los cambios de alternativa:
alfas(t2,precisionAlfa = 0.001,favorable = TRUE)
# La mejor alternativa es 2 para alfa perteneciente a [ 0 , 0.126 ], 
# la mejor alternativa es 3 para alfa perteneciente a [ 0.126 , 0.334 ] la mejor 
# alternativa es 4 para alfa perteneciente a [ 0.334 , 0.667 ] y la mejor 
# alternativa es 1 si alfa pertenece a [ 0.667 , 1 ]




