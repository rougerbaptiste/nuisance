rm(list=ls())

samplefx <- function(x, size = length(x), replace = FALSE) {
  if (length(x) > 1) {
    return(sample(x, size, replace))
  } else {
    return(x)
  }
}


nbJour <- 10 # nombre de jour que dure la modélisation

nbMal <- 10 # nombre de males dans l'experience
numMal <- 1:10 # numéro representant les males
numPartnMal <- rep(0,nbMal) # contient les numéros des partenaires de chaque male

nbFem <- 10 # nombre de femelles dans l'experience
numFem <- 1:10 # numéro représentant les femelles
numPartnFem <- rep(0,nbMal) # contient les numéros des partenaires de chaque femelle

lifeTime <- 15
timeLeftMal <- round(rnorm(nbMal , mean = lifeTime , sd=1)) # on cree des males ayant une duree de vie de moyenne 5 et de sd 1
timeLeftFem <- round(rnorm(nbFem , mean = lifeTime , sd=1)) # on cree des males ayant une duree de vie de moyenne 5 et de sd 1

dureeCycle <- 5 # duree du cycle des femelles
cycle <- sample(0:dureeCycle, nbFem, replace=T) # Là où la femelle en est dans le cycle
gestation <- sample(c(-1,1), nbFem, replace=T)  # permet de savoir si la femelle est en gestation (-1), prête à l'accouplement (0) ou pas prête
dureeGestation <- 5


resplenish <- round(rnorm(nbMal , mean = 0.5 , sd=0.5),2) # resplenish des males en %
resplenish[resplenish > 1] <- 1
resplenish[resplenish < 0] <- 0

jour <- 1 # compteur de jours
while(jour <= nbJour){
	
	print(cycle == 0 & gestation == -1)
	
	
	## Les femelles se reproduisent
	

    temps <- round(0.7*nbMal) # contient le nombre d'essai max de mate

    iTemps <- 1 #

    while(iTemps <= temps && ( sum(gestation==0 && timeLeftFem >0) > 0 )  ){

        pickedFem <- samplefx(numFem[gestation==0 && timeLeftFem >0],1,replace = F) #contient le numéro de la femelle choisie
        pickedMal <- samplefx(numMal[resplenish==1 && timeLeftMal >0],1,replace = F) # contient le numéro de la femelle

        gestation[pickedFem] <- -1
        cycle[pickedFem] <- dureeCycle

        resplenish[pickedMal] <- 0
        
        print(pickedFem)

        
        iTemps <- iTemps +1
    }
    

    cycle <- cycle - 1 # les femelles évoluent dans le cycle
    cycle[cycle < 0] <- 0
    

    resplenish <- resplenish + 0.1 # les males se resplenish
    resplenish[resplenish > 1] <- 1


    ## Si la femelle met bas :

    accouch <- numFem[cycle == 0 & gestation == -1]

                                        # nouvelle pop

    #print(cycle)
    #print(gestation)
    #print(accouch)
    cycle[accouch] <- dureeCycle
    gestation[accouch] <- 1

    #print(c("cycle",cycle))
    #print(c("gestation",gestation))
                                        #print(c("resplenish",resplenish))

    #print(cycle)
    #print(timeLeftFem)
    #print(resplenish)

    timeLeftMal <- timeLeftMal - 1
    timeLeftMal[timeLeftMal < 0] <- 0
    timeLeftFem <- timeLeftFem - 1
    timeLeftFem[timeLeftFem < 0] <- 0
    
    jour <- jour+1
}
