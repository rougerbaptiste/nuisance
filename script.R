rm(list=ls())

samplefx <- function(x, size = length(x), replace = FALSE) {
    if (length(x) > 1) {
        return(sample(x, size, replace))
    } else {
        return(x)
    }
}


nbJour <- 100 # nombre de jour que dure la modélisation

nbMal <- 10 # nombre de males dans l'experience
numMal <- 1:10 # numéro representant les males
numPartnMal <- rep(0,nbMal) # contient les numéros des partenaires de chaque male

nbFem <- 10 # nombre de femelles dans l'experience
numFem <- 1:10 # numéro représentant les femelles
numPartnFem <- rep(0,nbMal) # contient les numéros des partenaires de chaque femelle

lifeTime <- 30
timeLeftMal <- round(rnorm(nbMal , mean = lifeTime , sd=1)) # on cree des males ayant une duree de vie de moyenne 5 et de sd 1
timeLeftFem <- round(rnorm(nbFem , mean = lifeTime , sd=1)) # on cree des males ayant une duree de vie de moyenne 5 et de sd 1

dureeCycle <- 5 # duree du cycle des femelles
dureeGestation <- 7
maturiteFem <- 10

dCM <- dureeCycle + maturiteFem
cycle <- sample(0:dCM, nbFem, replace=T) # Là où la femelle en est dans le cycle
gestation <- sample(c(-1,1), nbFem, replace=T)  # permet de savoir si la femelle est en gestation (-1), prête à l'accouplement (0) ou pas prête


maturiteMal <- 10
resplDaily <- 0.1
resplenish <- round(rnorm(nbMal , mean = 0.5 , sd=0.5),2) # resplenish des males en %
resplenish[resplenish > 1] <- 1
resplenish[resplenish < 0] <- 0

tpsBMateMean <- 2
#tpsBMate <- round(rnorm(nbMal , mean = tpsBMateMean , sd=1))

jour <- 1 # compteur de jours
while(jour <= nbJour){
    
                                        #print(cycle == 0 & gestation == -1)
    
    
    cycle <- cycle - 1 # les femelles évoluent dans le cycle
    gestation[cycle == 0 && gestation == 1] <- 0
    cycle[cycle < 0] <- dureeCycle
    

    resplenish <- resplenish + resplDaily # les males se resplenish
    resplenish[resplenish > 1] <- 1

    ## Accouplement
    pretes <- numFem[gestation==0 && numPartnFem != 0]
    gestation[pretes] <- -1
    cycle[pretes] <- dureeGestation
    copieNumPartnFem <- numPartnFem
    copieNumPartnMal <- numPartnMal
    resplenish[numPartnFem[pretes]] <- 0
    
    numPartnMal[numPartnFem[pretes]] <-0
    numPartnFem[pretes] <- 0
    
    #print(resplenish)
    
    
    ## mise bas de femelles

    if( sum(cycle == 0 && gestation == -1) ){

        num_mortes <- numFem[timeLeftFem==0]
        timeLeftFem[num_mortes] <- round(rnorm(length(num_mortes) , mean = lifeTime , sd=1))
        cycle[num_mortes] <- sample(0:dureeCycle, length(num_mortes), replace=T) + maturiteFem
        gestation[num_mortes] <- 1
        numPartnFem[num_mortes] <- 0
    
        ## mise bas de mâles
        
        num_morts <- numMal[timeLeftMal==0]
        timeLeftMal[num_morts] <- round(rnorm(length(num_morts) , mean = lifeTime , sd=1))
        resplenish[num_morts] <- 0 - resplDaily*maturiteMal  
        numPartnMal[num_morts] <- 0

    
        ## les femelles ont mis bas
        # Il faut s'occuper de ça
    
    }
    
    
    temps <- round(0.7*nbMal) # contient le nombre d'essai max de mate

    iTemps <- 1 #

    while(iTemps <= temps & sum(numPartnFem==0) > 0 & sum(numPartnMal==0) ){ # on apparie les individus

        
        if(   sum((resplenish==1 & timeLeftMal >0  &  (numPartnMal == 0)  )) >0  & sum(  (gestation==0 | (gestation==1 & cycle <= tpsBMateMean) ) & timeLeftFem >0 & numPartnFem == 0) >0  ){
            pickedFem <- samplefx(numFem[(gestation==0 | (gestation==1 & cycle <= tpsBMateMean) ) & timeLeftFem >0 & numPartnFem == 0],1,replace = F) #contient le numéro de la femelle choisie
            pickedMal <- samplefx(numMal[resplenish==1 & timeLeftMal >0 & numPartnMal==0],1,replace = F) # contient le numéro du male choisi

                                        #print(timeLeftFem[pickedFem])

            gestation[pickedFem] <- -1
            cycle[pickedFem] <- dureeGestation +1
            numPartnFem[pickedFem] <- pickedMal

            resplenish[pickedMal] <- 0 #il faut changer et mettre ça à l'acocuplement pas à l'appariement
            numPartnMal[pickedMal] <- pickedFem
            
        }


        
        iTemps <- iTemps +1
    }

    print("")
    print(numPartnFem)
    print(numPartnMal)
    print(cycle)
    print(gestation)
    
    
    timeLeftMal <- timeLeftMal - 1
    timeLeftMal[timeLeftMal < 0] <- 0
    timeLeftFem <- timeLeftFem - 1
    timeLeftFem[timeLeftFem < 0] <- 0
    
    jour <- jour+1
}
