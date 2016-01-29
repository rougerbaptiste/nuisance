rm(list=ls())

nbJour <- 10 # nombre de jour que dure la modélisation

nbMal <- 10 # nombre de males dans l'experience
numMal <- 1:10 # numéro representant les males

nbFem <- 10 # nombre de femelles dans l'experience
numFem <- 1:10 # numéro représentant les femelles

timeLeftMal <- round(rnorm(nbMal , mean = 5 , sd=1)) # on cree des males ayant une duree de vie de moyenne 5 et de sd 1
timeLeftFem <- round(rnorm(nbFem , mean = 5 , sd=1)) # on cree des males ayant une duree de vie de moyenne 5 et de sd 1

dureeCycle <- 5 # duree du cycle des femelles
cycle <- sample(0:dureeCycle, nbFem, replace=T) # Là où la femelle en est dans le cycle
gestation <- sample(c(-1,1), nbFem, replace=T)  # permet de savoir si la femelle est en gestation (-1), prête à l'accouplement (0) ou pas prête
gestation[cycle==0] <- 0

resplenish <- round(rnorm(nbMal , mean = 0.5 , sd=0.5),2) # resplenish des males en %
resplenish[resplenish > 1] <- 1
resplenish[resplenish < 0] <- 0

jour <- 1 # compteur de jours
while(jour <= nbJour){
    
    cycle <- cycle - 1 # les femelles évoluent dans le cycle
    cycle[cycle < 0] <- 0
    gestation[cycle==0] <- 0
    

    resplenish <- resplenish + 0.1 # les males se resplenish
    resplenish[resplenish > 1] <- 1

    print(cycle)
    print(gestation)
    print(resplenish)
    
    jour <- jour+1
}
