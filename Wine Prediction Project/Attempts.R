setwd("/Users/suoyiyang/Documents/Stats101A")
set.seed(1020)
newWineTraining <-read.csv("Wine2017Training2.csv", stringsAsFactors = TRUE)
newWineTraining <- newWineTraining[-c(453,4135),];

TrainInd <- sample(seq_len(nrow(newWineTraining)), size = 5998)
Train <- newWineTraining[TrainInd, ]
Test <- newWineTraining[-TrainInd, ]

library(car)
library(MASS)

TwineColor <- Train$Wine.Color
TfixedAcidity <- Train$fixed.acidity #not really
TvolatileAcidity <- Train$volatile.acidity #little more
TcitricAcid <- Train$citric.acid #not really
TresidualSugar <- Train$residual.sugar
Tchlorides <- Train$chlorides
TfreeSulfurDioxide <- Train$free.sulfur.dioxide
TtotalSulfurDioxide <- Train$total.sulfur.dioxide
TwineDensity <- Train$density
Tph <- Train$pH
Tsulphates <- Train$sulphates
Talcohol <- Train$alcohol
Tquality <- Train$Quality


TewineColor <- Test$Wine.Color
TefixedAcidity <- Test$fixed.acidity #not really
TevolatileAcidity <- Test$volatile.acidity #little more
TecitricAcid <- Test$citric.acid #not really
TeresidualSugar <- Test$residual.sugar
Techlorides <- Test$chlorides
TefreeSulfurDioxide <- Test$free.sulfur.dioxide
TetotalSulfurDioxide <- Test$total.sulfur.dioxide
TewineDensity <- Test$density
Teph <- Test$pH
Tesulphates <- Test$sulphates
Tealcohol <- Test$alcohol
Tequality <- Test$Quality


wineColor <- newWineTraining$Wine.Color
fixedAcidity <- newWineTraining$fixed.acidity #not really
volatileAcidity <- newWineTraining$volatile.acidity #little more
citricAcid <- newWineTraining$citric.acid #not really
residualSugar <-newWineTraining$residual.sugar
chlorides <- newWineTraining$chlorides
freeSulfurDioxide <- newWineTraining$free.sulfur.dioxide
totalSulfurDioxide <- newWineTraining$total.sulfur.dioxide
wineDensity <- newWineTraining$density
ph <- newWineTraining$pH
sulphates <- newWineTraining$sulphates
alcohol <- newWineTraining$alcohol
quality <- newWineTraining$Quality

m1 <- lm(quality~ wineColor + fixedAcidity + volatileAcidity + citricAcid + residualSugar + chlorides 
         + freeSulfurDioxide + totalSulfurDioxide + wineDensity + ph + sulphates + alcohol)

summary(m1)
plot(m1)

tea <- newWineTraining
tea[citricAcid==0,]$citric.acid <- 0.00000001
summary(powerTransform(cbind(tea$citric.acid)~1))


c

Tfa <- TfixedAcidity^(1/3) 
Tva <- log(TvolatileAcidity) #mmmaaaayyybbbee
Trs <- TresidualSugar^(1/3) 
Tchlr <- Tchlorides^(1/2) #YES
Tfsd <- TfreeSulfurDioxide^(1/3) #YES
Ttsd <- TtotalSulfurDioxide^(2/3) #mmmaaaayyybbbee
Tnewph <- Tph^(1/2)
Tslpht <- Tsulphates^(1/3) #yeah
Td <- TwineDensity^40 
Ta <- Talcohol^(1/2)

Tefa <- TefixedAcidity^(1/3) 
Teva <- log(TevolatileAcidity) #mmmaaaayyybbbee
Ters <- TeresidualSugar^(1/3) 
Techlr <- Techlorides^(1/2) #YES
Tefsd <- TefreeSulfurDioxide^(1/3) #YES
Tetsd <- TetotalSulfurDioxide^(2/3) #mmmaaaayyybbbee
Tenewph <- Teph^(1/2)
Teslpht <- Tesulphates^(1/3) #yeah
Ted <- TewineDensity^40 
Tea <- Tealcohol^(1/2)

fa <- fixedAcidity^(1/3) 
va <- log(volatileAcidity) #mmmaaaayyybbbee
rs <- residualSugar^(1/3) 
chlr <- chlorides^(1/2) #mmmaaaayyybbbee
fsd <- freeSulfurDioxide^(1/6) #YES
tsd <- totalSulfurDioxide^(2/3) #mmmaaaayyybbbee
newph <- ph^(1/2)
slpht <- sulphates^(1/3) #yeah
d <- wineDensity^40 
a <- alcohol^(1/2) 

summary(powerTransform(cbind(freeSulfurDioxide,
                             totalSulfurDioxide)~1))

summary(lm(quality~ wineColor + fixedAcidity + va + citricAcid + residualSugar + chlr 
           + fsd + tsd + ph + slpht + alcohol + wineDensity))

summary(lm(quality~ wineColor + fixedAcidity + volatileAcidity + citricAcid + residualSugar + chlr 
           + fsd + tsd + ph + slpht + alcohol + wineDensity))

m2 <- lm(quality~ wineColor + fixedAcidity + va + citricAcid + residualSugar + chlr 
         + fsd + tsd + ph + slpht + alcohol + wineDensity)

Tm2 <-  lm(Tquality~ TwineColor + TfixedAcidity + Tva + TcitricAcid + TresidualSugar + Tchlr 
           + Tfsd + Ttsd + Tph + Tslpht + Talcohol + TwineDensity)

mmps(m2)
plot(quality,freeSulfurDioxide)

vif(m2)

cor(quality, fixedAcidity)
cor(quality, va)
cor(quality, citricAcid)
cor(quality, residualSugar)
cor(quality, chlr)
cor(quality, fsd)
cor(quality, tsd)
cor(quality, ph )
cor(quality, slpht)
cor(quality, alcohol)
cor(quality, wineDensity)

summary(lm(quality~wineColor))
summary(lm(quality~wineColor:alcohol)) #!!!!!!!!!
summary(lm(quality~fixedAcidity:wineColor))
summary(lm(quality~va:wineColor)) #!!
summary(lm(quality~citricAcid:wineColor))
summary(lm(quality~ residualSugar:wineColor))
summary(lm(quality~ chlr:wineColor)) #!!
summary(lm(quality~ fsd:wineColor))
summary(lm(quality~ tsd:wineColor))#!
summary(lm(quality~ ph:wineColor)) #!
summary(lm(quality~ slpht:wineColor)) #!
summary(lm(quality~ wineDensity:wineColor)) #!!!!

m3 <- lm(quality~ wineColor + fixedAcidity + va + citricAcid + residualSugar + chlr 
        + fsd + tsd + ph + slpht + alcohol + wineDensity + wineColor:alcohol + wineDensity:wineColor
        + va:wineColor +  chlr:wineColor +tsd:wineColor + ph:wineColor + slpht:wineColor)


forwardBIC1 <- step(lm(quality~1), scope=list(lower = ~1, upper = ~ wineColor + fixedAcidity + va + citricAcid + residualSugar + chlr 
                                              + fsd + tsd + ph + slpht + alcohol + wineDensity), direction="forward", k = log(6998))

#quality ~ alcohol + va + slpht + fsd + tsd + residualSugar + ph


backBIC1 <- step(lm(quality ~ wineColor + fixedAcidity + va + citricAcid + residualSugar + chlr 
                    + fsd + tsd + ph + slpht + alcohol + wineDensity), direction="backward", k=log(6999))

#fixedAcidity + va + residualSugar + fsd + tsd + ph + slpht + alcohol + wineDensity




forwardAIC1 <- step(lm(quality~1), scope=list(lower = ~1, upper = ~ wineColor + fixedAcidity + va + citricAcid + residualSugar + chlr 
                                              + fsd + tsd + ph + slpht + alcohol + wineDensity), direction="forward")

#quality ~ wineColor + fixedAcidity + va + residualSugar + fsd + tsd + ph + slpht + alcohol + wineDensity




backAIC1 <- step(lm(quality ~ wineColor + fixedAcidity + va + citricAcid + residualSugar + chlr 
                   + fsd + tsd + ph + slpht + alcohol + wineDensity), direction="backward")

#wineColor + fixedAcidity + va + residualSugar + fsd + tsd + ph + slpht + alcohol + wineDensity

```````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````













forwardBIC <- step(lm(quality~1), scope=list(lower = ~1, upper = ~ wineColor + fixedAcidity + va + citricAcid + residualSugar + chlr 
                                             + fsd + tsd + ph + slpht + alcohol + wineDensity+ wineColor:wineDensity 
                                             + wineColor:fixedAcidity +  wineColor:va + wineColor:chlr + wineColor:tsd + wineColor:ph +  
                                               wineColor:slpht + wineColor:citricAcid + wineColor:residualSugar + wineColor:fsd + wineColor:alcohol), direction="forward", k = log(6998))
#quality ~ alcohol + va + slpht + fsd + tsd + residualSugar + ph



# quality ~ wineColor + fixedAcidity + va + citricAcid + residualSugar +
#   chlr + fsd + tsd + ph + slpht + alcohol + wineDensity + wineColor:wineDensity +
#   wineColor:fixedAcidity + wineColor:chlr + wineColor:tsd +
#   wineColor:ph + wineColor:slpht + wineColor:citricAcid + wineColor:residualSugar +
#   wineColor:fsd + wineColor:alcohol

#update:
quality ~ wineColor + fixedAcidity + va + citricAcid + residualSugar + 
  chlr + fsd + tsd + ph + slpht + alcohol + wineDensity + wineColor:wineDensity + 
  wineColor:fixedAcidity + wineColor:chlr + wineColor:tsd + 
  wineColor:ph + wineColor:slpht + wineColor:citricAcid + wineColor:residualSugar + 
  wineColor:alcohol


forwardAIC <- step(lm(quality~1), scope=list(lower = ~1, upper = ~ wineColor + fixedAcidity + va + citricAcid + residualSugar + chlr 
                                             + fsd + tsd + ph + slpht + alcohol + wineDensity+ wineColor:wineDensity 
                                             + wineColor:fixedAcidity +  wineColor:va + wineColor:chlr + wineColor:tsd + wineColor:ph +  
                                               wineColor:slpht + wineColor:citricAcid + wineColor:residualSugar + wineColor:fsd + wineColor:alcohol), direction="forward")
# quality ~ alcohol + va + slpht + fsd + tsd + residualSugar +
#   ph + wineDensity + fixedAcidity + wineColor + ph:wineColor +
#   tsd:wineColor + wineDensity:wineColor + residualSugar:wineColor +
#   alcohol:wineColor + fixedAcidity:wineColor + fsd:wineColor

#update:
quality ~ alcohol + va + slpht + fsd + tsd + residualSugar + 
  ph + fixedAcidity + wineDensity + wineColor + ph:wineColor + 
  tsd:wineColor + wineDensity:wineColor + slpht:wineColor + 
  residualSugar:wineColor + alcohol:wineColor + fixedAcidity:wineColor


forwardAIC <- step(lm(quality~1), scope=list(lower = ~1, upper = ~ fixedAcidity + va + citricAcid + residualSugar + chlr 
                                             + fsd + tsd + ph + slpht + alcohol + wineDensity+ wineColor:wineDensity 
                                             + wineColor:fixedAcidity +  wineColor:va + wineColor:chlr + wineColor:tsd + wineColor:ph +  
                                               wineColor:slpht + wineColor:citricAcid + wineColor:residualSugar + wineColor:fsd + wineColor:alcohol), direction="forward")



#missing: citricAcid, chlr, wineColor:chlr, wineColor:citricAcid, wineColor:slpht, wineColor:citricAcid, 



backBIC <- step(lm(quality ~ wineColor + fixedAcidity + va + citricAcid + residualSugar + chlr 
                   + fsd + tsd + ph + slpht + alcohol + wineDensity+ wineColor:wineDensity 
                   + wineColor:fixedAcidity +  wineColor:va + wineColor:chlr + wineColor:tsd + wineColor:ph +  
                     wineColor:slpht + wineColor:citricAcid + wineColor:residualSugar + wineColor:fsd + wineColor:alcohol), direction="backward", k=log(6998))
# quality ~ wineColor + fixedAcidity + va + residualSugar + fsd +
# tsd + ph + slpht + alcohol + wineDensity + wineColor:wineDensity +
# wineColor:fixedAcidity + wineColor:tsd + wineColor:ph + wineColor:residualSugar +
# wineColor:alcohol

#missing: wineColor:fsd
































```````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````````
summary(m4 <-lm(quality ~ alcohol + va + slpht + fsd + tsd + residualSugar + ph))
summary(m5 <-lm(quality ~ wineColor + fixedAcidity + va + residualSugar + fsd + tsd + ph + slpht + 
                  alcohol + wineDensity + wineColor:tsd + wineColor:ph))
summary(m6 <-lm(quality ~ alcohol + va + slpht + fsd + tsd + residualSugar + ph + wineDensity + fixedAcidity + 
                  wineColor + chlr + ph:wineColor + tsd:wineColor + wineDensity:wineColor + slpht:wineColor +
                  va:wineColor + alcohol:wineColor + wineColor:chlr))
summary(m7 <-lm(quality ~ wineColor + fixedAcidity + va + residualSugar + chlr + 
                  fsd + tsd + ph + slpht + alcohol + wineDensity + wineColor:wineDensity +  wineColor:va + 
                  wineColor:chlr + wineColor:tsd + wineColor:ph +  wineColor:slpht))

vif(m7)
vif(m6)
vif(m5)
vif(m4)


par(mfrow=c(2,2))
plot(m2)

modelNoInteract <- lm(quality ~ fixedAcidity + wineColor + va + residualSugar + chlr + fsd + tsd + ph + slpht + alcohol)

tooMuchModel <- lm(quality~ wineColor + fixedAcidity + va + citricAcid + residualSugar + chlr 
   + fsd + tsd + ph + slpht + alcohol + wineDensity+ wineColor:wineDensity 
   + wineColor:fixedAcidity +  wineColor:va + wineColor:chlr + wineColor:tsd + wineColor:ph +  
     wineColor:slpht + wineColor:citricAcid + wineColor:residualSugar + wineColor:fsd + wineColor:alcohol)



















backAIC <- step(lm(quality ~ fixedAcidity + wineColor + va + residualSugar + chlr + fsd + tsd + ph + slpht + alcohol + wineColor:wineDensity 
                   + wineColor:fixedAcidity +  wineColor:va + wineColor:chlr + wineColor:tsd + wineColor:ph +  
                     wineColor:slpht + wineColor:citricAcid + wineColor:residualSugar + wineColor:fsd + wineColor:alcohol), direction="backward")
# quality ~ fixedAcidity + wineColor + va + residualSugar + chlr + fsd + tsd + ph + slpht + alcohol +
# wineColor:wineDensity + fixedAcidity:wineColor + wineColor:va + wineColor:chlr + wineColor:tsd + wineColor:ph +
# wineColor:slpht + wineColor:residualSugar + wineColor:fsd + wineColor:alcohol



backBIC <- step(lm(quality ~ fixedAcidity + wineColor + va + residualSugar + chlr + fsd + tsd + ph + slpht + alcohol + wineDensity + wineColor:wineDensity 
                   + wineColor:fixedAcidity +  wineColor:va + wineColor:chlr + wineColor:tsd + wineColor:ph +  
                     wineColor:slpht + wineColor:citricAcid + wineColor:residualSugar + wineColor:fsd + wineColor:alcohol), direction="backward", k=log(6999))
# quality ~ fixedAcidity + wineColor + va + residualSugar + fsd + tsd + ph + slpht + alcohol + wineColor:wineDensity +
# fixedAcidity:wineColor + wineColor:tsd + wineColor:ph + wineColor:residualSugar + wineColor:alcohol

#missing: chlr, wineColor:va, wineColor:chlr, wineColor:slpht, wineColor:fsd

quality ~ fixedAcidity + wineColor + va + residualSugar + chlr + fsd + tsd + ph + slpht + alcohol +
  wineColor:wineDensity + wineColor:fixedAcidity + wineColor:va + wineColor:chlr + wineColor:tsd + wineColor:ph +
  wineColor:slpht + wineColor:residualSugar + wineColor:fsd + wineColor:alcohol

#removed wineColor:fsd
quality ~ fixedAcidity + wineColor + va + residualSugar + chlr + fsd + tsd + ph + slpht + alcohol +
  wineColor:wineDensity + wineColor:fixedAcidity + wineColor:va + wineColor:chlr + wineColor:tsd + wineColor:ph +
  wineColor:slpht + wineColor:residualSugar + wineColor:alcohol

#removed wineColor:va
quality ~ fixedAcidity + wineColor + va + residualSugar + chlr + fsd + tsd + ph + slpht + alcohol +
  wineColor:wineDensity + wineColor:fixedAcidity + wineColor:chlr + wineColor:tsd + wineColor:ph +
  wineColor:slpht + wineColor:residualSugar + wineColor:alcohol

#removed wineColor:slpht
quality ~ fixedAcidity + wineColor + va + residualSugar + chlr + fsd + tsd + ph + slpht + alcohol +
  wineColor:wineDensity + wineColor:fixedAcidity + wineColor:chlr + wineColor:tsd + wineColor:ph +
  wineColor:residualSugar + wineColor:alcohol

#removed wineColor:va
quality ~ fixedAcidity + wineColor + va + fsd + tsd + ph + slpht + alcohol +
  wineColor:wineDensity + wineColor:fixedAcidity + wineColor:chlr + wineColor:tsd + wineColor:ph +
  wineColor:residualSugar + wineColor:fsd + wineColor:alcohol

#remove wineColor:fsd
quality ~ fixedAcidity + wineColor + va + fsd + tsd + ph + slpht + alcohol +
  wineColor:wineDensity + wineColor:fixedAcidity + wineColor:chlr + wineColor:tsd + wineColor:ph +
  wineColor:residualSugar + wineColor:alcohol

par(mfrow=c(2,2))




