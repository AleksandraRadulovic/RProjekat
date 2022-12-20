#4. zadatak

library(imager)

library(MASS)  
library(nnet)

#učitavamo slike
setwd("C:\\Users\\Radulovic\\Desktop\\cetvrti_zadatak_obucavanje")
files <- list.files(path=getwd(),all.files=T, full.names=F, no.. = T)
slike <- lapply(files, load.image)




#pravim funkciju koja ordeđuje granice, kao sa časa,
#pošto su na nekim slikama odsečene krajnje granice slike
#da ne bismo imali problem pri prolasku kroz sva polja
#dodajemo veštački kranje granice, odnosno ako prva granice počinje tek oko 80. piksela,
#stavićemo da je prvi element vektora koji sadrži granice 1, da ne bismo gubili prvo polje
#analogno za krajnji element vektora

granice<-function(matrica,epsilon,prob,by.row=T){
  if(by.row){
    Mean<-rowMeans}
  
  else{
    Mean<-colMeans}
  vec<-Mean(matrica<epsilon)>=prob
  granica=which(vec!=F)
  
  if(granica[1]>79)
    
    granica=c(1,granica)
  if(granica[length(granica)]<720)
    
    granica=c(granica,721)
  return(granica)
}

#funkcija koja sređuje granice

sredjivanje <- function(vektor)
{
  novi_vektor <- c()
  for(i in 1:(length(vektor)-1))
  {
    if(vektor[i+1]-vektor[i]>5){
      novi_vektor <- c(novi_vektor, vektor[i])
    }
  }
  for(j in length(vektor):2){
    if(vektor[j]-vektor[j-1]>5)
  {
      novi_vektor <- c(novi_vektor, vektor[j])
    }
  }
  return(sort(novi_vektor))
}

#vektori sa vrednostima za kategoričke promenljive Y1...Y18
# Zatvoreno polje : 0
# Broj 1 : 10
# Broj 2 : 20
# Broj 3 : 30
# Broj 4 : 40
# Broj 5 : 50
# Broj 6 : 60
# Broj 7 : 70
# Broj 8 : 80
# Mina : -100
# Prazno polje : 100


#plot(slike[[1]])
Y1 <-c(0,0,0,0,0,0,0,0,0,
       0,0,0,20,10,10,-100,10,0,
       0,0,30,-100,10,10,10,10,0,
       20,30,-100,20,10,100,10,10,0,
       -100,20,10,10,100,100,10,-100,0,
       10,10,100,100,10,10,20,10,0,
       100,10,10,10,10,-100,20,10,0,
       100,10,-100,0,0,0,0,0,0,
       100,10,0,0,0,0,0,0,0)
    
#plot(slike[[2]])  

Y2 <-c(0,0,0,10,100,10,10,10,100,
       0,20,0,20,10,10,-100,10,100,
       0,0,30,-100,10,10,10,10,100,
       20,30,-100,20,10,100,10,10,10,
       -100,20,10,10,100,100,10,-100,10,
       10,10,100,100,10,10,20,10,10,
       100,10,10,10,10,-100,20,10,100,
       100,10,-100,10,10,20,0,10,100,
       100,10,10,10,100,10,0,10,100)
  
#plot(slike[[3]])
Y3 <-c(100,100,100,100,100,100,10,0,0,
       10,10,100,100,100,10,20,0,0,
       0,10,100,100,100,20,0,0,0,
       0,10,100,100,100,20,0,30,10,
       0,10,10,10,100,10,10,10,100,
       0,0,0,10,100,100,100,100,100,
       0,0,0,10,100,10,10,10,100,
       0,0,0,10,10,10,0,20,10,
       0,0,0,0,0,0,0,0,0)



#plot(slike[[4]])
Y4 <-c(100,100,100,100,100,100,100,10,0,
       10,10,100,100,100,100,100,10,0,
       0,10,100,100,10,20,30,30,0,
       20,20,100,100,10,-100,-100,-100,10,
       0,10,100,100,10,20,30,20,10,
       0,10,10,10,10,100,100,100,100,
       10,10,10,0,20,10,10,100,100,
       0,0,20,10,20,0,10,100,100,
       0,0,10,100,10,0,10,100,100)

#plot(slike[[5]])
Y5 <-c(100,100,100,100,100,100,100,100,100,
       100,100,100,100,100,10,10,10,100,
       100,100,100,100,100,20,0,30,10,
       10,10,100,100,100,20,0,0,0,
       0,10,100,10,20,30,30,30,0,
       10,10,100,10,0,0,10,10,0,
       100,10,10,20,20,20,10,10,10,
       10,20,0,10,100,100,100,10,0,
       0,20,10,10,100,100,100,10,0)

#plot(slike[[6]])
Y6 <-c(100,100,100,100,100,100,10,20,0,
       20,20,20,10,10,100,10,0,20,
       -100,-100,0,0,30,10,10,10,10,
       20,20,30,0,0,10,100,100,100,
       100,100,10,20,20,10,100,100,100,
       100,10,10,10,100,100,100,100,100,
       100,10,0,10,100,100,100,100,100,
       100,20,20,30,10,10,100,100,100,
       100,10,0,20,0,10,100,100,100)

#plot(slike[[7]])
Y7 <- c(100,100,100,100,10,0,20,10,100,
      100,100,100,100,10,30,0,20,100,
      100,100,100,100,100,20,0,20,100,
      100,100,100,100,100,10,20,20,10,
      10,10,10,10,10,100,10,0,10,
      0,0,0,0,10,100,10,10,10,
      0,0,0,0,20,10,100,100,100,
      0,0,0,0,0,10,10,10,10,
      0,0,0,0,0,0,0,0,0)
      
#plot(slike[[8]])
Y8 <-c(100,10,10,10,100,100,100,100,100,
       100,10,0,10,100,100,10,20,20,
       10,20,10,10,10,20,30,0,0,
       0,10,100,100,10,0,0,40,30,
       10,10,10,10,20,20,20,20,0,
       100,100,10,0,20,10,100,10,10,
       100,100,10,20,0,10,100,100,100,
       100,100,100,10,10,10,10,10,10,
       100,100,100,100,100,100,10,0,0)

#plot(slike[[9]])
Y9 <-c(0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,
       0,0,0,0,40,30,40,0,0,
       0,0,0,0,20,100,10,20,0,
       0,0,0,0,20,100,100,10,0,
       0,0,0,0,20,20,20,20,0,
       0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0)

#plot(slike[[10]])
Y10 <- c(0,0,0,0,0,0,0,0,0,
       0,30,10,30,0,0,0,0,0,
       0,20,100,10,20,20,0,0,0,
       10,10,100,100,100,10,0,0,0,
       100,100,100,10,20,40,0,0,0,
       10,20,10,20,0,0,0,0,0,
       0,0,0,0,0,0,0,0,0,
       0,0,0,0,0,0,0,40,0,
       0,0,0,0,0,0,0,0,0)
#plot(slike[[11]])
Y11 <- c(0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,30,0,
         0,0,-100,-100,-100,-100,30,0,0,
         0,0,40,40,30,30,30,0,0,
         0,0,-100,20,100,10,-100,30,-100,
         0,0,-100,20,100,10,10,20,10,
         0,0,40,20,100,100,100,10,10,
         0,-100,-100,20,10,10,100,10,-100,
         0,-100,30,20,-100,10,100,10,10)

#plot(slike[[12]])

Y12 <- c(0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,40,10,30,0,0,0,
         0,0,0,30,100,30,0,0,0,
         0,0,0,30,100,30,0,0,0,
         0,0,0,30,20,40,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0)

#plot(slike[[13]])

Y13 <- c(0,0,0,0,0,0,0,0,0,
         0,0,0,40,20,40,-100,0,0,
         0,0,0,20,100,30,-100,0,0,
         0,0,0,40,20,30,-100,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0)

#plot(slike[[14]])

Y14 <- c(0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,30,20,30,0,0,0,
         0,0,0,20,100,20,0,0,0,
         0,0,0,40,30,40,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0)

#plot(slike[[15]])
Y15 <- c(0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,40,30,30,0,0,
         0,0,0,0,10,100,20,0,0,
         0,0,0,0,40,30,40,0,0,
         0,0,0,0,0,0,0,0,0)

#plot(slike[[16]])

Y16 <- c(0,0,20,10,100,10,-100,0,0,
         0,0,-100,20,100,20,40,0,0,
         0,0,-100,30,100,10,-100,0,0,
         0,0,-100,30,20,30,40,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,0,0,0,0,0,0,0,0,
         0,40,0,0,0,0,0,0,0)

#plot(slike[[17]])

Y17 <- c(100,100,100,100,10,0,10,100,100,
         100,100,100,100,10,10,20,10,10,
         100,100,100,100,100,100,10,0,10,
         10,10,10,100,100,100,10,10,10,
         20,0,10,100,100,100,10,10,10,
         0,40,30,10,100,100,10,0,0,
         20,0,0,30,30,20,20,10,10,
         10,20,30,0,0,0,10,100,100,
         100,100,10,20,30,20,10,0,0)
#plot(slike[[18]])
Y18 <- c(100,100,100,100,100,100,10,10,10,
        10,10,100,100,100,10,20,-100,20,
        -100,10,100,100,100,20,-100,40,-100,
        10,10,100,100,100,20,-100,30,10,
        100,10,10,10,100,10,10,10,100,
        100,10,-100,10,100,100,100,100,100,
        100,10,10,10,100,10,10,10,100,
        10,10,20,10,10,10,-100,20,10,
        10,-100,20,-100,10,10,10,20,-100)  


#spajam ih sve u jedan vektor koji će predstavljati kategoričku promenljivu
Y=c(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,Y9,Y10,Y11,Y12,Y13,Y14,Y15,Y16,Y17,Y18)

#pravim prediktor u koliko piksela je najveća vrednost plave

predictor<-function(im,vrste,kolone){
  
  X=c()
  l=1
 
   for(j in 1:(length(vrste)/2)){
    
     for(i in 1:(length(kolone)/2)){
      s=array(im[vrste[2*j-1]:vrste[2*j],kolone[2*i-1]:kolone[2*i],1,1:3],
              c(vrste[2*j]-vrste[2*j-1]+1,kolone[2*i]-kolone[2*i-1]+1,1,3))
      nova=cimg(s)
      red=as.vector(nova[,,1,1])
      green=as.vector(nova[,,1,2])
      blue=as.vector(nova[,,1,3])
      brojac=mean(blue/(red+green+blue), na.rm=TRUE)
      X[l]=brojac
      l=l+1
    }}
  return(X)
    
}

# Disperzija polja u crno-belom spektru

predictor1 <- function(slika,vrste,kolone){

  X <- c()
  l <- 1
  # r širina odsecanja
  r <- ceiling((vrste[2]-vrste[1])/16)
  for(j in 1:9){
    for(i in 1:9){
      s <-array(slika[(vrste[2*j-1]+r):(vrste[2*j]-r), (kolone[2*i-1]+r):(kolone[2*i]-r), 1, 1:3],
                c(vrste[2*j]-vrste[2*j-1]-2*r+1,kolone[2*i]-kolone[2*i-1]-2*r+1, 1, 3))
      X[l] <- var(as.vector(grayscale(cimg(s))[,,,1]))
      l <-l+1
    }
  }
  return(X)
}

#prediktor srednja vrednost zelene+crvene boje 
predictor2 <- function(slika, vrste, kolone)
{
  prosek_zbira <- c()
  
  l <- 1
  for(j in 1:9)
  {
    for(i in 1:9)
    {
      #izdvojimo polje
      s <- array(slika[vrste[2*j-1]:vrste[2*j], kolone[2*i-1]:kolone[2*i], 1, 1:3],
                 c(vrste[2*j]-vrste[2*j-1]+1,kolone[2*i]-kolone[2*i-1]+1, 1, 3))
      nova <- cimg(s)
      crvena <- as.vector(nova[ , , 1, 1])
      zelena <- as.vector(nova[ , , 1, 2])
      plava <- as.vector(nova[ , , 1, 3])
      
      brojac_zbira <- mean((crvena+zelena)/(crvena+zelena+plava),na.rm = TRUE)
      prosek_zbira[l] <-brojac_zbira
      
      l=l+1
    }
  }
  
  return(prosek_zbira)
}



#za svaku sliku, pavimo odgovarajuće prediktore


X <-c()
X1 <-c()
X2 <-c()
for(i in 1: 18){
  
  slika=rm.alpha(slike[[i]]) #hoćemo da slike budu u RGB spektru
  slika=resize(slika,751,758,1,3)#podešavamo da sve slike budu istih dimenzija
  q11 <-slika [ , , 1, 1]
  q12 <-slika [ , , 1, 2]
  q13 <-slika [ , , 1, 3]
  M <-pmax(q11, q12, q13) #pmax dodeljuje maksimalnu vrednost po elementima 
  
  m_vrsta <-granice(M,0.2,0.3)
  m_kolona <-granice(M,0.2, 0.3,FALSE)
  
  v <-sredjivanje(m_vrsta)
  k <-sredjivanje(m_kolona)
  
  # za svaku sliku tražimo prediktore, i spajamo sve prediktore u jedan
  X <-c(X, predictor(slika, v, k))
  X1 <-c(X1,predictor1(slika,v,k)) 
  X2<-c(X2,predictor2(slika,v,k))
  }
#sada pravimo kategoričku promenljivu za slike iz kontrolnog skupa


setwd("C:\\Users\\Radulovic\\Desktop\\cetvrti_zadatak_kontrolni")
files <- list.files(path=getwd(),all.files=T, full.names=F, no.. = T)
slike_k <- lapply(files, load.image) 

#plot(slike_k[[1]])
Yk1 <- c(10,10,20,-100,10,100,100,100,100,
            20,-100,30,10,10,100,100,100,100,
            20,-100,20,100,100,10,10,10,100,
            0,20,10,100,100,10,-100,20,10,
            -100,10,100,10,10,20,20,0,0,
            10,10,100,10,-100,10,10,0,0,
            100,100,100,10,10,10,10,20,0,
            100,100,100,100,10,10,20,0,0,
            100,100,100,100,10,0,20,0,0)
#plot(slike_k[[2]])
Yk2 <- c(0,0,10,0,0,-100,10,100,100,
            10,10,10,10,20,20,10,10,10,
            100,100,100,100,100,100,100,10,0,
            100,100,100,100,100,100,100,10,10,
            100,100,100,100,100,10,10,20,10,
            100,100,100,100,100,20,-100,30,-100,
            100,100,100,10,10,40,-100,40,10,
            100,100,100,20,-100,40,-100,20,100,
            100,100,100,20,-100,30,10,10,100)

#spajamo ih u jedan vektor
Yk <- c(Yk1, Yk2)

#pravimo prediktore za kontrolni skup

Xk=c()
X1k=c()
X2k<-c()
for(i in 1:length(slike_k)) {
  slika <- rm.alpha(slike_k[[i]]) 
  
  q11 <-slika [ , , 1, 1]
  q12 <-slika [ , , 1, 2]
  q13 <-slika [ , , 1, 3]
  M <- pmax(q11, q12, q13) 
  
  m_vrsta2 <- granice(M,0.2, 0.3)
  m_kolona2<-granice(M,0.2, 0.3,FALSE)
  
  v2 <-sredjivanje(m_vrsta2)
  k2 <-sredjivanje(m_kolona2)
  
  Xk <- c(Xk, predictor(slika, v2, k2)) #dodajemo vrednosti prediktora za ovu sliku u vektore
  X1k <-c(X1k, predictor1(slika,v2,k2))
  X2k <-c(X2k,predictor2(slika,v2,k2))
  }

#sada pravimo modele

#qda
model.qda<-qda(Y~X1+X2)
model.qda2<-qda(Y~X+X1)
model.qda.X<-qda(Y~X)
model.qda.X2<-qda(Y~X2)
model.qda.X1<-qda(Y~X1)

summary(model.qda)

model.qda.pred<-predict(model.qda,newdata = data.frame(X1=X1k,X2=X2k))
table(model.qda.pred$class, Yk)
mean(model.qda.pred$class == Yk)
#pogađa oko 98%

model.qda2.pred<-predict(model.qda2,newdata = data.frame(X=Xk,X1=X1k))
table(model.qda2.pred$class, Yk)
mean(model.qda2.pred$class == Yk)
#pogađa oko 98%

model.qdaX.pred<-predict(model.qda.X,newdata = data.frame(X=Xk))
table(model.qdaX.pred$class, Yk)
mean(model.qdaX.pred$class == Yk)
#pogađa oko 94%

model.qdaX2.pred<-predict(model.qda.X2,newdata = data.frame(X2=X2k))
table(model.qdaX2.pred$class, Yk)
mean(model.qdaX2.pred$class == Yk)
#pogađa oko 94%
#X i X2k imaju istu verovatnocu pogađanja, sto je logično 
#gledajući kako su napravljeni

model.qdaX1.pred<-predict(model.qda.X1,newdata = data.frame(X1=X1k))
table(model.qdaX1.pred$class, Yk)
mean(model.qdaX1.pred$class == Yk)

#lda

model.lda<-lda(Y~X1+X2)
summary(model.lda)
model.lda.pred=predict(model.lda,newdata = data.frame(X1=X1k,X2=X2k))
table(model.lda.pred$class,Yk)
mean(model.lda.pred$class==Yk)
#96 posto pogađa

model.lda2<-lda(Y~X+X1)
summary(model.lda2)
model.lda.pred2=predict(model.lda2,newdata = data.frame(X=Xk,X1=X1k))
table(model.lda.pred2$class,Yk)
mean(model.lda.pred2$class==Yk)
#96% 

model.ldaX2<-lda(Y~X2)
summary(model.ldaX2)
model.lda.predX2=predict(model.ldaX2,newdata = data.frame(X2=X2k))
table(model.lda.predX2$class,Yk)
mean(model.lda.predX2$class==Yk)
#91 posto pogađa

model.ldaX<-lda(Y~X)
summary(model.ldaX)
model.lda.predX=predict(model.ldaX,newdata = data.frame(X=Xk))
table(model.lda.predX$class,Yk)
mean(model.lda.predX$class==Yk)
#91#

model.ldaX1<-lda(Y~X1)
summary(model.ldaX1)
model.lda.predX1=predict(model.ldaX1,newdata = data.frame(X1=X1k))
table(model.lda.predX1$class,Yk)
mean(model.lda.predX1$class==Yk)
#pogađa 55%



#multinomni

model.multinomX<-multinom(Y~X)
summary(model.multinomX)
model.mulpredX <- predict(model.multinomX, newdata = data.frame(X=Xk))
table(model.mulpredX, Yk)
mean(model.mulpredX == Yk)
#pogađa oko 94%

model.multinom<-multinom(Y~X+X1)
summary(model.multinom)
model.mulpred <- predict(model.multinom, newdata = data.frame(X=Xk,X1=X1k))
table(model.mulpred, Yk)
mean(model.mulpred == Yk)
#pogađa oko 98%

model.multinom2<-multinom(Y~X1+X2)
summary(model.multinom2)
model.mulpred2 <- predict(model.multinom2, newdata = data.frame(X1=X1k,X2=X2k))
table(model.mulpred2, Yk)
mean(model.mulpred2 == Yk)
#pogađa oko 98%


model.multinomX1<-multinom(Y~X1)
summary(model.multinomX1)
model.mulpredX1 <- predict(model.multinomX1, newdata = data.frame(X1=X1k))
table(model.mulpredX1, Yk)
mean(model.mulpredX1 == Yk)
#pogađa oko 80%


model.multinomX2<-multinom(Y~X2)
summary(model.multinomX2)
model.mulpredX2 <- predict(model.multinomX2, newdata = data.frame(X2=X2k))
table(model.mulpredX2, Yk)
mean(model.mulpredX2 == Yk)
#pogađa oko 94%

