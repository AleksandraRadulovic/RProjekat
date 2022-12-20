#6.zadatak
#a)
#imamo prosledjen broj mina, proverićemo da li je on isti sa brojem mina na tabli
#ukoliko na tabli imamo neki broj, u okolnim poljima imamo tačno toliko mina
#ako imamo prazno polje, oko njega nema mina

prava_matrica<-function(matrica,dimenzija,broj_mina){
 
   P<-matrica
  #proširimo matricu da bi mogli od svakog polja da uzmemo sva okolna
  #i proverimo da li zadovoljavaju uslove
  #bitno je samo da uzmemo cifre koje se razlikuju od onih kojima kodiramo polja
  
  P<-cbind(rep(1,dimenzija),P,rep(1,dimenzija))
  P<-rbind(rep(1,(dimenzija+2)),P,rep(1,(dimenzija+2)))
  
  #proveravamo uslove samo ako je broj mina na tabli isti kao prosleđeni
  
  if((sum(matrica==-100))!=broj_mina){return(F)}
  
  else{
   
     #za svako polje pojedincacno, posmatramo okolna polja i proveravam uslove
    for(i in 2:(dimenzija+1)){
      for(j in 2:(dimenzija+1)){
        
        okolna_polja=c(P[i-1,j-1],P[i-1,j],P[i-1,j+1],P[i,j-1],P[i,j+1],P[i+1,j-1],P[i+1,j],P[i+1,j+1])
       
         #prazno polje:
        if(P[i,j]==100){
         
           #broj mina u okolini je nula
          if(sum(okolna_polja==-100)!=0)
           
             return(FALSE)}
        
        #posmatramo polja sa brojem
        #broj na polju == broj mina u okolini
        for(k in 1:8){
          if(P[i,j]==10*k){
            if(sum(okolna_polja==-100)!=k)
              return(FALSE)
            
          }
          
        }
        
      }
      
    }
    return(T)}}

#provera     
M=matrix(c(100,100,100,100,100,10,-100,20,-100,
           10,10,100,100,100,10,10,20,10,
           -100,10,100,100,100,100,100,100,100,
           20,30,20,10,100,100,10,10,10,
           20,-100,-100,10,100,100,20,-100,20,
           -100,30,20,20,20,20,30,-100,20,
           10,10,100,10,-100,-100,20,10,10,
           100,100,100,10,20,20,10,100,100,
           100,100,100,100,100,100,100,100,100)  ,ncol=9,nrow = 9,
         byrow = T)  

prava_matrica(M,9,10)  #vraća TRUE

### b)
#postavljamo mine, potom
#pošto matrica ne mora niti kvadratna, dimenzija je vektor koji sadrži broj vrsta i broj kolona
generator_table<-function(dimenzija,broj_mina){
  
  v=dimenzija[1]
  k=dimenzija[2]
  tabla<-matrix(rep(0,v*k),nrow=v,ncol=k,byrow=T)
  
  #slučajno biramo pozicije gde su mine, i postavljamo ih u matricu
  pozicije_mina=sample(1:(v*k),broj_mina,replace=F)
  
  for(i in 1:length(pozicije_mina)){
    
    tabla[pozicije_mina[i]]=-100
    
  }
  
  #sada popunjavamo ostatak matrice. Opet je proširujemo kao u delu pod a,da bi svaki element imao 8 okolnih polja,
  #da olakšamo prolazak kroz matricu i proveravanje uslova
  P<-tabla
  P<-cbind(rep(1,v),P,rep(1,v))
  P<-rbind(rep(1,k+2),P,rep(1,k+2))
  
  #u for petlji prolazimo kroz sva polja table
  
  for(i in 2 : (v+1)){
    for(j in 2:(k+1)){
      
      okolna_polja=c(P[i-1,j-1],P[i-1,j],P[i-1,j+1],P[i,j-1],P[i,j+1],P[i+1,j-1],P[i+1,j],P[i+1,j+1])
      #ako je u ovom polju mina, polje je već popunjeno, idemo na sledeće polje
      if(P[i,j]==-100){
        next()
      }
      #proveravamo broj mina u okolini, ako je 0, P[i,j] je prazno polje
      if(sum(okolna_polja==-100)==0) {
        P[i,j]=100 }
      
      else{
        s=sum(okolna_polja==-100)
        P[i,j]=10*s
        
      }
      
    }
    
    
  }
  
  return(P[2:(v+1),2:(k+1)])#skidamo vrste i kolone koje smo dodali i vraćamo matricu
}

#provera  
#t1=generator_table(c(9,10),10)  
#t1

### v)

sakrivanje_polja<-function(matrica,broj_polja){
  
  nova<-matrica
  
  #čuvamo sve pozicije da bi mogli da izdvojimo dozvoljene, i pozicije mina
  sve_pozicije=1:length(nova)  
  pozicije_mina=which(nova==-100)
  dozvoljene_pozicije=sve_pozicije[-pozicije_mina]
 
   #slučajno biramo broj polja koja nisu mine koja zatvaramo 
  zatvorena_polja=sample(dozvoljene_pozicije,broj_polja,replace=F)
  nova[zatvorena_polja]=0
  nova[pozicije_mina]=0
  
  return(nova)
  }

#provera
t1<-generator_table(c(4,4),5)
t1
t1=sakrivanje_polja(t1,3)
sum(t1==0)
#8


###g)

#prvo pravimo pomoćnu funkciju koja koja tablu minolovca koja nije skroz otvorena, ali su otvorene sve mine,
#popunjava do kraja

popuni_kompletno=function(matrica){
  
  dimenzija=length(matrica[1,])
  P<-matrica
  P<-cbind(rep(1,dimenzija),P,rep(1,dimenzija))
  P<-rbind(rep(1,(dimenzija+2)),P,rep(1,(dimenzija+2)))
  
  for(i in 2:(dimenzija+1)){
    
    for(j in 2:(dimenzija+1)){
     
       #popunjavamo samo zatvorena polja, da bismo mogli da proverimo da li je matrica koju dobijemo prava
      if(P[i,j]==0){
        okolna_polja=c(P[i-1,j-1],P[i-1,j],P[i-1,j+1],P[i,j-1],P[i,j+1],P[i+1,j-1],P[i+1,j],P[i+1,j+1])
        
        
        if(sum(okolna_polja==-100)==0){
          P[i,j]=100
          
        }
        else{
          P[i,j]=10*sum(okolna_polja==-100)
          
        }
        
      }}}
  return(P[2:(dimenzija+1),2:(dimenzija+1)])
}


#Neka je X broj već otvorenih polja na kojima su mine. Ideja je da na slučajan nacin, od preostalih polja izaberemo
#broj mina-X polja na koje smeštamo preostale mine. Zatim popunimo matricu do kraja funkcijom popuni_kompletno, a
#onda funkcijom prava_matrica proveravamo da li je tabla koju smo mi dobili prava tabla minolovca( to je tačno samo ako smo mine
#rasporedili samo tamo gde smeju da budu). Ukoliko je to tačno, povećavamo verovatnocu da se na poljima koja smo izabrali nalazi mina.
#uradimo simulaciju n puta, i na kraju vratimo pozicije sa najvećom,i najmanjom verovatnoćom da je na njima mina.
montekarlo_simulacija<-function(matrica,broj_mina,...){
  
  
  M<- matrica
  dimenzija=length(M[1,])
  preostale_mine=broj_mina-sum(M==-100)
  pozicije=which(M==0)
  verovatnoce_pozicija=rep(0,length(pozicije))
  
  # n biramo tako da bude dovoljno veliko za bilo koji broj mina i pozicija.
  #Zato možemo uzeti za n k*10, gde je k broj načina na koji mozemo rasporediti preostale mine 
  #na naše pozicije. To radimo jer imamo proizvoljne argumente, pa da budemo sigurni da smo simulaciju 
  #uradili dovoljno puta.
  n=(choose(length(pozicije),preostale_mine))*10
  
  for(i in 1:n){
    
    #slučajno raspoređujemo preostale mine na moguca polja
    s=sample(pozicije,preostale_mine,replace=F)
    M1=M
    M1[s]=-100
    popunjena=popuni_kompletno(M1)
    
    #proverićemo da li je matrica koju smo popunili prava
    if(prava_matrica(popunjena,dimenzija,broj_mina))
      
      for(j in s){
        verovatnoce_pozicija[which(pozicije==j)]=verovatnoce_pozicija[which(pozicije==j)]+1
        
      }
    
  }
  
  max_pozicija=pozicije[which.max(verovatnoce_pozicija)]
  min_pozicija=pozicije[which.min(verovatnoce_pozicija)]
  
  
  return(c(max_pozicija,min_pozicija))}


#provera
N<- sakrivanje_polja(M,5)
N[3]=-100
N[6]=-100
N

c=montekarlo_simulacija(N,10)
c
M1=matrix(c(100,0,0,0,
            0,0,0,0,
            0,0,0,0,
            0,0,0,1),nrow=4)
M1
c=montekarlo_simulacija(M1,3)
c
