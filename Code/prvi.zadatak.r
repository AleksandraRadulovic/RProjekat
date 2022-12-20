library(imager) #učitavam paket imager

setwd("C:\\Users\\Radulovic\\Desktop\\RProjekat\\prvi_zadatak")
files <- list.files(path=getwd(),all.files=T, full.names=F, no.. = T) 
slike <- lapply(files, load.image)

#pošto imamo više slika, pravimo funkciju koju ćemo primeniti na svaku pojedinačno

funkcija=function(slika,b){ #b je kanal boje
  
  slika=renorm(slika)#Primenom funkcije “renorm()” trebalo bi da se dobije 
  #normalizovani oblike svake slike i svi objekti koju se drugacije zapisuju u R-u, 
  #ali izgledaju isto bi trebalo posle primene funkcije renorm() postanu (skoro) isti.
  matrica=slika[,,1,b]
  dimenzije_matrice=dim(matrica)
  I=(matrica>mean(matrica))  #ovo će nam biti indikator tačaka različitih od pozadine
  
  #postavljamo početne vrednosti za temena
  krajnja_leva_tacka=dimenzije_matrice
  krajnja_desna_tacka=c(0,0)
  krajnja_gornja_tacka=dimenzije_matrice
  krajnja_donja_tacka=c(0,0)
  
  #for petljom prolazimo kroz matricu da bismo našli temena 
  #prvo identifikujemo tačke različite od pozadine,
  #onda se kroz njih krećemo po širini i visini, tačka koja ima najmanju širinu je krajnja leva tačka,
  #ona sa najvećom širinom je krajnja desna,
  #krajnja gornja tačka ima najmanju vrednost promenljive visina
  #krajnja donja najveću vrednost te promenljive
  for(sirina in 1:dimenzije_matrice[1] ){
    
    for(visina in 1:dimenzije_matrice[2]){
     
       if(I[1,1]!=I[sirina,visina]){  
        
          if(krajnja_leva_tacka[1]>sirina) {krajnja_leva_tacka=c(sirina,visina)}
          
         
         if(krajnja_desna_tacka[1]<sirina) {krajnja_desna_tacka=c(sirina,visina)} 
      
         if(krajnja_gornja_tacka[2]>visina){krajnja_gornja_tacka=c(sirina,visina)}  
      
         if(krajnja_donja_tacka[2]<visina){krajnja_donja_tacka=c(sirina,visina)}  
        
      }
   }
}

  centar=(krajnja_gornja_tacka+krajnja_donja_tacka+krajnja_leva_tacka+krajnja_desna_tacka)/4
  
  #nalazimo ugao na način objašnjen u tekstu  
  xy=abs(krajnja_donja_tacka-krajnja_desna_tacka) 
  trazeni_ugao=atan2(xy[2],xy[1])*((180)/pi)
  return(c(trazeni_ugao,centar))}
  
#funkcija koja rotira sliku za dati ugao(imrotate()), i zatim prikazuje sliku:  
rotacija_za_dati_ugao<-function(slika,ugao,cx,cy){
  rotirana_slika=imrotate(slika,ugao,round(cx),round(cy))
  plot(rotirana_slika)
  return(rotirana_slika)
}

##pravimo folder u kome ćemo čuvati slike
dir.create(file.path(getwd(),"slike"))

#prelazimo na rad sa slikama:

## prva slika
par(mfrow=c(1,2))
slika1=slike[[1]]
plot(slika1)

# a) 
#traženi ugao rotacije:
funkcija(slika1,2)[1]

#b)rotiramo sliku za dati ugao, pa će biti paralelna sa x osom
rotirana_slika1=rotacija_za_dati_ugao(slika1,funkcija(slika1,2)[1],funkcija(slika1,2)[2],funkcija(slika1,2)[3])

#v)čuvamo sliku
save.image(rotirana_slika1,paste("slike\\",strsplit(files[1],".",fixed = T)[[1]][1],".png",sep=""))

## druga slika
slika2=slike[[2]]
plot(slika2)

#traženi ugao rotacije:
funkcija(slika2,2)[1]

#b)rotiramo sliku za dati ugao, pa će biti paralelna sa x osom
rotirana_slika2=rotacija_za_dati_ugao(slika2,funkcija(slika2,2)[1],funkcija(slika2,2)[2],funkcija(slika2,2)[3])

#v)cuvamo sliku
save.image(rotirana_slika2,paste("slike\\",strsplit(files[2],".",fixed = T)[[1]][1],".png",sep=""))


## treca slika
slika3=slike[[3]]
plot(slika3)

#traženi ugao rotacije:
funkcija(slika3,2)[1]

#b)rotiramo sliku za dati ugao, pa će biti paralelna sa x osom
rotirana_slika3=rotacija_za_dati_ugao(slika3,funkcija(slika3,2)[1],funkcija(slika3,2)[2],funkcija(slika3,2)[3])

#v)čuvamo sliku
save.image(rotirana_slika3,paste("slike\\",strsplit(files[3],".",fixed = T)[[1]][1],".png",sep=""))

## četvrta slika
slika4=slike[[4]]
plot(slika4)

#traženi ugao rotacije:
funkcija(slika4,2)[1]

#b)rotiramo sliku za dati ugao, pa će biti paralelna sa x osom
rotirana_slika4=rotacija_za_dati_ugao(slika4,funkcija(slika4,2)[1],funkcija(slika4,2)[2],funkcija(slika4,2)[3])

#v)čuvamo sliku
save.image(rotirana_slika4,paste("slike\\",strsplit(files[4],".",fixed = T)[[1]][1],".png",sep=""))


## peta slika
slika5=slike[[5]]
plot(slika5)

#traženi ugao rotacije:
funkcija(slika5,2)[1]

#b)rotiramo sliku za dati ugao, pa će biti paralelna sa x osom
rotirana_slika5=rotacija_za_dati_ugao(slika5,funkcija(slika5,2)[1],funkcija(slika5,2)[2],funkcija(slika5,2)[3])

#v)čuvamo sliku
save.image(rotirana_slika5,paste("slike\\",strsplit(files[5],".",fixed = T)[[1]][1],".png",sep=""))

## šesta slika
slika6=slike[[6]]
plot(slika6)

#traženi ugao rotacije:
funkcija(slika6,2)[1]

#b)rotiramo sliku za dati ugao, pa će biti paralelna sa x osom
rotirana_slika6=rotacija_za_dati_ugao(slika6,funkcija(slika6,2)[1],funkcija(slika6,2)[2],funkcija(slika6,2)[3])

#v)čuvamo sliku
save.image(rotirana_slika6,paste("slike\\",strsplit(files[6],".",fixed = T)[[1]][1],".png",sep=""))

## sedma slika
slika7=slike[[7]]
plot(slika7)

#traženi ugao rotacije:
funkcija(slika7,2)[1]

#b)rotiramo sliku za dati ugao, pa će biti paralelna sa x osom
rotirana_slika7=rotacija_za_dati_ugao(slika7,funkcija(slika7,2)[1],funkcija(slika7,2)[2],funkcija(slika7,2)[3])

#v)čuvamo sliku
save.image(rotirana_slika7,paste("slike\\",strsplit(files[7],".",fixed = T)[[1]][1],".png",sep=""))

