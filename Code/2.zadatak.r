#install.packages("lava")
library(lava)

napravi_potez<-function(N,matrica,id_prvi){
  vektorizacija=as.vector(matrica)#matricu pretvaramo u vektor 
  M=matrix(rep(0,9),ncol=3)#matrica u kojoj ćemo čuvati verovatnoće pozicija
  
  moguce_pozicije=which(vektorizacija==' ')#pozicije koje su još neotvorene 
  
  #preostali broj x i o koji mogu da se iskoriste 
  preostalih_x=5-sum(vektorizacija=='X')
  preostalih_o=4-sum(vektorizacija=='O')
  
  #ako je na redu prvi igrač, on će iskoristiti jedan X, pa smanjujemo br preostalih ikseva
  if(id_prvi==1){
    preostali_xo=c(rep('X',(preostalih_x)-1),rep('O',preostalih_o))
  }
  
  #analogno za drugog igrača i preostale o simbole
  else{
    preostali_xo=c(rep('X',preostalih_x),rep('O',(preostalih_o-1)))
  }
  
  #prolazimo kroz sve moguće pozicije for petljom 
  for(i in moguce_pozicije){
    moguce=moguce_pozicije[moguce_pozicije!=i]
    
    for(k in 1:N){
      if(length(moguce)>1){
        x=sample(moguce,length(moguce),replace=F)}
      else
        x=moguce
      
      
      y=preostali_xo
      
      for(l in 1:length(moguce)){
        vektorizacija[x[l]]=y[l]
        
      }
      vektorizacija[i]=ifelse(id_prvi,'X','O')
      matr=vektorizacija
      dim(matr)=c(3,3)
      
      #proveravamo da li je prvi igrač na potezu i na tabli imamo 3 x simbola u vrsti, koloni ili na nekoj od dijagonala
      if(id_prvi==1 & (all(matr[1,]=='X') | all(matr[2,]=='X') | all(matr[3,]=='X') | all(matr[,1]=='X') | all(matr[,2]=='X') | all(matr[,3]=='X') )){
        M[i]=M[i]+1
      }
      else if(id_prvi==1 & (all(diag(matr)=='X') | all(revdiag(matr)=='X'))){
        M[i]=M[i]+1
      }
      #analogno za drugog igrača
      else if(id_prvi==0 & (all(matr[1,]=='O') | all(matr[2,]=='O') | all(matr[3,]=='O') | all(matr[,1]=='O') | all(matr[,2]=='O') | all(matr[,3]=='O') )){
        M[i]=M[i]+1
      }
      else if(id_prvi==0 & (all(diag(matr)=='O') | all(revdiag(matr)=='O'))){
        M[i]=M[i]+1}
      
      else{
        
      }  }
  }
  
  M=M/N
  return(M)}#vraćamo matricu verovatnoća

pusti_simulaciju<-function(){
matrica=matrix(rep(" ",9),ncol=3)

#najviše može biti 9 koraka
for(i in 1:9){
  
  message("pocinje ",i, ".  potez")
  id_prvi=i%%2
  m=napravi_potez(N=i*3000,matrica=matrica,id_prvi)
  verovatnoca=m[which.max(m)]
  pozicija=which(verovatnoca==m)
  matrica[pozicija[1]]=ifelse(id_prvi,'X','O')
  print(matrica)
  
  #igra je gotova kada je neki od igrača popunio 3 polja u vrsti,koloni, ili jednoj od dijagonala
  
  if((id_prvi==1) & (all(matrica[1,]=='X') | all(matrica[2,]=='X') | all(matrica[3,]=='X') | all(matrica[,1]=='X') | all(matrica[,2]=='X') | all(matrica[,3]=='X') | all(diag(matrica)=='X') | all(revdiag(matrica)=='X') )){
    
  
  print("Pobedio je 1. igrac")
  break}
  else if((id_prvi==0) & (all(matrica[1,]=='O') | all(matrica[2,]=='O') | all(matrica[3,]=='O') | all(matrica[,1]=='O') | all(matrica[,2]=='O') | all(matrica[,3]=='O') | all(diag(matrica)=='O') | all(revdiag(matrica)=='O') )){
    print("pobedio je 2. igrac")
    break}
    
    }
}
  
  
pusti_simulaciju()
