library(Rsymphony)
library(readxl)
#mat=read_excel("C:/Users/USER/Documents/DFBA/RED/Matriz_SOA_1135.xlsx",sheet=1,col_names=F,skip = 1)
mat=read_excel("C:/Users/USER/Documents/DFBA/RED/Matriz_PCA_Juli_SOA.xlsx",sheet=1,col_names=F,skip = 1)
lower=read_excel("C:/Users/USER/Documents/DFBA/RED/loweryupperbounds.xlsx",sheet=1,col_names=F,skip = 1)
upper=read_excel("C:/Users/USER/Documents/DFBA/RED/loweryupperbounds.xlsx",sheet=2,col_names=F,skip = 1)
vi=55.5
MK=(-10*vi)/(0.015+vi)
con=c(vi)
n=50
flux1=c()
flux2=c()
h=19/n
for (i in 1:n){
  lower[1040]=MK
  upper[1040]=MK
  lower[1080]=-3
  upper[1080]=-3
#Ecuacion objetivo
  objfun=914
#CONTRUCCION DE LA FUNCION OBJETIVO
  obj=c(rep(0,objfun-1),rep(1,1),rep(0,ncol(mat)-objfun))
#CONSTRUCCION MATRIZ ESTEQUIOMETRICA
  mat=data.matrix(mat, rownames.force = NA)
#CONSTRUCCION DE dir
  dir=c('==')
  i1=1
  while (i1<(nrow(mat))){
    dir=c(dir,'==')
    i1=i1+1
}
#dir=c(dir,'>=')
#print(dir)
#length(dir)
#CONSTRUCCION DE rhs
  rhs=numeric(nrow(mat))
#rhs=c(rhs,MK)
#CONSTRUCCION DE BOUNDS
####
  lower=as.numeric(lower)
  vallower=lower
####
  upper=as.numeric(upper)
  valupper=upper
#-----------------------------------
  ind=c()
  for (i in 1:ncol(mat)){
    ind=c(ind,i)
} 
#------------------------------------ 
  bounds=list(lower=list(ind=ind,val=vallower),upper=list(ind=ind,val=valupper))
#bounds
#CONSTRUCCION DE types
  types=c("C")
  i2=1
  while (i2<ncol(mat)){
    types=c(types,"C")
    i2=i2+1
}
#length(types)
#print(types)
#MAXIMIZACION
  max=TRUE
  options(max.print = ncol(mat)+1)
  S=Rsymphony_solve_LP(obj, mat, dir, rhs, bounds , types ,max , verbosity = -2, time_limit = -1,node_limit = -1, gap_limit = -1, first_feasible = FALSE,write_lp = FALSE, write_mps = FALSE)
#Calculos de concentraciones  
  k=S[1]
  f1=k$solution[1040]
  f2=k$solution[1134]
  vi=vi+f1*h*f2
  MK=(-10*vi)/(0.015+vi)
  flux1=c(flux1,f1)
  flux2=c(flux2,f2)
  con=c(con,vi)
  
}
