shell("cls")
library("dplyr")
SampleProGres_1_ <- read_dta("SampleProGres(1).dta")
View(SampleProGres_1_)

df = SampleProGres_1_

##########################################################

# PUNTO 1 - Familias con hijo

#Sacando Columna Esposo
esposo = vector()

for(i in 1:nrow(df))  # Recorre cada fila
{
  if(df[i,]$relPA== "HUS" & df[i,]$age >= 18 & df[i,]$disable == 0 ){
    esposo = append(esposo,1)
  }
  else{
    esposo = append(esposo,0)
  }
}

df = cbind(df,esposo) # Agrega columna esposo

#Sacando Columna esposa
esposa = vector()

for(i in 1:nrow(df))  # Recorre cada fila
{
  if(df[i,]$relPA== "WIF" & df[i,]$age >= 18 & df[i,]$disable == 0){
    esposa = append(esposa,1)
  }
  else{
    esposa = append(esposa,0)
  }
}

df = cbind(df,esposa)

#Sacando Columna hijos

children = vector()

for(i in 1:nrow(df))  # Recorre cada fila
{
  if( df[i,]$relPA== "SON" | df[i,]$relPA== "DAU" | df[i,]$relPA=="SCM" | df[i,]$relPA=="SCF" & df[i,]$age <= 18){
    children = append(children,1)
  }
  else{
    children= append(children,0)
  }
}

df = cbind(df,children)

#Sacando Columna abuelos
abuelo = vector()

for(i in 1:nrow(df))  # Recorre cada fila
{
  if(df[i,]$relPA== "GPM" & df[i,]$elderly==0 & df[i,]$disable == 0){
    abuelo= append(abuelo,1)
  }
  else{
    abuelo = append(abuelo,0)
  }
}

df = cbind(df,abuelo)

#Sacando Columna abuelas
abuela = vector()

for(i in 1:nrow(df))  # Recorre cada fila
{
  if(df[i,]$relPA== "GPM" & df[i,]$elderly==0 & df[i,]$disable == 0){
    abuela = append(abuela,1)
  }
  else{
    abuela = append(abuela,0)
  }
}

df = cbind(df,abuela)

#Sacando Columna ex-esposas

exposas = vector()

for(i in 1:nrow(df))  # Recorre cada fila
{
  if(df[i,]$relPA== "EXF" & df[i,]$age >= 18){
    exposas = append(exposas,1)
  }
  else{
    exposas = append(exposas,0)
  }
}

df = cbind(df,exposas)

#Sacando Columna ex-esposos

exposos = vector()

for(i in 1:nrow(df))  # Recorre cada fila
{
  if(df[i,]$relPA== "EXM" & df[i,]$age >= 18){
    exposos = append(exposos,1)
  }
  else{
    exposos = append(exposos,0)
  }
}

df = cbind(df,exposos)

df1 <- df %>% group_by(CaseID) # Aca agrupo por familia

# Agrupamiento familias - nueva base de datos

familias = df1 %>% summarise(
  Expenditure = mean(Exp_Rent + Exp_Utilities+Exp_Food+Exp_Water+Exp_Treatment+Exp_Education+Exp_Transportation+Exp_InfantNeeds+Exp_BasicHH+Exp_BasicHygiene+Exp_DebtRepayment+Exp_Telecommunications+Exp_Telecommunications+Exp_Other),
  esposos = sum(esposo),
  esposas = sum(esposa),
  hijos = sum(children),
  abuelo = sum(abuelo),
  abuela = sum(abuela),
  exposos = sum(exposos),
  exposas = sum(exposas),
  Education = mean(Exp_Education)
)

familias1=familias

View(familias)

# creando dummi familiar
# Solo tome los casos nombrados en el primer punto, pudo haberse extendido más

familia_hijo = vector()

for(i in 1:nrow(familias))  # Recorre cada fila
{
  if(familias[i,]$esposas==1 | familias[i,]$esposos ==1 & familias[i,]$hijos>0) # Caso esposos con hijos
  {
    familia_hijo = append(familia_hijo,1)
  }
  else if(familias[i,]$esposas==1 & familias[i,]$esposos ==1 & familias[i,]$hijos>0) #Caso hijo
  {
    familia_hijo = append(familia_hijo,1)
  }
  else if(familias[i,]$exposas==1 | familias[i,]$exposos ==1 & familias[i,]$hijos>0) #Caso ex-esposos/sas
  {
    familia_hijo = append(familia_hijo,1)
  }
  else
  {
    familia_hijo = append(familia_hijo,0)
  }
}

familias = cbind(familias,familia_hijo)

# PUNTO 1 - Familias con hijo


num_familias = summarise(familias,sum(familia_hijo))
print("el numero de parejas con hijos es")
print(num_familias)


###################################################################
# PUNTO 2 - Rate pobreza


#Agregando numero de personas por familia

familias = cbind(familias,  personas = count(df1,CaseID)[2]) 

#Agregando expenditure percapita

exp_pp = vector()

for(i in 1:nrow(familias))  # Recorre cada fila
{
  exp_pp = append(exp_pp,familias[i,]$Expenditure / familias[i,]$n)
}

familias = cbind(familias,exp_pp)

# Creando dummie si es pobre

poverty= vector()

for(i in 1:nrow(familias))  # Recorre cada fila
{
  if(familias[i,]$exp_pp <= 68)
  {
    poverty = append(poverty,1)
  }
  else
  {
    poverty = append(poverty,0)
    
  }
}

# PUNTO 2 - Rate pobreza

familias = cbind(familias,poverty)

porverty_rate = sum(familias$poverty)/nrow(familias)

print("indice de pobreza")
print(porverty_rate)

######################################################

#Creando participacion de educacon en el  total expenditure

participacion= vector()

for(i in 1:nrow(familias))  # Recorre cada fila
{
  if(familias[i,]$Expenditure == 0){
    participacion = append(participacion,0)
  }
  else{
    cosa = familias[i,]$Education/familias[i,]$Expenditure
    participacion = append(participacion,as.numeric(cosa))
  }
  #participacion = append(participacion,
}

familias = append(familias,as.data.frame(participacion))
familias = as.data.frame(familias)

#Generando log al cuadrado

log2 =  vector()

for(i in 1:nrow(familias))  # Recorre cada fila
{
  logo = log(familias[i,]$Expenditure+1)*log(familias[i,]$Expenditure+1)
  log2 = append(log2,logo)
}

familias = cbind(familias,log2)

#calculando la curva de engel y se le suma +1 al logaritmo para que no salga error(de los expenditure == 0)

#curva engel
engel_curve = lm(familias$participacion  ~ log(familias$Expenditure+1)+log2)
summary(engel_curve)

#Elasticidad

wi = mean(familias$participacion)
yi = mean(familias$Education)

elasticidad = 1+0.03/wi +2*(-0.004)*log(yi)/wi
print("la elasticidad evuluada en el punto promedio es")
print(elasticidad)

