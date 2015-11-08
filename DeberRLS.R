# DEBER 09/10/2015
# Ajustar un modelo de regresion lineal simple entre
# ln(pib) e ln(inflacion) del archivo data_rls_pib.xlsx
# use la funcion log() para extraer el log natural de una variable
#Jonathan Troncoso 

library(readxl)
datarls <- read_excel("data_rls_pib.xlsx", sheet = 1, col_names = TRUE, na = "")
View(datarls)
str(datarls)
colnames(datarls)
summary(datarls)

#variables
pib <- datarls[,"pib"]
infl <- datarls[,"inflacion"]

ln_pib<-log(pib)
ln_inflacion<-log(infl)

regresion.simple<-lm(ln_pib~ln_inflacion)
summary(regresion.simple)

#grafico
plot(x = ln_inflacion,y = ln_pib,main = "ln_pib vs ln_inflacion", pch=16, col="red")

# ANOVA
anovas <- aov(regresion.simple)
summary(anovas)

# Objeto regs
str(regresion.simple)
names(regresion.simple)

# Residuos
u_t <- regresion.simple$residuals

# Hipotesis sobre los errores
mean(u_t)
hist(u_t)



# Recta de regresion
library(ggplot2)
g <- ggplot(data = NULL, aes(x=ln_inflacion, y=ln_pib))
g + geom_point(size=3, color="red")+ geom_smooth(method="lm", color="blue")
