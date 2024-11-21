# Análisis de datos                   
# ANOVA, Prueba de medias, Multivariada
# Tesis Díaz Luján, 2024
#----------------------------------------
#Directorio
setwd("~/Analisis_tesis")

#Cargar datos de registros morfología de plantas
data <- read.csv("~/Analisis_tesis/ANOVA.csv", sep=";")
data$id <- as.factor(data$id) # Para asegurar que la colunma id sea factor
dim(data)   # Para averiguar si cargo correctamente los datos (número de columnas y líneas)
str(data)  # Verificar estructura de los datos

summary(data) #resumen estadístico

#Prueba de normalidad

NH<- data$NH # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_NH<- shapiro.test(NH) 
print(shap_test_NH)

LR<- data$LR # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_LR<- shapiro.test(LR) 
print(shap_test_LR)

DR<- data$DR # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_DR<- shapiro.test(DR) 
print(shap_test_DR)

PHP<- data$PHP # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_PHP<- shapiro.test(PHP) 
print(shap_test_PHP)

PFR<- data$PFR # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_PFR<- shapiro.test(PFR) 
print(shap_test_PFR)

LT<- data$LT # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_LT<- shapiro.test(LT) 
print(shap_test_LT)

DT<- data$DT # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_DT<- shapiro.test(DT) 
print(shap_test_DT)

PHA<- data$PHA # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_PHA<- shapiro.test(PHA) 
print(shap_test_PHA)

PSA<- data$PSA # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_PSA<- shapiro.test(PSA) 
print(shap_test_PSA)

TN<- data$TN # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_TN<- shapiro.test(TN) 
print(shap_test_TN)

#Gráfico de normalidad

qqnorm(LR, main = "Gráfico Q-Q de Largo de raíz") 
qqline(LR, col = "red", lwd = 2)

qqnorm(DT, main = "Gráfico Q-Q de Diámetro de tallo") 
qqline(DT, col = "red", lwd = 2)

qqnorm(DR, main = "Gráfico Q-Q de Diámetro de raíz principal") 
qqline(DR, col = "red", lwd = 2)

# ANOVA de LR, DT, DR

anova_LR <- aov(LR ~ id, data = data)
summary(anova_LR)

anova_DT <- aov(DT ~ id, data = data)
summary(anova_DT)

anova_DR <- aov(DR ~ id, data = data)
summary(anova_DR)

# Prueba de medias Tukey

# Instalar los paquetes 
install.packages("ggplot2")
install.packages("agricolae") 

# Cargar los paquetes 
library(ggplot2)
library(agricolae)

tukey_LR <- HSD.test(anova_LR, "id", group = TRUE)
print(tukey_LR$groups)
tukey_LR_data <- as.data.frame(tukey_LR$groups)

ggplot(tukey_LR_data, aes(x = rownames(tukey_LR_data),
                          y = LR, fill = groups)) + geom_bar(stat = "identity", 
                          position = position_dodge()) + theme_minimal() + labs(x = "Variedad",
                          y = "Media (LR)", fill = "Grupos") + ggtitle("PRUEBA DE MEDIAS", subtitle = "Largo de raíz" )
                          + theme(axis.text.x = element_text(angle = 45, hjust = 1))

tukey_DR <- HSD.test(anova_DR, "id", group = TRUE)
print(tukey_DR$groups)
tukey_DR_data <- as.data.frame(tukey_DR$groups)

ggplot(tukey_DR_data, aes(x = rownames(tukey_DR_data),
                          y = DR, fill = groups)) + geom_bar(stat = "identity", 
                          position = position_dodge()) + theme_minimal() + labs(x = "Variedad",
                          y = "Media (DR)", fill = "Grupos") + ggtitle("PRUEBA DE MEDIAS", subtitle = "Diámetro de raíz principal" )
                          + theme(axis.text.x = element_text(angle = 45, hjust = 1))

tukey_DT <- HSD.test(anova_DT, "id", group = TRUE)
print(tukey_DT$groups)
tukey_DT_data <- as.data.frame(tukey_DT$groups)

ggplot(tukey_DT_data, aes(x = rownames(tukey_DT_data),
                          y = DT, fill = groups)) + geom_bar(stat = "identity", 
                          position = position_dodge()) + theme_minimal() + labs(x = "Variedad",
                          y = "Media (DT)", fill = "Grupos") + ggtitle("PRUEBA DE MEDIAS", subtitle = "Diámetro de tallo" )
                          + theme(axis.text.x = element_text(angle = 45, hjust = 1))
#____________________________________________________________________
#Tratamiento vs control

DR_C <- read.csv("~/Analisis_tesis/DR.csv")
NH_C <- read.csv("~/Analisis_tesis/NH.csv")
LR_C <- read.csv("~/Analisis_tesis/LR.csv")
DT_C <- read.csv("~/Analisis_tesis/DT.csv")
PHP_C <- read.csv("~/Analisis_tesis/PHP.csv")
PFR_C <- read.csv("~/Analisis_tesis/PFR.csv")
LT_C <- read.csv("~/Analisis_tesis/LT.csv")
PFA_C <- read.csv("~/Analisis_tesis/PFA.csv")
PSA_C <- read.csv("~/Analisis_tesis/PSA.csv")

#Normalidad

#NO
DR_norm1<- DR_C$Tratamiento # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_DR_norm1<- shapiro.test(DR_norm1) 
print(shap_test_DR_norm1)
#-----
#SI
NH_norm1<- NH_C$Tratamiento # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_NH_norm1<- shapiro.test(NH_norm1) 
print(shap_test_NH_norm1)

NH_norm2<- NH_C$Control # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_NH_norm2<- shapiro.test(NH_norm2) 
print(shap_test_NH_norm2)
#-----
#SI
LR_norm1<- LR_C$Tratamiento # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_LR_norm1<- shapiro.test(LR_norm1) 
print(shap_test_LR_norm1)

LR_norm2<- LR_C$Control # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_LR_norm2<- shapiro.test(LR_norm2) 
print(shap_test_LR_norm2)

#-----
#SI
DT_norm1<- DT_C$Tratamiento # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_DT_norm1<- shapiro.test(DT_norm1) 
print(shap_test_DT_norm1)

DT_norm2<- DT_C$Control # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_DT_norm2<- shapiro.test(DT_norm2) 
print(shap_test_DT_norm2)

#---
#SI
PHP_norm1<- PHP_C$Tratamiento# Sustituir 'variable' por el nombre de la columna a analizar
shap_test_PHP_norm1<- shapiro.test(PHP_norm1) 
print(shap_test_PHP_norm1)

#NO en CONTROL
PHP_norm2<- PHP_C$Control# Sustituir 'variable' por el nombre de la columna a analizar
shap_test_PHP_norm2<- shapiro.test(PHP_norm2) 
print(shap_test_PHP_norm2)

#----

#SI
PFR_norm1<- PFR_C$Tratamiento # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_PFR_norm1<- shapiro.test(PFR_norm1) 
print(shap_test_PFR_norm1)

PFR_norm2<- PFR_C$Control # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_PFR_norm2<- shapiro.test(PFR_norm2) 
print(shap_test_PFR_norm2)

#-----
#NO
LT_norm1<- LT_C$Tratamiento # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_LT_norm1<- shapiro.test(LT_norm1) 
print(shap_test_LT_norm1)

#---
#SI
PFA_norm1<- PFA_C$Tratamiento # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_PFA_norm1<- shapiro.test(PFA_norm1) 
print(shap_test_PFA_norm1)

PFA_norm2<- PFA_C$Control # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_PFA_norm2<- shapiro.test(PFA_norm2) 
print(shap_test_PFA_norm2)

#-----
#SI
PSA_norm1<- PSA_C$Tratamiento # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_PSA_norm1<- shapiro.test(PSA_norm1) 
print(shap_test_PSA_norm1)

#NO EN CONTROL
PSA_norm2<- PSA_C$Control # Sustituir 'variable' por el nombre de la columna a analizar
shap_test_PSA_norm2<- shapiro.test(PSA_norm2) 
print(shap_test_PSA_norm2)

#---------------------------------------------------------------------

# Realizar la prueba t para muestras pareadas normales

#t_test_DR <- t.test(DR_C$Tratamiento, DR_C$Control, paired = TRUE)
#print(t_test_DR)

#t_test_PHP <- t.test(PHP_C$Tratamiento, PHP_C$Control, paired = TRUE)
#print(t_test_PHP)

#t_test_LT <- t.test(LT_C$Tratamiento, LT_C$Control, paired = TRUE)
#print(t_test_LT)



t_test_NH <- t.test(NH_C$Tratamiento, NH_C$Control, paired = TRUE)
print(t_test_NH)

t_test_LR <- t.test(LR_C$Tratamiento, LR_C$Control, paired = TRUE)
print(t_test_LR)

t_test_DT <- t.test(DT_C$Tratamiento, DT_C$Control, paired = TRUE)
print(t_test_DT)

t_test_PFR <- t.test(PFR_C$Tratamiento, PFR_C$Control, paired = TRUE)
print(t_test_PFR)

t_test_PFA <- t.test(PFA_C$Tratamiento, PFA_C$Control, paired = TRUE)
print(t_test_PFA)

####################
#Multivariada

install.packages("vegan")
install.packages("cluster")
install.packages("ape")
install.packages("permute")
install.packages("factoextra")

library (vegan) 
library (cluster)
library (ape)
library(permute)
library (factoextra)


multivar <- read.table(file.choose(), header = TRUE, sep = ",", row.names = 1)
class(multivar)
str(multivar)

#Matriz Gower
gw<-vegdist(multivar, "gower") #essa função não requer a determinação prévia da natureza das variáveis (se são contínuas, categóricas, nominais, binárias)

gower_mat <- as.matrix(gw) #para transformar em formato matriz

write.table(gower_mat,"Gower.csv", sep=",", col.names = T, row.names = F) #salvar matriz de distância de Gower

ward<-hclust(gw,method='ward.D2') #agrupamento pelo método de Ward

cor(cophenetic(ward), gw) #para calcular o coeficiente cofenético maior que 0.7 é considerado bom, indica que o agrupamento é consistente

plot(ward) #para visualizar o dendrograma

(pto.corte <- mean(ward $height) + 1.25*sd(ward $height)) #para estabelecer o ponto de corte pelo método de Mojena

grupos<-cutree(ward,h=pto.corte)  #para incorporar o ponto de porte ao dendrograma

plot(ward);rect.hclust(ward, h=pto.corte, border="red") #para visualizar o dendrograma com o ponto de corte

#Coordenadas principais (PCoA) dos grupos estabelecidos pelo Método Ward com Mojena #análise complementar
pcoa <- pcoa(gw) #para calcular as PCoAs
plot(pcoa$vectors[,1:2], type="n", xlim=c(-.4,.4), ylim=c(-.4,.4), xlab="PCoA1", ylab="PCoA2") #para visualizar a PCoA – passo 1
text(pcoa$vectors[,1],pcoa$vectors[,2],rownames(pcoa$vectors) ,cex = 0.8,col=grupos) #para visualizar a PCoA – passo 2
abline(h=0,v=0,lty=2,col="gray26",lwd=1) #para visualizar a PCoA – passo 3 


