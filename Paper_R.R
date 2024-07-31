library(tidyverse)
library(magrittr)
library(ade4)

datos_or <- read.csv("data")
datos <- datos_or

# Repetir las n veces que se aplico la primera dosis
out <- vector("list", length(datos$primera))
for(i in 1:length(datos$primera)){
  
  if(datos[i, 7] != 1){
      
    out[[i]] <- do.call("rbind",  replicate(datos[i, 7], datos[i, ], simplify = F))
  }
  
  
}

# Combinar los datos en un solo dataframe
dt2 <- do.call(rbind.data.frame, out)

# filtrar de los datos orignales unicamente los que tengas como primera = 1 y unir con los datos repetidos
out2 <- datos_or %>% 
  filter(primera == 1) %>% 
  rbind(., dt2) %>% 
  mutate(primera2 = rep(1, sum(datos_or$primera))) %>% 
  select(., -c(primera, segunda)) 


datos2 <- out2
datos2$nom_vacuna <- factor(datos2$nom_vacuna, levels = c("CoronaVac SINOVAC", 
                                                          "CHADOX1S RECOMBINANTE ASTRAZENECA",
                                                          "BNT162b2 PFIZER"),
                            labels = c("SINOVAC", "ASTRAZENECA", "PFIZER"))
datos2$nom_vacuna <- as.character(datos2$nom_vacuna)

datos2

# provincias y vacunas
names(datos2)
datos0 <- data.frame(table(datos2[, c("provincia", "nom_vacuna")]))
datos0
datos <- data.frame(datos0[, c(1, 2, 3)]) %>% 
  pivot_wider(names_from = "nom_vacuna", values_from = "Freq")

datos
acs <- dudi.coa(datos[, -1])
round(acs$cw, 2)
round(acs$lw, 2)
acs$li
max(acs$lw)
which.max(acs$lw[-19])

# Prueba chi-cuadrado de pearson
tabla_pro <- table(datos2[, c("provincia", "nom_vacuna")])
chisq.test(tabla_pro)


# Dispersion
COP_1 <- FactoMineR::CA(tabla_pro, graph = F)
print(COP_1)
COP_1$col$coord
round(COP_1$row$inertia, 3)

factoextra::get_eigenvalue(COP_1)
factoextra::fviz_screeplot(COP_1, addlabels = T)
factoextra::fviz_ca_biplot(COP_1, repel = T)


# grupos de edad y vacunas
datos2 <- out2
datos2$nom_vacuna <- factor(datos2$nom_vacuna, levels = c("CoronaVac SINOVAC", 
                                                          "CHADOX1S RECOMBINANTE ASTRAZENECA",
                                                          "BNT162b2 PFIZER"),
                            labels = c("SINOVAC", "ASTRAZENECA", "PFIZER"))
datos2$nom_vacuna <- as.character(datos2$nom_vacuna)
names(datos2)
datos0 <- data.frame(table(datos2[, c("gedad", "nom_vacuna")]))
datos <- data.frame(datos0) %>% 
  pivot_wider(names_from = "gedad", values_from = "Freq")

acs <- dudi.coa(datos[, -1])
acs$cw
acs$lw
acs

# Prueba chi-cuadrado de pearson
tabla_pro <- table(datos2[, c("gedad", "nom_vacuna")])
chisq.test(tabla_pro)
acs$li
acs$l1
acs$co
acs$c1
scatter(acs, method=1)
tabla_pro <- table(datos2[, c("gedad", "nom_vacuna")])
COP_1 <- FactoMineR::CA(tabla_pro, graph = T)
factoextra::fviz_ca_biplot(COP_1, repel = T)

# Dispersion
COP_1 <- FactoMineR::CA(tabla_pro, graph = F)
print(COP_1)
COP_1$col$coord
COP_1$row$inertia

factoextra::get_eigenvalue(COP_1)
factoextra::fviz_screeplot(COP_1, addlabels = T)
factoextra::fviz_ca_biplot(COP_1, repel = T)


# segunda dosis -----------------

# repetir el numero de veces de priemras dosis
out_seg <- vector("list", length(datos$segunda))
for(i in 1:length(datos$segunda)){
  
  if(datos[i, 8] != 1){
    #n <- do.call("rbind",  replicate(datos[i, 7], datos[i, ], simplify = F))  
    out_seg[[i]] <- do.call("rbind",  replicate(datos[i, 8], datos[i, ], simplify = F))
  }
  
  
}

out_seg

# combinaarlo en un solo data.frame
dt3 <- do.call(rbind.data.frame, out_seg)

# filtrar de los datos orignales unicamente los que tengas como primera = 1 y unir con los datos repetidos
out3 <- datos_or %>% 
  filter(segunda == 1) %>% 
  rbind(., dt3) %>% 
  mutate(segunda2 = rep(1, sum(datos_or$segunda))) %>% 
  select(., -c(primera, segunda)) 

datos2 <- out3
datos2$nom_vacuna <- factor(datos2$nom_vacuna, levels = c("CoronaVac SINOVAC", 
                                                          "CHADOX1S RECOMBINANTE ASTRAZENECA",
                                                          "BNT162b2 PFIZER"),
                            labels = c("SINOVAC", "ASTRAZENECA", "PFIZER"))
datos2$nom_vacuna <- as.character(datos2$nom_vacuna)

# provincias y vacunas
names(datos2)
datos0 <- data.frame(table(datos2[, c("provincia", "nom_vacuna")]))
datos <- data.frame(datos0[, c(1, 2, 3)]) %>% 
  pivot_wider(names_from = "nom_vacuna", values_from = "Freq")

acs <- dudi.coa(datos[, -1])
round(acs$cw, 2)
round(acs$lw, 2)
which.max(acs$lw[-19])
acs
write.table(round(acs$lw, 2), "clipboard", sep = "\t", dec = ",", row.names = T)

# Prueba chi-cuadrado de pearson
tabla_pro <- table(datos2[, c("provincia", "nom_vacuna")])
chisq.test(tabla_pro)
acs$li
acs$l1
acs$co
acs$c1
scatter(acs, method=1)
COP_1 <- FactoMineR::CA(tabla_pro, graph = F)
factoextra::fviz_ca_biplot(COP_1, repel = T)

# Dispersion
COP_1 <- FactoMineR::CA(tabla_pro, graph = F)
print(COP_1)
COP_1$col$coord
COP_1$row$inertia

factoextra::get_eigenvalue(COP_1)
factoextra::fviz_screeplot(COP_1, addlabels = T)
factoextra::fviz_ca_biplot(COP_1, repel = T)



# grupos de edad y vacunas
names(datos2)
datos0 <- data.frame(table(datos2[, c("gedad", "nom_vacuna")]))
datos <- data.frame(datos0[, c(1, 2, 3)]) %>% 
  pivot_wider(names_from = "gedad", values_from = "Freq")

acs <- dudi.coa(datos[, -1])
acs$cw
acs$lw
acs

# Prueba chi-cuadrado de pearson
tabla_pro <- table(datos2[, c("gedad", "nom_vacuna")])
chisq.test(tabla_pro)
acs$li
acs$l1
acs$co
acs$c1
scatter(acs, method=1)
tabla_pro <- table(datos2[, c("gedad", "nom_vacuna")])
COP_1 <- FactoMineR::CA(tabla_pro, graph = F)
factoextra::fviz_ca_biplot(COP_1, repel = T)


# Dispersion
COP_1 <- FactoMineR::CA(tabla_pro, graph = F)
print(COP_1)
COP_1$col$coord
COP_1$row$inertia

factoextra::get_eigenvalue(COP_1)
factoextra::fviz_screeplot(COP_1, addlabels = T)
factoextra::fviz_ca_biplot(COP_1, repel = T)
