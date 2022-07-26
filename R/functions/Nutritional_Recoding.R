###############################################################
# NUTRICION
###############################################################
# NINOS DE 0 A 23 MESES
###############################################################
#% de ninos de 0 a 23 meses que han recibido leche materna el dia anterior a la recogida de datos
loop_nutri_ninos023$n1 <- ifelse(loop_nutri_ninos023$recibio_leche_materna_ayer == "si", 1,0)

#% de ninos de 0 a 23 meses que han comido menos de tres veces el dia anterior a la recogida de datos
loop_nutri_ninos023$n2 <- ifelse(loop_nutri_ninos023$nr_alimentos_solidos_ayer %in% c("ninguna", "una_vez", "dos_veces"), 1,0)

#% de ninos de 0 a 23 meses que reciben parte de su alimentacion mediante un biberon o tetero
loop_nutri_ninos023$n3 <- ifelse(loop_nutri_ninos023$uso_biberon == "si", 1,0)



#% de niños de 0 a 23 meses que han recibido una de las siguientes intervenciones
loop_nutri_ninos023$n4_i <- ifelse(grepl("ninguna_de_las_anteriores", loop_nutri_ninos023$intervenciones),1,0)
loop_nutri_ninos023$n4_ii <- ifelse(grepl("desparasitacion", loop_nutri_ninos023$intervenciones),1,0)
loop_nutri_ninos023$n4_iii <- ifelse(grepl("orientacion_o_informacion_sobre_lactancia_materna_a_padres_y_o_cuidadores", loop_nutri_ninos023$intervenciones),1,0)
loop_nutri_ninos023$n4_iv <- ifelse(grepl("evaluacion_nutricional__toma_de_peso_y_talla_y_medicion_del_brazo", loop_nutri_ninos023$intervenciones),1,0)
loop_nutri_ninos023$n4_v <- ifelse(grepl("entrega_de_micronutrientes__en_polvo__sulfato_ferroso_y_o_vitamina_a_desparasitacion", loop_nutri_ninos023$intervenciones),1,0)


#Promedio de hemoglobina en la sangre de los niños de 0 a 23 meses
loop_nutri_ninos023$hemoglobina <- as.numeric(as.character(gsub("_", ".", loop_nutri_ninos023$hemoglobina)))
loop_nutri_ninos023$n5 <- loop_nutri_ninos023$hemoglobina
loop_nutri_ninos023[loop_nutri_ninos023==999]<-NA


# NINOS DE 24 A 59 MESES
###############################################################
#% de niños de 24-59 meses que han recibido una de las siguientes intervenciones
loop_nutri_ninos2459$n6_i <- ifelse(grepl("ninguna_de_las_anteriores", loop_nutri_ninos2459$intervenciones),1,0)
loop_nutri_ninos2459$n6_ii <- ifelse(grepl("desparasitacion", loop_nutri_ninos2459$intervenciones),1,0)
loop_nutri_ninos2459$n6_iii <- ifelse(grepl("orientacion_o_informacion_sobre_lactancia_materna_a_padres_y_o_cuidadores", loop_nutri_ninos2459$intervenciones),1,0)
loop_nutri_ninos2459$n6_iv <- ifelse(grepl("evaluacion_nutricional__toma_de_peso_y_talla_y_medicion_del_brazo", loop_nutri_ninos2459$intervenciones),1,0)
loop_nutri_ninos2459$n6_v <- ifelse(grepl("entrega_de_micronutrientes__en_polvo__sulfato_ferroso_y_o_vitamina_a_desparasitacion", loop_nutri_ninos2459$intervenciones),1,0)


#Promedio de hemoglobina en sangre para ninos de 24 a 59 meses
loop_nutri_ninos2459$hemoglobina <- as.numeric(as.character(gsub("_", ".", loop_nutri_ninos2459$hemoglobina)))
loop_nutri_ninos2459$n7 <- loop_nutri_ninos2459$hemoglobina
loop_nutri_ninos2459[loop_nutri_ninos2459==999]<-NA


#% de mujeres embarazadas que consumen los siguientes suplementos, vitaminas y minerales
loop_nutri_embarazos$n8_i <- ifelse(grepl("acido", loop_nutri_embarazos$suplementos),1,0)
loop_nutri_embarazos$n8_ii <- ifelse(grepl("calcio", loop_nutri_embarazos$suplementos),1,0)
loop_nutri_embarazos$n8_iii <- ifelse(grepl("hierro", loop_nutri_embarazos$suplementos),1,0)
loop_nutri_embarazos$n8_iv <- ifelse(grepl("ninguno_de_los_anteriores", loop_nutri_embarazos$suplementos),1,0)


#% de mujeres embarazadas por razón de no consumir ningún suplemento, vitamina o mineral
loop_nutri_embarazos$n9_i <- ifelse(loop_nutri_embarazos$razon_no_consumio_suplementos == "no_le_gusta_tomar_medicamentos",1,0)
loop_nutri_embarazos$n9_ii <- ifelse(loop_nutri_embarazos$razon_no_consumio_suplementos == "no_tiene_recursos_para_comprarlos",1,0)


#% de mujeres embarazadas que recibieron una de las siguentes intervenciones
loop_nutri_embarazos$n10_i <- ifelse(grepl("orientacion_o_informacion_sobre_lactancia_materna_y_o_nutricion", loop_nutri_embarazos$intervenciones),1,0)
loop_nutri_embarazos$n10_ii <- ifelse(grepl("toma_de_peso_y_talla_y_medicion_del_brazo", loop_nutri_embarazos$intervenciones),1,0)
loop_nutri_embarazos$n10_iii <- ifelse(grepl("entrega_de_micronutrientes__hierro__calcio_y_o_acido_folico", loop_nutri_embarazos$intervenciones),1,0)
loop_nutri_embarazos$n10_iv <- ifelse(grepl("desparasitacion", loop_nutri_embarazos$intervenciones),1,0)
loop_nutri_embarazos$n10_v <- ifelse(grepl("ninguna_de_las_anteriores", loop_nutri_embarazos$intervenciones),1,0)


#% de mujeres embarazadas que asisten al control prenatal
loop_nutri_embarazos$n11 <- ifelse(loop_nutri_embarazos$control_prenatal_si == "si",1,0)


#Promedio de hemoglobina en sangre de las embarazadas
loop_nutri_embarazos$hemoglobina <- as.numeric(as.character(gsub("_", ".", loop_nutri_embarazos$hemoglobina)))
loop_nutri_embarazos$n12 <- loop_nutri_embarazos$hemoglobina
loop_nutri_embarazos[loop_nutri_embarazos==999]<-NA


#% de hogares que consumen alimentos vegetales ricos en vitamina A (verduras y tubérculos ricos en vitamina A, verduras de hoja verde oscura o frutas o frutas ricas en vitamina A).
r$n13 <- ifelse(r$fcs_raices_ayer == "si" | r$fcs_vegetales_verdes_ayer == "si" | 
                  r$fcs_frutas_naranja_ayer == "si",1,0)


#% de hogares que consumen alimentos de origen animal ricos en vitamina A (carne de órganos, huevos o leche y productos lácteos)
r$n14 <- ifelse(r$fcs_visceras_rojo_ayer == "si" | r$fcs_huevos_ayer == "si" | 
                  r$fcs_leche_ayer == "si",1,0)


#% de hogares que consumen una fuente vegetal o animal de vitamina A (verduras y tubérculos ricos en vitamina A o verduras de hoja verde oscura o frutas ricas en vitamina A o carne de órganos, o huevos, o leche y productos lácteos).
r$n15 <- ifelse(r$n14 == 1 | r$n13 == 1, 1,0)


#% de hogares que consumen carne de órganos, carne o pescado
r$n16 <- ifelse(r$fcs_visceras_rojo_ayer == "si" | r$fcs_carne_ayer == "si" | 
                  r$fcs_pescado_ayer == "si",1,0)


#% de hogares por rangos del Household Dietary Diversity Scale
r$hdds_1 <- ifelse(r$fcs_cereales_ayer == "si",1,0)
r$hdds_2 <- ifelse(r$fcs_raices_ayer == "si",1,0)
r$hdds_3 <- ifelse(r$fcs_vegetales_ayer == "si",1,0)
r$hdds_4 <- ifelse(r$fcs_frutas_ayer == "si",1,0)
r$hdds_5 <- ifelse(r$fcs_carne_ayer == "si",1,0)
r$hdds_6 <- ifelse(r$fcs_huevos_ayer == "si",1,0)
r$hdds_7 <- ifelse(r$fcs_pescado_ayer == "si",1,0)
r$hdds_8 <- ifelse(r$fcs_leguminosas_ayer == "si",1,0)
r$hdds_9 <- ifelse(r$fcs_leche_ayer == "si",1,0)
r$hdds_10 <- ifelse(r$fcs_grasas_ayer == "si",1,0)
r$hdds_11 <- ifelse(r$fcs_azucares_ayer == "si",1,0)
r$hdds_12 <- ifelse(r$fcs_condimentos_ayer == "si",1,0)

r$hdds <- apply(r[,c("hdds_1", "hdds_2", "hdds_3","hdds_4","hdds_5",
                     "hdds_6","hdds_7","hdds_8","hdds_9","hdds_10",
                     "hdds_11","hdds_12")], 1, sum)

r$n17_i <- ifelse(r$hdds <= 3,1,0)
r$n17_ii <- ifelse(r$hdds > 3 & r$hdds <= 6,1,0)
r$n17_iii <- ifelse(r$hdds > 6 & r$hdds <= 9,1,0)
r$n17_iv <- ifelse(r$hdds > 9 & r$hdds <= 12,1,0)


#Promedio del Household Dietary Diversity Scale
r$n18 <- r$hdds


#% de niños de 6 a 23 meses de edad que consumieron alimentos y bebidas de al menos cinco de los ocho grupos de alimentos definidos durante el día anterior (MDD)
loop_nutri_ninos023$n19_i <- ifelse(grepl("leche_materna", loop_nutri_ninos023$consumo_ayer),1,0)
loop_nutri_ninos023$n19_ii <- ifelse(grepl("arroz__pasta__cereales", loop_nutri_ninos023$consumo_ayer),1,0)
loop_nutri_ninos023$n19_iii <- ifelse(grepl("carne_de_res__pollo__pescado", loop_nutri_ninos023$consumo_ayer),1,0)
loop_nutri_ninos023$n19_iv <- ifelse(grepl("papa__yuca__platano", loop_nutri_ninos023$consumo_ayer),1,0)
loop_nutri_ninos023$n19_v <- ifelse(grepl("huevos", loop_nutri_ninos023$consumo_ayer),1,0)
loop_nutri_ninos023$n19_vi <- ifelse(grepl("tomate__mango", loop_nutri_ninos023$consumo_ayer),1,0)
loop_nutri_ninos023$n19_vii <- ifelse(grepl("otras_frutas", loop_nutri_ninos023$consumo_ayer),1,0)

loop_nutri_ninos023$mdd <- apply(loop_nutri_ninos023[,c("n19_i", "n19_ii", "n19_iii","n19_iv","n19_v",
                     "n19_vi","n19_vii")], 1, sum)
loop_nutri_ninos023$n19 <- ifelse(loop_nutri_ninos023$mdd >= 5,1,0)


#% de niños de 6 a 23 meses de edad que consumieron alimentos sólidos, semisólidos o blandos (pero también incluye la alimentación con leche para los niños no amamantados) al menos el número mínimo de veces durante el día anterior. 
loop_nutri_ninos023 <- loop_nutri_ninos023 %>% mutate(n20 = case_when(
  loop_nutri_ninos023$edad <= 8 & loop_nutri_ninos023$nr_alimentos_solidos_ayer != "ninguna" & 
    loop_nutri_ninos023$nr_alimentos_solidos_ayer != "una_vez" ~ 1,
  loop_nutri_ninos023$edad > 8 & loop_nutri_ninos023$edad <= 23 & loop_nutri_ninos023$nr_alimentos_solidos_ayer != "ninguna" & 
    loop_nutri_ninos023$nr_alimentos_solidos_ayer != "una_vez" & loop_nutri_ninos023$nr_alimentos_solidos_ayer != "dos_veces" ~ 1,
  TRUE ~ 0
))


#% de niños de 6 a 23 meses de edad que consumieron una dieta mínima dieta aceptable durante el día anterior 
loop_nutri_ninos023$n21 <- ifelse(loop_nutri_ninos023$n19 == 1 & loop_nutri_ninos023$n20 == 1, 1,0)


#% de niños de 6 a 23 meses de edad que consumieron huevos y/o alimentos de carne durante el día anterior.
loop_nutri_ninos023$n22 <- ifelse(loop_nutri_ninos023$n19_iii == 1 | loop_nutri_ninos023$n19_v == 1,1,0)


#% de niños de 6 a 23 meses de edad que no consumieron verduras o frutas durante el día anterior.
loop_nutri_ninos023$n23 <- ifelse(loop_nutri_ninos023$n19_vi == 0 & loop_nutri_ninos023$n19_vii == 0,1,0)




library(anthro)

#######################################
# calculate the z-scores for children 023
names(loop_nutri_ninos023)
registro_hh <- dplyr::select(loop, registro, sexo, nombre)
loop_nutri_ninos023 <- merge(registro_hh, loop_nutri_ninos023, by.x = c("registro", "nombre"), all.y = T)
loop_nutri_ninos023 <- loop_nutri_ninos023 %>% mutate(sexo = case_when(
  loop_nutri_ninos023$sexo == "hombre" ~ "m",
  loop_nutri_ninos023$sexo == "mujer" ~ "f",
))
loop_nutri_ninos023$peso_1 <- as.numeric(as.character(loop_nutri_ninos023$peso_1))
loop_nutri_ninos023$talla_1 <- as.numeric(as.character(loop_nutri_ninos023$talla_1))
loop_nutri_ninos023$measurement <- "h"

growth_zscores023 <-  anthro_zscores(
  sex = loop_nutri_ninos023$sexo,
  age = loop_nutri_ninos023$edad,
  is_age_in_month = TRUE,
  weight = loop_nutri_ninos023$peso_1,
  lenhei = loop_nutri_ninos023$talla_1
)

#select zlen (length for age), zwei (weight for age), zwfl (weight for length), zbmi (bmi for age)
growth_zscores023 <- growth_zscores023[,c("zlen", "zwei", "zwfl", "zbmi")]
loop_nutri_ninos023 <- cbind(loop_nutri_ninos023, growth_zscores)


# % de niños menores de 5 años con retraso en el crecimiento (< -2 z-score)
#extreme and moderate stunting
loop_nutri_ninos023$n24_i <- ifelse(loop_nutri_ninos023$zlen < -2 & loop_nutri_ninos023$zlen >= -3,1,0)
loop_nutri_ninos023$n24_ii <- ifelse(loop_nutri_ninos023$zlen < -3,1,0)

  
# % de niños menores de 5 años con peso inferior al normal (< -2  z-score)
#severe and moderate underweight
loop_nutri_ninos023$n25_i <- ifelse(loop_nutri_ninos023$zwei < -2 & loop_nutri_ninos023$zwei >= -3,1,0)
loop_nutri_ninos023$n25_ii <- ifelse(loop_nutri_ninos023$zwei < -3,1,0)


#% de niños menores de 5 años con emaciación (< -2 z-score)
#severe wasting and moderate wasting
loop_nutri_ninos023$n26_i <- ifelse(loop_nutri_ninos023$zwfl < -2 & loop_nutri_ninos023$zwfl >= -3,1,0)
loop_nutri_ninos023$n26_ii <- ifelse(loop_nutri_ninos023$zwfl < -3,1,0)


#% de niños menores de 5 años con sobrepeso (> +2 puntuación z)
#overweight and obesity
loop_nutri_ninos023$n27_i <- ifelse(loop_nutri_ninos023$zwfl <= 3 & loop_nutri_ninos023$zwfl > 2,1,0)
loop_nutri_ninos023$n27_ii <- ifelse(loop_nutri_ninos023$zwfl > 3,1,0)






