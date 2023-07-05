library(anthro)

loop_nutri <- read.csv("Input/datasets/dirty/nutrition/n_df_original_intervenciones.csv", sep = ",",
                               stringsAsFactors=F, check.names=T,
                               na.strings = c("", " ", "NA", "#N/A", "N/A"))

loop_nutri <- response %>% select("registro", "weights", "pop_group") %>%
                                      right_join(loop_nutri, by = "registro")
loop_nutri$weights <- as.numeric(as.character(loop_nutri$weights))


loop_nutri <- loop_nutri %>% 
  mutate(across(c(5:last_col()), na_if, 999)) %>%
  mutate(across(c(5:last_col()), na_if, 999.0)) 

loop_nutri$edad_ninos059 <- ifelse(loop_nutri$edad_ninos023 != 0, 
                                   loop_nutri$edad_ninos023, loop_nutri$edad_ninos2459)

#loop_nutri <- loop_nutri %>% mutate(final_peso_ninos059 = case_when(
#  edad_ninos023 >= 0 ~ loop_nutri$final_peso_infante_ninos023, 
#  edad_ninos2459 >= 0 ~ loop_nutri$final_peso_ninos2459))

#loop_nutri <- loop_nutri %>% mutate(final_talla_ninos059 = case_when(
#  edad_ninos023 >= 0 ~ loop_nutri$final_talla_infantil_ninos023, 
#  edad_ninos2459 >= 0 ~ loop_nutri$final_talla_ninos2459))


###########################
#exclude cases that CNC excluded
#loop_nutri <- loop_nutri %>% filter(is.na(INCLUIR)) 
##########################





loop_nutri_ninos059 <- n_df_ninos059





recoding_antro_ninos059 <- function(loop_nutri_ninos059) {


#######################################
# calculate the z-scores for children 023
names(loop_nutri_ninos059)
loop_nutri_ninos059 <- loop_nutri_ninos059 %>% mutate(sexo = case_when(
  sexo == "hombre" ~ "m",
  sexo == "mujer" ~ "f",
))
loop_nutri_ninos059$measurement <- "h"

growth_zscores059 <-  anthro_zscores(
  sex = loop_nutri_ninos059$sexo,
  age = loop_nutri_ninos059$edad_meses_ninos059,
  is_age_in_month = TRUE,
  weight = loop_nutri_ninos059$final_peso_ninos059,
  lenhei = loop_nutri_ninos059$final_talla_ninos059
)

#select zlen (length for age), zwei (weight for age), zwfl (weight for length), zbmi (bmi for age)
growth_zscores059 <- growth_zscores059[,c("zlen", "zwei", "zwfl", "zbmi")]
loop_nutri_ninos059 <- cbind(loop_nutri_ninos059, growth_zscores059)


#WEIGHT FOR HEIGHT
#Obesity> 3
loop_nutri_ninos059$wh1 <- ifelse(loop_nutri_ninos059$zwfl > 3,1,0)
#Sobrepeso <= 3 & > 2
loop_nutri_ninos059$wh2 <- ifelse(loop_nutri_ninos059$zwfl <= 3 & loop_nutri_ninos059$zwfl > 2,1,0)
#riesgo de sobrepeso <= 2 & >1
loop_nutri_ninos059$wh3 <- ifelse(loop_nutri_ninos059$zwfl <= 2 & loop_nutri_ninos059$zwfl > 1,1,0)
#peso adecuado para la talla <=1 & >=-1
loop_nutri_ninos059$wh4 <- ifelse(loop_nutri_ninos059$zwfl <= 1 & loop_nutri_ninos059$zwfl >= -1,1,0)
#riesgo de desnutricion aguda <-1 & >= -2
loop_nutri_ninos059$wh5 <- ifelse(loop_nutri_ninos059$zwfl < -1 & loop_nutri_ninos059$zwfl >= -2,1,0)
#desnutricion aguda moderada < -2 & >=-3
loop_nutri_ninos059$wh6 <- ifelse(loop_nutri_ninos059$zwfl < -2 & loop_nutri_ninos059$zwfl >= -3,1,0)
#desnutricion aguda severa < -3
loop_nutri_ninos059$wh7 <- ifelse(loop_nutri_ninos059$zwfl < -3,1,0)


#HEIGHT FOR AGE
#Talla adecuada para la edad >= -1
loop_nutri_ninos059$ha1 <- ifelse(loop_nutri_ninos059$zlen >= -1,1,0)
#riesgo de talla baja >= -2 & <-1
loop_nutri_ninos059$ha2 <- ifelse(loop_nutri_ninos059$zlen >= -2 & loop_nutri_ninos059$zlen < -1,1,0)
#talla baja para la edad o retraso de talla <-2
loop_nutri_ninos059$ha3 <- ifelse(loop_nutri_ninos059$zlen < -2,1,0)


#IMC PARA LA EDAD
#Obesidad > 3
loop_nutri_ninos059$imca1 <- ifelse(loop_nutri_ninos059$zbmi > 3,1,0)
#Sobrepeso > 2 & <= 3
loop_nutri_ninos059$imca2 <- ifelse(loop_nutri_ninos059$zbmi > 2 & loop_nutri_ninos059$zbmi <= 3,1,0)
#riesgo de sobrepeso > 1 & <=2
loop_nutri_ninos059$imca3 <- ifelse(loop_nutri_ninos059$zbmi > 1 & loop_nutri_ninos059$zbmi <= 2,1,0)
#No aplica <= 1
loop_nutri_ninos059$imca4 <- ifelse(loop_nutri_ninos059$zbmi <= 1,1,0)


#WEIGHT FOR AGE
#No aplica > 1
loop_nutri_ninos059$wa1 <- ifelse(loop_nutri_ninos059$zwei > 1,1,0)
#Peso adequado para la edad >= -1 & <= 1
loop_nutri_ninos059$wa2 <- ifelse(loop_nutri_ninos059$zwei >= -1 & loop_nutri_ninos059$zwei <=1,1,0)
#Riesgo de desnutricion global >= -2 & <-1
loop_nutri_ninos059$wa3 <- ifelse(loop_nutri_ninos059$zwei >= -2 & loop_nutri_ninos059$zwei < -1,1,0)
#Desnutricion global < -2
loop_nutri_ninos059$wa4 <- ifelse(loop_nutri_ninos059$zwei < -2,1,0)



#% de niños de 6 a 23 meses de edad que consumieron alimentos y bebidas de al menos cinco de los ocho grupos de alimentos definidos durante el día anterior (MDD)
loop_nutri_ninos059$consumo_ayer <- paste0(loop_nutri_ninos059$consumo_ayer_alimento1,
                                          loop_nutri_ninos059$consumo_ayer_alimento2,
                                          loop_nutri_ninos059$consumo_ayer_alimento3,
                                          loop_nutri_ninos059$consumo_ayer_alimento4,
                                          loop_nutri_ninos059$consumo_ayer_alimento5,
                                          loop_nutri_ninos059$consumo_ayer_alimento6,
                                          loop_nutri_ninos059$consumo_ayer_alimento7,
                                          loop_nutri_ninos059$consumo_ayer_alimento8,
                                          loop_nutri_ninos059$consumo_ayer_alimento9,
                                          loop_nutri_ninos059$consumo_ayer_alimento10,
                                          loop_nutri_ninos059$consumo_ayer_alimento11,
                                          loop_nutri_ninos059$consumo_ayer_alimento12)

loop_nutri_ninos059$n19_i <- ifelse(grepl("leche_materna", loop_nutri_ninos059$consumo_ayer),1,0)
loop_nutri_ninos059$n19_ii <- ifelse(grepl("arroz__pasta__cereales", loop_nutri_ninos059$consumo_ayer),1,0)
loop_nutri_ninos059$n19_iii <- ifelse(grepl("carne_de_res__pollo__pescado", loop_nutri_ninos059$consumo_ayer),1,0)
loop_nutri_ninos059$n19_iv <- ifelse(grepl("ahuyama__espinaca", loop_nutri_ninos059$consumo_ayer),1,0)
loop_nutri_ninos059$n19_v <- ifelse(grepl("huevos", loop_nutri_ninos059$consumo_ayer),1,0)
loop_nutri_ninos059$n19_vi <- ifelse(grepl("leche_de_vaca", loop_nutri_ninos059$consumo_ayer),1,0)
loop_nutri_ninos059$n19_vii <- ifelse(grepl("otras_frutas", loop_nutri_ninos059$consumo_ayer),1,0)
loop_nutri_ninos059$n19_viii <- ifelse(grepl("queso", loop_nutri_ninos059$consumo_ayer),1,0)
loop_nutri_ninos059$n19_ix <- ifelse(grepl("yogurt_kumis", loop_nutri_ninos059$consumo_ayer),1,0)
loop_nutri_ninos059$n19_x <- ifelse(grepl("agua__aromatica", loop_nutri_ninos059$consumo_ayer),1,0)
loop_nutri_ninos059$n19_xi <- ifelse(grepl("frijoles", loop_nutri_ninos059$consumo_ayer),1,0)
loop_nutri_ninos059$n19_xii <- ifelse(grepl("ninguna_de_las_anteriores", loop_nutri_ninos059$consumo_ayer),1,0)
loop_nutri_ninos059$n19_xiii <- ifelse(grepl("leche_de_tarro", loop_nutri_ninos059$consumo_ayer),1,0)


loop_nutri_ninos059$n19_dairy <- ifelse(grepl("leche_de_tarro", loop_nutri_ninos059$consumo_ayer) | grepl("leche_de_vaca", loop_nutri_ninos059$consumo_ayer) | 
                                        grepl("yogurt_kumis", loop_nutri_ninos059$consumo_ayer) | grepl("queso", loop_nutri_ninos059$consumo_ayer),1,0)



loop_nutri_ninos059$mdd <- apply(loop_nutri_ninos059[,c("n19_i", "n19_ii", "n19_iii","n19_iv","n19_v",
                                                        "n19_dairy","n19_vii", "n19_xi")], 1, sum)
loop_nutri_ninos059$n19 <- ifelse(loop_nutri_ninos059$mdd >= 5,1,0)



######################
#ANEMIA
#####################
loop_nutri_ninos059$anemia <- case_when(loop_nutri_ninos059$CLASIFICACION.HB == "1__anemia" ~ 1,
                                        loop_nutri_ninos059$CLASIFICACION.HB == "2__normal" ~ 0,
                                        TRUE ~ NA_real_)
loop_nutri_ninos059$CLASIFICACION.ANEMIA <- ifelse(loop_nutri_ninos059$CLASIFICACION.ANEMIA == "", NA_real_, 
                                                   loop_nutri_ninos059$CLASIFICACION.ANEMIA)


loop_nutri_ninos059$anemia_i <- ifelse(loop_nutri_ninos059$CLASIFICACION.ANEMIA == "sin_anemia",1,0)
loop_nutri_ninos059$anemia_ii <- ifelse(loop_nutri_ninos059$CLASIFICACION.ANEMIA == "1__anemia_leve",1,0)
loop_nutri_ninos059$anemia_iii <- ifelse(loop_nutri_ninos059$CLASIFICACION.ANEMIA == "2__anemia_moderada",1,0)
loop_nutri_ninos059$anemia_iv <- ifelse(loop_nutri_ninos059$CLASIFICACION.ANEMIA == "3__anemia_grave",1,0)




####################
#GROUPS FOR MALNUTRITION
###################
loop_nutri_ninos059$peso_talla_category_grouped <- case_when(loop_nutri_ninos059$peso_talla_category %in% c("1__desnutricion_aguda_severa", "2__desnutricion_aguda_moderada") ~ "1_desnutricion_severa_moderada",
                                                             loop_nutri_ninos059$peso_talla_category == "3__riesgo_de_desnutricion_aguda" ~ "2_riesgo_de_desnutricion_aguda",
                                                             loop_nutri_ninos059$peso_talla_category == "4__peso_adecuado_para_la_talla" ~ "3_peso_adecuado_para_la_talla",
                                                             loop_nutri_ninos059$peso_talla_category == "5__riesgo_de_sobrepeso" ~ "4_riesgo_de_sobrepeso",
                                                             loop_nutri_ninos059$peso_talla_category %in% c("6__sobrepeso", "7__obesidad") ~ "5_sobrepeso_obesidad",
                                                             
                                        TRUE ~ "")

names(loop_nutri_ninos059)[names(loop_nutri_ninos059) == "CLASIFICACION.ANEMIA"] <- "clasificacion_anemia"

loop_nutri_ninos059$anemia_categories <- case_when(loop_nutri_ninos059$clasificacion_anemia %in% c("1__anemia_leve", "2__anemia_moderada", "3__anemia_grave") ~ "1_con_anemia",
                                                             loop_nutri_ninos059$clasificacion_anemia == "sin_anemia" ~ "2_sin_anemia",
                                                             
                                                             TRUE ~ "")



loop_nutri_ninos059$fcsn_iron_0 <- ifelse(loop_nutri_ninos059$fcs_n_iron == "0",1,0)
loop_nutri_ninos059$fcsn_iron_1_6 <- ifelse(loop_nutri_ninos059$fcs_n_iron == "1_6",1,0)
loop_nutri_ninos059$fcsn_iron_7 <- ifelse(loop_nutri_ninos059$fcs_n_iron == "7_",1,0)

loop_nutri_ninos059$fcs_n_protein_0 <- ifelse(loop_nutri_ninos059$fcs_n_protein == "0",1,0)
loop_nutri_ninos059$fcs_n_protein_1_6 <- ifelse(loop_nutri_ninos059$fcs_n_protein == "1_6",1,0)
loop_nutri_ninos059$fcs_n_protein_7 <- ifelse(loop_nutri_ninos059$fcs_n_protein == "7_",1,0)

loop_nutri_ninos059$fcs_n_vita_0 <- ifelse(loop_nutri_ninos059$fcs_n_vita == "0",1,0)
loop_nutri_ninos059$fcs_n_vita_1_6 <- ifelse(loop_nutri_ninos059$fcs_n_vita == "1_6",1,0)
loop_nutri_ninos059$fcs_n_vita_7 <- ifelse(loop_nutri_ninos059$fcs_n_vita == "7_",1,0)



return(loop_nutri_ninos059)
}




##############################
#RECODE NUTRITION GESTANTES
#############################
recoding_nutri_gestantes <- function(n_df_gestantes){
  n_df_gestantes$anemia <- case_when(n_df_gestantes$CLASIFICACION.HB == "1__anemia" ~ 1,
                                     n_df_gestantes$CLASIFICACION.HB == "2__normal" ~ 0,
                                          TRUE ~ NA_real_)
  
  #% de mujeres embarazadas que consumen los siguientes suplementos, vitaminas y minerales
  n_df_gestantes$n8_i <- ifelse(n_df_gestantes$Acido.folico == "acido_folico",1,0)
  n_df_gestantes$n8_ii <- ifelse(n_df_gestantes$Calcio == "calcio",1,0)
  n_df_gestantes$n8_iii <- ifelse(n_df_gestantes$Hierro == "hierro",1,0)
  n_df_gestantes$n8_iv <- ifelse(n_df_gestantes$P109_1 == "ninguno_de_los_anteriores",1,0)
  
  

  #% de mujeres embarazadas que recibieron una de las siguentes intervenciones
  n_df_gestantes <- n_df_gestantes %>% 
    mutate(intervencion_gestantes_desparacion = ifelse(P111_1 == "desparasitacion" |
                                                       P111_2 == "desparasitacion" |
                                                       P111_3 == "desparasitacion" |
                                                       P111_4 == "desparasitacion" |
                                                       P111_5 == "desparasitacion",1,0),
    intervencion_gestantes_micronutrientes = ifelse(P111_1 == "entrega_de_micronutrientes__hierro__calcio_y_o_acido_folico" |
                                                                P111_2 == "entrega_de_micronutrientes__hierro__calcio_y_o_acido_folico" |
                                                                P111_3 == "entrega_de_micronutrientes__hierro__calcio_y_o_acido_folico" |
                                                                P111_4 == "entrega_de_micronutrientes__hierro__calcio_y_o_acido_folico" |
                                                                P111_5 == "entrega_de_micronutrientes__hierro__calcio_y_o_acido_folico",1,0),
           intervencion_gestantes_evaluacion = ifelse(P111_1 == "evaluacion_nutricional__toma_de_peso_y_talla_y_medicion_del_brazo" |
                                                                    P111_2 == "evaluacion_nutricional__toma_de_peso_y_talla_y_medicion_del_brazo" |
                                                                    P111_3 == "evaluacion_nutricional__toma_de_peso_y_talla_y_medicion_del_brazo" |
                                                                    P111_4 == "evaluacion_nutricional__toma_de_peso_y_talla_y_medicion_del_brazo" |
                                                                    P111_5 == "evaluacion_nutricional__toma_de_peso_y_talla_y_medicion_del_brazo",1,0),
                  intervencion_gestantes_ninguna = ifelse(P111_1 == "ninguna_de_las_anteriores" |
                                                                           P111_2 == "ninguna_de_las_anteriores" |
                                                                           P111_3 == "ninguna_de_las_anteriores" |
                                                                           P111_4 == "ninguna_de_las_anteriores" |
                                                                           P111_5 == "ninguna_de_las_anteriores",1,0),
                         intervencion_gestantes_orientacion_lactancia = ifelse(P111_1 == "orientacion_o_informacion_sobre_lactancia_materna_y_o_nutricion" |
                                                                          P111_2 == "orientacion_o_informacion_sobre_lactancia_materna_y_o_nutricion" |
                                                                          P111_3 == "orientacion_o_informacion_sobre_lactancia_materna_y_o_nutricion" |
                                                                          P111_4 == "orientacion_o_informacion_sobre_lactancia_materna_y_o_nutricion" |
                                                                          P111_5 == "orientacion_o_informacion_sobre_lactancia_materna_y_o_nutricion",1,0),
                                intervencion_gestantes_prevencion_bajo_peso = ifelse(P111_1 == "prevencion_del_bajo_peso_gestacional" |
                                                                                               P111_2 == "prevencion_del_bajo_peso_gestacional" |
                                                                                               P111_3 == "prevencion_del_bajo_peso_gestacional" |
                                                                                               P111_4 == "prevencion_del_bajo_peso_gestacional" |
                                                                                               P111_5 == "prevencion_del_bajo_peso_gestacional",1,0)
                                       )
                  
                  
  
  n_df_gestantes$fcsn_iron_0 <- ifelse(n_df_gestantes$fcs_n_iron == "0",1,0)
  n_df_gestantes$fcsn_iron_1_6 <- ifelse(n_df_gestantes$fcs_n_iron == "1_6",1,0)
  n_df_gestantes$fcsn_iron_7 <- ifelse(n_df_gestantes$fcs_n_iron == "7_",1,0)
  
  n_df_gestantes$fcs_n_protein_0 <- ifelse(n_df_gestantes$fcs_n_protein == "0",1,0)
  n_df_gestantes$fcs_n_protein_1_6 <- ifelse(n_df_gestantes$fcs_n_protein == "1_6",1,0)
  n_df_gestantes$fcs_n_protein_7 <- ifelse(n_df_gestantes$fcs_n_protein == "7_",1,0)
  
  n_df_gestantes$fcs_n_vita_0 <- ifelse(n_df_gestantes$fcs_n_vita == "0",1,0)
  n_df_gestantes$fcs_n_vita_1_6 <- ifelse(n_df_gestantes$fcs_n_vita == "1_6",1,0)
  n_df_gestantes$fcs_n_vita_7 <- ifelse(n_df_gestantes$fcs_n_vita == "7_",1,0)
  
  
  
  return(n_df_gestantes)
}




##############################
#RECODE NUTRITION MAYORES
#############################
recoding_nutri_mayores <- function(n_df_mayores){
  n_df_mayores$obesidad <- ifelse(n_df_mayores$CLASIF.IMC %in% c("10__obesidad_grado_iv", "7__obesidad_grado_i", "8__obesidad_grado_ii", "9__obesidad_grado_iii"),1,0)
  
  n_df_mayores$sobrepeso <- ifelse(n_df_mayores$CLASIF.IMC == "6_sobrepeso",1,0)
  
  
  return(n_df_mayores)
}



recoding_intervenciones <- function(loop_nutri){
  loop_nutri_ninos <- loop_nutri %>% filter(
                                 edad_ninos023 >= 0 |
                                 edad_ninos2459 >= 0) %>%
    mutate(intervencion_023_desparasitacion = ifelse(intervenciones_1_ninos023 == "Desparasitación" |
                                                     intervenciones_2_ninos023 == "Desparasitación" |
                                                     intervenciones_3_ninos023 == "Desparasitación" |
                                                     intervenciones_4_ninos023 == "Desparasitación" |
                                                     intervenciones_5_ninos023 == "Desparasitación",1,0),
           intervencion_023_micronutrientes = ifelse(intervenciones_1_ninos023 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                       intervenciones_2_ninos023 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                       intervenciones_3_ninos023 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                       intervenciones_4_ninos023 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                       intervenciones_5_ninos023 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A",1,0),
           intervencion_023_evaluacion_nutricional = ifelse(intervenciones_1_ninos023 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                       intervenciones_2_ninos023 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                       intervenciones_3_ninos023 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                       intervenciones_4_ninos023 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                       intervenciones_5_ninos023 == "Evaluación nutricional: toma de peso y talla y medición del brazo",1,0),
           intervencion_023_ninguna = ifelse(intervenciones_1_ninos023 == "Ninguna de las anteriores" |
                                                              intervenciones_2_ninos023 == "Ninguna de las anteriores" |
                                                              intervenciones_3_ninos023 == "Ninguna de las anteriores" |
                                                              intervenciones_4_ninos023 == "Ninguna de las anteriores" |
                                                              intervenciones_5_ninos023 == "Ninguna de las anteriores",1,0),
           intervencion_023_lactancia_materna = ifelse(intervenciones_1_ninos023 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                               intervenciones_2_ninos023 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                               intervenciones_3_ninos023 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                               intervenciones_4_ninos023 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                               intervenciones_5_ninos023 == "Orientación o información sobre lactancia materna a padres y/o cuidadores",1,0),
           intervencion_023_prevencion = ifelse(intervenciones_1_ninos023 == "Prevención para la desnutrición aguda" |
                                                         intervenciones_2_ninos023 == "Prevención para la desnutrición aguda" |
                                                         intervenciones_3_ninos023 == "Prevención para la desnutrición aguda" |
                                                         intervenciones_4_ninos023 == "Prevención para la desnutrición aguda" |
                                                         intervenciones_5_ninos023 == "Prevención para la desnutrición aguda",1,0),
           intervencion_023_tratamiento = ifelse(intervenciones_1_ninos023 == "PTratamiento de la desnutrición aguda" |
                                                  intervenciones_2_ninos023 == "Tratamiento de la desnutrición aguda" |
                                                  intervenciones_3_ninos023 == "Tratamiento de la desnutrición aguda" |
                                                  intervenciones_4_ninos023 == "Tratamiento de la desnutrición aguda" |
                                                  intervenciones_5_ninos023 == "Tratamiento de la desnutrición aguda",1,0),
           
           
           
           intervencion_2459_desparasitacion = ifelse(intervenciones_1_ninos2459 == "Desparasitación" |
                                                       intervenciones_2_ninos2459 == "Desparasitación" |
                                                       intervenciones_3_ninos2459 == "Desparasitación" |
                                                       intervenciones_4_ninos2459 == "Desparasitación" |
                                                       intervenciones_5_ninos2459 == "Desparasitación",1,0),
           intervencion_2459_micronutrientes = ifelse(intervenciones_1_ninos2459 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                       intervenciones_2_ninos2459 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                       intervenciones_3_ninos2459 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                       intervenciones_4_ninos2459 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                       intervenciones_5_ninos2459 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A",1,0),
           intervencion_2459_evaluacion_nutricional = ifelse(intervenciones_1_ninos2459 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                              intervenciones_2_ninos2459 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                              intervenciones_3_ninos2459 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                              intervenciones_4_ninos2459 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                              intervenciones_5_ninos2459 == "Evaluación nutricional: toma de peso y talla y medición del brazo",1,0),
           intervencion_2459_ninguna = ifelse(intervenciones_1_ninos2459 == "Ninguna de las anteriores" |
                                               intervenciones_2_ninos2459 == "Ninguna de las anteriores" |
                                               intervenciones_3_ninos2459 == "Ninguna de las anteriores" |
                                               intervenciones_4_ninos2459 == "Ninguna de las anteriores" |
                                               intervenciones_5_ninos2459 == "Ninguna de las anteriores",1,0),
           intervencion_2459_lactancia_materna = ifelse(intervenciones_1_ninos2459 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                                         intervenciones_2_ninos2459 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                                         intervenciones_3_ninos2459 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                                         intervenciones_4_ninos2459 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                                         intervenciones_5_ninos2459 == "Orientación o información sobre lactancia materna a padres y/o cuidadores",1,0),
           intervencion_2459_prevencion = ifelse(intervenciones_1_ninos2459 == "Prevención para la desnutrición aguda" |
                                                  intervenciones_2_ninos2459 == "Prevención para la desnutrición aguda" |
                                                  intervenciones_3_ninos2459 == "Prevención para la desnutrición aguda" |
                                                  intervenciones_4_ninos2459 == "Prevención para la desnutrición aguda" |
                                                  intervenciones_5_ninos2459 == "Prevención para la desnutrición aguda",1,0),
           intervencion_2459_tratamiento = ifelse(intervenciones_1_ninos2459 == "PTratamiento de la desnutrición aguda" |
                                                   intervenciones_2_ninos2459 == "Tratamiento de la desnutrición aguda" |
                                                   intervenciones_3_ninos2459 == "Tratamiento de la desnutrición aguda" |
                                                   intervenciones_4_ninos2459 == "Tratamiento de la desnutrición aguda" |
                                                   intervenciones_5_ninos2459 == "Tratamiento de la desnutrición aguda",1,0) )  %>%
             
    
           mutate(intervencion_023_desparasitacion = ifelse(is.na(intervencion_023_desparasitacion) & edad_ninos023 >= 0,0,
                                                            intervencion_023_desparasitacion),
                  intervencion_023_micronutrientes = ifelse(is.na(intervencion_023_micronutrientes) & edad_ninos023 >= 0,0,
                                                            intervencion_023_micronutrientes),
                  intervencion_023_evaluacion_nutricional = ifelse(is.na(intervencion_023_evaluacion_nutricional) & edad_ninos023 >= 0,0,
                                                                   intervencion_023_evaluacion_nutricional),
                  intervencion_023_ninguna = ifelse(is.na(intervencion_023_ninguna) & edad_ninos023 >= 0,0,
                                                    intervencion_023_ninguna),
                  intervencion_023_lactancia_materna = ifelse(is.na(intervencion_023_lactancia_materna) & edad_ninos023 >= 0,0,
                                                              intervencion_023_lactancia_materna),
                  intervencion_023_prevencion = ifelse(is.na(intervencion_023_prevencion) & edad_ninos023 >= 0,0,
                                                       intervencion_023_prevencion),
                  intervencion_023_tratamiento = ifelse(is.na(intervencion_023_tratamiento) & edad_ninos023 >= 0,0,
                                                        intervencion_023_tratamiento),
                  
                  
                  intervencion_2459_desparasitacion = ifelse(is.na(intervencion_2459_desparasitacion) & edad_ninos2459 >= 0,0,
                                                             intervencion_2459_desparasitacion),
                  intervencion_2459_micronutrientes = ifelse(is.na(intervencion_2459_micronutrientes) & edad_ninos2459 >= 0,0,
                                                             intervencion_2459_micronutrientes),
                  intervencion_2459_evaluacion_nutricional = ifelse(is.na(intervencion_2459_evaluacion_nutricional) & edad_ninos2459 >= 0,0,
                                                                    intervencion_2459_evaluacion_nutricional),
                  intervencion_2459_ninguna = ifelse(is.na(intervencion_2459_ninguna) & edad_ninos2459 >= 0,0,
                                                     intervencion_2459_ninguna),
                  intervencion_2459_lactancia_materna = ifelse(is.na(intervencion_2459_lactancia_materna) & edad_ninos2459 >= 0,0,
                                                               intervencion_2459_lactancia_materna),
                  intervencion_2459_prevencion = ifelse(is.na(intervencion_2459_prevencion) & edad_ninos2459 >= 0,0,
                                                        intervencion_2459_prevencion),
                  intervencion_2459_tratamiento = ifelse(is.na(intervencion_2459_tratamiento) & edad_ninos2459 >= 0,0,
                                                         intervencion_2459_tratamiento),
                  )

}







recoding_intervenciones_2 <- function(loop_nutri){
  loop_nutri_ninos <- response_with_composites %>% select("registro", "pop_group") %>%
                                                   left_join(loop_nutri, by = "registro") %>%
    filter(
    edad_ninos023 >= 0 |
      edad_ninos2459 >= 0) %>%
    mutate(intervencion_023_desparasitacion = ifelse(intervenciones_1_ninos023 == "Desparasitación" |
                                                       intervenciones_2_ninos023 == "Desparasitación" |
                                                       intervenciones_3_ninos023 == "Desparasitación" |
                                                       intervenciones_4_ninos023 == "Desparasitación" |
                                                       intervenciones_5_ninos023 == "Desparasitación",1,0),
           intervencion_023_micronutrientes = ifelse(intervenciones_1_ninos023 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                       intervenciones_2_ninos023 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                       intervenciones_3_ninos023 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                       intervenciones_4_ninos023 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                       intervenciones_5_ninos023 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A",1,0),
           intervencion_023_evaluacion_nutricional = ifelse(intervenciones_1_ninos023 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                              intervenciones_2_ninos023 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                              intervenciones_3_ninos023 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                              intervenciones_4_ninos023 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                              intervenciones_5_ninos023 == "Evaluación nutricional: toma de peso y talla y medición del brazo",1,0),
           intervencion_023_ninguna = ifelse(intervenciones_1_ninos023 == "Ninguna de las anteriores" |
                                               intervenciones_2_ninos023 == "Ninguna de las anteriores" |
                                               intervenciones_3_ninos023 == "Ninguna de las anteriores" |
                                               intervenciones_4_ninos023 == "Ninguna de las anteriores" |
                                               intervenciones_5_ninos023 == "Ninguna de las anteriores",1,0),
           intervencion_023_lactancia_materna = ifelse(intervenciones_1_ninos023 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                                         intervenciones_2_ninos023 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                                         intervenciones_3_ninos023 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                                         intervenciones_4_ninos023 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                                         intervenciones_5_ninos023 == "Orientación o información sobre lactancia materna a padres y/o cuidadores",1,0),
           intervencion_023_prevencion = ifelse(intervenciones_1_ninos023 == "Prevención para la desnutrición aguda" |
                                                  intervenciones_2_ninos023 == "Prevención para la desnutrición aguda" |
                                                  intervenciones_3_ninos023 == "Prevención para la desnutrición aguda" |
                                                  intervenciones_4_ninos023 == "Prevención para la desnutrición aguda" |
                                                  intervenciones_5_ninos023 == "Prevención para la desnutrición aguda",1,0),
           intervencion_023_tratamiento = ifelse(intervenciones_1_ninos023 == "PTratamiento de la desnutrición aguda" |
                                                   intervenciones_2_ninos023 == "Tratamiento de la desnutrición aguda" |
                                                   intervenciones_3_ninos023 == "Tratamiento de la desnutrición aguda" |
                                                   intervenciones_4_ninos023 == "Tratamiento de la desnutrición aguda" |
                                                   intervenciones_5_ninos023 == "Tratamiento de la desnutrición aguda",1,0),
           
           
           
           intervencion_2459_desparasitacion = ifelse(intervenciones_1_ninos2459 == "Desparasitación" |
                                                        intervenciones_2_ninos2459 == "Desparasitación" |
                                                        intervenciones_3_ninos2459 == "Desparasitación" |
                                                        intervenciones_4_ninos2459 == "Desparasitación" |
                                                        intervenciones_5_ninos2459 == "Desparasitación",1,0),
           intervencion_2459_micronutrientes = ifelse(intervenciones_1_ninos2459 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                        intervenciones_2_ninos2459 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                        intervenciones_3_ninos2459 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                        intervenciones_4_ninos2459 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A" |
                                                        intervenciones_5_ninos2459 == "Entrega de micronutrientes: en polvo, sulfato ferroso y/o vitamina A",1,0),
           intervencion_2459_evaluacion_nutricional = ifelse(intervenciones_1_ninos2459 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                               intervenciones_2_ninos2459 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                               intervenciones_3_ninos2459 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                               intervenciones_4_ninos2459 == "Evaluación nutricional: toma de peso y talla y medición del brazo" |
                                                               intervenciones_5_ninos2459 == "Evaluación nutricional: toma de peso y talla y medición del brazo",1,0),
           intervencion_2459_ninguna = ifelse(intervenciones_1_ninos2459 == "Ninguna de las anteriores" |
                                                intervenciones_2_ninos2459 == "Ninguna de las anteriores" |
                                                intervenciones_3_ninos2459 == "Ninguna de las anteriores" |
                                                intervenciones_4_ninos2459 == "Ninguna de las anteriores" |
                                                intervenciones_5_ninos2459 == "Ninguna de las anteriores",1,0),
           intervencion_2459_lactancia_materna = ifelse(intervenciones_1_ninos2459 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                                          intervenciones_2_ninos2459 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                                          intervenciones_3_ninos2459 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                                          intervenciones_4_ninos2459 == "Orientación o información sobre lactancia materna a padres y/o cuidadores" |
                                                          intervenciones_5_ninos2459 == "Orientación o información sobre lactancia materna a padres y/o cuidadores",1,0),
           intervencion_2459_prevencion = ifelse(intervenciones_1_ninos2459 == "Prevención para la desnutrición aguda" |
                                                   intervenciones_2_ninos2459 == "Prevención para la desnutrición aguda" |
                                                   intervenciones_3_ninos2459 == "Prevención para la desnutrición aguda" |
                                                   intervenciones_4_ninos2459 == "Prevención para la desnutrición aguda" |
                                                   intervenciones_5_ninos2459 == "Prevención para la desnutrición aguda",1,0),
           intervencion_2459_tratamiento = ifelse(intervenciones_1_ninos2459 == "PTratamiento de la desnutrición aguda" |
                                                    intervenciones_2_ninos2459 == "Tratamiento de la desnutrición aguda" |
                                                    intervenciones_3_ninos2459 == "Tratamiento de la desnutrición aguda" |
                                                    intervenciones_4_ninos2459 == "Tratamiento de la desnutrición aguda" |
                                                    intervenciones_5_ninos2459 == "Tratamiento de la desnutrición aguda",1,0) )  %>%
    
    
    mutate(intervencion_023_desparasitacion = ifelse(is.na(intervencion_023_desparasitacion) & edad_ninos023 >= 0,0,
                                                     intervencion_023_desparasitacion),
           intervencion_023_micronutrientes = ifelse(is.na(intervencion_023_micronutrientes) & edad_ninos023 >= 0,0,
                                                     intervencion_023_micronutrientes),
           intervencion_023_evaluacion_nutricional = ifelse(is.na(intervencion_023_evaluacion_nutricional) & edad_ninos023 >= 0,0,
                                                            intervencion_023_evaluacion_nutricional),
           intervencion_023_ninguna = ifelse(is.na(intervencion_023_ninguna) & edad_ninos023 >= 0,0,
                                             intervencion_023_ninguna),
           intervencion_023_lactancia_materna = ifelse(is.na(intervencion_023_lactancia_materna) & edad_ninos023 >= 0,0,
                                                       intervencion_023_lactancia_materna),
           intervencion_023_prevencion = ifelse(is.na(intervencion_023_prevencion) & edad_ninos023 >= 0,0,
                                                intervencion_023_prevencion),
           intervencion_023_tratamiento = ifelse(is.na(intervencion_023_tratamiento) & edad_ninos023 >= 0,0,
                                                 intervencion_023_tratamiento),
           
           
           intervencion_2459_desparasitacion = ifelse(is.na(intervencion_2459_desparasitacion) & edad_ninos2459 >= 0,0,
                                                      intervencion_2459_desparasitacion),
           intervencion_2459_micronutrientes = ifelse(is.na(intervencion_2459_micronutrientes) & edad_ninos2459 >= 0,0,
                                                      intervencion_2459_micronutrientes),
           intervencion_2459_evaluacion_nutricional = ifelse(is.na(intervencion_2459_evaluacion_nutricional) & edad_ninos2459 >= 0,0,
                                                             intervencion_2459_evaluacion_nutricional),
           intervencion_2459_ninguna = ifelse(is.na(intervencion_2459_ninguna) & edad_ninos2459 >= 0,0,
                                              intervencion_2459_ninguna),
           intervencion_2459_lactancia_materna = ifelse(is.na(intervencion_2459_lactancia_materna) & edad_ninos2459 >= 0,0,
                                                        intervencion_2459_lactancia_materna),
           intervencion_2459_prevencion = ifelse(is.na(intervencion_2459_prevencion) & edad_ninos2459 >= 0,0,
                                                 intervencion_2459_prevencion),
           intervencion_2459_tratamiento = ifelse(is.na(intervencion_2459_tratamiento) & edad_ninos2459 >= 0,0,
                                                  intervencion_2459_tratamiento)
    
    ) %>% 
    
    mutate(intervencion_059_desparasitacion = case_when(
      intervencion_023_desparasitacion == 1  ~ 1,
      intervencion_2459_desparasitacion == 1  ~ 1,
      TRUE ~ 0
    )) %>% 
    
    mutate(intervencion_059_micronutrientes = case_when(
      intervencion_023_micronutrientes == 1  ~ 1,
      intervencion_2459_micronutrientes == 1  ~ 1,
      TRUE ~ 0
    )) %>% 
    
    mutate(intervencion_059_evaluacion_nutricional = case_when(
      intervencion_023_evaluacion_nutricional == 1  ~ 1,
      intervencion_2459_evaluacion_nutricional == 1  ~ 1,
      TRUE ~ 0
    )) %>% 
    
    mutate(intervencion_059_ninguna = case_when(
      intervencion_023_ninguna == 1  ~ 1,
      intervencion_2459_ninguna == 1  ~ 1,
      TRUE ~ 0
    )) %>% 
    
    mutate(intervencion_059_lactancia_materna = case_when(
      intervencion_023_lactancia_materna == 1  ~ 1,
      intervencion_2459_lactancia_materna == 1  ~ 1,
      TRUE ~ 0
    )) %>% 
    
    mutate(intervencion_059_prevencion = case_when(
      intervencion_023_prevencion == 1  ~ 1,
      intervencion_2459_prevencion == 1  ~ 1,
      TRUE ~ 0
    )) %>% 
    
    mutate(intervencion_059_tratamiento = case_when(
      intervencion_023_tratamiento == 1  ~ 1,
      intervencion_2459_tratamiento == 1  ~ 1,
      TRUE ~ 0
    ))
    
  
}






###############################################################
# NUTRITION
###############################################################
# NINOS DE 0 A 23 MESES
###############################################################
#% de ninos de 0 a 23 meses que han recibido leche materna el dia anterior a la recogida de datos
loop_nutri_ninos023$n1 <- ifelse(loop_nutri_ninos023$recibio_leche_materna_ayer == "si", 1,0)

#% de ninos de 0 a 23 meses que han comido menos de tres veces el dia anterior a la recogida de datos
loop_nutri_ninos023$n2 <- ifelse(loop_nutri_ninos023$nr_alimentos_solidos_ayer %in% c("ninguna", "una_vez", "dos_veces"), 1,0)

#% de ninos de 0 a 23 meses que reciben parte de su alimentacion mediante un biberon o tetero
loop_nutri_ninos023$n3 <- ifelse(loop_nutri_ninos023$uso_biberon == "si", 1,0)


#% de ninos de 0 a 23 meses que NO han recibido intervenciones


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
loop_nutri_ninos023[loop_nutri_ninos023==999.0]<-NA



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
loop_nutri_ninos023$peso_2 <- as.numeric(as.character(loop_nutri_ninos023$peso_2))
loop_nutri_ninos023$talla_2 <- as.numeric(as.character(loop_nutri_ninos023$talla_2))
loop_nutri_ninos023$measurement <- "h"

growth_zscores023 <-  anthro_zscores(
  sex = loop_nutri_ninos023$sexo,
  age = loop_nutri_ninos023$edad,
  is_age_in_month = TRUE,
  weight = loop_nutri_ninos023$peso_2,
  lenhei = loop_nutri_ninos023$talla_2
)

#select zlen (length for age), zwei (weight for age), zwfl (weight for length), zbmi (bmi for age)
growth_zscores023 <- growth_zscores023[,c("zlen", "zwei", "zwfl", "zbmi")]
loop_nutri_ninos023 <- cbind(loop_nutri_ninos023, growth_zscores023)


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



# anemia gestantes

# anemia ninos 0-59

