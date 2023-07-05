######################################################################################################################################################################################
######################################################################################################################################################################################
# logit analysis of household characteristics
######################################################################################################################################################################################
######################################################################################################################################################################################
df <- response_with_composites %>% filter(pop_group %in% c("vocacion_de_permanencia"))

Y <- df$cari


X <- df[, c( "d1_1", "d1_2", "d1_3", # sexo del jefe del hogar
             "d16_i", "d16_ii", "d16_iii", "d16_iv", "d16_v",  # estado civil del jefe del hogar
             "d6_i", "d6_ii", "d6_iii", "d6_iv",  # nivel educativo del jefe del hogar
             "d7", # discapacidad jefe del hogar
             "d8", # enfermedad cronica del jefe del hogar
             "d9", # enfermedad mental del jefe del hogar
             "d10_i", "d10_ii", "d10_iii", "d10_iv", "d10_v", "d10_vi", "d10_vii", # pertenencia etnica del respondiente
             "so1_i", "so1_ii", "so1_iii", "so1_iv", "so1_v", "so1_vi", "so1_vii", "so1_viii", # miembro que aporta la mayor parte de los recursos
             "so2_i", "so2_ii", "so2_iii", "so2_iv", "so2_v", "so2_vi", "so2_vii", "so2_viii", "so2_ix", # tipo de empleo
             "so6", # presencia de una deuda
             "so8_i", "so8_ii", "so8_iii", "so8_iv", "so8_v", "so8_vi", "so8_vii", "so8_viii", # motivo de la deuda
             "so9_i", "so9_ii", "so9_iii", "so9_iv",  # cambio de ingresos
             "ah2", # recibio asistencia del gobierno en los utlimos 6 meses
             "v1_i", "v1_ii", "v1_iii", "v1_iv", "v1_v", "v1_vi", "v1_vii", # tipo de la vivienda
             "v12_i", "v12_ii", "v12_iii", "v12_iv", "v12_v", "v12_vi", "v12_vii", "v12_viii", # tipo de los pisos
             "v13_i", "v13_ii", "v13_iii", "v13_iv", "v13_v", "v13_vi", "v13_vii", "v13_viii", "v13_ix", "v13_x", "v13_xi", # material de los paredes exteriores
             "v19_i", "v19_ii", "v19_iii", "v19_iv", "v19_v", "v19_vi", "v19_vii", "v19_viii", "v19_ix", # material del techo
             "v9_i", "v9_ii", "v9_iii", "v9_iv", "v9_v",  # energia para cocinar
             "v2_i", "v2_ii", "v2_iii", "v2_iv", "v2_v",  # servicios
             "v3", # acueducto 24h
             "v8_i", "v8_ii", "v8_iii", "v8_iv", "v8_v", "v8_vi", "v8_vii",  # lugar en que preparan los alimentos
             "v6_i", "v6_ii", "v6_iii", "v6_iv", "v6_v", "v6_vi", "v6_vii", "v6_viii",  # servicio sanitario
             "v7_i", "v7_ii", "v7_iii", "v7_iv", "v7_v", "v7_vi", "v7_vii", "v7_viii", "v7_ix", "v7_x", "v7_xi", # fuente de agua
             "v11_i", "v11_ii", "v11_iii", "v11_iv", "v11_v", "v11_vi", "v11_vii", "v11_viii", "v11_ix", "v11_x", "v11_xi", "v11_xii", "v11_xiii", "v11_xiv", "v11_xv", "v11_xvi", "v11_xvii", "v11_xviii", "v11_xix", "v11_xx", "v11_xxi", # bienes del hogar
             "d17_i", "d17_ii", # urbano rural
             "d18_i", "d18_ii", "d18_iii", "d18_iv", "d18_v",  # edad del jefe del hogar
             "d15_i", "d15_ii", "d15_iii", "d15_iv", "d15_v", "d15_vi",  # tamano del hogar
             "d3", # presencia embarazos
             "d4", # presencia mayores 65
             "d5", # presencia ninos 0-5
             "m4_i", "m4_ii", "m4_iii", # nacionalidad del jefe del hogar
             "m2_i", "m2_ii", "m2_iii", "m2_iv", "m2_v",  # tiempo en el pais
             "m3_i", "m3_ii", "m3_iii", "m3_iv", "m3_v", "m3_vi", "m3_vii", "m3_viii", "m3_ix", # presencia de documentos
             "v17_i", "v17_ii", "v17_iii", "v17_iv", "v17_v", "v17_vi", "v17_vii", # acuerdo de ocupacion
             "d13", # hogar monoparental
             "p4_i", "p4_ii", "p4_iv", "p4_v",  # tasa de depdencencia
             "v4_i", "v4_ii", "v4_iii")]





X_1 <- df[, c("d1_2", "d1_3")] # rc: hombre

X_2 <- df[, c("d16_i", "d16_ii", "d16_iii", "d16_iv")] # rc: vive en union libre

X_3 <- df[, c("d6_i", "d6_ii", "d6_iii")] #rc: universitario

X_4 <- df[, c("d7")] 

X_5 <- df[, c("d8")]

X_6 <- df[, c("d9")]

X_7 <- df[, c("d10_i", "d10_ii", "d10_iii", "d10_iv", "d10_vi", "d10_vii")] #rc: ninguno

X_8 <- df[, c("so1_ii", "so1_iii", "so1_iv", "so1_v", "so1_vi", "so1_vii", "so1_viii")] #rc: jefe del hogar

X_9 <- df[, c("so2_i", "so2_ii", "so2_iii", "so2_iv", "so2_v", "so2_vii", "so2_viii", "so2_ix")] #rc: trabajo_asalariado_con_salario_regular_en_el_sector_publico_o_privado

X_10 <- df[, c("so6")]

X_11 <- df[, c("so8_i", "so8_iii", "so8_iv", "so8_v", "so8_vi", "so8_vii", "so8_viii")] #rc: comprar_insumos_productivos

X_12 <- df[, c("so9_ii", "so9_iii", "so9_iv")] #rc: aumentaron_los_ingresos

X_13 <- df[, c("ah2")]

X_14 <- df[, c("v1_ii", "v1_iii", "v1_iv", "v1_v", "v1_vi", "v1_vii")] #rc: apartamento

X_15 <- df[, c("v12_i", "v12_ii", "v12_iv", "v12_v", "v12_vi", "v12_vii", "v12_viii")] #rc: cemento__gravilla

X_16 <- df[, c("v13_ii", "v13_iii", "v13_iv", "v13_v", "v13_vi", "v13_vii", "v13_viii", "v13_ix", "v13_x", "v13_xi")] #rc: adobe_o_tapia_pisada

X_17 <- df[, c("v19_ii", "v19_iii", "v19_iv", "v19_v", "v19_vi", "v19_vii", "v19_viii", "v19_ix")] #rc: cemento/gravilla

X_18 <- df[, c("v9_ii", "v9_iii", "v9_iv", "v9_v")] #rc: electricidad

X_19 <- df[, c("v2_i", "v2_ii", "v2_iii", "v2_iv", "v2_v")]

X_20 <- df[, c("v3")]

X_21 <- df[, c("v8_ii", "v8_iii", "v8_iv", "v8_v", "v8_vi", "v8_vii")] #rc: en_ninguna_parte__no_preparan_alimentos

X_22 <- df[, c("v6_ii", "v6_iii", "v6_iv", "v6_v", "v6_vi", "v6_vii", "v6_viii")] #rc: bajamar

X_23 <- df[, c("v7_i", "v7_ii", "v7_iii", "v7_iv", "v7_vi", "v7_vii", "v7_viii", "v7_ix", "v7_x", "v7_xi")] #rc: de_acueducto_por_tuberia

X_24 <- df[, c("v11_i", "v11_ii", "v11_iii", "v11_iv", "v11_v", "v11_vi", "v11_vii", "v11_viii", "v11_ix", "v11_x", "v11_xi", "v11_xii", "v11_xiii", "v11_xiv", "v11_xv", "v11_xvi", "v11_xvii", "v11_xviii", "v11_xix", "v11_xx", "v11_xxi")]

X_24 <- df[, c("d17_i")] #rc: urbano

X_25 <- df[, c("d18_ii", "d18_iii", "d18_iv", "d18_v")] #rc: 18-30

X_26 <- df[, c("d15_ii", "d15_iii", "d15_iv", "d15_v", "d15_vi")] #rc: 1 persona

X_27 <- df[, c("d3")]

X_28 <- df[, c("d4")]

X_29 <- df[, c("d5")]

X_30 <- df[, c("m4_ii", "m4_iii")] #rc: venezolano

X_31 <- df[, c("m2_ii", "m2_iii", "m2_iv", "m2_v")] #rc: menos_1m

X_32 <- df[, c("m3_ii", "m3_iii", "m3_iv", "m3_v", "m3_vi", "m3_vii", "m3_viii", "m3_ix")] #rc: cedula_venezolana_vigente

X_33 <- df[, c("v17_ii", "v17_iii", "v17_iv", "v17_v", "v17_vi", "v17_vii")] #rc: en_arriendo_subarriendo

X_34 <- df[, c("d13")]

X_35 <- df[, c("p4_ii", "p4_iv", "p4_v")] #rc: 1

X_36 <- df[, c("v4_ii", "v4_iii")] #rc: una persona o menos


X_completo <- cbind(X_1, X_2, X_3, X_4, X_5, X_6, X_7, X_8, X_9, X_10, X_11, X_12, X_13, X_14, X_15, X_16, X_17, X_18, X_19, X_20, X_21, X_22, X_23, 
                    X_24, X_25, X_26, X_27, X_28, X_29, X_30, X_31, X_32, X_33, X_34, X_35, X_36)

X_completo <- as.matrix(X_completo)

model <- glm(Y ~ X_completo)
summary(model)


########################################################################
# INDIVIDUAL CORRELATION ANALYSIS
#######################################################################
# List of variables for correlation tests (excluding "cari")
df <- r
variable <- df[, c("cari", "d1_1", "d1_2", "d1_3", # sexo del jefe del hogar
                   "d16_i", "d16_ii", "d16_iii", "d16_iv", "d16_v",  # estado civil del jefe del hogar
                   "d6_i", "d6_ii", "d6_iii", "d6_iv",  # nivel educativo del jefe del hogar
                   "d7", # discapacidad jefe del hogar
                   "d8", # enfermedad cronica del jefe del hogar
                   "d9", # enfermedad mental del jefe del hogar
                   "d10_i", "d10_ii", "d10_iii", "d10_iv", "d10_v", "d10_vi", "d10_vii", # pertenencia etnica del respondiente
                   "so1_i", "so1_ii", "so1_iii", "so1_iv", "so1_v", "so1_vi", "so1_vii", "so1_viii", # miembro que aporta la mayor parte de los recursos
                   "so2_i", "so2_ii", "so2_iii", "so2_iv", "so2_v", "so2_vi", "so2_vii", "so2_viii", "so2_ix", # tipo de empleo
                   "so6", # presencia de una deuda
                   "so8_i", "so8_ii", "so8_iii", "so8_iv", "so8_v", "so8_vi", "so8_vii", "so8_viii", # motivo de la deuda
                   "so9_i", "so9_ii", "so9_iii", "so9_iv",  # cambio de ingresos
                   "ah2", # recibio asistencia del gobierno en los utlimos 6 meses
                   "v1_i", "v1_ii", "v1_iii", "v1_iv", "v1_v", "v1_vi", "v1_vii", # tipo de la vivienda
                   "v12_i", "v12_ii", "v12_iii", "v12_iv", "v12_v", "v12_vi", "v12_vii", "v12_viii", # tipo de los pisos
                   "v13_i", "v13_ii", "v13_iii", "v13_iv", "v13_v", "v13_vi", "v13_vii", "v13_viii", "v13_ix", "v13_x", "v13_xi", # material de los paredes exteriores
                   "v19_i", "v19_ii", "v19_iii", "v19_iv", "v19_v", "v19_vi", "v19_vii", "v19_viii", "v19_ix", # material del techo
                   "v9_i", "v9_ii", "v9_iii", "v9_iv", "v9_v",  # energia para cocinar
                   "v2_i", "v2_ii", "v2_iii", "v2_iv", "v2_v",  # servicios
                   "v3", # acueducto 24h
                   "v8_i", "v8_ii", "v8_iii", "v8_iv", "v8_v", "v8_vi", "v8_vii",  # lugar en que preparan los alimentos
                   "v6_i", "v6_ii", "v6_iii", "v6_iv", "v6_v", "v6_vi", "v6_vii", "v6_viii",  # servicio sanitario
                   "v7_i", "v7_ii", "v7_iii", "v7_iv", "v7_v", "v7_vi", "v7_vii", "v7_viii", "v7_ix", "v7_x", "v7_xi", # fuente de agua
                   "v11_i", "v11_ii", "v11_iii", "v11_iv", "v11_v", "v11_vi", "v11_vii", "v11_viii", "v11_ix", "v11_x", "v11_xi", "v11_xii", "v11_xiii", "v11_xiv", "v11_xv", "v11_xvi", "v11_xvii", "v11_xviii", "v11_xix", "v11_xx", "v11_xxi", # bienes del hogar
                   "d17_i", "d17_ii", # urbano rural
                   "d18_i", "d18_ii", "d18_iii", "d18_iv", "d18_v",  # edad del jefe del hogar
                   "d15_i", "d15_ii", "d15_iii", "d15_iv", "d15_v", "d15_vi",  # tamano del hogar
                   "d3", # presencia embarazos
                   "d4", # presencia mayores 65
                   "d5", # presencia ninos 0-5
                   "m4_i", "m4_ii", "m4_iii", # nacionalidad del jefe del hogar
                   "m2_i", "m2_ii", "m2_iii", "m2_iv", "m2_v",  # tiempo en el pais
                   "m3_i", "m3_ii", "m3_iii", "m3_iv", "m3_v", "m3_vi", "m3_vii", "m3_viii", "m3_ix", # presencia de documentos
                   "v17_i", "v17_ii", "v17_iii", "v17_iv", "v17_v", "v17_vi", "v17_vii", # acuerdo de ocupacion
                   "d13", # hogar monoparental
                   "p4_i", "p4_ii", "p4_iii", "p4_iv", "p4_v",  # tasa de depdencencia
                   "v4_i", "v4_ii", "v4_iii", # hacinamiento
                   "m5_i", "m5_ii", "m5_iii", "m5_iv")]


# Calculate correlation between each variable and "cari"
correlation_results <- data.frame(variable_name = character(),
                                  correlation_coefficient = numeric(),
                                  p_value = numeric(),
                                  stringsAsFactors = FALSE)

for (col_name in colnames(variable)) {
  if (col_name != "cari") {
    if (is.numeric(variable[[col_name]])) {
      correlation <- cor.test(variable[[col_name]], variable[["cari"]])
      
      correlation_results <- rbind(correlation_results, 
                                   data.frame(variable_name = col_name,
                                              correlation_coefficient = correlation$estimate,
                                              p_value = correlation$p.value,
                                              stringsAsFactors = FALSE))
    } else {
      warning(paste("Skipping non-numeric variable:", col_name))
    }
  }
}

# View the correlation results
correlation_results$p_value <- format(correlation_results$p_value, scientific = FALSE)
correlation_results$p_value <- round(as.numeric(correlation_results$p_value), 3)
correlation_results$correlation_coefficient <- round(as.numeric(correlation_results$correlation_coefficient), 3)

print(correlation_results)
write.csv(correlation_results, "Output/dataset/targeting/correlation_results_targeting analysis.csv")



############################
#Calculate Cramer's V for all variables
install.packages("vcd")
library(vcd)
library(rcompanion)
# Create a contingency table between the two variables
cont_table <- table(r$presencia_embarazo, cut(r$cari, breaks = 4))  # Adjust the number of breaks as per your requirements

# Calculate Cramer's V
cramer_v <- cramerV(cont_table)
print(cramer_v)




######################################################################################################################################################################################
######################################################################################################################################################################################
# calculate vulnerability scores based on new tool
######################################################################################################################################################################################
######################################################################################################################################################################################
r <- response_with_composites %>% filter(pop_group %in% c("vocacion_de_permanencia"))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_1 = case_when(
  r$sexo_jh == "hombre" ~ 0.5 * 0.01,
  r$sexo_jh == "mujer" ~ 1 * 0.01, 
  r$sexo_jh == "otro__cual_" ~ 1 * 0.01))


r <- r %>% ungroup() %>% dplyr::mutate(vuln_2 = case_when(
  r$nivel_estudios_grupo == "primaria" ~ 1 * 0.071,
  r$nivel_estudios_grupo == "secundaria" ~ 0.5 * 0.071,
  r$nivel_estudios_grupo == "sin_educacion" ~ 1 * 0.071,
  r$nivel_estudios_grupo == "universitario_tecnico" ~ 0 * 0.071
))


r <- r %>% ungroup() %>% dplyr::mutate(vuln_3 = case_when(
  r$discapacidad_jefe == 1 ~ 1 * 0.064,
  r$discapacidad_jefe == 0 ~ 0 * 0.064
))


r <- r %>% ungroup() %>% dplyr::mutate(vuln_4 = case_when(
  r$enfermedad_cronica_jh == "si" ~ 1 * 0.052,
  r$enfermedad_cronica_jh == "no" ~ 0 * 0.052
))



r <- r %>% ungroup() %>% dplyr::mutate(vuln_5 = case_when(
  r$ind1_pertenencia_etnica == "afrodescendiente__negro__mulato_" ~ 0.8 * 0.053,
  r$ind1_pertenencia_etnica == "indigena" ~ 1 * 0.053,
  r$ind1_pertenencia_etnica == "mestizo" ~ 0 * 0.053,
  r$ind1_pertenencia_etnica == "ninguno" ~ 0 * 0.053, 
  r$ind1_pertenencia_etnica == "raizal" ~ 0.8 * 0.053,
  r$ind1_pertenencia_etnica == "ns_nr" ~ 0 * 0.053,
  r$ind1_pertenencia_etnica == "otra__cual_" ~ 0 * 0.053
  
))


r <- r %>% ungroup() %>% dplyr::mutate(vuln_6 = case_when(
  r$tipo_vivienda == "apartamento" ~ 0 * 0.025,
  r$tipo_vivienda == "casa" ~ 0 * 0.025,
  r$tipo_vivienda == "habitacion_cuarto_pieza_en_otro_tipo_de_estructura__parqueaderos__depositos__bodegas__iglesias__colegios__fabricas__cuarto_para_portero_o_celador_en_un_edificio_de_apartamentos_" ~ 0.8 * 0.025,
  r$tipo_vivienda == "habitacion_cuarto_pieza_en_un_inquilinato" ~ 0.8 * 0.025,
  r$tipo_vivienda == "vivienda_improvisada__construcciones_informales_con_materiales_menos_durables__cambuches__etc__" ~ 1 * 0.025, 
  r$tipo_vivienda == "ns_nr__strong__1__strong_" ~ 0 * 0.025, 
  r$tipo_vivienda == "otro__cual_" ~ 0 * 0.025))



r <- r %>% ungroup() %>% dplyr::mutate(vuln_7 = case_when(
  r$material_pisos == "alfombra_o_tapete_de_pared_a_pared" ~ 0 * 0.03,
  r$material_pisos == "baldosin__ladrillo__vinisol__otros_materiales_sinteticos" ~ 0.2 * 0.03,
  r$material_pisos == "cemento__gravilla" ~ 0.3 * 0.03,
  r$material_pisos == "madera_burda__tabla__tablon__otro_vegetal" ~ 0.5 * 0.03,
  r$material_pisos == "madera_pulida" ~ 0 * 0.03,
  r$material_pisos == "marmol" ~ 0 * 0.03,
  r$material_pisos == "ns_nr__strong__1__strong_" ~ 0 * 0.03,
  r$material_pisos == "otro__cual_" ~ 0 * 0.03,
  r$material_pisos == "tierra__arena" ~ 1 * 0.03,
  is.na(r$material_pisos) ~ 0 * 0.03
))



r <- r %>% ungroup() %>% dplyr::mutate(vuln_8 = case_when(
  r$tipo_energia_cocinar == "carbon_mineral" ~ 0.8 * 0.031,
  r$tipo_energia_cocinar == "electricidad" ~ 0 * 0.031,
  r$tipo_energia_cocinar == "gas_natural_conectado_a_red_publica" ~ 0 * 0.031,
  r$tipo_energia_cocinar == "gas_propano_en_cilindro_o_pipeta" ~ 0.5 * 0.031,
  r$tipo_energia_cocinar == "lena__madera_o_carbon_de_lena" ~ 1 * 0.031,
  r$tipo_energia_cocinar == "no_cocinan" ~ 1 * 0.031,
  r$tipo_energia_cocinar == "petroleo__gasolina__kerosene__alcohol" ~ 0.7 * 0.031
))



r <- r %>% ungroup() %>% dplyr::mutate(vuln_9 = case_when(
  r$acueducto_24h == "si" ~ 0 * 0.037,
  r$acueducto_24h == "no" ~ 1 * 0.037,
  is.na(r$acueducto_24h) ~ 0 * 0.037))


r <- r %>% ungroup() %>% dplyr::mutate(vuln_10 = case_when(
  r$lugar_preparacion_alimentos == "en_ninguna_parte__no_preparan_alimentos" ~ 0.5 * 0.027,
  r$lugar_preparacion_alimentos == "en_un_cuarto_usado_solo_para_cocinar" ~ 0 * 0.027,
  r$lugar_preparacion_alimentos == "en_un_cuarto_usado_tambien_para_dormir" ~ 1 * 0.027,
  r$lugar_preparacion_alimentos == "en_un_patio__corredor__enramada__al_aire_libre" ~ 0.8 * 0.027,
  r$lugar_preparacion_alimentos == "en_una_sala_comedor_con_lavaplatos" ~ 0.2 * 0.027,
  r$lugar_preparacion_alimentos == "en_una_sala_comedor_sin_lavaplatos" ~ 0.4 * 0.027,
  r$lugar_preparacion_alimentos == "ns_nr__strong__1__strong_" ~ 0 * 0.027, 
  is.na(r$lugar_preparacion_alimentos) ~ 0 * 0.027
))


r <- r %>% ungroup() %>% dplyr::mutate(vuln_11 = case_when(
  r$tipo_servicio_sanitario == "bajamar" ~ 0.8 * 0.02,
  r$tipo_servicio_sanitario == "inodoro_conectado_a_alcantarillado" ~ 0 * 0.02,
  r$tipo_servicio_sanitario == "inodoro_conectado_a_pozo_septico" ~ 0.5 * 0.02,
  r$tipo_servicio_sanitario == "inodoro_sin_conexion" ~ 0.9 * 0.02,
  r$tipo_servicio_sanitario == "letrina" ~ 0.9 * 0.02,
  r$tipo_servicio_sanitario == "no_tiene_servicio_sanitario" ~ 1 * 0.02,
  r$tipo_servicio_sanitario == "ns_nr__strong__1__strong_" ~ 0 * 0.02
))


r <- r %>% ungroup() %>% dplyr::mutate(vuln_12 = case_when(
  r$fuente_agua == "agua_embotellada_o_en_bolsa" ~ 1 * 0.015,
  r$fuente_agua == "aguas_lluvias" ~ 1 * 0.015,
  r$fuente_agua == "aguatero" ~ 1 * 0.015,
  r$fuente_agua == "carrotanque" ~ 1 * 0.015,
  r$fuente_agua == "de_acueducto_por_tuberia" ~ 0 * 0.015,
  r$fuente_agua == "otro" ~ 1 * 0.015,
  r$fuente_agua == "de_otra_fuente_por_tuberia" ~ 0.2 * 0.015,
  r$fuente_agua == "rio__quebrada__nacimiento_o_manantial" ~ 1 * 0.015,
  r$fuente_agua == "de_pila_publica" ~ 1 * 0.015,
  r$fuente_agua == "de_pozo_con_bomba" ~ 1 * 0.015,
  r$fuente_agua == "de_pozo_sin_bomba__aljibe__jaguey_o_barreno" ~ 1 * 0.015
))



r <- r %>% ungroup() %>% dplyr::mutate(vuln_13 = case_when(
  r$bienes_cama == "si" ~ 0 * 0.031,
  r$bienes_cama == "no" ~ 1 * 0.031, 
  is.na(r$bienes_cama) ~ 0 * 0.031))


r <- r %>% ungroup() %>% dplyr::mutate(vuln_14 = case_when(
  r$bienes_nevera == "si" ~ 0 * 0.031,
  r$bienes_nevera == "no" ~ 1 * 0.031, 
  is.na(r$bienes_nevera) ~ 0 * 0.031))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_15 = case_when(
  r$bienes_celular == "si" ~ 0 * 0.015,
  r$bienes_celular == "no" ~ 1 * 0.015, 
  is.na(r$bienes_celular) ~ 0 * 0.015))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_16 = case_when(
  r$bienes_mesa == "si" ~ 0 * 0.023,
  r$bienes_mesa == "no" ~ 1 * 0.023, 
  is.na(r$bienes_mesa) ~ 0 * 0.023))



r <- r %>% ungroup() %>% dplyr::mutate(vuln_17 = case_when(
  r$acuerdo_ocupacion == "en_arriendo_subarriendo" ~ 0.6 * 0.015,
  r$acuerdo_ocupacion == "paga_diario" ~ 0.8 * 0.015,
  r$acuerdo_ocupacion == "posesion_sin_titulo__ocupante_de_hecho__o_propiedad_colectiva" ~ 0.6 * 0.015,
  r$acuerdo_ocupacion == "propia__la_estan_pagando" ~ 0.3 * 0.015,
  r$acuerdo_ocupacion == "propia__totalmente_pagada" ~ 0 * 0.015))


r <- r %>% ungroup() %>% dplyr::mutate(vuln_18 = case_when(
  r$p4_i == 1 ~ 0 * 0.1,
  r$p4_ii == 1 ~ 0.5 * 0.1,
  r$p4_iii == 1 ~ 0.7 * 0.1,
  r$p4_iv == 1 ~ 0.8 * 0.1,
  r$p4_v == 1 ~ 1 * 0.1))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_19 = case_when(
  r$v4_i == 1 ~ 0 * 0.1,
  r$v4_ii == 1 ~ 0.5 * 0.1,
  r$v4_iii == 1 ~ 0.7 * 0.1, 
  r$v4_iv == 1 ~ 1 * 0.1))



#############################################################################


r$vuln_final <- (r$vuln_1)+(r$vuln_2)+(r$vuln_3)+(r$vuln_6)+(r$vuln_7)+(r$vuln_8)+(r$vuln_9)+(r$vuln_10)+(r$vuln_11)+
  (r$vuln_12)+(r$vuln_13)+(r$vuln_14)+(r$vuln_17)+(r$vuln_18)+(r$vuln_19)
  
table(r$vuln_final)
hist(r$vuln_final)
summary(r$vuln_final)
cor.test(r$vuln_final, r$cari)



r <- r %>% dplyr::mutate(categoria_vulnerabilidad = case_when(
  r$vuln_final < 0.1805  ~ "categoria_1",
  r$vuln_final > 0.1805   & r$vuln_final < 0.2255 ~ "categoria_2",
  r$vuln_final > 0.2255 & r$vuln_final < 0.2784 ~ "categoria_3",
  r$vuln_final > 0.2784 ~ "categoria_4"
))
table(r$categoria_vulnerabilidad, r$cari_categories)

# Create the scatter plot
ggplot(r, aes(x = vuln_final, y = cari)) +
  geom_point(color = "steelblue", size = 3) +  # Set point color and size
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  # Add the regression line with error margin
  labs(x = "Puntaje Vulnerabilidad", y = "CARI") +
  theme_minimal(base_family = "Arial") +  # Set theme to minimal with Arial font
  theme(plot.title = element_text(size = 16, face = "bold"),  # Customize title
        axis.title = element_text(size = 12, face = "bold"),  # Customize axis labels
        axis.text = element_text(size = 10),  # Customize axis tick labels
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10)) 





######################################################################################################################################################################################
######################################################################################################################################################################################
# create different combinations of 10 targeting indicators
######################################################################################################################################################################################
######################################################################################################################################################################################
r <- r %>% filter(tiempo_en_pais %in% c("menos_6m", "12m-24m", "6m-12m"))
r <- r %>% filter(tiempo_en_pais %in% c("24m-48m", "48m_72m", "mas_72m"))


# Create a sample data frame
profile_ten <- r %>% select("cari", "vuln_1", "vuln_2", "vuln_3", "vuln_4", 
                            "vuln_5", "vuln_6", "vuln_7", "vuln_8", "vuln_9", "vuln_10", 
                            "vuln_11", "vuln_12", "vuln_13", "vuln_15", "vuln_16", "vuln_17", "vuln_18", 
                            "vuln_19", "vuln_20", "vuln_21", "vuln_23", "vuln_24", "vuln_25", "vuln_26",
                            "vuln_28", "vuln_29", "vuln_30", "vuln_31")

#reduced list
profile_ten <- r %>% select("cari", "vuln_1", "vuln_2", "vuln_3", 
                            "vuln_7", "vuln_8", "vuln_9", "vuln_10", "vuln_12", "vuln_13", "vuln_14", "vuln_15", "vuln_16", "vuln_17", "vuln_18", 
                            "vuln_19", "vuln_20", "vuln_21", "vuln_24", "vuln_25", "vuln_29", "vuln_30")
library(dplyr)

# Load your data
# Select the numerical columns
num_cols <- names(profile_ten)[2:29]

# Get all combinations of 10 numerical columns
combs <- combinat::combn(num_cols, 8, simplify = FALSE)

# Calculate row sums of all combinations and store results in a list
results <- lapply(combs, function(x) {
  rowSums(profile_ten[, x], na.rm=T)
})
 
# Combine results into a data frame with column names as variable names
results_df <- as.data.frame(do.call(cbind, results))
results_df <- results_df %>% set_names(sapply(combs, paste, collapse = "_"))

results_df <- profile_ten %>% select("cari") %>% 
  cbind(results_df)


library(DescTools)

# Calculate correlation for each column
correlations <- apply(results_df[, 2:ncol(results_df)], 2, function(x) cor.test(results_df[, 1], x)$estimate)
cor_df <- data.frame(column_name = names(results_df)[2:ncol(results_df)], correlation = correlations)




##################
#create wordcloud
# keep only the first 150 rows
cor_df <- cor_df[order(cor_df$correlation, decreasing = TRUE), ]
df_subset_pair <- head(cor_df, n = 200)

# convert var_combinations to a tidy format
#wordcloud(df_subset_pair$var_combinations, max.words = 10, random.order = FALSE, colors=brewer.pal(8, "Greys"), rot.per = 0.15)

# count the most frequent characteristics
df_long <- separate_rows(df_subset_pair, column_name, sep = "_")

# Count frequency of individual strings
df_freq <- df_long %>% dplyr::count(column_name, sort = TRUE)






#########################################################################
############################
#test good
r$vuln_final_good <- (r$vuln_1)+(r$vuln_2)+(r$vuln_7)+(r$vuln_9)+(r$vuln_10)+(r$vuln_13)+(r$vuln_14)+(r$vuln_17)+(r$vuln_18)+(r$vuln_19)+(r$vuln_20)+(r$vuln_21)+(r$vuln_24)+(r$vuln_25)+(r$vuln_29)+(r$vuln_30)

table(r$vuln_final_good)
hist(r$vuln_final_good)
summary(r$vuln_final_good)


r <- r %>% dplyr::mutate(categoria_vulnerabilidad_good = case_when(
  r$vuln_final_good < -0.1365 ~ "categoria_1",
  r$vuln_final_good > -0.1365 & r$vuln_final_good < -0.0659 ~ "categoria_2",
  r$vuln_final_good > -0.0659 & r$vuln_final_good < 0.0107 ~ "categoria_3",
  r$vuln_final_good > 0.0107 ~ "categoria_4"
))
table(r$categoria_vulnerabilidad_good, r$cari_categories)
cor.test(r$vuln_final_good, r$cari)


############################
#test best best
r$vuln_final_best <- (r$vuln_1)+(r$vuln_2)+(r$vuln_3)+(r$vuln_5)+(r$vuln_6)+(r$vuln_7)+(r$vuln_13)+(r$vuln_14)+(r$vuln_17)+(r$vuln_18)+
  (r$vuln_20)+(r$vuln_21)+(r$vuln_24)+(r$vuln_25)+(r$vuln_29)+(r$vuln_30)
cor.test(r$vuln_final_best, r$cari)

table(r$vuln_final_best)
hist(r$vuln_final_bad)
summary(r$vuln_final_best)


r <- r %>% dplyr::mutate(categoria_vulnerabilidad_best = case_when(
  r$vuln_final_best < -0.1034 ~ "categoria_1",
  r$vuln_final_best > -0.1034 & r$vuln_final_best < -0.0169 ~ "categoria_2",
  r$vuln_final_best > -0.0169 & r$vuln_final_best < 0.075 ~ "categoria_3",
  r$vuln_final_best > 0.075 ~ "categoria_4"
))
table(r$categoria_vulnerabilidad_best, r$cari_categories)
table(r$categoria_vulnerabilidad_good, r$cari_category)

# Create the scatter plot
ggplot(r, aes(x = vuln_final_best, y = cari)) +
  geom_point(color = "steelblue", size = 3) +  # Set point color and size
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  # Add the regression line with error margin
  labs(x = "Puntaje Vulnerabilidad", y = "CARI") +
  theme_minimal(base_family = "Arial") +  # Set theme to minimal with Arial font
  theme(plot.title = element_text(size = 16, face = "bold"),  # Customize title
        axis.title = element_text(size = 12, face = "bold"),  # Customize axis labels
        axis.text = element_text(size = 10),  # Customize axis tick labels
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10)) 

##################
#create wordcloud
# keep only the first 150 rows
cor_df <- cor_df[order(cor_df$correlation, decreasing = TRUE), ]
df_subset_pair <- head(cor_df, n = 200)

# convert var_combinations to a tidy format
#wordcloud(df_subset_pair$var_combinations, max.words = 10, random.order = FALSE, colors=brewer.pal(8, "Greys"), rot.per = 0.15)

# count the most frequent characteristics
df_long <- separate_rows(df_subset_pair, column_name, sep = "_")

# Count frequency of individual strings
df_freq <- df_long %>% dplyr::count(column_name, sort = TRUE)














########################################################################
# INDIVIDUAL CORRELATION ANALYSIS
#######################################################################
# List of variables for correlation tests (excluding "cari")
variable <- df[, c("cari", "d1_1", "d1_2", "d1_3", # sexo del jefe del hogar
                       "d16_i", "d16_ii", "d16_iii", "d16_iv", "d16_v",  # estado civil del jefe del hogar
                       "d6_i", "d6_ii", "d6_iii", "d6_iv",  # nivel educativo del jefe del hogar
                       "d7", # discapacidad jefe del hogar
                       "d8", # enfermedad cronica del jefe del hogar
                       "d9", # enfermedad mental del jefe del hogar
                       "d10_i", "d10_ii", "d10_iii", "d10_iv", "d10_v", "d10_vi", "d10_vii", # pertenencia etnica del respondiente
                       "so1_i", "so1_ii", "so1_iii", "so1_iv", "so1_v", "so1_vi", "so1_vii", "so1_viii", # miembro que aporta la mayor parte de los recursos
                       "so2_i", "so2_ii", "so2_iii", "so2_iv", "so2_v", "so2_vi", "so2_vii", "so2_viii", "so2_ix", # tipo de empleo
                       "so6", # presencia de una deuda
                       "so8_i", "so8_ii", "so8_iii", "so8_iv", "so8_v", "so8_vi", "so8_vii", "so8_viii", # motivo de la deuda
                       "so9_i", "so9_ii", "so9_iii", "so9_iv",  # cambio de ingresos
                       "ah2", # recibio asistencia del gobierno en los utlimos 6 meses
                       "v1_i", "v1_ii", "v1_iii", "v1_iv", "v1_v", "v1_vi", "v1_vii", # tipo de la vivienda
                       "v12_i", "v12_ii", "v12_iii", "v12_iv", "v12_v", "v12_vi", "v12_vii", "v12_viii", # tipo de los pisos
                       "v13_i", "v13_ii", "v13_iii", "v13_iv", "v13_v", "v13_vi", "v13_vii", "v13_viii", "v13_ix", "v13_x", "v13_xi", # material de los paredes exteriores
                       "v19_i", "v19_ii", "v19_iii", "v19_iv", "v19_v", "v19_vi", "v19_vii", "v19_viii", "v19_ix", # material del techo
                       "v9_i", "v9_ii", "v9_iii", "v9_iv", "v9_v",  # energia para cocinar
                       "v2_i", "v2_ii", "v2_iii", "v2_iv", "v2_v",  # servicios
                       "v3", # acueducto 24h
                       "v8_i", "v8_ii", "v8_iii", "v8_iv", "v8_v", "v8_vi", "v8_vii",  # lugar en que preparan los alimentos
                       "v6_i", "v6_ii", "v6_iii", "v6_iv", "v6_v", "v6_vi", "v6_vii", "v6_viii",  # servicio sanitario
                       "v7_i", "v7_ii", "v7_iii", "v7_iv", "v7_v", "v7_vi", "v7_vii", "v7_viii", "v7_ix", "v7_x", "v7_xi", # fuente de agua
                       "v11_i", "v11_ii", "v11_iii", "v11_iv", "v11_v", "v11_vi", "v11_vii", "v11_viii", "v11_ix", "v11_x", "v11_xi", "v11_xii", "v11_xiii", "v11_xiv", "v11_xv", "v11_xvi", "v11_xvii", "v11_xviii", "v11_xix", "v11_xx", "v11_xxi", # bienes del hogar
                       "d17_i", "d17_ii", # urbano rural
                       "d18_i", "d18_ii", "d18_iii", "d18_iv", "d18_v",  # edad del jefe del hogar
                       "d15_i", "d15_ii", "d15_iii", "d15_iv", "d15_v", "d15_vi",  # tamano del hogar
                       "d3", # presencia embarazos
                       "d4", # presencia mayores 65
                       "d5", # presencia ninos 0-5
                       "m4_i", "m4_ii", "m4_iii", # nacionalidad del jefe del hogar
                       "m2_i", "m2_ii", "m2_iii", "m2_iv", "m2_v",  # tiempo en el pais
                       "m3_i", "m3_ii", "m3_iii", "m3_iv", "m3_v", "m3_vi", "m3_vii", "m3_viii", "m3_ix", # presencia de documentos
                       "v17_i", "v17_ii", "v17_iii", "v17_iv", "v17_v", "v17_vi", "v17_vii", # acuerdo de ocupacion
                       "d13", # hogar monoparental
                       "p4_i", "p4_ii", "p4_iv", "p4_v",  # tasa de depdencencia
                       "v4_i", "v4_ii", "v4_iii")]


# Calculate correlation between each variable and "cari"
correlation_results <- data.frame(variable_name = character(),
                                  correlation_coefficient = numeric(),
                                  p_value = numeric(),
                                  stringsAsFactors = FALSE)

for (col_name in colnames(variable)) {
  if (col_name != "cari") {
    if (is.numeric(variable[[col_name]])) {
      correlation <- cor.test(variable[[col_name]], variable[["cari"]])
      
      correlation_results <- rbind(correlation_results, 
                                   data.frame(variable_name = col_name,
                                              correlation_coefficient = correlation$estimate,
                                              p_value = correlation$p.value,
                                              stringsAsFactors = FALSE))
    } else {
      warning(paste("Skipping non-numeric variable:", col_name))
    }
  }
}

# View the correlation results
correlation_results$p_value <- format(correlation_results$p_value, scientific = FALSE)
correlation_results$p_value <- round(as.numeric(correlation_results$p_value), 3)
correlation_results$correlation_coefficient <- round(as.numeric(correlation_results$correlation_coefficient), 3)

print(correlation_results)








########################################################################
# EXISTING TARGETING TOOL
#######################################################################
df <- response_with_composites %>% filter(pop_group %in% c("vocacion_de_permanencia"))

Y <- df$cari

X <- df[, c("d1_2", # sexo del jefe del hogar
                   "d6_i", "d6_ii",  # nivel educativo del jefe del hogar
                   "d7", # discapacidad jefe del hogar
                   "d8", # enfermedad cronica del jefe del hogar
                   "d9", # enfermedad mental del jefe del hogar
                   "d10_i", "d10_ii", "d10_iii", "d10_iv", # pertenencia etnica del respondiente
                   "d18_v",  # edad del jefe del hogar
                   "d3", # presencia embarazos
                   "p4_iii", "p4_iv",  # tasa de depdencencia
                   "d5_i", "d5_ii", # presencia ninos 0-5
                   "v4_iii", "v4_iv", # hacinamiento
                   "v19_i", "v19_ii", "v19_iii", "v19_iv", # tipo de la vivienda
                   "v12", # tipo de los pisos
                   "v13_ii", "v13_iii", "v13_iv", "v13_vi", "v13_x", "v13_xi", # material de los paredes exteriores
                   "v7_i", "v7_ii", "v7_iii", "v7_iv", "v7_vi", "v7_vii", "v7_viii", "v7_ix", # fuente de agua
                   "v6_i", "v6_iv", "v6_v", "v6_vi",  # servicio sanitario
                   "v9_iii", "v9_iv", "v9_v",  # energia para cocinar
                   "v11_viii", # bienes del hogar celular
                   "m2_i", "m2_ii", "m2_iii", "m2_iv",  # tiempo en el pais
                   "m4")]


X <- as.matrix(X)

model <- glm(Y ~ X)
summary(model)

model <- glm(Y ~ X)

r <- df


r <- r %>% ungroup() %>% dplyr::mutate(vuln_1 = case_when(
  r$sexo_jh == "mujer" ~ 1.4,
  TRUE ~ 0))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_2 = case_when(
  r$discapacidad_jh == 1 ~ 5,
  TRUE ~ 0
))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_3 = case_when(
  r$nivel_estudios_jh == "sin_educacion" ~ 1.3,
  r$nivel_estudios_jh == "primaria_incompleta" ~ 1.3,
  r$nivel_estudios_jh == "primaria_completa" ~ 1.3,
  r$nivel_estudios_jh == "secundaria_incompleta" ~ 1.3,
  TRUE ~ 0
))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_4 = case_when(
  r$d18_v == 1 ~ 4,
  TRUE ~ 0
))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_5 = case_when(
  r$ind1_pertenencia_etnica == "indigena" ~ 1.6,
  r$ind1_pertenencia_etnica == "mestizo" ~ 1.4,
  r$ind1_pertenencia_etnica == "afrodescendiente__negro__mulato_" ~ 1.4,
  r$ind1_pertenencia_etnica == "gitano_rrom" ~ 1.4,
  TRUE ~ 0
))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_6 = case_when(
  r$presencia_embarazo == 1 ~ 1.8,
  TRUE ~ 0
))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_7 = case_when(
  r$enfermedad_cronica_jh == "si" ~ 1.7,
  TRUE ~ 0
))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_8 = case_when(
  r$enfermedad_mental_jh == "si" ~ 1.7,
  TRUE ~ 0
))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_9 = case_when(
  r$p4_iii == 1 ~ 1.26,
  r$p4_iv == 1 ~ 1.7,
  TRUE ~ 0
))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_10 = case_when(
  r$d5_i == 1 ~ 1.6,
  r$d5_ii == 1 ~ 2,
  TRUE ~ 0
))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_11 = case_when(
  r$v4_iii == 1 ~ 1.6,
  r$v4_iv == 1 ~ 2,
  TRUE ~ 0
))

r <- r %>% dplyr::mutate(vuln_12 = case_when(
  r$v19_i == 1 | r$v19_ii == 1 | r$v19_iii == 1 | r$v19_iv == 1 ~ 1.6,
  TRUE ~ 0))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_13 = case_when(
  r$v12 == 1 ~ 1.5,
  TRUE ~ 0
))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_14 = case_when(
  r$v13_ii == 1 ~ 1.5,
  r$v13_vi == 1 ~ 1,
  r$v13_iv == 1 ~ 1.5,
  r$v13_iii == 1 ~ 1.5,
  r$v13_xi == 1 ~ 1.5,
  r$v13_x == 1 ~ 1.5,
  TRUE ~ 0
))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_15 = case_when(
  r$v7_vi == 1 ~ 1.4,
  r$v7_viii == 1 ~ 1.4,
  r$v7_ix == 1 ~ 1.4,
  r$v7_ii == 1 ~ 1.4,
  r$v7_xii == 1 ~ 1.4,
  r$v7_vii == 1 ~ 1.4,
  r$v7_iv == 1 ~ 1.4,
  r$v7_iii == 1 ~ 1.4,
  r$v7_i == 1 ~ 1.4,
  TRUE ~ 0
))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_15 = case_when(
  r$v6_vi == 1 ~ 1.8,
  r$v6_iv == 1 ~ 1.5,
  r$v6_v == 1 ~ 1.5,
  r$v6_i == 1 ~ 1.5,
  TRUE ~ 0
))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_16 = case_when(
  r$v9_iv == 1 ~ 1.8,
  r$v9_v == 1 ~ 1.8,
  r$v9_iii == 1 ~ 1.2,
  TRUE ~ 0))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_17 = case_when(
  r$v11_viii == 0 ~ 1.8,
  TRUE ~ 0))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_18 = case_when(
  r$m2_i == 1 ~ 1.7,
  r$m2_ii == 1 ~ 1.7,
  r$m2_iii == 1 ~ 1.7,
  r$m2_iv == 1 ~ 1.35,
  TRUE ~ 0))

r <- r %>% ungroup() %>% dplyr::mutate(vuln_19 = case_when(
  r$m4 == 1 ~ 1.35,
  TRUE ~ 0))


r$vuln_final <- (r$vuln_1)+(r$vuln_2)+(r$vuln_3)+(r$vuln_4)+(r$vuln_5)+(r$vuln_6)+(r$vuln_7)+(r$vuln_8)+(r$vuln_9)+(r$vuln_10)+(r$vuln_11)+(r$vuln_12)+(r$vuln_13)+(r$vuln_14)+(r$vuln_15)+(r$vuln_16)+(r$vuln_17)+(r$vuln_18)+(r$vuln_19)
cor.test(r$vuln_final, r$cari)


summary(r$vuln_final)


r <- r %>% dplyr::mutate(categoria_vulnerabilidad_tool = case_when(
  r$vuln_final < 4.3 ~ "categoria_1",
  r$vuln_final > 4.3 & r$vuln_final < 5.95 ~ "categoria_2",
  r$vuln_final > 5.95 & r$vuln_final < 8 ~ "categoria_3",
  r$vuln_final > 8 ~ "categoria_4"
))
table(r$categoria_vulnerabilidad_tool, r$cari_categories)





# Create the scatter plot
ggplot(r, aes(x = vuln_final, y = cari)) +
  geom_point(color = "steelblue", size = 3) +  # Set point color and size
  geom_smooth(method = "lm", color = "blue", se = TRUE) +  # Add the regression line with error margin
  labs(x = "Puntaje Vulnerabilidad", y = "CARI") +
  theme_minimal(base_family = "Arial") +  # Set theme to minimal with Arial font
  theme(plot.title = element_text(size = 16, face = "bold"),  # Customize title
        axis.title = element_text(size = 12, face = "bold"),  # Customize axis labels
        axis.text = element_text(size = 10),  # Customize axis tick labels
        legend.title = element_blank(),  # Remove legend title
        legend.text = element_text(size = 10)) 



