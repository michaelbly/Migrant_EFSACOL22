
recoding_preliminary <- function(r, loop) {

r=response
r[r==99]<-NA

loop_05 <- loop[which(loop$edad < 5), ]
loop_512 <- loop[which(loop$edad <= 12 & loop$edad >= 5), ]

###############################################################
# CARACTERISTICAS SOCIODEMOGRAFICAS
###############################################################
# % de hogares por sexo del jefe del hogar
r$d1_1 <- ifelse(r$sexo_jh == "hombre", 1,0)
r$d1_2 <- ifelse(r$sexo_jh == "mujer", 1,0)
r$d1_3 <- ifelse(r$sexo_jh == "otro__cual_",1,0)


# Promedio miembros del hogar
r$d2 <- as.numeric(r$nr_personas_familia)


# % de hogares con al menos un miembro en estado de embarazo
r$d3 <- ifelse(r$presencia_embarazo == "si", 1, 0)


# % de hogares con al menos un miembro mayor de 65 anos
r$d4 <- ifelse(r$presencia_65 == "si", 1,0)


# % de hogares con al menos un miembro menor de 5 anos
r$d5 <- ifelse(r$presencia_0_59_meses == "si",1,0)


# % de hogares por nivel educativo del jefe del hogar
r$d6_i <- ifelse(r$nivel_estudios_jh == "primaria_completa", 1, 0)
r$d6_ii <- ifelse(r$nivel_estudios_jh == "primaria_incompleta", 1, 0)
r$d6_iii <- ifelse(r$nivel_estudios_jh == "secundaria_completa", 1, 0)
r$d6_iv <- ifelse(r$nivel_estudios_jh == "secundaria_incompleta", 1, 0)
r$d6_v <- ifelse(r$nivel_estudios_jh == "sin_educacion", 1, 0)
r$d6_vi <- ifelse(r$nivel_estudios_jh == "tecnico_tecnologico_completo", 1, 0)
r$d6_vii <- ifelse(r$nivel_estudios_jh == "tecnico_tecnologico_incompleto", 1, 0)
r$d6_viii <- ifelse(r$nivel_estudios_jh == "universitario_completo_o_postgrado", 1, 0)
r$d6_iv <- ifelse(r$nivel_estudios_jh == "universitario_incompleto", 1, 0)


# % de hogares en los que el jefe del hogar tiene una discapacidad
r$d7 <- ifelse(r$discapacidad_jh == "mucha_dificultad" | 
                 r$discapacidad_jh == "no_puede_hacer_nada", 1, 0)


# % de hogares en los que el jefe del hogar padece una enfermedad cronica
r$d8 <- ifelse(r$enfermedad_cronica_jh == "si", 1, 0)


# % de hogares en los que el jefe del hogar padece una enfermedad mental
r$d9 <- ifelse(r$enfermedad_mental_jh == "si", 1, 0)


# % de hogares por pertenencia étnica del encuestado
r$d10_i <- ifelse(r$ind1_pertenencia_etnica == "afrodescendiente__negro__mulato_", 1, 0)
r$d10_ii <- ifelse(r$ind1_pertenencia_etnica == "indigena", 1, 0)
r$d10_iii <- ifelse(r$ind1_pertenencia_etnica == "gitano_rrom", 1, 0)
r$d10_iv <- ifelse(r$ind1_pertenencia_etnica == "mestizo", 1, 0)
r$d10_v <- ifelse(r$ind1_pertenencia_etnica == "ninguno", 1, 0)
r$d10_vi <- ifelse(r$ind1_pertenencia_etnica == "palenquero", 1, 0)
r$d10_vii <- ifelse(r$ind1_pertenencia_etnica == "raizal", 1, 0)


# % de hogares en los que el encuestado se identifica como transgenero
r$d11 <- ifelse(r$ind1_genero == "transgenero" | 
                  r$ind1_genero == "mujer_trans", 1, 0)


# % de hogares por rango de edad del jefe 



# % de hogares monoparental



# % de hogares por rangos de tamano del hogar 
r$d15_i <- ifelse(r$nr_personas_familia == 1, 1,0)
r$d15_ii <- ifelse(r$nr_personas_familia > 1 & r$nr_personas_familia <= 2, 1,0)
r$d15_iii <- ifelse(r$nr_personas_familia > 2 & r$nr_personas_familia <= 3, 1,0)
r$d15_iv <- ifelse(r$nr_personas_familia > 3 & r$nr_personas_familia <= 4, 1,0)
r$d15_v <- ifelse(r$nr_personas_familia > 4 & r$nr_personas_familia <= 5, 1,0)
r$d15_vi <- ifelse(r$nr_personas_familia > 5, 1,0)



###############################################################
# ASISTENCIA ESCOLAR
###############################################################
# % de hogares en los que al menos un nino en edad escolar no asiste a la escuela

###############################################################
# SEGURIDAD ALIMENTARIA
###############################################################
# % de hogares con {poor, borderline, acceptable} Food Consumption Score
r$fcs <- 
  (as.numeric(r$fcs_cereales)*2) +(as.numeric(r$fcs_leguminosas)*3) +(as.numeric(r$fcs_leche)*4) + (as.numeric(r$fcs_carne)*4)+ 
  as.numeric(r$fcs_vegetales) + as.numeric(r$fcs_frutas) + (as.numeric(r$fcs_grasas)*0.5) + (as.numeric(r$fcs_azucares)*0.5)

r$sa1_poor <- ifelse(r$fcs <= 28, 1,0)
r$sa1_borderline <- ifelse(r$fcs > 28 & r$fcs <=42,1,0)
r$sa1_acceptable <- ifelse(r$fcs > 42,1,0)


# % de hogares por Coping Strategies Index Score 
r$csi_score <- 
  (as.numeric(r$csi_alimentos_menos_preferidos)*1) +(as.numeric(r$csi_pedir_prestados_alimentos)*2) +
  (as.numeric(r$csi_reducir_tamano_porciones)*1) + (as.numeric(r$csi_reducir_adultos)*3)+ 
  (as.numeric(r$csi_reducir_numero_comidas)*1) 

r$sa2_i <- ifelse(r$csi_score <= 3, 1,0)
r$sa2_ii <- ifelse(r$csi_score > 3 & r$csi_score <=18, 1,0)
r$sa2_iii <- ifelse(r$csi_score > 18, 1,0)


# % de hogares por FCS-CSI Ajuste
r$sa3_i <- ifelse(r$sa1_acceptable == 1 & r$sa2_i == 1, 1,0)
r$sa3_ii <- ifelse(r$sa1_acceptable == 1 & (r$sa2_i == 1 | r$sa2_ii == 1), 1,0)
r$sa3_iii <- ifelse(r$sa1_borderline == 1, 1,0)
r$sa3_iv <- ifelse(r$sa1_poor == 1, 1,0)


# cuota media del gasto en alimentacion (en % del gasto total)
r$exp_food <- as.numeric(apply(r[,c("gastos_cereales", "gastos_tuberculos", "gastos_legumbres", "gastos_vegetales",
                                    "gastos_frutas", "gastos_carne", "gastos_pescado", "gastos_huevos", "gastos_aceite",
                                    "gastos_leche", "gastos_azucar", "gastos_condimentos", "gastos_bebidas_non_alcoholicas",
                                    "gastos_comida_fuera_casa", "gastos_agua_beber")], 
                               1, sum))

r$exp_nonfood_30d <- as.numeric(apply(r[,c("gastos_renta", "gastos_electricidad", "gastos_basura", "gastos_higiene", 
                                  "gastos_transporte", "gastos_comunicacion", "gastos_gasolina", "gastos_otros")], 
                             1, sum))

r$exp_nonfood_6m <- as.numeric(apply(r[,c("gastos_medicos", "gastos_vestimenta", "gastos_educacion", "gastos_deudas", 
                                         "gastos_insumos", "gastos_construccion", "gastos_seguros", "gastos_textiles")], 
                                    1, sum))


r$exp_nonfood <- r$exp_nonfood_30d + (r$exp_nonfood_6m / 6)
r$exp_total <- r$exp_nonfood + r$exp_food

r$sa4 <- r$exp_food / r$exp_total


# % de hogares por puntaje vulnerabilidad economica de CARI
r <- r %>% mutate(sa5 = case_when(
  r$sa4 < 0.5 ~ 1,
  r$sa4 >= 0.5 & r$sa4 < 0.65 ~ 2,
  r$sa4 >= 0.65 & r$sa4 < 0.75 ~ 3,
  r$sa4 >= 0.75 ~ 4
))
r$sa5_i <- ifelse(r$sa5 == 1, 1,0)
r$sa5_ii <- ifelse(r$sa5 == 2, 1,0)
r$sa5_iii <- ifelse(r$sa5 == 3, 1,0)
r$sa5_iv <- ifelse(r$sa5 == 4, 1,0)


# % de hogares que recurren a estrategias de stress/crisis/emergency para hacer frente a la falta de alimentos o de dinero para comprarlos
r$stress <-
  ifelse(
    r$lcs_comprar_credito %in% c("no__porque_ya_lo_habia_hecho_durante_los_ultimos_12_meses_y_no_podia_seguir_haciendolo", "si") |
      r$lcs_gastar_ahorros %in% c("no__porque_ya_lo_habia_hecho_durante_los_ultimos_12_meses_y_no_podia_seguir_haciendolo", "si") |
      r$lcs_enviar_miembros_comer_familia %in% c("no__porque_ya_lo_habia_hecho_durante_los_ultimos_12_meses_y_no_podia_seguir_haciendolo", "si"), 
    1,
    0  
  )

r$crisis <-
  ifelse(
    r$lcs_vender_activos_produccion %in% c("no__porque_ya_lo_habia_hecho_durante_los_ultimos_12_meses_y_no_podia_seguir_haciendolo", "si") |
      r$lcs_reducir_gastos_salud_educacion %in% c("no__porque_ya_lo_habia_hecho_durante_los_ultimos_12_meses_y_no_podia_seguir_haciendolo", "si") |
      r$lcs_sacar_ninos_escuela %in% c("no__porque_ya_lo_habia_hecho_durante_los_ultimos_12_meses_y_no_podia_seguir_haciendolo", "si"),
    1,
    0
  )

r$emergency <-
  ifelse(
    r$lcs_actividades_riesgo %in% c("no__porque_ya_lo_habia_hecho_durante_los_ultimos_12_meses_y_no_podia_seguir_haciendolo", "si") |
      r$lcs_vender_casa %in% c("no__porque_ya_lo_habia_hecho_durante_los_ultimos_12_meses_y_no_podia_seguir_haciendolo", "si") |
      r$lcs_pedir_ayuda %in% c("no__porque_ya_lo_habia_hecho_durante_los_ultimos_12_meses_y_no_podia_seguir_haciendolo", "si"),
    1,
    0
  )

r$sa6_stress <- ifelse(r$stress == 1, 1,0)
r$sa6_crisis <- ifelse(r$crisis == 1, 1,0)
r$sa6_emergency <- ifelse(r$emergency == 1, 1,0)


# % de hogares por LCS para CARI
r$sa7_i <- ifelse(r$sa6_crisis == 0 & r$sa6_emergency == 0 & r$sa6_stress == 0, 1,0)
r$sa7_ii <- ifelse(r$sa6_crisis == 0 & r$sa6_emergency == 0 & r$sa6_stress == 1, 1,0)
r$sa7_iii <- ifelse(r$sa6_crisis == 1 & r$sa6_emergency == 0, 1,0)
r$sa7_iv <- ifelse(r$sa6_emergency == 0, 1,0)



# % de hogares por situacion de seguridad alimentaria segun la metodologia del CARI
r <- r %>% mutate(fcs_cari = case_when(
  r$sa1_poor == 1 ~ 4,
  r$sa1_borderline == 1 ~ 3,
  r$sa1_acceptable == 1 ~ 1
))
r <- r %>% mutate(lcs_cari = case_when(
  r$sa7_i == 1 ~ 1,
  r$sa7_ii == 1 ~ 2,
  r$sa7_iii == 1 ~ 3,
  r$sa7_iv == 1 ~ 4
))
r$fes_cari <- r$sa5

r$cari <- as.numeric(r$fcs_cari * 0.5) + as.numeric(r$lcs_cari * 0.25) + as.numeric(r$fes_cari * 0.25)
r$sa8_sa <- ifelse(r$cari < 1.5,1,0)
r$sa8_sam <- ifelse(r$cari >= 1.5 & r$cari < 2.5,1,0)
r$sa8_iam <- ifelse(r$cari >= 2.5 & r$cari < 3.5,1,0)
r$sa8_ias <- ifelse(r$cari >= 3.5,1,0)



# % de hogares que han comido menos de 3 veces el dia anterior a la recogida de datos
r$sa9 <- ifelse(r$nr_comidas_7d != "3_comidas_o_mas",1,0)


# % de hogares que han comido menos de 3 veces el dia anterior de la recogida de datos
r$sa10 <- ifelse(r$nr_comidas_ayer != "3_comidas_o_mas",1,0)





###############################################################
# SITUACION SOCIOECONOMICA DEL HOGAR
###############################################################
# % de hogares por quien aporta la mayor parte de los recursos
r$so1_i <- ifelse(r$miembro_mayor_recursos == "jefe_jefa_del_hogar", 1, 0)
r$so1_ii <- ifelse(r$miembro_mayor_recursos == "hermano__a__del_jefe__a__del_hogar", 1, 0)
r$so1_iii <- ifelse(r$miembro_mayor_recursos == "hijo_a__hijastro_a__del_jefe__a__del_hogar", 1, 0)
r$so1_iv <- ifelse(r$miembro_mayor_recursos == "nieto_a__del_jefe__a__del_hogar", 1, 0)
r$so1_v <- ifelse(r$miembro_mayor_recursos == "otro_no_pariente_del_jefe__a__del_hogar", 1, 0)
r$so1_vi <- ifelse(r$miembro_mayor_recursos == "otro_pariente_del_jefe__a__del_hogar", 1, 0)
r$so1_vii <- ifelse(r$miembro_mayor_recursos == "padre_o_madre_del_jefe__a__del_hogar", 1, 0)
r$so1_viii <- ifelse(r$miembro_mayor_recursos == "pareja_esposo_a__conyuge_companero_a__del_jefe__a__del_hogar", 1, 0)


# % de hogares segun la fuente prinicipal de ingresos 
r$so2_i <- ifelse(r$fuente_ingresos == "asistencia_de_gobierno_programas_de_proteccion_social_pension",1,0)
r$so2_ii <- ifelse(r$fuente_ingresos == "asistencia_de_naciones_unidas__ongs__organizaciones_caritativas",1,0)
r$so2_iii <- ifelse(r$fuente_ingresos == "comercio_informal_ventas_ambulantes",1,0)
r$so2_iv <- ifelse(r$fuente_ingresos == "nadie_trabaja_en_el_hogar__desempleo_",1,0)
r$so2_v <- ifelse(r$fuente_ingresos == "negocio_propio_independiente__formal_",1,0)
r$so2_vi <- ifelse(r$fuente_ingresos == "trabajo_asalariado_con_salario_regular_en_el_sector_publico_o_privado",1,0)
r$so2_vii <- ifelse(r$fuente_ingresos == "trabajo_de_jornaleo_con_salario_irregular",1,0)
r$so2_viii <- ifelse(r$fuente_ingresos == "trabajo_de_limpieza_de_cuidado_en_casa_de_otras_personas",1,0)

# ingresos medios mensuales por miembro del hogar
r$ingreso_pp <- r$ingreso / r$nr_personas_familia
r$so3 <- r$ingreso_pp


# % de hogares pobres (LP-DANE)
r <- r %>% mutate(so4 = case_when(
  r$ingreso_pp < 396182 & r$urbano_rural == "urbano" ~ 1,
  r$ingreso_pp < 228725 & r$urbano_rural == "rural" ~ 1,
  TRUE ~ 0
))


# % de hogares en pobreza extrema (LPE-DANE)
r <- r %>% mutate(so5 = case_when(
  r$ingreso_pp < 178906 & r$urbano_rural == "urbano" ~ 1,
  r$ingreso_pp < 125291 & r$urbano_rural == "rural" ~ 1,
  TRUE ~ 0
))


# % de hogares que declaran tener una deuda en el momento de la recogida de datos
r$so6 <- ifelse(r$deuda == "si",1,0)


# importe medio de la dueda
r$so7 <- as.numeric(r$valor_deuda)


# % de hogares por motivo de su deuda
r$so8_i <- ifelse(r$razon_deuda == "comprar_comida",1,0)
r$so8_ii <- ifelse(r$razon_deuda == "comprar_insumos_productivos",1,0)
r$so8_iii <- ifelse(r$razon_deuda == "comprar_ropa__zapatos",1,0)
r$so8_iv <- ifelse(r$razon_deuda == "cubrir_gastos_de_salud",1,0)
r$so8_v <- ifelse(r$razon_deuda == "cubrir_servicios_basicos__agua__electricidad_",1,0)
r$so8_vi <- ifelse(r$razon_deuda == "pagar_la_escuela_o_gastos_de_educacion",1,0)
r$so8_vii <- ifelse(r$razon_deuda == "pagar_renta_o_alquiler_de_la_vivienda",1,0)
r$so8_viii <- ifelse(r$razon_deuda == "pagar_viajes",1,0)


# % de hogares que declaran haber disminuido sus ingresos en los ultimos 12 meses
r$so9_i <- ifelse(r$cambio_ingresos == "aumentaron_los_ingresos",1,0)
r$so9_ii <- ifelse(r$cambio_ingresos == "disminuyeron_los_ingresos",1,0)
r$so9_iii <- ifelse(r$cambio_ingresos == "no_hubo_cambios",1,0)
r$so9_iv <- ifelse(r$cambio_ingresos == "se_perdieron_los_ingresos_por_completo",1,0)


# % de hogares por motivo de empeoramiento de su situacion economica en los ultimos 12 meses
r$so10_i <- ifelse(r$razon_cambio_ingresos == "algun_miembro_del_hogar_o_grupo_de_viaje_murio",1,0)
r$so10_ii <- ifelse(r$razon_cambio_ingresos == "algun_miembro_del_hogar_o_grupo_de_viaje_perdio_su_empleo_o_redujo_las_horas_de_trabajo",1,0)
r$so10_iii <- ifelse(r$razon_cambio_ingresos == "algun_miembro_del_hogar_o_grupo_de_viaje_se_enfermo_o_esta_incapacitado",1,0)
r$so10_iv <- ifelse(r$razon_cambio_ingresos == "dejo_de_recibir_ayuda_de_familia_o_amigos__incluye_remesas_",1,0)
r$so10_v <- ifelse(r$razon_cambio_ingresos == "dejo_de_recibir_la_asistencia_del_gobierno_o_de_una_organizacion",1,0)
r$so10_vi <- ifelse(r$razon_cambio_ingresos == "los_salarios_se_han_reducido",1,0)



# cuota media del gasto en renta (en % del gasto total)
r$so11 <- as.numeric(r$gastos_renta / 6) / as.numeric(r$exp_total)

# cuota media del gasto en gastos medicos o cuidado de la salud (en % del gasto total)
r$so12 <- as.numeric(r$gastos_medicos / 6) / as.numeric(r$exp_total)

# cuota media del gasto en educacion (en % del gasto total)
r$so13 <- as.numeric(r$gastos_educacion / 6) / as.numeric(r$exp_total)

# cuota media del gasto en gastos pago de deudas (en % del gasto total)
r$so14 <- as.numeric(r$gastos_deudas / 6) / as.numeric(r$exp_total)



# % de hogares que declaran haber ahorrado dinero en los ultimos 6 meses
r$so15 <- ifelse(r$ahorrado_dinero == "si",1,0)


# cuota media del ahorro
r$so16 <- as.numeric(r$monto_ahorrado)


###############################################################
# SITUACION MIGRATORIA
###############################################################
# % de hogares venezolanos que llegaron hace menos de 6 meses
r$date_assessment <- strptime(as.character(r$fecha_in), "%d_%m_%Y")
r$date_assessment <-  format(r$date_assessment, "%Y-%m-%d")

r$fecha_llegada <- strptime(as.character(r$fecha_llegada_completo), "%d_%m_%Y")
r$fecha_llegada <-  format(r$fecha_llegada, "%Y-%m-%d")

r$diff_dates = difftime(r$date_assessment, r$fecha_llegada, units = "days")
r$m1_i <- ifelse(r$diff_dates <= 180, 1,0)
r$m1_ii <- ifelse(r$diff_dates > 180 & r$diff_dates <= 360, 1,0)
r$m1_iii <- ifelse(r$diff_dates > 360, 1,0)


# % de hogares por documento que posee el jefe del hogar
r$m3_i <- ifelse(grepl("cedula_venezolana_vigente", r$documentos),1,0)
r$m3_ii <- ifelse(grepl("cedula_venezolana_vencida", r$documentos),1,0)
r$m3_iii <- ifelse(grepl("pasaporte_venezolano_vencido", r$documentos),1,0)
r$m3_iv <- ifelse(grepl("pasaporte_venezolano_vigente", r$documentos),1,0)
r$m3_v <- ifelse(grepl("cedula_colombiana", r$documentos),1,0)
r$m3_vi <- ifelse(grepl("registro_civil_de_nacimiento_colombiano", r$documentos),1,0)
r$m3_vii <- ifelse(grepl("registro_civil_de_nacimiento_venezolano", r$documentos),1,0)
r$m3_viii <- ifelse(grepl("ppt", r$documentos),1,0)
r$m3_ix <- ifelse(grepl("pep", r$documentos),1,0)
r$m3_x <- ifelse(grepl("sin_documentos", r$documentos),1,0)


# % de hogares venezolanos sin permiso legal para trabajar en Colombia
r$m4 <- case_when(r$nacionalidad_jefe_hogar %in% c("venezolano", "doble_nacionalidad__colombo__venezolano_") & r$m3_viii == 1 ~ 1,
                  r$nacionalidad_jefe_hogar %in% c("venezolano", "doble_nacionalidad__colombo__venezolano_") & r$m3_v == 1 ~ 1,
                  r$nacionalidad_jefe_hogar %in% c("venezolano", "doble_nacionalidad__colombo__venezolano_") & r$m3_viii == 0 ~ 0,
                  r$nacionalidad_jefe_hogar %in% c("venezolano", "doble_nacionalidad__colombo__venezolano_") & r$m3_v == 0 ~ 0,
                  TRUE ~ NA_real_)



# % de hogares venezolanos que han completado el registro del Estatuto Temporal de Proteccion (ETPV)
r$m5 <- ifelse(r$registracion_ETPV == "si_y_lo_finalizo",1,0)


# % de hogares venezolanos por motivo de no haber completado el registro del Estatuto Temporal de Proteccion (ETPV)
r$m6_i <- case_when(r$registracion_ETPV_porque_no == "falta_documentacion" ~ 1,
                    r$registracion_ETPV_porque_no == "_" ~ NA_real_,
                   TRUE ~ 0)
r$m6_ii <- case_when(r$registracion_ETPV_porque_no == "ha_intentado_registrarse__pero_se_enfrenta_a_otras_barreras" ~ 1,
                    r$registracion_ETPV_porque_no == "_" ~ NA_real_,
                    TRUE ~ 0)
r$m6_iii <- case_when(r$registracion_ETPV_porque_no == "ha_intentado_registrarse__pero_tenia_problemas_tecnicos_con_la_pagina_de_migracion_colombia__ej_deja_de_funcionar_durante_el_registro_" ~ 1,
                    r$registracion_ETPV_porque_no == "_" ~ NA_real_,
                    TRUE ~ 0)
r$m6_iv <- case_when(r$registracion_ETPV_porque_no == "no_conoce_el_proceso" ~ 1,
                    r$registracion_ETPV_porque_no == "_" ~ NA_real_,
                    TRUE ~ 0)
r$m6_v <- case_when(r$registracion_ETPV_porque_no == "no_quiere_registrarse" ~ 1,
                    r$registracion_ETPV_porque_no == "_" ~ NA_real_,
                    TRUE ~ 0)
r$m6_vi <- case_when(r$registracion_ETPV_porque_no == "no_sabe_donde_puede_hacer_el_registro" ~ 1,
                    r$registracion_ETPV_porque_no == "_" ~ NA_real_,
                    TRUE ~ 0)
r$m6_vii <- case_when(r$registracion_ETPV_porque_no == "no_tiene_acceso_al_internet" ~ 1,
                     r$registracion_ETPV_porque_no == "_" ~ NA_real_,
                    TRUE ~ 0)


###############################################################
# ASISTENCIA HUMANITARIA
###############################################################
# % de hogares que declaran haber recibido ayuda de una organizacion no gubernamental en los ultimos 6 meses
r$ah1 <- ifelse(r$asistencia_organizacion == "si",1,0)


# % de hogares que declaran haber recibido ayuda del gobierno en los ultimos 6 meses
r$ah2 <- ifelse(r$asistencia_gobierno == "si",1,0)


# % de hogares que declaran haber recibido ayuda del Programa Mundial de Alimentos en los ultimos 6 meses
r$ah3 <- ifelse(r$asistencia_PMA == "si",1,0)


# % de hogares que declaran haber recibido ayuda de su comunidad, familia o amigos para cubrir el costo de alimentos u otras necesidades en los ultimos 6 meses
r$ah4 <- ifelse(r$asistencia_familia == "si",1,0)



###############################################################
# SITUACION DE LA VIVIENDA Y ACTIVOS DEL HOGAR
###############################################################
# % de hogares por tipo de vivienda
r$v1_i <- ifelse(r$tipo_vivienda == "apartamento",1,0)
r$v1_ii <- ifelse(r$tipo_vivienda == "casa",1,0)
r$v1_iii <- ifelse(r$tipo_vivienda == "habitacion_cuarto_pieza_en_otro_tipo_de_estructura__parqueaderos__depositos__bodegas__iglesias__colegios__fabricas__cuarto_para_portero_o_celador_en_un_edificio_de_apartamentos_",1,0)
r$v1_iv <- ifelse(r$tipo_vivienda == "habitacion_cuarto_pieza_en_un_inquilinato",1,0)
r$v1_v <- ifelse(r$tipo_vivienda == "situacion_de_calle_con_espacio_para_alojarse__carpa__vagon__embarcacion__cueva__refugio_natural__etc__",1,0)
r$v1_vi <- ifelse(r$tipo_vivienda == "situacion_de_calle_sin_espacio_para_alojarse",1,0)
r$v1_vii <- ifelse(r$tipo_vivienda == "vivienda_improvisada__construcciones_informales_con_materiales_menos_durables__cambuches__etc__",1,0)


# % de hogares que declaran tener los siguentes servicios en su vivienda
r$v2_i <- ifelse(r$servicios_acueducto == "si", 1,0)
r$v2_ii <- ifelse(r$servicios_energia_electrica == "si", 1,0)
r$v2_iii <- ifelse(r$servicios_gas == "si", 1,0)
r$v2_iv <- ifelse(r$servicios_alcantarillado == "si", 1,0)
r$v2_v <- ifelse(r$servicios_recoleccion_basura == "si", 1,0)


# % de hogares que reportan que el agua del acueducto llega las 24 horas del dia durante los siete dias de la semana
r$v3 <- ifelse(r$acueducto_24h == "si", 1,0)


# % de hogares en los que todos los miembros del hogar duermen en la misma habitacion
r$v4 <- ifelse(r$nr_cuartos_duermen == 1 & r$nr_personas_familia != 0, 1,0)


# % de hogares en los que hay mas de 2 personas por habitacion
r$personas_por_habitacion <- r$nr_personas_familia / as.numeric(as.character(r$nr_cuartos_total))
r$v5_i <- ifelse(r$personas_por_habitacion == 1,1,0)
r$v5_ii <- ifelse(r$personas_por_habitacion > 1 & r$personas_por_habitacion < 2,1,0)
r$v5_iii <- ifelse(r$personas_por_habitacion > 2,1,0)


# % de hogares que utilizan servicios de saneamiento mejorados
r$v6 <- ifelse(r$tipo_servicio_sanitario %in% c("inodoro_conectado_a_alcantarillado", "inodoro_conectado_a_pozo_septico"),1,0)


# % de hogares con fuentes de agua mejoradas
r$v7 <- ifelse(r$fuente_agua %in% c("aguas_lluvias", "de_pozo_sin_bomba__aljibe__jaguey_o_barreno", "rio__quebrada__nacimiento_o_manantial"),0,1)


# % de hogares que declaran que cocinan en una habitacion que solo se utiliza para cocinar
r$v8 <- ifelse(r$lugar_preparacion_alimentos == "en_un_cuarto_usado_solo_para_cocinar",1,0)


# % de hogares por tipo de energia o combustible con que concinan en su hogar
r$v9_i <- ifelse(r$tipo_energia_cocinar == "electricidad",1,0)
r$v9_ii <- ifelse(r$tipo_energia_cocinar == "gas_natural_conectado_a_red_publica",1,0)
r$v9_iii <- ifelse(r$tipo_energia_cocinar == "gas_propano_en_cilindro_o_pipeta",1,0)
r$v9_iv <- ifelse(r$tipo_energia_cocinar == "lena__madera_o_carbon_de_lena",1,0)
r$v9_v <- ifelse(r$tipo_energia_cocinar == "petroleo__gasolina__kerosene__alcohol",1,0)


# % de hogares por sexo de la persona que tiene el titulo de propriedad de la vivienda
r$v10_i <- ifelse(r$sexo_titulo_propiedad == "hombre",1,0)
r$v10_ii <- ifelse(r$sexo_titulo_propiedad == "mujer",1,0)
r$v10_iii <- ifelse(r$sexo_titulo_propiedad == "ambos__mujer_y_hombre_",1,0)


# % de hogares sin telefono movil
r$v11 <- ifelse(r$bienes_celular == "no",1,0)


# % de hogares que declaran que la tierra o arena es el material principal de los pisos de la vivienda
r$v12 <- ifelse(r$material_pisos == "tierra__arena",1,0)


# % de hogares que declaran que la lata u otros materiales improvisados son el material principal de sus paredes
r$v13 <- ifelse(r$material_paredes_exteriores %in% c("zinc__tela__carton__latas__desechos__plastico", "cana__esterilla__otro_tipo_de_material_vegetal"),1,0)


# % de hogares que declaran que la fuente principal de agua potable es el grifo publico o compartido, el pozo, el rio o el agua lluvia
r$v14 <- ifelse(r$fuente_agua %in% c("aguas_lluvias", "de_pozo_sin_bomba__aljibe__jaguey_o_barreno", "rio__quebrada__nacimiento_o_manantial", "de_pila_publica", "de_pozo_con_bomba"),0,1)


# % de hogares que declaran la deificacion al aire libre o los espacios publicos como su tipo de instalacion de saneamiento


# % de hogares que declaran compartir su instalacion de saneamiento con otros hogares
r$v16 <- ifelse(r$servicio_sanitario_compartido == "compartido_con_personas_de_otros_hogares_o_grupos_de_viaje",1,0)


# % de hogares que declaran que el retrete privado no conectado es su instalacion de saneamiento



###############################################################
# NUTRICION
###############################################################


return(r)
}

