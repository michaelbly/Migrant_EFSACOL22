
#recoding_preliminary <- function(r, loop) {

r=response
loop_05 <- loop[which(loop$edad < 5), ]
loop_512 <- loop[which(loop$edad <= 12 & loop$edad >= 5), ]

###############################################################
# CARACTERISTICAS SOCIODEMOGRAFICAS
###############################################################
# % de hogares por sexo del jefe del hogar
r$D1_1 <- ifelse(r$sexo_jh == "hombre", 1,0)
r$D1_2 <- ifelse(r$sexo_jh == "mujer", 1,0)


# Promedio miembros del hogar
r$D2 <- as.numeric(r$nr_personas_familia)


# % de hogares con al menos un miembro en estado de embarazo
r$D3 <- ifelse(r$presencia_embarazo == "si", 1, 0)


# % de hogares con al menos un miembro mayor de 65 anos
r$D4 <- ifelse(r$presencia_65 == "si", 1,0)


# % de hogares con al menos un miembro menor de 5 anos
r$D5 <- ifelse(r$presencia_0_59_meses == "si",1,0)


# % de hogares en los que el jefe del hogar NO ha completado la educacion secundario
r$D6 <- ifelse(r$nivel_estudios_jh == "primaria_completa" | r$nivel_estudios_jh == "primaria_incompleta" |
                 r$nivel_estudios_jh == "sin_educacion", 1, 0)


# % de hogares en los que el jefe del hogar tiene una discapacidad
r$D7 <- ifelse(r$discapacidad_jh == "mucha_dificultad" | 
                 r$discapacidad_jh == "no_puede_hacer_nada", 1, 0)


# % de hogares en los que el jefe del hogar padece una enfermedad cronica
r$D8 <- ifelse(r$enfermedad_cronica_jh == "si", 1, 0)


# % de hogares en los que el jefe del hogar padece una enfermedad mental
r$D9 <- ifelse(r$enfermedad_mental_jh == "si", 1, 0)


# % de hogares en los que el encuestador se identifica como afrodescendiente
r$D10 <- ifelse(r$ind1_pertenencia_etnica == "afrodescendiente_negro_mulato_", 1, 0)


# % de hogares en los que el encuestado se identifica como indigena
r$D11 <- ifelse(r$ind1_pertenencia_etnica == "indigena", 1, 0)


# % de hogares en los que el encuestado se identifica como transgenero
r$D12 <- ifelse(r$ind1_genero == "transgenero" | 
                  r$ind1_genero == "mujer_trans", 1, 0)


# % de hogares en los que el jefe del hogar es mayor de 65 anos
# % de hogares en los que el jefe del hogar es monoparental


# % de hogares con al menos un nino menor de 5 anos
r$D17 <- case_when(loop_05$edad[match(r$registro, loop_05$registro)] < 5 ~ 1,
                     TRUE ~ 0)


# % de hogares con al menos dos ninos menores de 5 anos


# % de hogares con al menos un nino entre 5 y 12 anos
r$D19 <- case_when(loop_512$edad[match(r$registro, loop_512$registro)] >= 5 &
                     loop_512$edad[match(r$registro, loop_512$registro)] <= 12 ~ 1,
                   TRUE ~ 0)


# % de hogares con 5 o mas miembros
r$D20 <- ifelse(!is.na(r$ind5_edad), 1,0)


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

r$SA1_poor <- ifelse(r$fcs <= 21, 1,0)
r$SA1_borderline <- ifelse(r$fcs > 21 & r$fcs <=35,1,0)
r$SA1_acceptable <- ifelse(r$fcs > 35,1,0)


# cuota media del gasto en alimentacion (en % del gasto total)


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
      r$lcs_pedir_ayuda %in% c("no__porque_ya_lo_habia_hecho_durante_los_ultimos_12_meses_y_no_podia_seguir_haciendolo", "si") |
    1,
    0
  )

r$SA3_stress <- ifelse(r$stress == 1, 1,0)
r$SA3_crisis <- ifelse(r$crisis == 1, 1,0)
r$SA3_emergency <- ifelse(r$emergency == 1, 1,0)


# % de hogares por situacion de seguridad alimentaria segun la metodologia del CARI


# % de hogares que han comido menos de 3 veces el dia anterior a la recogida de datos
r$SA5 <- ifelse(r$nr_comidas_7d != "3_comidas_o_mas",1,0)


# % de hogares que han comido menos de 3 veces el dia anterior de la recogida de datos
r$SA5 <- ifelse(r$nr_comidas_ayer != "3_comidas_o_mas",1,0)


# % de hogares segun la Household Dietary Diversity Scale


# promedio csi Score de hogares por Coping Strategies Index Score
r$SA8 <- 
  (as.numeric(r$csi_alimentos_menos_preferidos)*1) +(as.numeric(r$csi_pedir_prestados_alimentos)*2) +
  (as.numeric(r$csi_reducir_tamano_porciones)*1) + (as.numeric(r$csi_reducir_adultos)*3)+ 
  (as.numeric(r$csi_reducir_numero_comidas)*1) 



###############################################################
# SITUACION SOCIOECONOMICA DEL HOGAR
###############################################################
# % de hogares en los que una persona distinta del jefe del hogar aporta la mayor parte de los recursos
r$SO1 <- ifelse(r$miembro_mayor_recursos != "jefe_jefa_del_hogar", 1, 0)


# % de hogares segun la fuente prinicipal de ingresos 


# ingresos medios mensuales por miembro del hogar
r$ingreso_pp <- r$ingreso / r$nr_personas_familia
r$SO3 <- r$ingreso_pp


# % de hogares que declaran tener una deuda en el momento de la recogida de datos
r$SO4 <- ifelse(r$deuda == "si",1,0)


# importe medio de la dueda
r$SO5 <- r$valor_deuda


# % de hogares por motivo de su deuda
r$SO6_i <- ifelse(r$razon_deuda == "comprar_comida")
r$SO6_ii <- ifelse(r$razon_deuda == "comprar_insumos_productivos")
r$SO6_iii <- ifelse(r$razon_deuda == "comprar_ropa__zapatos")
r$SO6_iv <- ifelse(r$razon_deuda == "cubrir_gastos_de_salud")
r$SO6_v <- ifelse(r$razon_deuda == "cubrir_servicios_basicos__agua__electricidad_")
r$SO6_vi <- ifelse(r$razon_deuda == "pagar_la_escuela_o_gastos_de_educacion")
r$SO6_vii <- ifelse(r$razon_deuda == "pagar_renta_o_alquiler_de_la_vivienda")
r$SO6_viii <- ifelse(r$razon_deuda == "pagar_viajes")


# % de hogares que declaran haber disminuido sus ingresos en los ultimos 12 meses
r$SO7 <- ifelse(r$cambio_ingresos == "disminuyeron_los_ingresos" | 
                  r$cambio_ingresos == "se_perdieron_los_ingresos_por_completo",
                1,0)


# % de hogares por motivo de empeoramiento de su situacion economica en los ultimos 12 meses
r$SO7_i <- ifelse(r$razon_cambio_ingresos == "algun_miembro_del_hogar_o_grupo_de_viaje_murio")
r$SO7_ii <- ifelse(r$razon_cambio_ingresos == "algun_miembro_del_hogar_o_grupo_de_viaje_perdio_su_empleo_o_redujo_las_horas_de_trabajo")
r$SO7_iii <- ifelse(r$razon_cambio_ingresos == "algun_miembro_del_hogar_o_grupo_de_viaje_se_enfermo_o_esta_incapacitado")
r$SO7_iv <- ifelse(r$razon_cambio_ingresos == "dejo_de_recibir_ayuda_de_familia_o_amigos__incluye_remesas_")
r$SO7_v <- ifelse(r$razon_cambio_ingresos == "dejo_de_recibir_la_asistencia_del_gobierno_o_de_una_organizacion")
r$SO7_vi <- ifelse(r$razon_cambio_ingresos == "los_salarios_se_han_reducido")


# cuota media del gasto en gastos medicos o cuidado en la salud (en % del gasto total)



###############################################################
# SITUACION MIGRATORIA
###############################################################


#return(r)
#}

