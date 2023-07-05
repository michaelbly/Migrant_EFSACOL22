library(ggplot2)
library(ggpubr)

r <- response_with_composites

##########################################################################################################################
##########################################################################################################################
# RENAME THE EXPRESSIONS OF VARIABLES WITH LONG NAMES THAT ARE DIFFICULT TO BE DISPLAYED IN A SINGLE CHART
##########################################################################################################################
##########################################################################################################################

r <- r %>% 
  mutate(sexo_jh = dplyr::recode(sexo_jh, "ns_nr__strong__1__strong_" = "ns_nr", 
                                         "otro__cual_" = "otro"))


r <- r %>% 
  mutate(discapacidad_jh = dplyr::recode(discapacidad_jh, "no_responde__strong__1__strong_" = "ns_nr"))


r <- r %>% 
  mutate(deuda = dplyr::recode(deuda, "ns_nr__strong__1__strong_" = "ns_nr"))


r <- r %>% 
  mutate(acueducto_24h = dplyr::recode(acueducto_24h, "ns_nr__strong__1__strong_" = "ns_nr"))


r <- r %>% 
  mutate(estado_civil_jh = dplyr::recode(estado_civil_jh, "esta_casado_a_" = "casado", 
                                         "esta_separado_a__o_divorciado_a_" = "divorciado", 
                                         "esta_soltero_a_" = "soltero", 
                                         "esta_viudo_a_" = "viudo", 
                                         "vive_en_union_libre" = "union_libre"))


r <- r %>% 
  mutate(ind1_pertenencia_etnica = dplyr::recode(ind1_pertenencia_etnica, 
                                                 "afrodescendiente__negro__mulato_" = "afrodesc.", 
                                                 "gitano_rrom" = "gitano", 
                                                 "otro__cual_" = "otro"))


r <- r %>% 
  mutate(fuente_ingresos = dplyr::recode(fuente_ingresos, 
                                                       "asistencia_de_gobierno_programas_de_proteccion_social_pension" = "asistencia_gobierno", 
                                                       "asistencia_de_naciones_unidas__ongs__organizaciones_caritativas" = "asistencia_humanitaria", 
                                                       "comercio_informal_ventas_ambulantes" = "comercio_informal",
                                                       "nadie_trabaja_en_el_hogar__desempleo_" = "nadie_trabaja", 
                                                       "negocio_propio_independiente__formal_" = "negocio_propio_formal", 
                                                       "remesas_de_migrantes_o_ayuda_de_familiares_amigos" = "remesas", 
                                                       "trabajo_asalariado_con_salario_regular_en_el_sector_publico_o_privado" = "trabajo_asalariado", 
                                                       "trabajo_de_jornaleo_con_salario_irregular" = "salario_irregular", 
                                                       "trabajo_de_limpieza_de_cuidado_en_casa_de_otras_personas" = "trabajo_domestico" 
  ))



r <- r %>% 
  mutate(miembro_mayor_recursos_empleo = dplyr::recode(fuente_ingresos, 
                                                       "asistencia_de_gobierno_programas_de_proteccion_social_pension" = "asistencia_gobierno", 
                                                       "asistencia_de_naciones_unidas__ongs__organizaciones_caritativas" = "asistencia_humanitaria", 
                                                       "comercio_informal_ventas_ambulantes" = "comercio_informal",
                                                       "nadie_trabaja_en_el_hogar__desempleo_" = "nadie_trabaja", 
                                                       "negocio_propio_independiente__formal_" = "negocio_propio_formal", 
                                                       "remesas_de_migrantes_o_ayuda_de_familiares_amigos" = "remesas", 
                                                       "trabajo_asalariado_con_salario_regular_en_el_sector_publico_o_privado" = "trabajo_asalariado", 
                                                       "trabajo_de_jornaleo_con_salario_irregular" = "salario_irregular", 
                                                       "trabajo_de_limpieza_de_cuidado_en_casa_de_otras_personas" = "trabajo_domestico" 
                                                       ))


r <- r %>% 
  mutate(razon_deuda = dplyr::recode(razon_deuda, 
                                     "compra_de_activos__casa__apartamento__carro__moto__electrodomesticos_etc__" = "compra_activos", 
                                     "comprar_comida" = "comida", 
                                     "comprar_insumos_productivos" = "insumos_productivos",
                                     "comprar_ropa__zapatos" = "ropa_zapatos", 
                                     "cubrir_gastos_de_salud" = "salud", 
                                     "cubrir_servicios_basicos__agua__electricidad_" = "servic_basicos", 
                                     "ns_nr__strong__1__strong_" = "ns_nr", 
                                     "pagar_la_escuela_o_gastos_de_educacion" = "educacion", 
                                     "pagar_renta_o_alquiler_de_la_vivienda" = "renta"))


r <- r %>% 
  mutate(material_paredes_exteriores = dplyr::recode(material_paredes_exteriores, 
                                                     "adobe_o_tapia_pisada" = "adobe", 
                                                     "cana__esterilla__otro_tipo_de_material_vegetal" = "cana", 
                                                     "ladrillo__bloque__material_prefabricado__piedra" = "ladrillo",
                                                     "madera_burda__tabla__tablon" = "madera_burda", 
                                                     "ns_nr__strong__1__strong_" = "ns_nr", 
                                                     "zinc__tela__carton__latas__desechos__plastico" = "zinc", 
                                                     "otro__cual_" = "otro"))



r <- r %>% 
  mutate(material_pisos = dplyr::recode(material_pisos, 
                                        "alfombra_o_tapete_de_pared_a_pared" = "afombra", 
                                        "baldosin__ladrillo__vinisol__otros_materiales_sinteticos" = "baldosin", 
                                        "cemento__gravilla" = "cemento",
                                        "madera_burda__tabla__tablon__otro_vegetal" = "madera_burda", 
                                        "ns_nr__strong__1__strong_" = "ns_nr", 
                                        "tierra__arena" = "tierra", 
                                        "otro__cual_" = "otro"))


r <- r %>% 
  mutate(material_techo = dplyr::recode(material_techo, 
                                        "madera_burda__tabla__tablon__otro_vegetal" = "madera_burda", 
                                        "paja_bambu_techo_de_paja" = "paja", 
                                        "tejas__barro__zinc__eternit_" = "tejas", 
                                        "ns_nr__strong__1__strong_" = "ns_nr",
                                        "otro__cual_" = "otro"))


r <- r %>% 
  mutate(lugar_preparacion_alimentos = dplyr::recode(lugar_preparacion_alimentos, 
                                                     "en_ninguna_parte__no_preparan_alimentos" = "ningun_parte", 
                                                     "en_un_cuarto_usado_solo_para_cocinar" = "cuart_cocinar", 
                                                     "en_un_cuarto_usado_tambien_para_dormir" = "cuart_dormir", 
                                                     "en_un_patio__corredor__enramada__al_aire_libre" = "aire_libre", 
                                                     "en_una_sala_comedor_con_lavaplatos" = "comedor_lavaplato", 
                                                     "en_una_sala_comedor_sin_lavaplatos" = "comedor_sin_lavaplatos", 
                                                     "ns_nr__strong__1__strong_" = "ns_nr"))




r <- r %>% 
  mutate(lugar_preparacion_alimentos = dplyr::recode(lugar_preparacion_alimentos, 
                                                     "gas_natural_conectado_a_red_publica" = "gas_red", 
                                                     "gas_propano_en_cilindro_o_pipeta" = "gas_cilindro", 
                                                     "lena__madera_o_carbon_de_lena" = "lena", 
                                                     "ns_nr__strong__1__strong_" = "ns_nr", 
                                                     "petroleo__gasolina__kerosene__alcohol" = "petroleo", 
                                                     "carbon_mineral" = "carbon"))


r <- r %>% 
  mutate(servicio_sanitario_compartido = dplyr::recode(servicio_sanitario_compartido, 
                                                       "compartido_con_personas_de_otros_hogares_o_grupos_de_viaje" = "compartido", 
                                                       "de_uso_exclusivo_de_las_personas_del__1_2" = "exclusivo", 
                                                       "ns_nr__strong__1__strong_" = "ns_nr"))


r <- r %>% 
  mutate(eliminacion_basura = dplyr::recode(eliminacion_basura, 
                                            "la_eliminan_de_otra_forma" = "otra_forma", 
                                            "la_queman_o_entierran" = "queman", 
                                            "la_tiran_a_un_patio__lote__zanja_o_baldio" = "tiran_patio", 
                                            "la_tiran_a_un_rio__quebrada__cano_o_laguna" = "tiran_rio", 
                                            "ns_nr__strong__1__strong_" = "ns_nr", 
                                            "por_recoleccion_publica_o_privada" = "recoleccion"))


r <- r %>% 
  mutate(fuente_agua = dplyr::recode(fuente_agua, 
                                     "agua_embotellada_o_en_bolsa" = "botella", 
                                     "aguas_lluvias" = "lluvia", 
                                     "de_otra_fuente_por_tuberia" = "otra_tuberia", 
                                     "de_acueducto_por_tuberia" = "acue_tuberia", 
                                     "de_pila_publica" = "pila_publica", 
                                     "de_pozo_con_bomba" = "pozo_bomba", 
                                     "de_pozo_sin_bomba__aljibe__jaguey_o_barreno" = "pozo_sin_bomba", 
                                     "ns_nr__strong__1__strong_" = "ns_nr",
                                     "rio__quebrada__nacimiento_o_manantial" = "rio", 
                                     "otro__cual_" = "otro"
  ))


r <- r %>% 
  mutate(tipo_vivienda = dplyr::recode(tipo_vivienda, 
                                       "apartamento" = "apartamento", 
                                       "habitacion_cuarto_pieza_en_otro_tipo_de_estructura__parqueaderos__depositos__bodegas__iglesias__colegios__fabricas__cuarto_para_portero_o_celador_en_un_edificio_de_apartamentos_" = "habitacion_apto", 
                                       "habitacion_cuarto_pieza_en_un_inquilinato" = "habitacion_inquilinato", 
                                       "ns_nr__strong__1__strong_" = "ns_nr", 
                                       "situacion_de_calle_con_espacio_para_alojarse__carpa__vagon__embarcacion__cueva__refugio_natural__etc__" = "calle_con_espacio", 
                                       "situacion_de_calle_sin_espacio_para_alojarse" = "calle_sin_espacio", 
                                       "vivienda_improvisada__construcciones_informales_con_materiales_menos_durables__cambuches__etc__" = "vivienda_improvisada", 
                                       "otro__cual_" = "otro"
  ))



r <- r %>% 
  mutate(cambio_ingresos = dplyr::recode(cambio_ingresos, 
                                         "aumentaron_los_ingresos" = "aumentaron", 
                                         "disminuyeron_los_ingresos" = "disminuyeron", 
                                         "no_hubo_cambios" = "no_cambios", 
                                         "ns_nr__strong__1__strong_" = "ns_nr", 
                                         "se_perdieron_los_ingresos_por_completo" = "perdieron"
  ))



##########################################################################################################################
##########################################################################################################################
# BINARY VARIABLES
##########################################################################################################################

# Sexo del jefe del hogar
plot_bin_1 <- ggplot(r, aes(x = sexo_jh, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por sexo jefatura")


# Discapacidad jefe del hogar
plot_bin_2 <- ggplot(r, aes(x = discapacidad_jh, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por discapacidad fisica del jh")


# Enfermedad cronica jefe del hogar
plot_bin_3 <- ggplot(r, aes(x = enfermedad_cronica_jh, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por enfermedad cronica del jh")


# Enfermedad mental jefe del hogar
plot_bin_4 <- ggplot(r, aes(x = enfermedad_mental_jh, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por enfermedad mental del jh")


# Urbano Rural
plot_bin_5 <- ggplot(r, aes(x = urbano_rural, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por urbano-rural")


# Presencia de deuda
plot_bin_6 <- ggplot(r, aes(x = deuda, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por presencia de deuda")


# Servicios: energia electrica
plot_bin_9 <- ggplot(r, aes(x = servicios_energia_electrica, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicio energia electrica")


# Servicios: gas
plot_bin_10 <- ggplot(r, aes(x = servicios_gas, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicio gas")


# Servicios: alcantarillado
plot_bin_11 <- ggplot(r, aes(x = servicios_alcantarillado, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicio alcantarillado")


# Servicios: recoleccion basura
plot_bin_12 <- ggplot(r, aes(x = servicios_recoleccion_basura, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicio recoleccion basura")


# Agua del acueducto llega las 24h los 7 dias de la semana
plot_bin_13 <- ggplot(r, aes(x = acueducto_24h, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por agua llega 24h")



# Servicio sanitario compartido
plot_bin_14 <- ggplot(r, aes(x = servicio_sanitario_compartido, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicio sanitario compartido")



# Presencia bienes: Colchon
plot_bin_15 <- ggplot(r, aes(x = bienes_colchon, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por presencia colchon")


# Presencia bienes: Computador
plot_bin_16 <- ggplot(r, aes(x = bienes_computador, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por presencia computador")


# Presencia bienes: Horno
plot_bin_17 <- ggplot(r, aes(x = bienes_horno, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por presencia horno")


# Presencia bienes: Horno
plot_bin_18 <- ggplot(r, aes(x = bienes_nevera, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por presencia nevera")


# Presencia bienes: Televisor
plot_bin_19 <- ggplot(r, aes(x = bienes_televisor, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por presencia televisor")


# Presencia bienes: Celular
plot_bin_20 <- ggplot(r, aes(x = bienes_celular, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por presencia celular")


# Presencia servicios: Celular
plot_bin_21 <- ggplot(r, aes(x = servicios_telefono_fijo, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicios telefono fijo")


# Presencia servicios: Internet
plot_bin_22 <- ggplot(r, aes(x = servicios_internet, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicios internet")


# Presencia servicios: Televisor subscripcion
plot_bin_23 <- ggplot(r, aes(x = servicios_television_subscripcion, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por servicios televisor subscripcion")


# Presencia medio transporte: bicicleta
plot_bin_24 <- ggplot(r, aes(x = transporte_bicicleta, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por transporte bicicleta")


# Presencia medio transporte: motocicleta
plot_bin_25 <- ggplot(r, aes(x = transporte_motocicleta, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por transporte motocicleta")


# Presencia medio transporte: carro
plot_bin_26 <- ggplot(r, aes(x = transporte_carro_particular, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por transporte carro")



multi_plot_1 <- ggarrange(plot_bin_1, plot_bin_2, plot_bin_3, plot_bin_4,
                          labels = c("1.", "2.", "3.", "4."),
                          ncol = 2, nrow = 2)
multi_plot_2 <- ggarrange(plot_bin_5, plot_bin_6,plot_bin_9, plot_bin_10,
                          labels = c("5.", "6.", "9.", "10."),
                          ncol = 2, nrow = 2)
multi_plot_3 <- ggarrange(plot_bin_11, plot_bin_12, plot_bin_13, plot_bin_14,
                          labels = c("11.", "12.", "13.", "14."),
                          ncol = 2, nrow = 2)
multi_plot_4 <- ggarrange(plot_bin_15, plot_bin_16, plot_bin_17, plot_bin_18,
                          labels = c("15.", "16.", "17.", "18."),
                          ncol = 2, nrow = 2)
multi_plot_5 <- ggarrange(plot_bin_19, plot_bin_20, plot_bin_21, plot_bin_22,
                          labels = c("19.", "20.", "21.", "22."),
                          ncol = 2, nrow = 2)
multi_plot_6 <- ggarrange(plot_bin_23, plot_bin_24, plot_bin_25, plot_bin_26,
                          labels = c("23.", "24.", "25.", "26."),
                          ncol = 2, nrow = 2)

multi_plot_1
multi_plot_2
multi_plot_3
multi_plot_4
multi_plot_5
multi_plot_6
multi_plot_7



##########################################################################################################################
##########################################################################################################################
# MULTIEXPRESSION VARIABLES
##########################################################################################################################


# Estado civil del jefe del hogar
plot_mul_1 <- ggplot(r, aes(x = estado_civil_jh, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por estado civil del jh")

# Nivel de estudios del jefe del hogar
plot_mul_2 <- ggplot(r, aes(x = nivel_estudios_grupo, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por nivel educativo del jh")


# Genero del respondiente
plot_mul_3 <- ggplot(r, aes(x = ind1_genero, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por genero del respondiente")


# Fuente de ingresos
plot_mul_4 <- ggplot(r, aes(x = fuente_ingresos, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por principal fuente de ingresos")


# Tipo de empleo
plot_mul_5 <- ggplot(r, aes(x = miembro_mayor_recursos_empleo, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por tipo de empleo")


# Pertenencia etnica del respondiente
plot_mul_6 <- ggplot(r, aes(x = ind1_pertenencia_etnica, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por pertenencia etnica")


# Cambio de ingresos
plot_mul_7 <- ggplot(r, aes(x = cambio_ingresos, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por cambio de ingresos")


# Tipo de vivienda
plot_mul_8 <- ggplot(r, aes(x = tipo_vivienda, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por tipo de vivienda")


# Material paredes exteriores
plot_mul_9 <- ggplot(r, aes(x = material_paredes_exteriores, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por material de los paredes exteriores")


# Material pisos
plot_mul_10 <- ggplot(r, aes(x = material_pisos, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por material de los pisos")


# Material techo
plot_mul_11 <- ggplot(r, aes(x = material_techo, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por material del techo")


# Lugar en que preparan los alimentos
plot_mul_12 <- ggplot(r, aes(x = lugar_preparacion_alimentos, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por lugar en que prepara alimentos")


# Eliminacion Basura
plot_mul_13 <- ggplot(r, aes(x = eliminacion_basura, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por eliminacion de basura")


# Fuente de agua
plot_mul_14 <- ggplot(r, aes(x = fuente_agua, y = cari)) +
  geom_boxplot(alpha=0.9) +
  scale_fill_brewer(palette="Dark2") +
  ggtitle("CARI por fuente de agua")



bin_plot_1 <- ggarrange(plot_mul_1, plot_mul_2,
                        labels = c("1.", "2."),
                        ncol = 1, nrow = 2)
bin_plot_2 <- ggarrange(plot_mul_3, plot_mul_4,
                        labels = c("3.", "4."),
                        ncol = 1, nrow = 2)
bin_plot_3 <- ggarrange(plot_mul_5, plot_mul_6,
                        labels = c("5.", "6."),
                        ncol = 1, nrow = 2)
bin_plot_4 <- ggarrange(plot_mul_7, plot_mul_8,
                        labels = c("7.", "8."),
                        ncol = 1, nrow = 2)
bin_plot_5 <- ggarrange(plot_mul_9, plot_mul_10,
                        labels = c("9.", "10."),
                        ncol = 1, nrow = 2)
bin_plot_6 <- ggarrange(plot_mul_11, plot_mul_12,
                        labels = c("11.", "12."),
                        ncol = 1, nrow = 2)
bin_plot_7 <- ggarrange(plot_mul_13, plot_mul_14,
                        labels = c("13.", "14."),
                        ncol = 1, nrow = 2)


bin_plot_1
bin_plot_2
bin_plot_3
bin_plot_4
bin_plot_5
bin_plot_6
bin_plot_7



##########################################################################################################################
##########################################################################################################################
# NUMERIC CORRELATIONS
##########################################################################################################################
#edad respondiente
cor.test(r$edad_respondiente, r$cari, method = "pearson", conf.level = 0.95)
cor.test(r$nr_personas_hogar, r$cari, method = "pearson", conf.level = 0.95)
cor.test(r$ingreso_pp, r$cari, method = "pearson", conf.level = 0.95)
cor.test(r$valor_deuda, r$cari, method = "pearson", conf.level = 0.95)



