# LOAD QUESTIONNAIRE
#questions <- read.csv("input/questionnaire/kobo_questions.csv", 
#                      stringsAsFactors=T, check.names=T)
#colnames(questions)[1] <- "type"#


#choices <- read.csv("input/questionnaire/kobo_choices.csv", 
#                    stringsAsFactors=F, check.names=T)
#colnames(choices)[1] <- "list_name"

# LOAD SAMPLINGFRAMES AND LOOKUP-TABLES
#cluster_lookup_table <- read.csv("input/lookup_tables/combined_sample_ids.csv", 
#                                 stringsAsFactors=F, check.names=F)
#cluster_lookup_table <- dplyr::distinct(cluster_lookup_table)
#write.csv(cluster_lookup_table, "input/combined_sample_ids.csv")
#cluster_lookup_table <- read.csv("input/combined_sample_ids.csv", 
#                                 stringsAsFactors=F, check.names=F)

lookup_table <- read.csv("input/lookup_tables/lookup_table_names.csv", stringsAsFactors = F, sep = ";")


samplingframe <- load_samplingframe("./input/sampling_frame/strata_population.csv")


# LOAD DATA AND MERGE REPRESENTATIVE AND INDICATIVE DATA
#data <- xlsform_fill(questions, choices, 500)
response <- read.csv("Input/datasets/cleaned/household_full_efsa_all_data_2022-08-22.csv", sep = ";",
                              stringsAsFactors=F, check.names=T,
                              na.strings = c("", " ", "NA", "#N/A", "N/A"))
loop <- read.csv("Input/datasets/cleaned/individual_full_efsa_all_data_2022-08-11.csv", sep = ";",
                 stringsAsFactors=F, check.names=T,
                 na.strings = c("", " ", "NA", "#N/A", "N/A"))

loop_nutri_ninos023 <- read.csv("Input/datasets/dirty/ninos023_nutritional_data_2022-07-22.csv", sep = ";",
                 stringsAsFactors=F, check.names=T,
                 na.strings = c("", " ", "NA", "#N/A", "N/A"))
loop_nutri_ninos2459 <- read.csv("Input/datasets/dirty/ninos2459_nutritional_data_2022-07-22.csv", sep = ";",
                                stringsAsFactors=F, check.names=T,
                                na.strings = c("", " ", "NA", "#N/A", "N/A"))
loop_nutri_embarazos <- read.csv("Input/datasets/dirty/embarazos_nutritional_data_2022-07-22.csv", sep = ";",
                                stringsAsFactors=F, check.names=T,
                                na.strings = c("", " ", "NA", "#N/A", "N/A"))
loop_nutri_mayores <- read.csv("Input/datasets/dirty/mayores_nutritional_data_2022-07-22.csv", sep = ";",
                                stringsAsFactors=F, check.names=T,
                                na.strings = c("", " ", "NA", "#N/A", "N/A"))
#names(response)[names(response) == 'ï..X_uuid'] <- "X_uuid"
#names(loop)[names(loop) == "ï..X_uuid"] <- "X_submission__uuid"

# import cleaned vocacion de permanencia data
#response_vdp <- read.csv("Input/datasets/cleaned/household_vdp_efsa_all_data_2022-08-03.csv", sep = ";",
#                     stringsAsFactors=F, check.names=T,
#                     na.strings = c("", " ", "NA", "#N/A", "N/A"))
#loop_vdp <- read.csv("Input/datasets/cleaned/individual_vdp_efsa_all_data_2022-08-03.csv", sep = ";",
#                 stringsAsFactors=F, check.names=T,
#                 na.strings = c("", " ", "NA", "#N/A", "N/A"))
#response <- response %>% filter(pop_group %in% c("pendular", "retornado", "transito", 
#                                              "comunidad_de_acogida"))
#common <- intersect(colnames(response), colnames(response_vdp))

#row-bind only on common column names
#response <- rbind(response[common], response_vdp[common])




#add HNO stratas and governorates to dataset
response$region <- case_when(response$departamento == "bogota_dc" | 
                                  response$departamento == "antioquia" | 
                                  response$departamento == "norte_de_santander" | 
                                  response$departamento == "valle_del_cauca" | 
                                  response$departamento == "cundinamarca" |
                                  response$departamento == "santander" |
                                  response$departamento == "cesar" ~ "andina",
                                  response$departamento == "atlantico" | 
                                  response$departamento == "la_guajira" | 
                                  response$departamento == "bolivar" |
                                  response$departamento == "magdalena" ~ "caribe",
                                  response$departamento == "arauca" ~ "orinoquia",
                                  response$departamento == "narino" ~ "pacifica",
                                )

response <-  response[c(which(!startsWith(names(response), "P1")))]  
response <-  response[c(which(!startsWith(names(response), "P9")))]  
#response <-  response[c(which(!startsWith(names(response), "ind")))]  
response <-  response[c(which(!startsWith(names(response), "P8")))]  
response <-  response[c(which(!startsWith(names(response), "V_")))]  
response <-  response[c(which(!startsWith(names(response), "CONTINUAR_")))]  


#response <- response[moveme(names(response), "governorate_gaza before governorate_wb")]
#response <- response[moveme(names(response), "hno_strata after strata")]

