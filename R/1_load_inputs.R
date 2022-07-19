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
response <- read.csv("Input/datasets/dirty/household_efsa_all_data_2022-07-18.csv", sep = ";",
                              stringsAsFactors=F, check.names=T,
                              na.strings = c("", " ", "NA", "#N/A", "N/A"))
loop <- read.csv("Input/datasets/dirty/individual_efsa_all_data_2022-07-18.csv", sep = ";",
                 stringsAsFactors=F, check.names=T,
                 na.strings = c("", " ", "NA", "#N/A", "N/A"))

#names(response)[names(response) == 'ï..X_uuid'] <- "X_uuid"
#names(loop)[names(loop) == "ï..X_uuid"] <- "X_submission__uuid"

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



#response <- response[moveme(names(response), "governorate_gaza before governorate_wb")]
#response <- response[moveme(names(response), "hno_strata after strata")]

