#SETUP
rm(list=ls(all=T))
R.version
  library(rlang)
  library(xlsx)
  library(plyr) # rbind.fill
  library(dplyr)
  library(expss)
  library(reshape)
  library(data.table)
  library(miceadds)
  library(questionr)
  library(koboquest) # manage kobo questionnairs
  library(kobostandards) # check inputs for inconsistencies
  library(xlsformfill) # generate fake data for kobo
  library(surveyweights) # calculate weights from samplingframes
  library(hypegrammaR) # simple stats 4 complex samples
  library(composr) # horziontal operations
  library(readr)

  source("R/functions/postprocessing_functions.R")
  source("R/functions/to_alphanumeric_lowercase.R")
  source("R/functions/analysisplan_factory.R")
  source("R/functions/Binary_Recoding.R")
 # source("R/functions/inflation_recoding/inflation_recoding_709.R")
 # source("R/functions/inflation_recoding/inflation_recoding_967.R")


#LOAD INPUT FILES 
  source("R/1_load_inputs.R",local = T)
#  names(response)[names(response) == 'ï..X_uuid'] <- "X_uuid"
  #' creates objects:
  #' 
  #'    response clean
  #'    analysisplan
  #'    cluster_lookup_table
  #'    loop
  #'    nutritional loop data
  #'    samplingframe


#PREPARE SAMPLING FRAMES AND STRATAS
  source("R/2_prepare_samplingframe.R", local = T)
  #' Prepare sampling frames and Strata names:
  #'     3.1.create strata-ID for all observations
  #'     3.2.combine the stratum sampling frames
  #'     3.3.add strata ids to the dataset
  #'     3.4.throw error if any don't match


# EXCLUDE TWO SURVEYS FROM PENDLARES IN VALLE DEL CAUCA
response <- filter(response, 
                   strata != "valle_del_cauca_pendular")
  

# CALCULATE STRATA WEIGHTS
strata_weight_fun <- map_to_weighting(sampling.frame = samplingframe,
                                      sampling.frame.population.column = "population",
                                      sampling.frame.stratum.column = "stratum",
                                      data.stratum.column = "strata",
                                      data = response)


weight_fun <-strata_weight_fun
response$weights<- weight_fun(response)

# CHANGE ALL WEIGHTS FOR MIGRANTES EN TRANSITO AND PENDULARES TO 1
response$weights <- ifelse(response$pop_group %in% c("pendular", "transito"),1,
                           response$weights)



#CREATE NEW FUNCTION FOR WEIGHTING
weight_fun<-function(df){
  df$weights
}



#RECODING OF INDICATORS
#response <- expenditure_cleaner_6m(response)
response_with_composites <- recoding_preliminary(response, loop)


# export data in one single datasdet with household and loop data in separate sheets
response_with_composites$weights <- round(response_with_composites$weights, 3)
wb <- createWorkbook()

# Add the first dataframe to the workbook as a new sheet
addWorksheet(wb, "hogar")
writeData(wb, sheet = "hogar", x = response_with_composites)

# Add the second dataframe to the workbook as a new sheet
addWorksheet(wb, "individual")
writeData(wb, sheet = "individual", x = loop)

# Save the workbook to a file
saveWorkbook(wb, "Output/dataset/gaggi_efsa_all_data.xlsx", overwrite = TRUE)




write_csv(response_with_composites, "Output/dataset/FINAL/dataset_with_composites_efsa22_final.csv")
xl_lst <- list('hogar' = response_with_composites, 'individual' = loop)
write.xlsx(xl_lst, file = sprintf("Output/dataset/efsa_all_data_%s.xlsx",today()))
write.csv(response_with_composites, "Output/dataset/efsa_all_data_con compuestos_310822.csv")
write.csv(loop, "Output/dataset/loop_efsa_all_data_con compuestos_310822.csv")


#LOAD ANALYSISPLAN
dap_name <- "cari"
analysisplan <- read.csv(sprintf("input/dap/dap_%s.csv",dap_name), stringsAsFactors = F, sep = ";")
#analysisplan$independent.variable <-  "tiempo_en_pais"

####disaggregations: pop_group, departamento, nacionalidad_jefe_hogar
#1_nivel_estudios_grupo, 2_sexo_jh, 3_discapacidad_jefe_nonbinary, 4_enfermedad_cronica_jh, 5_enfermedad_mental_jh
#6_presencia_embarazo, 7_presencia_0_59_meses, 8_dependency_ratio, 9_size_household, 10_d13
#11_ind1_pertenencia_etnica, 12_d12_vi
analysisplan$repeat.for.variable <- "presencia_0_59_meses"
response_with_composites$one <- "one"
analysisplan$independent.variable <- "pop_group"


analysisplan <- analysisplan %>% filter(grepl("so9", dependent.variable))
analysisplan <- analysisplan %>% filter(dependent.variable %in% c("sa8_sa", "sa8_sam", "sa8_iam", "sa8_ias", "so4", "so5"))


#AGGREGATE ACROSS DISTRICTS OR/AND POPULATION GROUPS
#analysisplan <- analysisplan_nationwide(analysisplan)
#analysisplan <- analysisplan_pop_group_aggregated(analysisplan)
#analysisplan$hypothesis.type <- "group_difference"



result <- from_analysisplan_map_to_output(response_with_composites, analysisplan = analysisplan,
                                          weighting = weight_fun, cluster_variable_name = NULL,
                                          questionnaire = NULL, confidence_level = 0.95)


name <- "cari_bogota_disagg popgroup presencia ninos"
saveRDS(result,paste(sprintf("output/RDS/result_%s.RDS", name)))

summary <- bind_rows(lapply(result[[1]], function(x){x$summary.statistic}))
write.csv(summary, sprintf("output/raw_results/raw_results_%s.csv", name), row.names=F)
summary <- read.csv(sprintf("output/raw_results/raw_results_%s.csv", name), stringsAsFactors = F)
summary <- correct.zeroes(summary)
summary <- summary %>% filter(dependent.var.value %in% c(NA,1))
summary$max <- ifelse(summary$numbers < 1 & summary$max > 1, 1, 
                      summary$max)
summary$min <- ifelse(summary$min < 0, 0, summary$min)


#summary$max <- ifelse(summary$max > 1, 1, summary$max)
summary$max <- NULL
#summary$min <- ifelse(summary$min < 0, 0, summary$min)
summary$min <- NULL
#summary$numbers <- as.character(as.numeric(round(summary$numbers,1)))

summary_fixed <- analysisplan %>% select("research.question", "sub.research.question", "dependent.variable") %>% 
                                       dplyr::rename(dependent.var = dependent.variable) %>%
                                       left_join(summary, by = "dependent.var") %>% 
                                       select("dependent.var", "research.question", "sub.research.question", 
                                              "independent.var.value", "repeat.var.value",
                                              "numbers")



write.csv(summary_fixed, sprintf("Output/raw_results/raw_results_%s_filtered.csv", name), row.names=F)
if(all(is.na(summary$independent.var.value))){summary$independent.var.value <- "all"}
groups <- unique(summary$independent.var.value)
groups <- groups[!is.na(groups)]
library(plyr)


for (i in 1:length(groups)) {
  df <- pretty.output(summary, groups[i], analysisplan, cluster_lookup_table, lookup_table, severity = name == "severity", camp = F)
  write.csv(df, sprintf("Output/summary_sorted/summary_sorted_%s_%s.csv", name, groups[i]), row.names = F)
  if(i == 1){
    write.xlsx(df, file=sprintf("Output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], row.names=FALSE, showNA = F)
  } else {
    write.xlsx(df, file=sprintf("Output/summary_sorted/summary_sorted_%s.xlsx", name), sheetName=groups[i], append=TRUE, row.names=FALSE, showNA = F)
  }
}









# create variable which indicates capitales
response_with_composites <- response_with_composites %>% dplyr::mutate(capitales = case_when(
  response_with_composites$municipio %in% c("leticia", "medellin", "arauca", "barranquilla", "bogota__d_c_", "cartagena_de_indias", 
                     "tunja", "manizales", "florencia", "yopal", "popayan", "valledupar", "quibdo", "monteria", 
                     "neiva", "riohacha", "santa_marta", "villavicencio", "pasto", "cucuta", "mocoa", "armenia", 
                     "pereira", "san_andres", "bucaramanga", "sincelejo", "ibague", "cali") ~ "capital",
  
  !(response_with_composites$municipio %in% c("leticia", "medellin", "arauca", "barranquilla", "bogota__d_c_", "cartagena_de_indias", 
                       "tunja", "manizales", "florencia", "yopal", "popayan", "valledupar", "quibdo", "monteria", 
                       "neiva", "riohacha", "santa_marta", "villavicencio", "pasto", "cucuta", "mocoa", "armenia", 
                       "pereira", "san_andres", "bucaramanga", "sincelejo", "ibague", "cali")) ~ "no_capital"
))
capitales <- response_with_composites %>% filter(capitales == "capital") %>% 
                                          filter(pop_group == "vocacion_de_permanencia")
gaggi_1 <- summaryBy(ingreso ~ c(municipio,cari_categories), data = capitales, 
                     FUN = list(mean, max, min, median, sd), na.rm=T)

gaggi_2 <- summaryBy(ingreso_pp ~ c(municipio,cari_categories), data = capitales, 
                     FUN = list(mean, max, min, median, sd), na.rm=T)
gaggi <- left_join(gaggi_1, gaggi_2, by=c("municipio", "cari_categories"))
write.xlsx2(gaggi, "output/summary_sorted/fng_analisis_ingresos_municipio_040423.xlsx")





