source("R/prepare_nutritional.R")
source("R/functions/Nutritional_Recoding.R")

response_nutritional <- recoding_intervenciones(loop_nutri)


response_nutritional <- response_with_composites %>% select("registro", "pop_group") %>%
  left_join(response_nutritional, by = "registro")

#response_nutritional$weights <- round(response_nutritional$weights)
#write.csv(response_nutritional, "Output/dataset/nutritional_dataset_merged.csv")


response_nutritional <- response_nutritional %>% drop_na(weights) %>%
                                                 drop_na(edad_meses_ninos059) %>%
                                                 drop_na(pop_group.y) 


response_nutritional <- n_df_ninos059 %>% mutate(zlen = na_if(zlen, zlen > 5 | zlen < -5)) %>%
                                                                   mutate(zwei = na_if(zwei, zwei > 5 | zwei < -5)) %>%
                                                                   mutate(zwfl = na_if(zwfl, zwfl > 5 | zwfl < -5)) %>%
                                                                   mutate(zbmi = na_if(zbmi, zbmi > 5 | zbmi < -5))
  

#CREATE NEW FUNCTION FOR WEIGHTING
weight_fun_antro<-function(df){
  response_nutritional$weights
}

#LOAD ANALYSISPLAN
dap_name <- "nutri_gestantes_informe"
analysisplan <- read.csv(sprintf("input/dap/nutrition/dap_%s.csv",dap_name), stringsAsFactors = F, sep = ";")
analysisplan$repeat.for.variable <- "pop_group.x"
analysisplan$independent.variable <- "one"
analysisplan$independent.variable.type <- "categorical"
response_nutritional$one <- "one"



result <- from_analysisplan_map_to_output(response_nutritional, analysisplan = analysisplan,
                                          weighting = NULL, cluster_variable_name = NULL,
                                          questionnaire = NULL, confidence_level = 0.95)


name <- "nutri_gestantes_interventions_unweighted_popgroupagg_nacional"
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


write.csv(summary, sprintf("Output/raw_results/raw_results_%s_filtered.csv", name), row.names=F)
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

