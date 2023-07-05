##################################
# IMPORT MAD DATASET FROM MARIANA
mad_ninos<- read.csv("Input/datasets/cleaned/nutrition/FINAL/mad_nutrition_analysis.csv", sep = ";"
                          , comment.char = "", strip.white = TRUE,
                          stringsAsFactors = F, encoding="UTF-8-BOM")


mad_ninos$mdd <- ifelse(mad_ninos$mdd_reference == 0, NA,mad_ninos$mdd)

mad_ninos$consumption_frequency_breastfed_2 <- ifelse(mad_ninos$consumption_frequency_breastfed_reference == 0, NA,mad_ninos$consumption_frequency_breastfed)
mad_ninos$consumption_frequency_no_breastfed_2 <- ifelse(mad_ninos$consumption_frequency_no_breastfed_reference == 0, NA,mad_ninos$consumption_frequency_no_breastfed)

mad_ninos <- mad_ninos %>% mutate(consumption_frequency_total = case_when(
  mad_ninos$consumption_frequency_breastfed_2 == 1 | mad_ninos$consumption_frequency_no_breastfed_2 == 1 ~ 1,
  mad_ninos$consumption_frequency_breastfed_reference == 0 & mad_ninos$consumption_frequency_no_breastfed_reference == 0 ~ NA_real_,
  TRUE ~ 0
))


mad_ninos$mad_breastfed_2 <- ifelse(mad_ninos$mad_breastfed_reference == 0, NA,mad_ninos$mad_breastfed)
mad_ninos$mad_no_breastfed_2 <- ifelse(mad_ninos$mad_no_breastfed_reference == 0, NA,mad_ninos$mad_no_breastfed)



mad_ninos <- mad_ninos %>% mutate(mad_total = case_when(
  mad_ninos$mad_no_breastfed == 1 | mad_ninos$mad_breastfed == 1 ~ 1,
  mad_ninos$mdd_reference == 0 ~ NA_real_,
  TRUE ~ 0
))


#LOAD ANALYSISPLAN
dap_name <- "mad"
analysisplan <- read.csv(sprintf("input/dap/nutrition/dap_%s.csv",dap_name), stringsAsFactors = F, sep = ";")
analysisplan$repeat.for.variable <- "one"
analysisplan$independent.variable <- "one"
mad_ninos$one <- "one"



result <- from_analysisplan_map_to_output(mad_ninos, analysisplan = analysisplan,
                                          weighting = NULL, cluster_variable_name = NULL,
                                          questionnaire = NULL, confidence_level = 0.95)


name <- "V2_mad_analysis_popgroupagg_national"
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

