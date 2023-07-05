source("R/functions/Nutritional_Recoding.R")
library(writexl)

##################################
# Ninos 0-59 
# read ninos data
n_df_ninos059 <- read.csv("Input/datasets/cleaned/nutrition/FINAL/final_nutrition_ninos059.csv", sep = ";"
                          , comment.char = "", strip.white = TRUE,
                          stringsAsFactors = F, encoding="UTF-8-BOM")
names(n_df_ninos059)[names(n_df_ninos059) == 'ï..Apl'] <- "Apl"
names(n_df_ninos059)[names(n_df_ninos059) == 'P.T'] <- "peso_talla_category"

# change column names original data
change_names_n_original <- read.csv("Input/Codebook_Nutritional_Questionnaire_EFSA22.csv", sep=";")
names(n_df_ninos059) <- plyr::mapvalues(names(n_df_ninos059), from = change_names_n_original$old_name, to=change_names_n_original$new_name)

#n_df_ninos059 <- recoding_antro_ninos059(n_df_ninos059)

n_df_ninos059 <- n_df_ninos059 %>% left_join(response_with_composites, by = "registro")
write_xlsx(n_df_ninos059, "Output/dataset/nutrition/nutrition_ninos059_with_composites.xlsx")


##################################
# GESTANTES 
# read gestantes data
n_df_gestantes <- read.csv("Input/datasets/cleaned/nutrition/FINAL/final_nutrition_gestantes.csv", sep = ";"
                           , comment.char = "", strip.white = TRUE,
                           stringsAsFactors = F, encoding="UTF-8-BOM")
names(n_df_gestantes)[names(n_df_gestantes) == 'ï..Apl'] <- "Apl"

# change column names original data
change_names_n_original <- read.csv("Input/Codebook_Nutritional_Questionnaire_EFSA22.csv", sep=";")
names(n_df_gestantes) <- plyr::mapvalues(names(n_df_gestantes), from = change_names_n_original$old_name, to=change_names_n_original$new_name)

n_df_gestantes <- n_df_gestantes %>% left_join(response_with_composites, by = "registro")
write_xlsx(n_df_gestantes, "Output/dataset/nutrition/nutrition_gestantes_with_composites.xlsx")


##################################
# MAYORES 
# read mayores data
n_df_mayores <- read.csv("Input/datasets/cleaned/nutrition/FINAL/final_nutrition_mayores.csv", sep = ";"
                         , comment.char = "", strip.white = TRUE,
                         stringsAsFactors = F, encoding="UTF-8-BOM")
names(n_df_mayores)[names(n_df_mayores) == 'ï..Apl'] <- "Apl"

# change column names original data
change_names_n_original <- read.csv("Input/Codebook_Nutritional_Questionnaire_EFSA22.csv", sep=";")
names(n_df_mayores) <- plyr::mapvalues(names(n_df_mayores), from = change_names_n_original$old_name, to=change_names_n_original$new_name)


n_df_mayores <- n_df_mayores %>% left_join(response_with_composites, by = "registro")
write_xlsx(n_df_mayores, "Output/dataset/nutrition/nutrition_mayores65_with_composites.xlsx")





###########################################
#CHANGE ACCENTED LETTERS AND NUMERIC LOWERCASE
###########################################
############################
# replace accented letters with regular ones
accented_letters <- function (x){
  stri_replace_all_fixed(x,
                         c("á","é","ń","í","ó","ú","ñ","ü","Á","Ó","Í","Ñ"),
                         c("a","e","n","i","o","u","n","u","A","O","I","N"),
                         vectorize_all = FALSE)}

n_df_ninos059 <- rapply(n_df_ninos059, f = accented_letters, classes = c("factor", "character"), how = "replace")
n_df_gestantes <- rapply(n_df_gestantes, f = accented_letters, classes = c("factor", "character"), how = "replace")
n_df_mayores <- rapply(n_df_mayores, f = accented_letters, classes = c("factor", "character"), how = "replace")



#############################
# set string to lower case and replace everything that is not alphanumeric or underscore by a dot "."
to_alphanumeric_lowercase <- function (x)
{
  tolower(gsub("[^a-zA-Z0-9_]", "\\_", x))
}

n_df_ninos059 <- rapply(n_df_ninos059, f = to_alphanumeric_lowercase, classes = c("factor", "character"), how = "replace")
n_df_gestantes <- rapply(n_df_gestantes, f = to_alphanumeric_lowercase, classes = c("factor", "character"), how = "replace")
n_df_mayores <- rapply(n_df_mayores, f = to_alphanumeric_lowercase, classes = c("factor", "character"), how = "replace")


#write_xlsx(n_df_ninos059_with_composites, "Output/dataset/nutrition/n_df_ninos059_with_composites.xlsx")
