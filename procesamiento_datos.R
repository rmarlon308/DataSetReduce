library(readr)
library(dplyr)
library(FSelector)

setwd("/home/marlon/mainfolder/marlon/USFQ/DataMining/4_ProcesamientoDatos/P4")

SPECTF_train <- read_csv("SPECTF.train", col_names = FALSE)
SPECTF_test <- read_csv("SPECTF.test", col_names = FALSE)

SPECTF = rbind(SPECTF_train, SPECTF_test)

names(SPECTF) = c("OVERALL_DIAGNOSIS", "F1R", "F1S","F2R","F2S","F3R","F3S","F4R","F4S","F5R","F5S","F6R","F6S","F7R","F7S","F8R","F8S","F9R","F9S","F10R","F10S","F11R","F11S","F12R","F12S","F13R","F13S","F14R","F14S","F15R","F15S","F16R","F16S","F17R","F17S","F18R","F18S","F19R","F19S","F20R","F20S","F21R","F21S","F22R","F22S")

normalized_min_max = SPECTF
for(i in 2:ncol(normalized_min_max)){
    normalized_min_max[i] =  (normalized_min_max[i] - min(normalized_min_max[i]))/ (max(normalized_min_max[i] - min(normalized_min_max[i]))) * (1-0) + 0
}

weights <- information.gain(OVERALL_DIAGNOSIS~., normalized_min_max, unit = "log2")

important_attributes = weights %>%
    filter(attr_importance != 0) %>%
    arrange(desc(attr_importance))

SPECTF_filter = normalized_min_max[, which((names(normalized_min_max) %in% row.names(important_attributes)) == T)]

correlation_matrix = as.data.frame(cor(SPECTF_filter))

Final_data_SPECTF = normalized_min_max %>%
    select(OVERALL_DIAGNOSIS, F3S, F13S, F20S, F21R)

saveRDS(Final_data_SPECTF, "SPECTF_FINAL.rds")



