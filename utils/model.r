library(tidyverse)

outbreak_predict_r <- read_csv("data/risk-assessment-of-resurgence.csv")

outbreak_predict <- outbreak_predict_r %>%
    select(Coverage, `Outbreak Probability`) %>%
    rename(outbreak_prob = `Outbreak Probability`)

write_csv(outbreak_predict, "data/prediction_model.csv")
