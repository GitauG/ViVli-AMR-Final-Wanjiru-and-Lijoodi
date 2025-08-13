##Data Importation
install.packages("here")
Y
library(here)
install.packages("readxl")
Y
library(readxl)
AMR<- read_excel(here("AMR", "2025_03_11 atlas_antibiotics.xlsx"))
head(AMR)
structure(AMR)

##Filtering Kenyan data for the bacteria of interest
install.packages("tidyverse")
Y
library(tidyverse)
KenyaAMR <- AMR %>%
  filter(
    Country == "Kenya", 
    Species %in% c(
      "Escherichia coli",
      "Klebsiella pneumoniae",
      "Acinetobacter baumannii",
      "Staphylococcus aureus",
      "Streptococcus pneumoniae",
      "Shigella species",
      "Salmonella species",
      "Pseudomonas aeruginosa"
    )
  )
view(KenyaAMR)
summary(KenyaAMR$Year)
str(KenyaAMR2)
##Only data from 2013-2023 - 10 years instead of initially planned 20
###Removing unnecessary collumn
structure(KenyaAMR$State)  ##All NA
KenyaAMR$State <-NULL
view(KenyaAMR)
##Checking for any missing data
anyNA(KenyaAMR)
which(rowSums(is.na(KenyaAMR)) > 0)
library(dplyr)

##Pivoting data to long format
KenyaAMR2 <- KenyaAMR %>%
  pivot_longer(
    cols = c(
      Amikacin, `Amoxycillin clavulanate`, Ampicillin, Azithromycin, Cefepime,
      Cefoxitin, Ceftazidime, Ceftriaxone, Clarithromycin, Clindamycin,
      Erythromycin, Levofloxacin, Linezolid, Meropenem, Metronidazole,
      Minocycline, Penicillin, `Piperacillin tazobactam`
    ),
    names_to = "Antibiotic",
    values_to = "Result"
  )

view(KenyaAMR2)

###Creating a reference table based on species and antibiotics used
library(tibble)
library(tibble)

bactantibio <- tribble(
  ~Species,                  ~Antibiotic,
  "Escherichia coli",         "Amikacin",
  "Escherichia coli",         "Amoxycillin clavulanate",
  "Escherichia coli",         "Ampicillin",
  "Escherichia coli",         "Cefepime",
  "Escherichia coli",         "Ceftazidime",
  "Escherichia coli",         "Levofloxacin",
  "Escherichia coli",         "Meropenem",
  "Escherichia coli",         "Piperacillin tazobactam",
  "Klebsiella pneumoniae",    "Amikacin",
  "Klebsiella pneumoniae",    "Amoxycillin clavulanate",
  "Klebsiella pneumoniae",    "Ampicillin",
  "Klebsiella pneumoniae",    "Cefepime",
  "Klebsiella pneumoniae",    "Ceftazidime",
  "Klebsiella pneumoniae",    "Levofloxacin",
  "Klebsiella pneumoniae",    "Meropenem",
  "Klebsiella pneumoniae",    "Piperacillin tazobactam",
  "Pseudomonas aeruginosa",   "Amikacin",
  "Pseudomonas aeruginosa",   "Amoxycillin clavulanate",
  "Pseudomonas aeruginosa",   "Ampicillin",
  "Pseudomonas aeruginosa",   "Cefepime",
  "Pseudomonas aeruginosa",   "Ceftazidime",
  "Pseudomonas aeruginosa",   "Levofloxacin",
  "Pseudomonas aeruginosa",   "Meropenem",
  "Pseudomonas aeruginosa",   "Piperacillin-tazobactam",
  "Staphylococcus aureus",    "Amikacin",
  "Staphylococcus aureus",    "Ampicillin",
  "Staphylococcus aureus",    "Cefepime",
  "Staphylococcus aureus",    "Ceftazidime",
  "Staphylococcus aureus",    "Levofloxacin",
  "Staphylococcus aureus",    "Meropenem",
  "Staphylococcus aureus",    "Piperacillin tazobactam",
  "Streptococcus pneumoniae", "Amoxycillin clavulanate",
  "Streptococcus pneumoniae", "Ceftazidime",
  "Streptococcus pneumoniae", "Ceftriaxone",
  "Streptococcus pneumoniae", "Clindamycin",
  "Streptococcus pneumoniae", "Erythromycin",
  "Streptococcus pneumoniae", "Levofloxacin",
  "Streptococcus pneumoniae", "Linezolid",
  "Streptococcus pneumoniae", "Meropenem",
  "Streptococcus pneumoniae", "Minocycline",
  "Streptococcus pneumoniae", "Penicillin",
  "Acinetobacter baumannii",  "Amikacin",
  "Acinetobacter baumannii",  "Amoxycillin clavulanate",
  "Acinetobacter baumannii",  "Ampicillin",
  "Acinetobacter baumannii",  "Cefepime",
  "Acinetobacter baumannii",  "Ceftazidime",
  "Acinetobacter baumannii",  "Levofloxacin",
  "Acinetobacter baumannii",  "Meropenem",
  "Acinetobacter baumannii",  "Piperacillin tazobactam"
)

str(KenyaAMR2)

###Further Cleaning
#Checking and harmonising inconsistent names in antibiotics column
#Ensuring antibiotic values match those in bactantibio
KenyaAMR2$Antibiotic <- str_replace(KenyaAMR2$Antibiotic, "-", " ")

#Joining the table to df and filtering to only valid organism-antibiotic combinations
KenyaAMR2 <- KenyaAMR2 %>%
  inner_join(bactantibio, by = c("Species", "Antibiotic"))
###Standardise resistance result column and converting to column
KenyaAMR2 <- KenyaAMR2 %>%
  mutate(
    Result = ifelse(Result %in% c("Susceptible", "Intermediate", "Resistant"), Result, NA),
    Result = factor(Result, levels = c("Susceptible", "Intermediate", "Resistant"))
  )

###Categorically coding resistance for analysis
KenyaAMR2 <- KenyaAMR2 %>%
  mutate(
    Resistant = case_when(
      Result == "Susceptible" ~ 0,
      Result == "Intermediate" ~ 1,
      Result == "Resistant" ~ 2,
      TRUE ~ NA_real_
    )
  )

###Checking for any duplicate rows
duplicated_rows <- KenyaAMR2 %>%
  group_by(`Isolate Id`, Antibiotic) %>%
  filter(n() > 1)
view(duplicated_rows)
###none

###Checking % missingness
# Check percentage of missing values per column
sapply(KenyaAMR2, function(x) mean(is.na(x))) * 100

view(KenyaAMR2)
str(KenyaAMR2)


```{r}
###Temporal Speed ANimated
# For GIF output
install.packages("gifski")
library(gifski)
# For video output (MP4, etc.):
install.packages("av")
library(av)
# Restart your R session after installation
install.packages("gganimate")
library(gganimate)
ggplot(IsolateSummary, aes(x = Species, y = Any_Resistant, fill = Species)) +
  geom_boxplot() +
  transition_time(Year) +
  labs(title = "Resistance Trends: Year {round(frame_time, 1)}") +
  shadow_mark()  # Retains past years' data
```


```{r}
###Local explanations using fastshap
library(fastshap)
library(ggplot2)
library(patchwork)
library(doParallel)

# 1. Prepare data for SHAP (must match training structure)
explain_data <- model_data %>% 
  dplyr::select(-Resistant) %>%
  mutate(across(where(is.factor), ~factor(., levels = levels(model$trainingData[[cur_column()]]))))

# 2. Create robust prediction wrapper
pred_wrapper <- function(object, newdata) {
  # Convert to data.frame with correct factor levels
  newdata <- as.data.frame(newdata) %>%
    mutate(across(where(is.factor), 
                  ~factor(., levels = levels(model$trainingData[[cur_column()]]))))
  
  # Return probability of resistance
  predict(object, newdata, type = "prob")[,"Resistant"]
}

# 3. Compute SHAP values for specific observations
# Parallel SHAP computation
cl <- makePSOCKcluster(4)
registerDoParallel(cl)

###Exporting required objects to all workers
clusterExport(cl, c("model", "explain_data"))

###Export required functions to workers
clusterEvalQ(cl, {
  library(dplyr)  # Load dplyr in each worker
})

###Defining prediction wrapper without pipes for parallel safety
pred_wrapper <- function(object, newdata) {
  newdata <- as.data.frame(newdata)
  for(col in colnames(newdata)) {
    if(is.factor(newdata[[col]])) {
      newdata[[col]] <- factor(newdata[[col]], 
                               levels = levels(model$trainingData[[col]]))
    }
  }
  predict(object, newdata, type = "prob")[,"Resistant"]
}

###Computing shap values
shap_values <- fastshap::explain(
  model$finalModel,
  X = explain_data[1:20, ],  # First 20 observations
  pred_wrapper = pred_wrapper,
  nsim = 50,
  parallel = TRUE
)

stopCluster(cl)

# 4. Create interpretation plots

## A. Individual force plot (for single observation)
plot_force <- function(obs_idx) {
  shap_df <- data.frame(
    Feature = colnames(shap_values),
    Value = explain_data[obs_idx, ] %>% as.numeric(),
    SHAP = shap_values[obs_idx, ]
  ) %>%
    arrange(desc(abs(SHAP))) %>%
    head(10)  # Top 10 features
  
  ggplot(shap_df, aes(x = reorder(Feature, SHAP), y = SHAP, fill = SHAP)) +
    geom_col() +
    geom_text(aes(label = sprintf("%.3f", SHAP)), hjust = -0.1) +
    coord_flip() +
    scale_fill_gradient2(low = "blue", high = "red") +
    labs(title = paste("SHAP Values - Observation", obs_idx),
         x = "", y = "Impact on Resistance Probability") +
    theme_minimal()
}

## B. Multi-observation summary
plot_summary <- shap_values %>%
  as.data.frame() %>%
  mutate(Observation = row_number()) %>%
  tidyr::gather(Feature, SHAP, -Observation) %>%
  group_by(Feature) %>%
  summarise(Mean_Impact = mean(abs(SHAP))) %>%
  arrange(desc(Mean_Impact)) %>%
  head(15) %>%
  ggplot(aes(x = reorder(Feature, Mean_Impact), y = Mean_Impact)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Mean Absolute SHAP Values",
       x = "", y = "Average Impact on Prediction") +
  theme_minimal()

# Display plots
plot_force(1) / plot_summary  # Using patchwork

```