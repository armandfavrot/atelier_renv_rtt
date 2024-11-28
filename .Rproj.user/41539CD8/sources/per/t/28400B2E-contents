library ("ggplot2") 
library ("dplyr") 
library ("tibble") 
library ("tidyr") 
library ("ALFAM2") 



# files from the ALFAM2 zenodo repository
data_interval_level = read.csv(file = "data/ALFAM2_interval.csv")
data_plot_level = read.csv(file = "data/ALFAM2_plot.csv")

# files from the supplementary material of Hafner et al (2019)
evaluation_subset_pmids = read.csv(file = "data/1-s2.0-S1352231018308069-mmc2.csv")
calibration_subset_pmids = read.csv(file = "data/1-s2.0-S1352231018308069-mmc1.csv")


df_pmids = rbind (evaluation_subset_pmids %>% mutate (dataset = "Evaluation subset"),
                  calibration_subset_pmids %>% mutate (dataset = "Calibration subset"))


data = data_interval_level %>%
  
  # keeping only the same trial as in Hafner et al (2019)
  filter (pmid %in% df_pmids$pmid) %>%
  
  # adding the 'dataset' variable (either 'Calibration subset' or 'Evaluation subset')
  left_join (df_pmids, by = "pmid") %>%
  
  # removing measurements made after more than 78h after fertilizer application
  filter (ct <= 78) %>%
  
  # selecting variables of interest
  select (e.cum, j.NH3, pmid, ct, dt, air.temp, wind.2m, rain.rate, rain.cum, dataset)  %>%
  
  # adding plot level informations
  left_join (
    
    data_plot_level %>%
      filter (pmid %in% df_pmids$pmid) %>%
      select (pmid, country, meas.tech, tan.app, app.method, app.rate, man.dm, man.ph, man.source, incorp, time.incorp) %>%
      rename (app.mthd = app.method, t.incorp = time.incorp), 
    
    by = "pmid"
    
  ) %>% 
  
  # dealing with NAs for the rain
  replace_na (list (rain.rate = 0)) %>% 
  replace_na (list (rain.cum = 0)) %>% 
  
  # dealing with NAs for the wind and the temperature
  mutate (wind.2m = replace_na (wind.2m, mean (wind.2m, na.rm = TRUE)), .by = pmid) %>%
  mutate (air.temp = replace_na (air.temp, mean (air.temp, na.rm = TRUE)), .by = pmid) %>%
  
  # recoding time of incorporation values that are below 0.25 : setting them to 0
  mutate (t.incorp = ifelse (t.incorp <= 0.25, 0, t.incorp)) %>% 
  
  # setting time of incorporation to 1000 in the case of no incorporation
  mutate (t.incorp = ifelse (t.incorp == 0 & incorp == "none", 1000, t.incorp)) %>%
  mutate (t.incorp = replace_na (t.incorp, 1000)) %>%
  
  # dealing with NAs for the pH
  mutate (man.ph = replace_na (man.ph, median (man.ph, na.rm = TRUE)), .by = man.source) %>%
  
  rename (time = ct) %>%
  
  mutate_if (is.character, as.factor) %>%
  
  {.}


data_predictions = data 










data_description_plot = data %>%
  
  filter (pmid %in% unique (data$pmid)[c (1, 101, 201, 301, 401, 501)]) %>%
  
  ggplot() +
  geom_point (aes (x = time, y = e.cum)) +
  geom_line (aes (x = time, y = e.cum)) +
  facet_wrap (~ pmid, scales = "free", ncol = 2) +
  xlab ("Time after fertilizer application (h)") +
  ylab ("Cumul of ammonia volatilization (kg/ha)")

png (file = "results/figure1.png")
data_description_plot
dev.off()


## ALFAM2


alfam2_predictions =  alfam2 (
  pars = alfam2pars01,
  dat = data %>% select (- j.NH3, - e.cum, - dt, - dataset, - country, - meas.tech, - man.ph),
  app.name = "tan.app",
  time.name = "time",
  time.incorp = "t.incorp",
  group = "pmid",
  prep = TRUE,
  warn = FALSE
)

alfam2_predictions = alfam2_predictions %>%
  
  select (pmid, time, j, e) %>%
  
  mutate (truth_j = data$j.NH3) %>%
  mutate (truth_e = data$e.cum) %>%
  
  mutate (dataset = data$dataset) %>%
  
  select (pmid, time, j, e, truth_j, truth_e, dataset)



data_predictions = data_predictions %>%
  mutate (e.cum_hat_ALFAM2 = alfam2_predictions$e, 
          j.NH3_hat_ALFAM2 = alfam2_predictions$j,
          .after = e.cum)


df_predictions_for_evaluation_metrics = rbind (
  
  # flux --------
  alfam2_predictions %>% 
    select (truth = truth_j, prediction = j, dataset) %>%
    mutate (response = "flux"),
  
  # cumulative emission --------
  alfam2_predictions %>% 
    filter (time == max (time), .by = pmid) %>% 
    select (truth = truth_e, prediction = e, dataset) %>%
    mutate (response = "72h cum. emission")
  
)


evalutation_metrics = df_predictions_for_evaluation_metrics %>%
  
  summarise (
    Pearsons_r = cor (prediction, truth),
    ME = 1 - (sum ( (prediction - truth) ^ 2) / sum ( (truth - mean (truth)) ^ 2)),
    MAE = mean (abs (prediction - truth)),
    MBE = mean (prediction - truth),
    .by = c (dataset, response)
  ) %>%
  
  mutate_if (is.numeric, round, digits = 3) %>%
  
  arrange (desc (dataset))

write.csv(evalutation_metrics, file = "results/table1.csv")


# Comparison


observed_vs_predicted_values_plot = data_predictions %>% 
  
  filter (pmid %in% c (1685, 974, 1232, 1504, 1450, 584)) %>%
  select (e.cum, e.cum_hat_ALFAM2, time, pmid) %>%
  pivot_longer (cols = c (e.cum, e.cum_hat_ALFAM2)) %>%
  mutate (name = recode (name,
                         "e.cum" = "Observed values",
                         "e.cum_hat_ALFAM2" = "ALFAM2")) %>%
  
  mutate (name = factor (name, levels = c ("Observed values", "ALFAM2"))) %>%
  
  ggplot () +
  geom_point (aes (x = time, y = value, color = name), size = 2) +
  geom_line (aes (x = time, y = value, color = name), linewidth = 0.75) +
  facet_wrap (~ pmid, scales = "free", ncol = 2) +
  theme (legend.position = "bottom") +
  ylab ("Cumulative emissions (kg/ha)") +
  xlab ("Time after fertilizer application") +
  ggtitle ("Observed vs predicted values for some trials") +
  labs (color = "")

png (file = "results/figure2.png")
observed_vs_predicted_values_plot
dev.off()



