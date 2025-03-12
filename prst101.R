library(marinecs100b)
library(tidyverse)


# Frequency of occurrence -------------------------------------------------

# P1 Filter cctd_meso to rows representing the predator with id 99112. What was
# species was the predator? How many prey items were in their diet sample? How
# many of those prey were mesopelagic or coastal pelagic fish species?

sea_lion <- cctd_meso[cctd_meso$predator_id == 99112, ]
sum(sea_lion$meso_prey) # 1 mesopelagic pray
sum(sea_lion$cpf_prey) # 3 coastal pray

# P2 Summarize cctd_meso at the predator level (i.e., each row in the summary
# should represent one predator), indicating whether the predator’s diet sample
# contained mesopelagic and/or coastal pelagic fish species. Call the summary
# columns any_meso and any_cpf.
predators <- cctd_meso %>%
  group_by(predator_id, predator_scientific_name) %>%
  summarize(any_meso = any(meso_prey),
            any_cpf = any(cpf_prey),
            .groups = "drop")



# P3 Using the data frame you created in P2, create a species-level summary that
# contains columns for mesopelagic FO (meso_fo), coastal pelagic fish FO
# (cpf_fo), and predator sample size (n).
FOs <- predators %>%
  group_by(predator_scientific_name) %>%
  summarize(meso_fo = mean(any_meso),
            cpf_fo = mean(any_cpf),
            n_predators = n())

# P4 How many predator species had a mesopelagic FO greater than 0.5? Which of
# those predator species had the largest sample size?
nrow(FOs[FOs$meso_fo >= 0.5, ]) # 7 predator species
max(FOs[ , 4]) # n = 20197

# Simulating samples ------------------------------------------------------

set.seed(123)

# P5 In the sample, Lissodelphis had a slightly higher mesopelagic FO than
# Delphinus. Do you think that relative order reflects the true difference in
# the population? Why or why not?
View(FOs[FOs$predator_scientific_name == "Lissodelphis borealis", ]) # meso_fo = 0.89, n = 56
View(FOs[FOs$predator_scientific_name == "Delphinus delphis delphis", ]) # meso_fo = 0.86, n = 259
# Doesn't necessarily reflect a true difference. Lisso meso_fo is only slightly
# higher and has a much smaller sample size, so the difference could be due to
# random chance.

# P6 Fill in the blanks below to simulate 1000 new samples. Keep the size and
# probabilities the same as the original sample. Of the 1000 simulated samples,
# what fraction show the wrong order (i.e., mesopelagic FO greater in Delphinus
# than Lissodelphis)?

lissodelphis_samples <- rbinom(1000,
                               size = 56,
                               prob = 0.89)
lissodelphis_fo <- lissodelphis_samples/56
delphinus_samples <- rbinom(1000,
                            size = 259,
                            prob = 0.86)
delphinus_fo <- delphinus_samples/259

wrong_order <- sum(delphinus_fo > lissodelphis_fo)/1000

# P7 How does your result in P6 influence your confidence in the sample result
# that Lissodelphis consume mesopelagic prey more frequently than Delphinus?
# It strengthens my conviction that this difference is due to random change. 23% is very high

# P8 If you were to get new samples of the same size for these two taxa, which
# mesopelagic FO do you think would change more? Why?

# Lisso, because the larger sample size for delphinus suggests it's more
# representative. More lisso samples would cut down random chance and therefore
# bring the sample mean closer to the population mean. Delphinus is likely
# already closer to its population mean, so more samples wouldn't change it that
# much. Additionally, adding samples to a smaller pool mathematically changes the
# mean more than adding the same number of samples to a larger pool.

# P9 Generate 1000 new simulated samples for Histioteuthidae and Dosidicus
# gigas, keeping the sample sizes and probabilities the same.
View(FOs[FOs$predator_scientific_name == "Histioteuthidae", ]) # meso_fo = 0.56, n = 47
View(FOs[FOs$predator_scientific_name == "Dosidicus gigas", ]) # meso_fo = 0.52, n = 1136

# P10 What’s the mean mesopelagic FO of the 1000 Histioteuthidae simulated
# samples? How about Dosidicus gigas? How do these means compare to the original
# sample?
histioteuthidae_samples <- rbinom(1000,
                               size = 47,
                               prob = 0.56)
histioteuthidae_fo <- histioteuthidae_samples/47
mean(histioteuthidae_fo) # 0.558
dosidicus_samples <- rbinom(1000,
                            size = 1136,
                            prob = 0.52)
dosidicus_fo <- dosidicus_samples/1136
mean(dosidicus_fo) # 0.520

# P11 What’s the standard deviation of mesopelagic FO across simulated samples
# for the two taxa?
sd(histioteuthidae_fo) # 0.0711
sd(dosidicus_fo) # 0.0149

# P12 How frequently did Histioteuthidae mesopelagic FO fall outside the range
# 0.45 - 0.65? How about Dosidicus gigas?
sum(histioteuthidae_fo < 0.45 | histioteuthidae_fo > 0.65)/1000 # 0.178
sum(dosidicus_fo < 0.45 | dosidicus_fo > 0.65)/1000 # 0

# P13 Based on your answers to P10-P12, what effect does sample size have on
# sample accuracy?
# A larger sample size greatly increases sample accuracy

