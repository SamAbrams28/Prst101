library(marinecs100b)
library(tidyverse)


# Frequency of occurrence -------------------------------------------------

# P1 Filter cctd_meso to rows representing the predator with id 99112. What was
# species was the predator? How many prey items were in their diet sample? How
# many of those prey were mesopelagic or coastal pelagic fish species?

# P2 Summarize cctd_meso at the predator level (i.e., each row in the summary
# should represent one predator), indicating whether the predator’s diet sample
# contained mesopelagic and/or coastal pelagic fish species. Call the summary
# columns any_meso and any_cpf.

# P3 Using the data frame you created in P2, create a species-level summary that
# contains columns for mesopelagic FO (meso_fo), coastal pelagic fish FO
# (cpf_fo), and predator sample size (n).

# P4 How many predator species had a mesopelagic FO greater than 0.5? Which of
# those predator species had the largest sample size?


# Simulating samples ------------------------------------------------------

set.seed(123)

# P5 In the sample, Lissodelphis had a slightly higher mesopelagic FO than
# Delphinus. Do you think that relative order reflects the true difference in
# the population? Why or why not?

# P6 Fill in the blanks below to simulate 1000 new samples. Keep the size and
# probabilities the same as the original sample. Of the 1000 simulated samples,
# what fraction show the wrong order (i.e., mesopelagic FO greater in Delphinus
# than Lissodelphis)?

lissodelphis_samples <- rbinom(1000,
                               size = ???,
                               prob = ???)
lissodelphis_fo <- ???
delphinus_samples <- rbinom(1000,
                            size = ???,
                            prob = ???)
delphinus_fo <- ???

wrong_order <- ???

# P7 How does your result in P6 influence your confidence in the sample result
# that Lissodelphis consume mesopelagic prey more frequently than Delphinus?

# P8 If you were to get new samples of the same size for these two taxa, which
# mesopelagic FO do you think would change more? Why?

# P9 Generate 1000 new simulated samples for Histioteuthidae and Dosidicus
# gigas, keeping the sample sizes and probabilities the same.

# P10 What’s the mean mesopelagic FO of the 1000 Histioteuthidae simulated
# samples? How about Dosidicus gigas? How do these means compare to the original
# sample?

# P11 What’s the standard deviation of mesopelagic FO across simulated samples
# for the two taxa?

# P12 How frequently did Histioteuthidae mesopelagic FO fall outside the range
# 0.45 - 0.65? How about Dosidicus gigas?

# P13 Based on your answers to P10-P12, what effect does sample size have on
# sample accuracy?

