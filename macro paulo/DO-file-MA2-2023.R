# Load required libraries
library(dplyr)
library(arrow)

# Change working directory
setwd("C:/Users/estev/OneDrive/Desktop/projetos-estevao-RStudio/macro paulo")
caminho_parquet <- "C:/Users/estev/OneDrive/Desktop/projetos-estevao-RStudio/macro paulo/Replication.parquet"
# Open the dataset
data <- read_parquet(caminho_parquet)


# Salve os dados em formato Parquet



# Configure the panel data, specifying the ID and the variable
data <- data %>% 
  arrange(firmid, year) %>%
  mutate(id = 1:n()) %>%
  group_by(firmid) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(survivor = count == 8,
         has95 = ifelse(year == 2002, 1, NA),
         has95 = ifelse(lag(has95) == 1, 1, has95),
         has95 = ifelse(is.na(has95), 0, has95),
         has_gaps = ifelse(lag(year) != year - 1 & id != 1, 1, NA),
         has_gaps = ifelse(lag(has_gaps) == 1, 1, has_gaps),
         has_gaps = ifelse(is.na(has_gaps), 0, has_gaps),
         exit = survivor == 0 & has95 == 0 & has_gaps != 1 & id == max(id) & year != 2002)

# Data cleaning to calculate productivity and misallocation
data <- data %>%
  filter(sale > 0 & !is.na(sale),
         emp > 0 & !is.na(emp),
         xrent > 0 & !is.na(xrent),
         ppent > 0 & !is.na(ppent),
         capx > 0 & !is.na(capx))

# Summary statistics
summary_data <- summary(data$sale, data$emp, data$xrent, data$ppent, data$capx, data$exit)

# Define variables for regressions and organize the summary
data <- data %>%
  mutate(lny = log(sale),
         t = group_indices(., year),
         lnkop = log(ppent),
         lnl = log(emp),
         lninv = log(capx),
         lnm = log(xrent),
         lnExp = log(xad + xrd),
         LBprodty = sale / emp,
         lnLBprodty = log(sale / emp))

# Calculate sector weights
sector_weights <- data %>%
  group_by(sic, year) %>%
  summarize(sic_sale = sum(sale)) %>%
  ungroup() %>%
  group_by(firmid, year) %>%
  mutate(firmshare = sale / sic_sale)

# Rename variables
data <- data %>%
  rename(age = age,
         t = t,
         lnkop = lnkop,
         lnl = lnl,
         lnm = lnm,
         lninv = lninv,
         lny = lny)

# Generate plant-level production function parameters
data <- data %>%
  group_by(firmid, year) %>%
  summarize(K_expenditure = sum(ppent),
            L_expenditure = sum(labx)) %>%
  ungroup() %>%
  mutate(KLterm = median(ln(K_expenditure / L_expenditure)),
         aki = 1 / (1 + exp(-KLterm)),
         ali = 1 - aki) %>%
  select(-c(K_expenditure, L_expenditure, KLterm))

# Aggregate by sector
sector_aggregates <- data %>%
  group_by(sic, year) %>%
  summarize(PYs = sum(sale),
            WLs = sum(labx),
            Ks = sum(ppent))

# Calculate als and ali
sector_factors <- sector_aggregates %>%
  mutate(Ns = n(),
         Kterm = ((Ks * aki / ppent)^(aki * (sigma - 1))),
         Lterm = ((WLs * ali / labx)^(ali * (sigma - 1))),
         Pterm = ((sale / PYs * Ns)^sigma),
         Coef = Kterm * Lterm * Pterm)

for (i in 0:100) {
  a <- i / 100
  sector_factors <- sector_factors %>%
    mutate(testsum = sum((Coef * ((a^aki * (1 - a)^ali)^(1 - sigma))) * (aki - a)))
}

sector_factors <- sector_factors %>%
  mutate(aks = (testsum > 0) * (testsum < 0) * ((index + lag(index)) / 2) / 100) %>%
  filter(!is.na(aks)) %>%
  mutate(als = 1 - aks)

# Merge back with data
data <- left_join(data, sector_factors %>% select(-c(sic, year, PYs, WLs, Ks, Ns, Kterm, Lterm, Pterm, Coef, testsum)), by = c("sic", "year"))

# Compute capital and labor wedges
data <- data %>%
  mutate(TKi = ((aki * sale / ppent) / (aks * PYs / Ks)),
         TLi = ((ali * sale / labx) / (als * PYs / WLs)))

# Trimming outliers
TKi_quantiles <- quantile(data$TKi, c(0.01, 0.99))
TLi_quantiles <- quantile(data$TLi, c(0.01, 0.99))

data <- data %>%
  filter(TKi > TKi_quantiles[1] & TKi < TKi_quantiles[2],
         TLi > TLi_quantiles[1] & TLi < TLi_quantiles[2])

# Calculate TFPRi
data <- data %>%
  mutate(TFPRi = sale / ((ppent^aki) * (emp^ali)))

# Summary statistics for TFPRi
summary_TFPRi <- summary(data$TFPRi)

# Plot histograms
hist(data$TFPRi, main = "TFPRi Distribution", xlab = "TFPRi")
hist(data$TKi, main = "TKi Distribution", xlab = "TKi")
hist(data$TLi, main = "TLi Distribution", xlab = "TLi")

# Save results
write.table(data, "data_R.csv", sep = ",", row.names = FALSE)
