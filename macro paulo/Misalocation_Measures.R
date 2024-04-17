# Clear the workspace
rm(list = ls())

# Set working directory (replace with your actual path)
setwd("C:/Users/estev/OneDrive/Desktop/projetos-estevao-RStudio/macro paulo")

# Open log file for recording output (replace with desired filename)
#sink("log_MA2_2022.txt", append = TRUE)

# Load data (replace "Replication.dta" with your actual data file)
data <- Replication

# String manipulation for gvkey (assuming it's a string variable)
data$gvkey <- as.character(data$gvkey)

# Panel setup (assuming firmid and year are your panel variables)
data <- data.table(data)  # Convert to data.table for panel functionalities
setDT(data)
setkey(data, gvkey, year)

# Filter data by SIC codes
data <- data[sic >= 2000 & sic <= 3999]

# Entry and Exit calculations
data <- data[order(firmid, year)]  # Sort by firmid and year
data[, count := .N, by = firmid]
data[, survivor := count == 8, by = firmid]
data[year == 2002, has95 := 1]
data[, has95 := has95[lag(1)] , by = firmid]
data[is.na(has95), has95 := 0]
data <- data[order(firmid, year)]
data[, has_gaps := year != year[lag(1)] & .N != 1, by = firmid]
data[, has_gaps := has_gaps[lag(1)] , by = firmid]
data[is.na(has_gaps), has_gaps := 0]
data <- data[order(firmid, year)]
data[, exit := survivor == 0 & has95 == 0 & has_gaps != 1 & .N == .N, by = firmid]
data[year == 2002, exit := 0]

# Drop outliers (adjust variable names if needed)
data <- data[sale > 0 & emp > 0 & xrent > 0 & ppent > 0 & capx > 0]

# Summary statistics (adjust variable names if needed)
summary(data[, c(sale, emp, xrent, ppent, capx)])

# Variable definitions
data[, lny := log(sale), by = firmid]
data[, t := year - min(year), by = firmid]  # Assuming year starts from 0
data[, lnkop := log(ppent), by = firmid]
data[, lnl := log(emp), by = firmid]
data[, lninv := log(capx), by = firmid]
data[, lnm := log(xrent), by = firmid]

# Alternative intermediate input (advertising + R&D expenses)
data[, lnExp := log(xad + xrd), by = firmid]

# Labor productivity
data[, LBprodty := sale / emp, by = firmid]
data[, lnLBprodty := log(LBprodty), by = firmid]

# Market share calculation
data <- data.table(data)  # Convert back to data.table for efficient grouping
data[, sic_sale := sum(sale), by = .(sic, year)]
data[, firmshare := sale / sic_sale, by = .(firmid, year)]

# Close log file
sink()

# The data manipulation and variable definitions are now complete within the R environment.
