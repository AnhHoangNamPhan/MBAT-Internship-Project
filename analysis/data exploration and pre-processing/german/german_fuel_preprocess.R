#!/usr/bin/env Rscript

# German Fuel Price Preprocessing Pipeline
# Your original German diesel preprocessing approach

library(DBI)
library(duckdb)
library(dplyr)
library(lubridate)

# Set working directory
if (!grepl("MBAT-Internship-Project$", getwd())) {
  setwd("../../")
}

cat("German Fuel Price Preprocessing Pipeline\n")
cat("======================================\n\n")

# Connect to German database
con <- dbConnect(duckdb(), "databases/german_fuel_data.duckdb")

# Your original German preprocessing logic here
# This was the reference implementation for the multi-country approach

cat("German preprocessing completed!\n")

# Disconnect
dbDisconnect(con, shutdown = TRUE)
