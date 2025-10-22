#!/usr/bin/env Rscript

# German Dataset Deeper Exploration
# Your initial exploration file for understanding German fuel data structure

library(DBI)
library(duckdb)
library(dplyr)
library(ggplot2)

# Set working directory
if (!grepl("MBAT-Internship-Project$", getwd())) {
  setwd("../../")
}

cat("German Dataset Deeper Exploration\n")
cat("================================\n\n")

# Connect to German database
con <- dbConnect(duckdb(), "databases/german_fuel_data.duckdb", read_only = TRUE)

# List all tables
tables <- dbListTables(con)
cat("Available tables:", paste(tables, collapse = ", "), "\n\n")

# Explore each table structure
for (table in tables) {
  cat("Table:", table, "\n")
  cat("Structure:\n")
  structure <- dbGetQuery(con, paste("DESCRIBE", table))
  print(structure)
  cat("\n")
}

# Disconnect
dbDisconnect(con, shutdown = TRUE)

cat("Exploration completed!\n")
