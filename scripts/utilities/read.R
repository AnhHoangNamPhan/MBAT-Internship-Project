
library("sf")

f <- list.files("data/")
f <- f[grepl("geojson", f)]

x <- lapply(f, \(f) {x <- st_read(paste0("/vsizip/data/", f)); x$retrieved <- gsub("^prices-(.*)_geojson.zip$", "\\1", f); x})
saveRDS(x, "data.rds")

y <- do.call(rbind, x)
saveRDS(y, "stacked.rds")

x <- readRDS("stacked.rds")
st_write(x, "data.gpkg")

library("duckdb")

con <- dbConnect(duckdb::duckdb(), dbdir = "database.duckdb")

dbExecute(con, "INSTALL spatial")
dbExecute(con, "LOAD spatial")

dbExecute(con, "CREATE TABLE fuel_table AS SELECT * FROM ST_READ('data.gpkg')")

dbGetQuery(con, "SELECT * FROM fuel_table LIMIT 5")

dbDisconnect(con)
