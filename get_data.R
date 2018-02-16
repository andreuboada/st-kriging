devtools::install_github("gaborcsardi/dotenv")

library(tidyverse)
library(dbplyr)
library(DBI)
library(dotenv)

dotenv::load_dot_env(".env")


PGDATABASE = Sys.getenv("PGDATABASE")
POSTGRES_PASSWORD = Sys.getenv('POSTGRES_PASSWORD')
POSTGRES_USER = Sys.getenv('POSTGRES_USER')
PGHOST = Sys.getenv('PGHOST')
PGPORT = Sys.getenv('PGPORT')


con <- DBI::dbConnect(RPostgres::Postgres(),
                      host = PGHOST,
                      port = PGPORT,
                      dbname = PGDATABASE,
                      user = POSTGRES_USER,
                      password = POSTGRES_PASSWORD
)

query_temperaturas <- "select estacion, tmedia, to_date(fecha, 'dd/mm/yyyy') as fecha_obs
                        from raw.conagua_temperaturas
                        where to_date(fecha, 'dd/mm/yyyy') >= '12/29/2017'"
temperaturas <- tbl(con, sql(query_temperaturas)) %>% collect()

query_precipitacion <- "select estacion, prec, to_date(fecha, 'dd/mm/yyyy') as fecha_obs
                          from raw.conagua_precipitacion
                          where to_date(fecha, 'dd/mm/yyyy') >= '12/29/2017'"
precipitacion <- tbl(con, sql(query_precipitacion)) %>% collect()

query_estaciones <- "select estacion, latitud as lat, longitud as lon, nombre
                      from raw.conagua_estaciones"
estaciones <- tbl(con, sql(query_estaciones)) %>% collect()

dbDisconnect(con)

write_csv(x = temperaturas, path = "datos/temperaturas.csv")
write_csv(x = precipitacion, path = "datos/precipitacion.csv")
write_csv(x = estaciones, path = "datos/estaciones.csv")
