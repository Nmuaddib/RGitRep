#library(rstudioapi)
library(DBI)
library(odbc)

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "SQL Server",
                      Server   = "10.10.0.216",
                      Database = "DW_DATAMART",
                      UID      = "dwh",#rstudioapi::askForPassword("Database user"),
                      PWD      = "ti!@#123",#rstudioapi::askForPassword("Database password"),
                      Port     = 1433,
                      encoding = "latin1")

Sys.getlocale()

dbGetInfo(con)

??DBI
