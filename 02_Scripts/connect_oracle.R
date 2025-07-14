#renv::init()
renv::install("ROracle")
library(ROracle)

db_host <- "picdb.nmfs.local"
db_port <- 1521
db_service_name <- "pic.pifscproddbsn.pifscprodvcn.oraclevcn.com" # This looks like a service name

db_user <- keyring::key_get("PIFSC_BFISH_user")
db_pwd <- keyring::key_get("PIFSC_BFISH_pwd")

connection_string <- paste0("(DESCRIPTION=", "(ADDRESS=(PROTOCOL=tcp)(HOST=", db_host, ")(PORT=", db_port, "))", "(CONNECT_DATA=(SERVICE_NAME=", db_service_name, "))",")")

# Establish the connection using ROracle
con <- DBI::dbConnect(dbDriver("Oracle"), username = db_user, password = db_pwd, dbname = connection_string, timeout = 10)
# Download tables for research fishing
schema_name <- "BFISH"   #stays the same for all tables         
table_name <- "CRF_CATCH"                
CRF_CATCH <- dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".", table_name))
table_name <- "CRF_DRIFT" 
CRF_DRIFT <- dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".", table_name))
table_name <- "SPECIES_LOOKUP" 
sps_lookup <- dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".", table_name))
table_name <- "CRF_SAMPLE" 
CRF_SAMPLE <- dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".", table_name))
# Disconnect from database
dbDisconnect(con)
# Save CSVs 
write.csv(CRF_CATCH, file.path("01_Data", "CRF_CATCH.csv"))
write.csv(CRF_DRIFT, file.path("01_Data", "CRF_DRIFT.csv"))
write.csv(sps_lookup, file.path("01_Data", "sps_lookup.csv"))
write.csv(CRF_SAMPLE, file.path("01_Data", "CRF_SAMPLE.csv"))
# Zip files and save to shared drive
files2zip <- dir(file.path(getwd(), "01_Data/"), full.names = TRUE)
zip(zipfile = "A:/Shared drives/NMFS PIC ISAP/02_Projects/2025_BFISH_RnD/BFISH_data.zip", files = files2zip)

# Download tables for camera survey
schema_name <- "BFISH"   #stays the same for all tables         
table_name <- "CAM_LENGTHS"                
CAM_LENGTHS <- dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".", table_name))
table_name <- "CAM_MAXN" 
CAM_MAXN <- dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".", table_name))
table_name <- "CAM_SAMPLE" 
CAM_SAMPLE <- dbGetQuery(con, paste0("SELECT * FROM ", schema_name, ".", table_name))
# Disconnect from database
dbDisconnect(con)
# Save CSVs 
write.csv(CAM_LENGTHS, file.path("01_Data", "CAM_LENGTHS.csv"))
write.csv(CAM_MAXN, file.path("01_Data", "CAM_MAXN.csv"))
write.csv(CAM_SAMPLE, file.path("01_Data", "CAM_SAMPLE.csv"))

# Zip files and save to shared drive
files2zip <- dir(file.path(getwd(), "01_Data/"), full.names = TRUE)
zip(zipfile = "A:/Shared drives/NMFS PIC ISAP/02_Projects/2025_BFISH_RnD/BFISH_data.zip", files = files2zip)
