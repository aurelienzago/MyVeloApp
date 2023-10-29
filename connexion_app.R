if(!require("RMySQL")){
  install.packages("RMySQL")
  library("RMySQL")
}


# Création de la connexion

con <- dbConnect(MySQL(),
                 user = "sql11646681",
                 password = "8f4mGIdyLh",
                 host = "sql11.freesqldatabase.com",
                 dbname = "sql11646681",
                 port = 3306)


# Test de la connexion

summary(con)
dbGetInfo(con)


# Ecriture des tables dans la base de données

dbWriteTable(con, "Stations", iris)
dbWriteTable(con, "Communes", iris)
dbWriteTable(con, "Disponibilités_des_stations", iris)

dbListTables(con)