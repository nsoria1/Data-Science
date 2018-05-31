####### Hive Connections #######

library(RODBC)

db <- odbcConnect(dsn="J&J EMEA", believeNRows=FALSE )

sqlTables(db)

#sqlQuery(db, 'USE dia.poc_infor_demo_view')

resultset <- sqlQuery(db, "select * from POC_INFOR_DEMO")

View(resultset)

db = odbcClose(db) 