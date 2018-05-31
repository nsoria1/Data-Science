options(java.parameters = "-Xmx8000m")

if (!require("RJDBC")) {
  install.packages("RJDBC",repos="http://cran.rstudio.com/")
  library("RJDBC")
}

jdbcDriver <- JDBC(driverClass="com.sap.db.jdbc.Driver",  
                   classPath="C:/Users/nsoria/Downloads/JDBC/ngdbc.jar")

library("RJDBC")

jdbcConnection <- dbConnect(jdbcDriver,
                            "jdbc:sap://ec2-52-53-86-181.us-west-1.compute.amazonaws.com:30015/?autocommit=false"
                            ,"DIA"
                            ,"Assa2018")

result <- dbGetQuery(jdbcConnection, 'SELECT * FROM "_SYS_BIC"."DIA/POC_SALES_DATA" limit 10')

print(result)

dbDisconnect(jdbcConnection)