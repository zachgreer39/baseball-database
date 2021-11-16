local_db=function(db_name) {
  DBI::dbConnect(drv=RMySQL::MySQL(), 
                 username=rstudioapi::askForPassword("Database username"), 
                 password=rstudioapi::askForPassword("Database password"), 
                 host="localhost", 
                 port=3306, 
                 dbname=db_name);
};
db_baseball=local_db("db_baseball");

disconnect=function(db_name) {
  DBI::dbDisconnect(db_name)
};
disconnect(db_baseball);

db_tables=function(db_name) {
  DBI::dbListTables(db_name)
};
db_tables(db_baseball);

db_read=function(db_name, tbl_name) {
  DBI::dbReadTable(db_name, tbl_name)
};
db_read(db_baseball, "tbl_games");

db_write=function(db_name, tbl_name, df) {
  DBI::dbWriteTable(db_name, tbl_name, df, append=TRUE)
};
