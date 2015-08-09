
#enviar la salida a archivo de texto
sink("personsTeam.txt");

# setear driver
m<-dbDriver("MySQL");

#Conectarse a la Base de datos MySQL
con<-dbConnect(m,user='root',password='admin',host='localhost',dbname='tsppacedb');

#incrementar cantidad de filas
options(max.print=1000000);
n = 1;

for (n in 1:35) { 
  
  cat(paste("Team: ", n), "\n");
  
  #ensamblar sql command
  sqlcmd <- paste("select count(*) from data_block WHERE team_key =", n);
  #val<-write.table(dbGetQuery(con, sqlcmd), quote = FALSE, row.names = FALSE, col.names = FALSE);
  print(dbGetQuery(con, sqlcmd));
  cat("--------", "\n");
  
}

#desconectarse de la base
dbDisconnect(con);

#dejar de enviar la salida al archivo de texto
sink();

