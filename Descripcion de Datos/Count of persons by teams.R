
library(plotly)

# setear driver
m<-dbDriver("MySQL");

#Conectarse a la Base de datos MySQL
con<-dbConnect(m,user='root',password='admin',host='localhost',dbname='tsppacedb');

#incrementar cantidad de filas
options(max.print=1000000);

vectorT<-c();
vectorX<-c();
vectorY<-c();

#get count of projects
sqlcmd_1 <- paste("select count(*) from team");
teamCantQ <- dbSendQuery(con, sqlcmd_1);
teamAux <- dbFetch(teamCantQ, n = -1);
teamCant <- teamAux[1,1];

t = 1;

for (t in 1:teamCant) { 

  #get count of persons
  sqlcmd_2 <- paste("select count(*) from data_block WHERE team_key =", t);
  personsCantQ <- dbSendQuery(con, sqlcmd_2);
  personsAux <- dbFetch(personsCantQ, n = -1);
  personsCant <- personsAux[1,1];
  
  vectorX[t] <- t;
  vectorY[t] <- personsCant;
  
}

#desconectarse de la base
dbClearResult(dbListResults(con)[[1]]);
dbDisconnect(con);

#PLOTTING
py <- plotly()

data <- list(
  list(
    x = vectorX, 
    y = vectorY,
    text = vectorT,
    mode = "markers",
    marker = list(
      color = "rgb(164, 194, 244)",
      size = 12,
      line = list(
        color = "white",
        width = 0.5
      )
    ),
    type = "bar"
  )
)

layout <- list(
  title = "Cantidad de Personas por Equipo",
  font = list(family = "Raleway, sans-serif"),
  showlegend = FALSE,
  xaxis = list(
    title = "Equipos",
    tickangle = -45
  ),
  yaxis = list(
    title = "# Personas",
    zeroline = FALSE,
    gridwidth = 2
  ),
  bargap = 0.05
)

response <- py$plotly(data, kwargs=list(layout=layout, filename="#PersonsInTeam", fileopt="overwrite"))
url <- response$url

