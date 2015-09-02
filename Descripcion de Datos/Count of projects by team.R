############################################################
##############Plotting Count of wbs by project##############
############################################################

library(plotly)

# set driver
m<-dbDriver("MySQL");

#connect to the DB in MySQL
con<-dbConnect(m,user='root',password='admin',host='localhost',dbname='tsppacedb');

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
  
  #get total wbs
  sqlcmd_3 <- paste("(select count(*) from (SELECT distinct pi.project_key
                      FROM defect_log_fact_hist d
                      INNER JOIN plan_item pi ON d.plan_item_key = pi.plan_item_key
                      INNER JOIN data_block db ON d.data_block_key = db.data_block_key
                      where db.team_key = ", t, " ) as b)
                      UNION
                      (select count(*) from (SELECT distinct pi.project_key
                      FROM time_log_fact_hist d
                      INNER JOIN plan_item pi ON d.plan_item_key = pi.plan_item_key
                      INNER JOIN data_block db ON d.data_block_key = db.data_block_key
                      where db.team_key = ", t, " ) as b)
                      UNION
                      (select count(*) from (SELECT distinct pi.project_key
                      FROM task_date_fact_hist d
                      INNER JOIN plan_item pi ON d.plan_item_key = pi.plan_item_key
                      INNER JOIN data_block db ON d.data_block_key = db.data_block_key
                      where db.team_key = ", t, " ) as b)
                      UNION
                      (select count(*) from (SELECT distinct pi.project_key
                      FROM size_fact_hist d
                      INNER JOIN plan_item pi ON d.plan_item_key = pi.plan_item_key
                      INNER JOIN data_block db ON d.data_block_key = db.data_block_key
                      where db.team_key = ", t, " ) as b)");
  cantProjQ <- dbSendQuery(con, sqlcmd_3);
  cantProjAux <- dbFetch(cantProjQ, n = -1);
  cantProj <- cantProjAux[1,1];
  
  vectorX[t] <- t;
  vectorY[t] <- cantProj;
  
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
  title = "Cantidad de Proyectos por Equipo",
  font = list(family = "Raleway, sans-serif"),
  showlegend = FALSE,
  xaxis = list(
    title = "Equipos",
    tickangle = -45
  ),
  yaxis = list(
    title = "# Proyectos",
    zeroline = FALSE,
    gridwidth = 2
  ),
  bargap = 0.05
)

response <- py$plotly(data, kwargs=list(layout=layout, filename="#ProjectsInTeam", fileopt="overwrite"))
url <- response$url

