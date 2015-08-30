
#project defect counts

library(plotly)

# set driver
m<-dbDriver("MySQL");

#connect to the DB in MySQL
con<-dbConnect(m,user='root',password='admin',host='localhost',dbname='tsppacedb');

options(max.print=1000000);
vectorT<-c();
vectorX<-c();
vectorY<-c();

#get total projects
sqlcmd_3 <- paste("select count(*) from project");
cantProjQ <- dbSendQuery(con, sqlcmd_3);
cantProjAux <- dbFetch(cantProjQ, n = -1);
projectCant <- cantProjAux[1,1];

proj = 1;
contCompGraf = 1;

for (proj in 1:projectCant) {
      
  sqlcmd_4 <- paste("select ifnull(count(*), 0) FROM defect_log_fact_hist d
                    INNER JOIN plan_item pi ON pi.plan_item_key = d.plan_item_key
                    WHERE d.row_current_flag = 1 and pi.task_key IS NOT NULL and pi.project_key  = ", proj);
  countD <- dbSendQuery(con, sqlcmd_4);
  countDAux <- dbFetch(countD, n = -1);
  contar <- countDAux[1,1];
      
  vectorX[contCompGraf] <- proj;
  vectorY[contCompGraf] <- contar;
  contCompGraf <- contCompGraf + 1;
                  
}

#desconectarse de la base
dbClearResult(dbListResults(con)[[1]]);
dbDisconnect(con);

py <- plotly()

data <- list(
  list(
    x = vectorX,
    y = vectorY,
    type = "bar"
  )
)

layout <- list(
  title = "Cantidad de Defectos por Proyecto",
  font = list(family = "Raleway, sans-serif"),
  showlegend = FALSE,
  xaxis = list(
    title = "Proyectos",
    tickangle = -45
  ),
  yaxis = list(
    title = "# Defectos",
    zeroline = FALSE,
    gridwidth = 2
  ),
  bargap = 0.05
)

response <- py$plotly(data, kwargs=list(layout=layout, filename="#DefectsInProjects", fileopt="overwrite"))
url <- response$url
