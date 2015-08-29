#################
##Time/Projects## 
#################

library(plotly)

# set driver
m<-dbDriver("MySQL");

#connect to the DB in MySQL
con<-dbConnect(m,user='root',password='admin',host='localhost',dbname='tsppacedb');

options(max.print=1000000);
vectorT<-c();
vectorX<-c();
vectorY<-c();

#get the list of projects
sqlcmd_2 <- paste("select distinct(pi.project_key)  from  plan_item pi
                   JOIN defect_log_fact_hist d  where d.plan_item_key=pi.plan_item_key;");  
projectsQ <- dbSendQuery(con, sqlcmd_2);
projectsAux <- dbFetch(projectsQ, n = -1);

#get total projects
sqlcmd_3 <- paste("select count(*) from (select distinct(pi.project_key)  from  plan_item pi
                   JOIN defect_log_fact_hist d  where d.plan_item_key=pi.plan_item_key) as b");
cantProjQ <- dbSendQuery(con, sqlcmd_3);
cantProjAux <- dbFetch(cantProjQ, n = -1);
projectCant <- cantProjAux[1,1];

p = 1;
contCompGraf = 1;

for (p in 1:projectCant) {
  
  proj <- projectsAux[p,1];
             
  #time log
  sqlcmd_7 <- paste("select ifnull(sum(tl.time_log_delta_minutes), 0) FROM time_log_fact_hist tl
                    INNER JOIN plan_item pi ON pi.plan_item_key = tl.plan_item_key
                    WHERE tl.row_current_flag = 1 and pi.project_key  =  ", proj);
  timeQ <- dbSendQuery(con, sqlcmd_7);
  timeAux <- dbFetch(timeQ, n = -1);
  time <- timeAux[1,1];

  vectorX[contCompGraf] <- proj;
  vectorY[contCompGraf] <- time;
  contCompGraf <- contCompGraf + 1;
  
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
  title = "Tiempos por Proyecto",
  font = list(family = "Raleway, sans-serif"),
  showlegend = FALSE,
  xaxis = list(
    title = "Proyectos",
    tickangle = -45
  ),
  yaxis = list(
    title = "Tiempos en minutos",
    zeroline = FALSE,
    gridwidth = 2
  ),
  bargap = 0.05
)

response <- py$plotly(data, kwargs=list(layout=layout, filename="TimeByProject", fileopt="overwrite"))
url <- response$url

