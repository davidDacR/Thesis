#################
##Size/Projects## 
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
             
  #size log
  sqlcmd_7 <- paste("select ifnull(sum(s.size_added_and_modified), 0) FROM size_fact_hist s 
                    INNER JOIN plan_item pi ON pi.plan_item_key = s.plan_item_key
                    INNER JOIN measurement_type m ON m.measurement_type_key = s.measurement_type_key
                    INNER JOIN size_metric e ON e.size_metric_key = s.size_metric_key
                    INNER JOIN phase ph ON ph.phase_key = pi.phase_key
                    WHERE s.row_current_flag = 1 and  m.measurement_type_name = 'Actual' and e.size_metric_name = 'Lines of Code' and (ph.phase_name like 'Code' or ph.phase_name like 'TSP - Code') and pi.project_key  = ", proj);
  sizeQ <- dbSendQuery(con, sqlcmd_7);
  sizeQAux <- dbFetch(sizeQ, n = -1);
  sumSize <- sizeQAux[1,1];
  
  size <- round((sumSize/1000),2);

  vectorX[contCompGraf] <- proj;
  vectorY[contCompGraf] <- size;
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
  title = "Size by Project",
  font = list(family = "Raleway, sans-serif"),
  showlegend = FALSE,
  xaxis = list(
    title = "Projects",
    tickangle = -45
  ),
  yaxis = list(
    title = "Size in KLOCS",
    zeroline = FALSE,
    gridwidth = 2
  ),
  bargap = 0.05
)

response <- py$plotly(data, kwargs=list(layout=layout, filename="SizeByProject", fileopt="overwrite"))
url <- response$url

