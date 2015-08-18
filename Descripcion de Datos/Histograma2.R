
#component defect counts

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
  
  #get the list of wbs
  sqlcmd_2 <- paste("select distinct(w.wbs_element_key) FROM time_log_fact_hist tl
                              INNER JOIN plan_item pi ON pi.plan_item_key = tl.plan_item_key
                              INNER JOIN wbs_element w ON w.wbs_element_key = pi.wbs_element_key
                              INNER JOIN phase ph ON ph.phase_key = pi.phase_key
                              WHERE tl.row_current_flag = 1 and pi.task_key IS NOT NULL and pi.project_key  =  ", proj);  
  componentsQ <- dbSendQuery(con, sqlcmd_2);
  componentsAux <- dbFetch(componentsQ, n = -1);
  
  #get total wbs
  sqlcmd_3 <- paste("select count(*) from (select distinct(w.wbs_element_key) FROM time_log_fact_hist tl
                              INNER JOIN plan_item pi ON pi.plan_item_key = tl.plan_item_key
                              INNER JOIN wbs_element w ON w.wbs_element_key = pi.wbs_element_key
                              INNER JOIN phase ph ON ph.phase_key = pi.phase_key
                              WHERE tl.row_current_flag = 1 and pi.task_key IS NOT NULL and pi.project_key  =  ", proj, ") as b");
  cantCompQ <- dbSendQuery(con, sqlcmd_3);
  cantCompAux <- dbFetch(cantCompQ, n = -1);
  cantComp <- cantCompAux[1,1];
  
  comp <- 1;
  
  comp <- 1;
  
  if (cantComp > 0) {
    
    for (comp in 1:cantComp) {
      
      c <- componentsAux[comp,1];
      
      sqlcmd_4 <- paste("select ifnull(count(*), 0) FROM defect_log_fact_hist d 
                  INNER JOIN plan_item pi ON pi.plan_item_key = d.plan_item_key
                  INNER JOIN wbs_element w ON w.wbs_element_key = pi.wbs_element_key 
                  INNER JOIN phase ph2 ON ph2.phase_key = d.defect_removed_phase_key
                  WHERE d.row_current_flag = 1 and pi.task_key IS NOT NULL and pi.project_key  = ", proj, " and pi.wbs_element_key = ", c);
      countD <- dbSendQuery(con, sqlcmd_4);
      countDAux <- dbFetch(countD, n = -1);
      contar <- countDAux[1,1];
          
      vectorX[contCompGraf] <- paste(proj, " - ", c);
      vectorY[contCompGraf] <- contar;
      contCompGraf <- contCompGraf + 1;
          
    }
                    
                    
  }  
                  
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
  title = "Component defect counts",
  font = list(family = "Raleway, sans-serif"),
  showlegend = FALSE,
  xaxis = list(
    title = "WBS",
    tickangle = -45
  ),
  yaxis = list(
    title = "# Defects",
    zeroline = FALSE,
    gridwidth = 2
  ),
  bargap = 0.05
)

response <- py$plotly(data, kwargs=list(layout=layout, filename="Histograma2", fileopt="overwrite"))
url <- response$url
