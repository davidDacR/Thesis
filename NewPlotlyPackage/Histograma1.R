
#the component size data


library(plotly)

# set driver
m<-dbDriver("MySQL");

#connect to the DB in MySQL
con<-dbConnect(m,user='root',password='root',host='localhost',dbname='tsppacedb');

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
  
  if (cantComp > 0) {
    
    for (comp in 1:cantComp) {
      
      c <- componentsAux[comp,1];
      
      #get Size of the Component
      sqlcmd_4 <- paste("select ifnull(sum(s.size_added_and_modified), 0) FROM size_fact_hist s 
                        INNER JOIN plan_item pi ON pi.plan_item_key = s.plan_item_key
                        INNER JOIN wbs_element w ON w.wbs_element_key = pi.wbs_element_key 
                        INNER JOIN measurement_type m ON m.measurement_type_key = s.measurement_type_key
                        INNER JOIN size_metric e ON e.size_metric_key = s.size_metric_key
                        INNER JOIN phase ph ON ph.phase_key = pi.phase_key
                        WHERE s.row_current_flag = 1 and  m.measurement_type_name = 'Actual' and e.size_metric_name = 'Lines of Code' and (ph.phase_name like 'Code' or ph.phase_name like 'TSP - Code') and pi.project_key  = ", proj, " and pi.wbs_element_key = ", c);
      sumSizeQ <- dbSendQuery(con, sqlcmd_4);
      sumSizeAux <- dbFetch(sumSizeQ, n = -1);
      sumSize <- sumSizeAux[1,1];
      
      if (sumSize > 0){
        vectorX[contCompGraf] <- paste(proj, " - ", c);
        vectorY[contCompGraf] <- sumSize;
        contCompGraf <- contCompGraf + 1;
        
      }
      
  }
  }
  
  }

#desconectarse de la base
dbClearResult(dbListResults(con)[[1]]);
dbDisconnect(con);
#PLOTTING
#Este codigo plotly usa la version 1.0
WBS = vectorX
Size = vectorY;
p <- plot_ly( x = WBS, 
              y = Size,
              name="the component size data",
              type = "bar",
              filename="histogramas/histograma1")
layout(p, title = "the component size data")
print(p)

#Este codigo plotly usa la version anterior
# py <- plotly()
# 
# data <- list(
#   list(
#     x = vectorX,
#     y = vectorY,
#     type = "bar"
#   )
# )
# 
# layout <- list(
#   title = "the component size data",
#   font = list(family = "Raleway, sans-serif"),
#   showlegend = FALSE,
#   xaxis = list(
#     title = "WBS",
#     tickangle = -45
#   ),
#   yaxis = list(
#     title = "Size",
#     zeroline = FALSE,
#     gridwidth = 2
#   ),
#   bargap = 0.05
# )
# 
# response <- py$plotly(data, kwargs=list(layout=layout, filename="Histograma1", fileopt="overwrite"))
# url <- response$url
