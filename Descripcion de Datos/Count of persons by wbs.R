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
sqlcmd_1 <- paste("select count(*) from project");
projectCantQ <- dbSendQuery(con, sqlcmd_1);
projectCantAux <- dbFetch(projectCantQ, n = -1);
projectCant <- projectCantAux[1,1];

proj = 1;
contCompGraf = 1;

for (proj in 1:projectCant) {
  
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

      #get total wbs
      sqlcmd_3 <- paste("(select count(*) from (SELECT distinct db.person_key, pi.project_key, pi.wbs_element_key
                      FROM defect_log_fact_hist d
                      INNER JOIN plan_item pi ON d.plan_item_key = pi.plan_item_key
                      INNER JOIN data_block db ON d.data_block_key = db.data_block_key
                      where pi.project_key = ", proj, " and pi.wbs_element_key = ", c,") as b)
                      UNION
                      (select count(*) from (SELECT distinct db.person_key, pi.project_key, pi.wbs_element_key
                      FROM time_log_fact_hist d
                      INNER JOIN plan_item pi ON d.plan_item_key = pi.plan_item_key
                      INNER JOIN data_block db ON d.data_block_key = db.data_block_key
                      where pi.project_key = ", proj, " and pi.wbs_element_key = ", c,") as b)
                      UNION
                      (select count(*) from (SELECT distinct db.person_key, pi.project_key, pi.wbs_element_key
                      FROM task_date_fact_hist d
                      INNER JOIN plan_item pi ON d.plan_item_key = pi.plan_item_key
                      INNER JOIN data_block db ON d.data_block_key = db.data_block_key
                      where pi.project_key = ", proj, " and pi.wbs_element_key = ", c,") as b)
                      UNION
                      (select count(*) from (SELECT distinct db.person_key, pi.project_key, pi.wbs_element_key
                      FROM size_fact_hist d
                      INNER JOIN plan_item pi ON d.plan_item_key = pi.plan_item_key
                      INNER JOIN data_block db ON d.data_block_key = db.data_block_key
                      where pi.project_key = ", proj, " and pi.wbs_element_key = ", c,") as b);");
      cantPersQ <- dbSendQuery(con, sqlcmd_3);
      cantPersAux <- dbFetch(cantPersQ, n = -1);
      cantPers <- cantPersAux[1,1];
         
      #vectorT[contCompGraf] <- paste(proj, " - ", c);
      vectorX[contCompGraf] <- paste(proj, " - ", c);
      vectorY[contCompGraf] <- cantPers;
      contCompGraf <- contCompGraf + 1;
  
    }
  
  }
  
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
  title = "Count of persons in WBS",
  font = list(family = "Raleway, sans-serif"),
  showlegend = FALSE,
  xaxis = list(
    title = "WBS",
    tickangle = -45
  ),
  yaxis = list(
    title = "#Persons",
    zeroline = FALSE,
    gridwidth = 2
  ),
  bargap = 0.05
)

response <- py$plotly(data, kwargs=list(layout=layout, filename="#PersonsInWBS", fileopt="overwrite"))
url <- response$url

