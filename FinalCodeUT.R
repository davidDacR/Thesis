############################################################
#Plotting PQI* of WBSs (X) with the defects found in UT (Y)#
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
                        WHERE s.row_current_flag = 1 and  m.measurement_type_name = 'Actual' and e.size_metric_name = 'Lines of Code' and pi.project_key  = ", proj, " and pi.wbs_element_key = ", c);
      sumSizeQ <- dbSendQuery(con, sqlcmd_4);
      sumSizeAux <- dbFetch(sumSizeQ, n = -1);
      sumSize <- sumSizeAux[1,1];
      
      if (sumSize > 0){
             
            #time log in phase CR 
            sqlcmd_7 <- paste("select ifnull(sum(tl.time_log_delta_minutes), 0) FROM time_log_fact_hist tl
                              INNER JOIN plan_item pi ON pi.plan_item_key = tl.plan_item_key
                              INNER JOIN wbs_element w ON w.wbs_element_key = pi.wbs_element_key
                              INNER JOIN phase ph ON ph.phase_key = pi.phase_key
                              WHERE tl.row_current_flag = 1 and pi.project_key  =  ", proj, " and pi.wbs_element_key = ", c, " and (ph.phase_name like 'Code Review' or ph.phase_name like 'TSP - Code Review');");
            sumCRQ <- dbSendQuery(con, sqlcmd_7);
            sumCRAux <- dbFetch(sumCRQ, n = -1);
            sumCR <- sumCRAux[1,1];
            
            if (sumCR > 0) {
              #time log in phase DR
              sqlcmd_8 <- paste("select ifnull(sum(tl.time_log_delta_minutes), 0) FROM time_log_fact_hist tl
                              INNER JOIN plan_item pi ON pi.plan_item_key = tl.plan_item_key
                              INNER JOIN wbs_element w ON w.wbs_element_key = pi.wbs_element_key
                              INNER JOIN phase ph ON ph.phase_key = pi.phase_key
                              WHERE tl.row_current_flag = 1 and pi.project_key  =  ", proj, " and pi.wbs_element_key = ", c, " and (ph.phase_name like 'Detailed Design Review' or ph.phase_name like 'TSP - Detailed Design Review');");
              sumDRQ <- dbSendQuery(con, sqlcmd_8);
              sumDRAux <- dbFetch(sumDRQ, n = -1);
              sumDR <- sumDRAux[1,1];
              
              if (sumDR > 0) {
                #time log in phase D
                sqlcmd_9 <- paste("select ifnull(sum(tl.time_log_delta_minutes), 0) FROM time_log_fact_hist tl
                              INNER JOIN plan_item pi ON pi.plan_item_key = tl.plan_item_key
                              INNER JOIN wbs_element w ON w.wbs_element_key = pi.wbs_element_key
                              INNER JOIN phase ph ON ph.phase_key = pi.phase_key
                              WHERE tl.row_current_flag = 1 and pi.project_key  =  ", proj, " and pi.wbs_element_key = ", c, " and (ph.phase_name like 'Detailed Design' or ph.phase_name like 'TSP - Detailed Design');");
                sumDQ <- dbSendQuery(con, sqlcmd_9);
                sumDAux <- dbFetch(sumDQ, n = -1);
                sumD <- sumDAux[1,1];
                
                if (sumD > 0) {
                  #time log in phase C
                  sqlcmd_10 <- paste("select ifnull(sum(tl.time_log_delta_minutes), 0) FROM time_log_fact_hist tl
                                     INNER JOIN plan_item pi ON pi.plan_item_key = tl.plan_item_key
                                     INNER JOIN wbs_element w ON w.wbs_element_key = pi.wbs_element_key
                                     INNER JOIN phase ph ON ph.phase_key = pi.phase_key
                                     WHERE tl.row_current_flag = 1 and pi.project_key  =  ", proj, " and pi.wbs_element_key = ", c, " and (ph.phase_name like 'Code' or ph.phase_name like 'TSP-Code');");
                  sumCQ <- dbSendQuery(con, sqlcmd_10);
                  sumCAux <- dbFetch(sumCQ, n = -1);
                  sumC <- sumCAux[1,1];
                  
                  if (sumC > 0) {
                    
                    #time log in phase UT
                    sqlcmd_11 <- paste("select ifnull(sum(tl.time_log_delta_minutes), 0) FROM time_log_fact_hist tl
                                      INNER JOIN plan_item pi ON pi.plan_item_key = tl.plan_item_key
                                      INNER JOIN wbs_element w ON w.wbs_element_key = pi.wbs_element_key
                                      INNER JOIN phase ph ON ph.phase_key = pi.phase_key
                                      WHERE tl.row_current_flag = 1 and pi.project_key  =  ", proj, " and pi.wbs_element_key = ", c, " and (ph.phase_name like 'Unit Test');");
                    sumUTQ <- dbSendQuery(con, sqlcmd_11);
                    sumUTAux <- dbFetch(sumUTQ, n = -1);
                    sumUT <- sumUTAux[1,1];
                    
                    if (sumUT > 0){
                      #total defects in UT
                      sqlcmd_12 <- paste("select ifnull(count(*), 0) FROM defect_log_fact_hist d
                                        INNER JOIN plan_item pi ON pi.plan_item_key = d.plan_item_key
                                        INNER JOIN wbs_element w ON w.wbs_element_key = pi.wbs_element_key
                                        INNER JOIN phase ph ON ph.phase_key = d.defect_removed_phase_key
                                        WHERE d.row_current_flag = 1 and pi.project_key  =  ", proj, " and pi.wbs_element_key = ", c, "and (ph.phase_name like 'Unit Test')");
                      defQ <- dbSendQuery(con, sqlcmd_12);
                      defAux <- dbFetch(defQ, n = -1);
                      def <- defAux[1,1];
                      
                      #Normalize
                      def <- round((def*1000)/sumSize,2);
                      
                      #calculate for D and DR
                      DRT <- round((2 * sumDR )/ sumD,1);
                      if (DRT > 1){
                        DRT <- 1;
                      }
                      
                      #calculate for C and CR
                      CRT <- round((2 * sumCR )/ sumC,1);
                      if (CRT > 1){
                        CRT <- 1;
                      }
                      
                      #calculate for C and D
                      CDRT <- round(sumD/sumC,1);
                      if (CDRT > 1){
                        CDRT <- 1;
                      }
                      
                      pqiA <- round(DRT*CRT*CDRT,2);
                      
                      vectorT[contCompGraf] <- paste(proj, " - ", c);
                      vectorX[contCompGraf] <- pqiA;
                      vectorY[contCompGraf] <- def;
                      contCompGraf <- contCompGraf + 1;
                      
                    }
                    
                    
                  }  
                  
                }
              }  
         }
        
      }

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
    type = "scatter"
  )
)

layout <- list(
  title = "#Def(UT / KLOC) - PQI*",
  font = list(family = "Raleway, sans-serif"),
  showlegend = FALSE,
  xaxis = list(
    title = "PQI* Componente",
    tickangle = -45
  ),
  yaxis = list(
    title = "#Def(UT / KLOC)",
    zeroline = FALSE,
    gridwidth = 2
  ),
  bargap = 0.05
)

response <- py$plotly(data, kwargs=list(layout=layout, filename="UniTestAllRPQI", fileopt="overwrite"))
url <- response$url

