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
  
  #get total wbs
  sqlcmd_3 <- paste("(SELECT team.team_key, project.project_key, person.person_key
                    FROM defect_log_fact_hist
                    INNER JOIN plan_item ON defect_log_fact_hist.plan_item_key = plan_item.plan_item_key
                    INNER JOIN project ON plan_item.project_key = project.project_key
                    INNER JOIN data_block ON defect_log_fact_hist.data_block_key = data_block.data_block_key
                    INNER JOIN person ON data_block.person_key = person.person_key
                    INNER JOIN team ON data_block.team_key = team.team_key
                    GROUP BY project.project_key, person.person_key, team_key
                    ORDER BY person_key)
                    UNION
                    (SELECT team.team_key, project.project_key, person.person_key
                    FROM time_log_fact_hist
                    INNER JOIN plan_item ON time_log_fact_hist.plan_item_key = plan_item.plan_item_key
                    INNER JOIN project ON plan_item.project_key = project.project_key
                    INNER JOIN data_block ON time_log_fact_hist.data_block_key = data_block.data_block_key
                    INNER JOIN person ON data_block.person_key = person.person_key
                    INNER JOIN team ON data_block.team_key = team.team_key
                    GROUP BY project.project_key, person.person_key, team_key
                    ORDER BY person_key)
                    UNION
                    (SELECT team.team_key, project.project_key, person.person_key
                    FROM task_date_fact_hist
                    INNER JOIN plan_item ON task_date_fact_hist.plan_item_key = plan_item.plan_item_key
                    INNER JOIN project ON plan_item.project_key = project.project_key
                    INNER JOIN data_block ON task_date_fact_hist.data_block_key = data_block.data_block_key
                    INNER JOIN person ON data_block.person_key = person.person_key
                    INNER JOIN team ON data_block.team_key = team.team_key
                    GROUP BY project.project_key, person.person_key, team_key
                    ORDER BY person_key)
                    UNION
                    (SELECT team.team_key, project.project_key, person.person_key
                    FROM size_fact_hist
                    INNER JOIN plan_item ON size_fact_hist.plan_item_key = plan_item.plan_item_key
                    INNER JOIN project ON plan_item.project_key = project.project_key
                    INNER JOIN data_block ON size_fact_hist.data_block_key = data_block.data_block_key
                    INNER JOIN person ON data_block.person_key = person.person_key
                    INNER JOIN team ON data_block.team_key = team.team_key
                    GROUP BY project.project_key, person.person_key, team_key
                    ORDER BY person_key)");
  cantCompQ <- dbSendQuery(con, sqlcmd_3);
  cantCompAux <- dbFetch(cantCompQ, n = -1);
  cantComp <- cantCompAux[1,1];
                      
  vectorX[contCompGraf] <- proj;
  vectorY[contCompGraf] <- cantComp;
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
  title = "Count of wbs by project",
  font = list(family = "Raleway, sans-serif"),
  showlegend = FALSE,
  xaxis = list(
    title = "Projects",
    tickangle = -45
  ),
  yaxis = list(
    title = "#WBS",
    zeroline = FALSE,
    gridwidth = 2
  ),
  bargap = 0.05
)

response <- py$plotly(data, kwargs=list(layout=layout, filename="#WBSByProj", fileopt="overwrite"))
url <- response$url

