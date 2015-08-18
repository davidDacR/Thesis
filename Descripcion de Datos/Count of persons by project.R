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

for (proj in 1:projectCant) {

  #get total wbs
  sqlcmd_3 <- paste("(select count(*) from (SELECT distinct db.person_key
                  FROM defect_log_fact_hist d
                  INNER JOIN plan_item pi ON d.plan_item_key = pi.plan_item_key
                  INNER JOIN data_block db ON d.data_block_key = db.data_block_key
                  where pi.project_key = ", proj, ") as b)
                  UNION
                  (select count(*) from (SELECT distinct db.person_key
                  FROM time_log_fact_hist d
                  INNER JOIN plan_item pi ON d.plan_item_key = pi.plan_item_key
                  INNER JOIN data_block db ON d.data_block_key = db.data_block_key
                  where pi.project_key = ", proj, ") as b)
                  UNION
                  (select count(*) from (SELECT distinct db.person_key
                  FROM task_date_fact_hist d
                  INNER JOIN plan_item pi ON d.plan_item_key = pi.plan_item_key
                  INNER JOIN data_block db ON d.data_block_key = db.data_block_key
                  where pi.project_key = ", proj, ") as b)
                  UNION
                  (select count(*) from (SELECT distinct db.person_key
                  FROM size_fact_hist d
                  INNER JOIN plan_item pi ON d.plan_item_key = pi.plan_item_key
                  INNER JOIN data_block db ON d.data_block_key = db.data_block_key
                  where pi.project_key = ", proj, ") as b);");
  cantPersQ <- dbSendQuery(con, sqlcmd_3);
  cantPersAux <- dbFetch(cantPersQ, n = -1);
  cantPers <- cantPersAux[1,1];
     
  vectorX[proj] <- proj
  vectorY[proj] <- cantPers;
  
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
  title = "Count of persons in Project",
  font = list(family = "Raleway, sans-serif"),
  showlegend = FALSE,
  xaxis = list(
    title = "Projects",
    tickangle = -45
  ),
  yaxis = list(
    title = "#Persons",
    zeroline = FALSE,
    gridwidth = 2
  ),
  bargap = 0.05
)

response <- py$plotly(data, kwargs=list(layout=layout, filename="#PersonsInProjects", fileopt="overwrite"))
url <- response$url

