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
  sqlcmd_3 <- paste("select count(*) from (select distinct(pi.wbs_element_key) FROM plan_item pi
                              WHERE pi.project_key  = ", proj, ") as b;");
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
  title = "Count of wbs in project",
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

response <- py$plotly(data, kwargs=list(layout=layout, filename="#WBSInProj", fileopt="overwrite"))
url <- response$url

