library(shiny)
library(tidyverse)
library(dashboardthemes)
library(gsheet)
library(DT)
library(plotly)
library(shinydashboard)
library(shinythemes)

Pfad='https://docs.google.com/spreadsheets/d/1pxBP-J_CUCamMeEdShhxomC7PJO_IjyzICGJfhUO-1Y/edit#gid=0'
hei=NULL

sortbyprio=function(df){
  vec=c()
  for (i in df$Priority){
    
    if(i == "Sehr Hoch"){
      vec=c(vec,1)
    } else if (i == "Hoch"){
      vec=c(vec,2)
    } else if (i == "Mittel"){
      vec=c(vec,3)
    } else if (i == "Niedrig"){
      vec=c(vec,4)
    } 
  }
  df$sorter=vec
  df=df %>% arrange(sorter)
  return(df[,1:6])
}

getToDoDaten=function(DF){
  
  DF=DF %>% filter(Status != "Done")
  return(DF)
}

getDoneDaten=function(DF){
  
  DF=DF %>% filter(Status == "Done")
  return(DF)
}

getandmanip=function(Path){
  
  DF=gsheet2tbl(Path)
  DF=sortbyprio(DF)
  return(DF)
}

getBarplotPrio=function(DF){
  plota=DF %>% ggplot(aes(x=Priority,fill=Priority))+geom_bar()+
    theme_bw()+scale_x_discrete(limits=c("Sehr Hoch","Hoch","Mittel","Niedrig"))+ 
    labs(title="Priorityverteilung",y="Anzahl")
  plota=ggplotly(plota,height = hei)%>% 
    layout(plot_bgcolor='transparent') %>% 
    layout(paper_bgcolor='transparent') %>% layout(height = hei)
  return(plota)
  
}

makeTableBesidesPrio=function(DF){
  
  DF=DF %>% group_by(Priority) %>% count() %>% rename(Anzahl=n)
  return(datatable(DF,options = list(dom="t")))
}

getBarplotStatus=function(DF){
  plota=DF %>% ggplot(aes(x=Status,fill=Status))+geom_bar()+
    theme_bw()+scale_x_discrete(limits=c("To Do","Partly Done","Nur noch implementieren","Done"))+ 
    labs(title="Statusverteilung",y="Anzahl")
  plota=ggplotly(plota,height = hei)%>% 
    layout(plot_bgcolor='transparent') %>% 
    layout(paper_bgcolor='transparent')
  return(plota)
}

makeTableBesidesStatus=function(DF){
  
  DF=DF %>% group_by(Status) %>% count() %>% rename(Anzahl=n)
  return(datatable(DF,options = list(dom="t")))
}

getBarplotAutors=function(DF){
  DF %>% pivot_longer(-c(Task, Status, Priority, Notizen), values_to = "Autor") %>% filter(!is.na(Autor)) %>%
    ggplot(aes(x=Autor, fill=Priority))+geom_bar()+theme_bw()+labs(y="Anzahl",title="Tasks nach Autor") -> plota
  
  plota=ggplotly(plota,height = hei)%>% 
    layout(plot_bgcolor='transparent') %>% 
    layout(paper_bgcolor='transparent')
  return(plota)
}

makeTableAutors=function(DF){
  DF %>% pivot_longer(-c(Task, Status, Priority, Notizen), values_to = "Autor") %>%
    group_by(Autor) %>%
    summarise(Done=sum(Status == "Done"),
              ToDo=sum(Status == "To Do"),
              Partly=sum(Status == "Partly Done"),
              implem=sum(Status == "Nur noch implementieren"),
              Summe=sum(Done,ToDo,Partly,implem)) %>% 
    na.omit() -> DF
  colnames(DF)=c("Autor","Done","To Do", "Partly Done","Nur noch implementieren","Summe")
  return(datatable(DF,options = list(dom="t")))
}


formatDT=function(DF){
  
  Dttable= datatable(DF, class = 'cell-border stripe',filter = 'top',
                     options = list(pageLength = 15,
                                    columnDefs = list(
                                      list(orderable=TRUE, targets=0)
                                    ))) %>% 
    formatStyle(
      columns = c("Status"),
      valueColumns = c("Status"),
      backgroundColor = styleEqual(c("To Do","Partly Done","Nur noch implementieren","Done"), 
                                   c("red","rgb(205,155,29)","rgb(255,193,37)","rgb(179,238,58)"))
    ) %>%
    formatStyle(
      columns = c("Bearbeiter","Mitbearbeiter"),
      valueColumns = c("Bearbeiter","Mitbearbeiter"),
      color = styleEqual(
        c("Flo","Simon","Lena","Emil","Kasper","Andi","nicht zugeteilt"), 
        c('blue', 'rgb(64,224,208)','mediumorchid','red','orange','lightgreen', 'rgb(0,0,128')
      )
    ) %>%
    formatStyle(
      columns = c("Priority"),
      valueColumns = c("Priority"),
      backgroundColor = "rgb(211,211,211)",
      color = styleEqual(
        c("Sehr Hoch","Hoch","Mittel","Niedrig"),
        c("darkred","red", "darkorange", "blue")
      )
    )
  return(Dttable)
}


#### UI 


Header<- dashboardHeader(title ="Orbis Astea Übersicht")
#style = "color: #e30613; font-size: 38px"))

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text="Offene Aufgaben",tabName="DatenOffen"),
    menuItem(text="Alle Aufgaben",tabName="Daten"),
    menuItem(text="Gesamtanalytics",tabName="Analytics"),
    menuItem(text="Running Analytics",tabName="RunningAnalytics"),
    menuItem(text="Done Analytics",tabName="DoneAnalytics"),
    menuItem(text="Zusatzliches",tabName="Zusatz")
  )
)


body <- dashboardBody(
  # Create a tabBox
  tabItems(
    tabItem(
      tabName = "DatenOffen",
      DTOutput("datenToDO")
    ),
    tabItem(
      tabName = "Daten",
      DTOutput("datenalle")
    ),
    tabItem(
      tabName = "Analytics",
      fluidPage(
        h2("Analysen"),
        column(width=7,plotlyOutput("barplodPrio")),
        column(width=3, DTOutput("tableBarplotPrio")),
        
        column(width=8,plotlyOutput("barplodStatus")),
        column(width=2, DTOutput("tableBarplotStatus")),        
    
        column(width=7,plotlyOutput("barplodPrioAutoren")),
        column(width=3, DTOutput("tableBarplotAutoren"))
      )),
    tabItem(
      tabName = "RunningAnalytics",
      fluidPage(
        h2("Running Analysen"),
        column(width=7,plotlyOutput("barplodPrioRN")),
        column(width=3, DTOutput("tableBarplotPrioRN")),
        
        column(width=8,plotlyOutput("barplodStatusRN")),
        column(width=2, DTOutput("tableBarplotStatusRN")),        
        
        column(width=7,plotlyOutput("barplodPrioAutorenRN")),
        column(width=3, DTOutput("tableBarplotAutorenRN"))
      )),
    tabItem(
      tabName = "DoneAnalytics",
      fluidPage(
        h2("Done Analysen"),
        column(width=7,plotlyOutput("barplodPrioDN")),
        column(width=3, DTOutput("tableBarplotPrioDN")),
        column(width=7,plotlyOutput("barplodPrioAutorenDN")),
        column(width=3, DTOutput("tableBarplotAutorenDN"))
      )),
    tabItem(tabName = "Zusatz",
            fluidRow(
              column(width=10,includeMarkdown("Info.Rmd")),
            ))
  )
)

# Use the new sidebar
ui <- dashboardPage(skin="green",
                    header = Header,
                    sidebar = sidebar,
                    body = body)


server <- function(input, output, session) {
  
  DF <- reactiveFileReader(
    intervalMillis = 2*1000,
    session = session,
    filePath = Pfad,
    readFunc = function(filePath) {
      getandmanip(filePath)
    }
  )
  DFTD=reactive({getToDoDaten(DF())})
  DFDO=reactive({getDoneDaten(DF())})
  
  ## Daten
  output$datenalle=renderDT(formatDT(DF()))
  output$datenToDO=renderDT(formatDT(DFTD()))
  
  ## Analytics Gesamt
  output$barplodPrio=renderPlotly(getBarplotPrio(DF()))
  output$tableBarplotPrio=renderDT(makeTableBesidesPrio(DF()),width = "100%")
  output$barplodStatus=renderPlotly(getBarplotStatus(DF()))
  output$tableBarplotStatus=renderDT(makeTableBesidesStatus(DF()),width = "100%")
  output$barplodPrioAutoren=renderPlotly(getBarplotAutors(DF()))
  output$tableBarplotAutoren=renderDT(makeTableAutors(DF()),width = "100%")
  
  ## Analytics laufend
  output$barplodPrioRN=renderPlotly(getBarplotPrio(DFTD()))
  output$tableBarplotPrioRN=renderDT(makeTableBesidesPrio(DFTD()),width = "100%")
  output$barplodStatusRN=renderPlotly(getBarplotStatus(DFTD()))
  output$tableBarplotStatusRN=renderDT(makeTableBesidesStatus(DFTD()),width = "100%")
  output$barplodPrioAutorenRN=renderPlotly(getBarplotAutors(DFTD()))
  output$tableBarplotAutorenRN=renderDT(makeTableAutors(DFTD()),width = "100%")
  
  #Done Analytics
  output$barplodPrioDN=renderPlotly(getBarplotPrio(DFDO()))
  output$tableBarplotPrioDN=renderDT(makeTableBesidesPrio(DFDO()),width = "100%")  
  output$barplodPrioAutorenDN=renderPlotly(getBarplotAutors(DFDO()))
  output$tableBarplotAutorenDN=renderDT(makeTableAutors(DFDO()),width = "100%")
}


shinyApp(ui, server)
