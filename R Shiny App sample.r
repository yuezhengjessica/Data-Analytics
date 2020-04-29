## app.R##

#source('C:/User/YZheng/Export Reports by Cust-Item Sales and Margin.R',echo=TRUE)

install.packages('shinydashboard')
install.packages('DT')
install.packages('rsconnect')
install.packages('installr')
library(shinydashboard)
library(DT)
library(installr)


#=========================UI==========================================
#===============Header

header<-dashboardHeader(title='ImperialDade Sales Analysis Dashboard')

sidebar<-dashboardSidebar(
      ##Sidebar Content
      dashboardSidebar(
            sidebarMenu(
                  menuItem("Dashboard",tabName="dashboard",icon=icon("dashboard")),
                  menuItem("S2K",tabName="S2K",icon=icon("th")),
                  menuItem("DPC",tabName="DPC",icon=icon("th")),
                  menuItem("Other Acquistions",tabName="Other Acquisitions",icon=icon("th"))
            )
      )
)


#==============Body

body<-dashboardBody(
      tabItems(
            #===============first tab content==============
            tabItem(tabName="dashboard",
                    h2("Warehouse Top Customers Performance"),
                    fluidRow(
                          box(plotOutput("plot1",height=250)),
                          box(
                                title="Controls",
                                sliderInput("Slider","Number of observations:",1,100,50))
                    )),
            #================second tab content===============
            tabItem(tabName="S2K",
                    h2("S2K Location Daily Sales Performance"),
                    fluidRow(
                          #A Static infoBox
                          infoBoxOutput("yesterdayBox"),
                          #Dynamic infoBoxs
                          infoBoxOutput("progressBox"),
                          infoBoxOutput("approvalBox")
                    ),
                    fluidRow(
                          box(title="Top 10 Sales Customers",solidHeader=TRUE,collapsible=TRUE,
                              tableOutput("Results"),height=450,weight=100),
                          box(title="Top 10 Sales Items",solidHeader=TRUE,collapsible=TRUE,
                              tableOutput("itemResults"),height=450,weight=100),
                          box(title="Top 10 Sales Customer Daily Margin Trend",solidHeader=TRUE,
                              collapsible=TRUE,
                              tableOutput("trendResults"),height=450),
                          box(title="Details",solidHeader=TRUE,collapsible=TRUE,
                              tableOutput("custDetails"),height=450),
                          box(title="S2K Location",
                              status="warning",collapsible=TRUE,
                              selectInput("cust",
                                          "Customer:",
                                          c("All",
                                            unique(as.character(IMP_Report$ParentCust))))
                              ,
                              radioButtons("countryInput","S2K Location",
                                           choice=c("JC","NJBT","MA","NC","FLT1","KC","GAA1"),
                                           selected="JC")
                    )
                          
                    )),
            #=========================third tab content=============
            tabItem(tabname="DPC",
                    h2("Dade Location Daily Sales Performance"),
                    fluidRow(
                          #A static infoBox
                          infoBoxOutput("dadeyesterdayBox"),
                          #Dynamic infoBoxes
                          infoBoxOutput("dadeprogressBox"),
                          infoBoxOutput("dadeapprovalBox")
                    ),
                    fluidRow(
                          box(title="Top 10 Sales Customers",solidHeader=TRUE,collapsible=TRUE,
                              tableOutput("dadeResults"),height=450,weight=100),
                          box(title="Top 10 Sales Items",solidHeader=TRUE,collapsible=TRUE,
                              tableOutput("dadeitemResults"),height=450,weight=100),
                          box(title="Top 10 Sales Customer Daily Margin Trend",solidHeader=TRUE,collapsible=TRUE,
                              tableOutput("dadetrendResults"),height=450),
                          box(title="Details",solidHeader=TRUE,collapsible=TRUE,
                              tableOutput("dadecustDetails"),height=450),
                          box(title="Inputs",status="warning",collapsible=TRUE,
                              selectInput("dadecust",
                                          "Customer:",
                                          c("All",
                                            unique(as.character(DPC_Report$CustomerName))))
                              ,
                              radioButtons("dadeInput","Dade Location",
                                           choices=c("Miami","Orlando","Jessup","Loxley","Jacksonville","Greensboro","Bellingham"),
                                           selected="Miami"))
                    )
            )))



# dashboard Page

ui<-dashboardPage(header,sidebar,body)

server<-function(input,output){
      set.seed(122)
      histdata<-rnorm(500)
      check_margin<-reactive({
            Data<-IMP_Report%>%
                  filter(SBLOC==input$countryInput,SBINDT==Sys.Date()-1)
            Data$Sales<-ifelse(is.na(Data$Sales),0,Data$Sales)
            Data$Net_Cost<-ifelse(is.na(Data$Net_Cost),0,Data$Net_Cost)
            margin<-(sum(Data$Sales)-sum(Data$Net_Cost))/sum(Data$Sales)
            margin
      })
      
      check_Margin_without_100Sales<-reactive({
            #GP without 100% margin
            yesterday<-IMP_Report%>%
                  filter(SBLOC==input$countryInput,Margin<1,SBINDT==Sys.Date()-1)
            Margin_without_100Sales<-(sum(yesterday$Sales)-sum(yesterday$Net_Cost))/sum(yesterday$Sales)
            Margin_without_100Sales
      })
      
      check_Margin_without_credits<-reactive({
            #GP without credits
            yesterday_WO_credit<-IMP_Report%>%
                  filter(SBLOC==input$countryInput,SBTYPE=="O",SBINDT==Sys.Date()-1)
            Margin_without_credits<-(sum(yesterday_WO_credit$Sales)-sum(yesterday_WO_credit$Net_Cost))/sum(yesterday_WO_credit$Sales)
            Margin_without_credits
      })
      
      dade_check_margin<-reactive({
            Data<-DPC_Report%>%
                  filter(Location==input$dadeInput,INVOICE_DATE==Sys.Date()-1)
            Data$Sales=ifelse(is.na(Data$Sales),0,Data$Sales)
            Data$COGS_Net=ifelse(is.na(Data$COGS_Net),0,Data$COGS_Net)
            margin<-(sum(Data$Sales)-sum(Data$COGS_Net))/sum(Data$Sales)
            margin
      })
      
      dade_check_Margin_without_100Sales<-reactive({
            #GP without 100% margin Sales
            yesterday<-DPC_Report%>%
                  filter(Location==input$dadeInput,Margin<=0.99,INVOICE_DATE==Sys.Date()-1)
            Margin_without_100Sales<-(sum(yesterday$Sales)-sum(yesterday$COGS_Net))/sum(yesterday$Sales)
            Margin_without_100Sales
      })
      
      dade_check_Margin_without_credits<-reactive({
            yesterday1<-DPC_Report%>%
                  filter(Location==input$dadeInput,STATUS=="30",INVOICE_DATE==Sys.Date()-1)
            Margin_without_credits<-(sum(yesterday1$Sales)-sum(yesterday1$COGS_Net))/sum(yesterday1$Sales)
            Margin_without_credits
      })
      
      check_location<-function(Entity,Loc,N){
            list(Margin_Check=check_LastNday_margin(Entity,Loc,N))
      }
      
      
      #============IMP check margin - parent customer
      
      IMP_custName_margin<-reactive({
            IMP_Report$SBCUST<-str_trim(IMP_Report$SBCUST)
            
            table1<-IMP_Report%>%
                  filter(SBLOC==input$countryInput,SBINDT<Sys.Date()-1)%>%
                  group_by(ParentCust)%>%
                  summarise(Sales=sum(Sales),COGS_Net=sum(Net_Cost))%>%
                  mutate(Margin=(Sales-COGS_Net)/Sales)%>%
                  mutate(Sales_Mix=Sales/sum(Sales))%>%
                  arrange(desc(Sales_Mix),Margin)
            
            table<-IMP_Report%>%
                  filter(SBLOC==input$countryInput,SBINDT==Sys.Date()-1)%>%
                  group_by(ParentCust)%>%
                  summarise(Sales=sum(Sales),COGS_Net=sum(Net_Cost))%>%
                  mutate(Margin=(Sales-COGS_Net)/Sales)%>%
                  mutate(QTD_Average_Margin=table1$Margin[match(ParentCust,table1$ParentCust)])%>%
                  mutate(Sales_Mix=Sales/sum(Sales))%>%
                  arrange(desc(Sales_Mix),Margin)
            table
      })
      
      dade_custName_margin<-reactive({
            result1<-DPC_Report%>%filter(Location==input$dadeInput)%>%
                  filter(INVOICE_DATE<Sys.Date()-1)%>%
                  group_by(CustomerName)%>%
                  summarise(Sales=sum(Sales),COGS_Net=sum(COGS_Net))%>%
                  mutate(Margin=(Sales-COGS_Net)/Sales)%>%
                  mutate(Sales_Mix=Sales/sum(Sales))%>%
                  arrange(desc(Sales_Mix),desc(Margin))
            
            result<-DPC_Report%>%filter(Location==input$dadeInput)%>%
                  filter(INVOICE_DATE==Sys.Date()-1)%>%
                  group_by(CustomerName)%>%
                  summarise(Sales=sum(Sales),COGS_Net=sum(COGS_Net))%>%
                  mutate(Margin=(Sales-COGS_Net)/Sales)%>%
                  mutate(QTD_Avg_Margin=result1$Margin[match(CustomerName,result1$CustomerName)])%>%
                  mutate(Sales_Mix=Sales/sum(Sales))%>%
                  arrange(desc(Sales_Mix),Margin)
            
            result
      })

      
      IMP_Item_margin<-reactive({
            IMP_Report$SBCUST<-str_trim(IMP_Report$SBCUST)
            
            table1$IMP_Report%>%
                  filter(SBLOC==input$countryInput,SBINDT<Sys.Date()-1)%>%
                  group_by(SBITEM)%>%
                  summarise(Sales=sum(Sales),COGS_Net=sum(Net_Cost))%>%
                  mutate(Margin=(Sales-COGS_Net)/Sales)%>%
                  mutate(Sales_Mix=Sales/sum(Sales))%>%
                  arrange(desc(Sales_Mix),Margin)
            
            table<-IMP_Report%>%
                  filter(SBLOC==input$countryInput,SBINDT==Sys.Date()-1)%>%
                  group_by(SBITEM)%>%
                  summarise(Sales=sum(Sales),COGS_Net=sum(Net_Cost))%>%
                  mutate(Margin=(Sales-COGS_Net)/Sales)%>%
                  mutate(QTD_Average_Margin=table1$Margin[match(SBITEM,table1$SBITEM)])%>%
                  mutate(Sales_Mix=Sales/sum(Sales))%>%
                  arrange(desc(Sales_Mix),Margin)
            
            table
      })
      
      dade_item_margin<-reactive({
            result1<-DPC_Report%>%filter(Location==input$dadeInput)%>%
                  filter(INVOICE_DATE<Sys.Date()-1)%>%
                  group_by(PRODUCT_CODE)%>%
                  summarise(Sales=sum(Sales),COGS_Net=sum(COGS_Net))%>%
                  mutate(Margin=(Sales-COGS_Net)/Sales)%>%
                  mutate(Sales_Mix=Sales/sum(Sales))%>%
                  arrange(desc(Sales_Mix),desc(Margin))
            
            result<-DPC_Report%>%filter(Location==input$dadeInput)%>%
                  filter(INVOICE_DATE==Sys.Date()-1)%>%
                  group_by(PRODUCT_CODE)%>%
                  summarise(Sales=sum(Sales),COGS_Net=sum(COGS_Net))%>%
                  mutate(Margin=(Sales-COGS_Net)/Sales)%>%
                  mutate(QTD_Avg_Margin=result1$Margin[match(PRODUCT_CODE,result1$PRODUCT_CODE)])%>%
                  mutate(Sales_Mix=Sales/sum(Sales))%>%
                  arrange(desc(Sales_Mix),Margin)
            result
      })  
       
      IMP_custName_Margin_trend<-reactive({
            IMP_Report$SBCUST<-str_trim(IMP_Report$SBCUST)
            table<-IMP_Report%>%
                  filter(SBLOC==input$countryInput)%>%
                  group_by(SBINDT,ParentCust)%>%
                  summarise(Cust_Sales=sum(Sales),Cust_COGS_Net=sum(Net_Cost))%>%
                  mutate(Cust_Margin=(Cust_Sales-Cust_COGS_Net)/Cust_Sales)%>%
                  select(ParentCust,SBINDT,Cust_Margin)%>%
                  spread(key=ParentCust,value=Cust_Margin)%>%
                  select(SBINDT,head(IMP_custName_margin(),10)$ParentCust)%>%
                  arrange(desc(SBINDT))
            table$SBINDT<-as.character(table$SBINDT)
            table
      })
      
      
      Top_cust_details<-reactive({
            tab<-IMP_Report%>%
                  filter(SBLOC==input$countryInput,SBINDT==Sys.Date()-1)%>%
                  group_by(ParentCust)%>%
                  summarise(Sales=sum(Sales),COGS_Net=sum(Net_Cost))%>%
                  slice(1:10)
            top_cust<-head(tab,10)$ParentCust
            
            All_IMP<-IMP_Report%>%
                  filter(SBLOC==input$countryInput)%>%
                  filter(SBINDT<Sys.Date()-1&SBINDT>as.Date("2019-11-01")&ParentCust%in%tab$ParentCust)%>%
                  group_by(`Cust-Item-Loc`)%>%
                  summarise(Sales=sum(Sales),Cost=sum(Net_Cost))%>%
                  mutate(Margin=(Sales-Cost)/Sales)
            
            if(input$cust=="All"){
                  IMP_wacky_Record<-IMP_Report%>%
                        filter(SBLOC==input$countryInput,SBINDT==Sys.Date()-1)%>%
                        group_by(ParentCust,`Cust-Item-Loc`)%>%
                        summarise(Sales=sum(Sales),Cost=sum(Net_Cost))%>%
                        mutate(Margin=(Sales-Cost)/Sales)%>%
                        mutate(Avg_Margin=All_IMP$Margin[match(`Cust-Item-Loc`,All_IMP$`Cust-Item-Loc`)])%>%
                        mutate(var=Margin-Avg_Margin)%>%
                        arrange(desc(Sales))
            }else{
                  IMP_Report$ParentCust<-str_trim(IMP_Report$ParentCust)
                  IMP_wacky_Record<-IMP_Report%>%
                        filter(SBLOC==input$countryInput,SBINDT==Sys.Date()-1,ParentCust==input$cust)%>%
                        group_by(ParentCust,`Cust-Item-Loc`)%>%
                        summarise(Sales=sum(Sales),Cost=sum(Net_Cost))%>%
                        mutate(Margin=(Sales-Cost)/Sales)%>%
                        mutate(Avg_Margin=All_IMP$Margin[match(`Cust-Item-Loc`,All_IMP$`Cust-Item-Loc`)])%>%
                        mutate(var=Margin-Avg_Margin)%>%arrange(desc(Sales))
            }
            IMP_wacky_Record
      })
      
      
      
      #========================dade cust details===========
      
      dade_Top_cust_details<-reactive({
            tab<-DPC_Report%>%
                  filter(Location==input$dadeInput)%>%
                  filter(INVOICE_DATE<Sys.Date()-1)%>%
                  group_by(CustomerName)%>%
                  summarise(Sales=sum(Sales),COGS_Net=sum(COGS_Net))%>%
                  slice(1:10)
            
            top_cust<-head(tab,10)$CustomerName
            
            All_DPC<-DPC_Report%>%
                  filter(Location==input$dadeInput)%>%
                  filter(INVOICE_DATE<Sys.Date()-1)&INVOICE_DATE>as.Date("2019-10-01")&CustomerName%in%tab$CustomerName)%>%
            group_by(`Cust-Item-Loc`)%>%
            summarise(Sales=sum(Sales),Cost=sum(COGS_Net))%>%
            mutate(Margin=(Sales-Cost)/Sales)
      
      if(input$dadecust=='All'){
            DPC_wacky_Record<-DPC_Report%>%
                  filter(Location==input$dadeInput,INVOICE_DATE==Sys.Date()-1)%>%
                  group_by(CustomerName,`Cust-Item-Loc`)%>%
                  summarise(Sales=sum(Sales),Cost=sum(COGS_Net))%>%
                  mutate(Margin=(Sales-Cost)/Sales)%>%
                  mutate(Avg_Margin=All_DPC$Margin[match(`Cust-Item-Loc`,All_DPC$`Cust-Item-Loc`)])%>%
                  mutate(var=Margin-Avg_Margin)%>%arrange(desc(Sales))
      }else{
            DPC_wacky_Record<-DPC_Report%>%
                  filter(Location==input$dadeInput,INVOICE_DATE==Sys.Date()-1,
                         CustomerName==input$dadecust)%>%
                  group_by(CustomerName,`Cust-Item-Loc`)%>%
                  summarise(Sales=sum(Sales),Cost=sum(COGS_Net))%>%
                  mutate(Margin=(Sales-Cost)/Sales)%>%
                  mutate(Avg_Margin=All_DPC$Margin[match(`Cust-Item-Loc`,All_DPC$`Cust-Item-Loc`)])%>%
                  mutate(var=Margin-Avg_Margin)%>%arrange(desc(Sales))
      }
      
      DPC_wacky_Record
      })
      


#==========================OUTPUT=============
output$plot1<-renderPlot({
      data<-histdata[seq_len(input$slider)]
      hist(data)
})
output$Results<-renderTable(head(IMP_custName_margin(),10))
output$dadeResults<-renderTable(dade_custName_margin(),10))
output$itemResults<-renderTable(head(IMP_Item_margin(),10))
output$dadeitemResults<-renderTable(head(dade_item_margin(),10))
output$custDetails<-renderTable(Top_cust_details())
output$dadecustDetails<-renderTable(dade_Top_cust_details())
output$trendResults<-renderTable(head(IMP_custName_Margin_trend(),10))
output$yesterdayBox<-renderInfoBox({
      infoBox("Yesterday Margin",check_margin(),icon=icon("credit-card"))
})
output$progressBox<-renderInfoBox({
      infoBox("Margin_without_100%Sales",check_Margin_without_100Sales(),icon=icon("list"),
              color="purple")
})
output$approvalBox<-renderInfoBox({
      infoBox("Margin_without_Credits",check_Margin_without_credits(),icon=icon("list"),
              color="green")
})
output$dadeyesterdayBox<-renderInfoBox({
      infoBox("Yesterday Margin",dade_check_margin(),icon=icon("credit-card"))
})
output$dadeprogressBox<-renderInfoBox({
      infoBox("Margin_without_100%Sales",dade_check_Margin_without_100Sales(),icon=icon("list"),
              color="purple")
})
output$dadeapprovalBox<-renderInfoBox({
      infoBox("Margin_without_Credits",dade_check_Margin_without_credits(),icon=icon("list"),
              color="green")
})
}

shinyApp(ui,server)