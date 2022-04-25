#---------------------------BUILD THE APP-------------------------------
library(shiny)
library(shinythemes)
library(DT)
library(dplyr)
library(ggplot2)
library(dummies)
library(corrplot)
library(caret)
library(stats)
library(forecast)
library(gains)
library(cluster)
library(factoextra)
library(purrr)
library(nnet)
library(neuralnet)
library(NeuralNetTools)
library(OneR)
library(FNN)
options(scipen=999)

# I could not locate the issue while deploying the app, so this code is used to trace
options(shiny.trace=TRUE)

setwd("~/Sales")
# Load dataset 
sales_file <- readRDS("data/sales_file.rds")
sales.df <- data.frame(sales_file)

# Load training/validation data - stepwise
train.df<- readRDS("data/stepwise_train.rds")
valid.df <- readRDS("data/stepwise_valid.rds")

# Load training/validation data - neural network
train.nn <- readRDS("data/train.norm.nn.rds")
valid.nn <- readRDS("data/valid.norm.nn.rds")

# Load training/validation data - knn
train.norm.df<- readRDS("data/train.norm.df.rds")
valid.norm.df <- readRDS("data/valid.norm.df.rds")

# ROI calculation
Total.Profit <- sum(sales.df$Profit)
Total.Unit.Cost <- sum(sales.df$Calc.Unit.Cost)
Total.Tax <- sum(sales.df$Tax.Rate)
Total.Cost <- Total.Unit.Cost+Total.Tax
Total.ROI <- ((Total.Profit-Total.Cost)/Total.Cost)*100

# Load dataset for correlation plot
corr.sales <- readRDS("data/corr.sales.rds")

# Load dataset for sales model
model.sales <- readRDS("data/model.sales.rds")

# Select Data to reduce multicollinearity
model.sales.df.norm <- readRDS("data/model.sales.norm.rds")

# Original linear model
fit.lm <- lm(Profit~., model.sales.df.norm)

# use step() to run backward regression.
sales.lm.step.back <- step(fit.lm, direction = "backward")
summary(sales.lm.step.back) 
sales.lm.step.back.pred <- predict(sales.lm.step.back, valid.df)
accuracy(sales.lm.step.back.pred, valid.df$Profit)

# use step() to run forward regression.
sales.lm.null <- lm(Profit~1, data = train.df)
sales.lm.step.forward <- step(sales.lm.null, 
                              scope=list(lower=sales.lm.null, upper=fit.lm), 
                              direction = "forward")
summary(sales.lm.step.forward)
sales.lm.step.forward.pred <- predict(sales.lm.step.forward, valid.df)
accuracy(sales.lm.step.forward.pred, valid.df$Profit)

# use step() to run both
sales.lm.step <- step(fit.lm, direction = "both")
summary(sales.lm.step)
sales.lm.step.pred <- predict(sales.lm.step, valid.df)
accuracy(sales.lm.step.pred, valid.df$Profit)

# dataframe creation to compare results 
comparison <- data.frame(
  Backward=c(accuracy(sales.lm.step.back.pred, valid.df$Profit)),
  Forward= c(accuracy(sales.lm.step.forward.pred, valid.df$Profit)),
  Step=c(accuracy(sales.lm.step.pred, valid.df$Profit))
)

# Lift charts
actual <- valid.df$Profit
gainBack<- gains(actual, sales.lm.step.back.pred, groups=10)
gainFwd <- gains(actual, sales.lm.step.forward.pred, groups=10)
gainStep <- gains(actual, sales.lm.step.pred, groups=10)

# Train NN on training set and plot
nn.sales1 <- nnet(Profit~Sale.Key+
                    Month+
                    Stock.Item.Key+
                    Quantity+
                    Tax.Rate+
                    Calc.Unit.Cost, train.nn, size=5, linout=T)


# Train NN on test set and plot
nn.sales2 <- nnet(Profit~Sale.Key+
                    Month+
                    Stock.Item.Key+
                    Quantity+
                    Tax.Rate+
                    Calc.Unit.Cost, valid.nn, size=5, linout=T)

# rsme - error on training set
sales.pred1 <- predict(nn.sales1, train.nn)
rsme1 <- sqrt(mean(sales.pred1^2))

# rsme - error on training set
sales.pred2 <- predict(nn.sales2, valid.nn)
rsme2 <- sqrt(mean(sales.pred2^2))

# KNN using the best k.
knn.pred <- knn(train.norm.df, valid.norm.df,
                cl = train.norm.df[, "SupplierLitware"], k = 7)

# Define UI ----
    userguide_page <- tabPanel(img(src='WWI_Logo.png', align="center"),
                        title = "User Information",
                                           titlePanel(""),
        h3("Introduction"),
        "Wide World Importers (WWI) is based out of 
        CA, USA with customers throughout the USA. While the team at WWI use SQL 
        Server Reporting Services to generate operational reports from the 
        WideWorld Importers database, an analysis was needed to perform analytics on their 
        data by generating strategic reports. The team have created a 
        dimensional data model in a database WideWorldImportersDW. This database 
        is populated by an Integration Services package.",
        br(),
        br(),
        "This R Shiny application was created to review the results of is 
        used to create dashboards from a dataset of 413,076 records produced from 
        the reporting. The application can be used on websites, phones and 
        tablets.",
        br(),
        h3("Analytic Theme and Logic"),
        "The analytic theme of this analysis is Sales Performance. To explore 
        sales reporting that shows sales reporting that includes territory, 
        salesperson, customer, and supplier.  This can aid the business to 
        determine who frequent customers, frequent suppliers are and the products 
        that are typically purchased.  Additionally, the analysis will cover the 
        profits associated with the sales that are made.  The analysis can also 
        help identify most profitable sales territories to determine where to 
        make strategic efforts to market in those areas.",
        br(),
        "The business desires to have basic sales reporting that shows the orders 
        by current territory, by rank, so that the sales reps can place their 
        focus on the customers ranked by orders. This query extracts the data 
        from the sales, customer, and territory tables to produce a report that 
        shows the customer’s total orders, ranked by those orders, to help sales 
        reps identify the top customers in each region and group.",
        br(),
        h3("Dashboard"),
        "Prior to developing the dashboard, a report was developed using the data
        stored in the SQL data warehouse using the applicable data points per
        the analytic theme. Upon conclusion of the exploratory analysis, there 
        were multiple predictive models created using the dataset: Linear, 
        Stepwise Regression, K Means Clustering, Neural Network, and K Nearest 
        Neighbors. The models were then evaluated for accuracy.",
        br(),
        h3("Getting Started"),
        "The analysis is comprised of the User Information, WWI Sales 
        Dashboard, and About tabs.  To access the WWI sales performance analysis results, 
        select the WWI Sales Dashboard. As you review the data, keep in mind that 
        the outputs are mostly interactive and will populate with the sample criteria 
        selected. To change the criteria, follow these steps:",
        br(),
        h4("Step 1:"), 
        "After accessing the dashboard, retrieve the sample population 
        that you would like to view by selecting the Region and Supplier of your 
        choice.the sample data will automatically populate several of the data visualizations
        on the dashboard.",
        h4("Step 2:"), 
        "Current sales metrics will appear on the 'Data Exploration' sub-tab. To 
        review the findings and proposals from the analysis, review the 'Findings' tab.",
        br(),
        "*Please note that some components of the dashboard are not interactive.",
        br(),
        h4("Step 3:"), 
        "Conduct your review of the output. Once you’ve set your criteria, the 
        dashboard will not automatically reformat itself. If another sales
        territory or supplier needs to be reviewed, simply select new Sample Criteria.",
        br(),
        h4("Step 4:"), 
        "The dashboard is comprised of seven sub-sections: Data Exploration, Linear
        Modeling, Stepwise Regression, K Means Clustering, Neural Network, K Nearest 
        Neighbors and Findings. These display the initial exploration and the predictive modeling 
        analysis. The features help to provide context to the analysis and support 
        the Findings.",
        br(),
        h4("Step 5:"), 
        "For a conclusion of the analysis, review he Findings sub-tab. The findings 
        briefly describes the results data exploration and the predictive models for 
        your information, and provide several proposals for process efficiencies.",
        br(),
        h3("Data Dictionary"),
        "Please click the image below to view the data dictionary.",
        br(),
        br(),
        tags$a(img(src="dict.png", height="10%", width="10%", align = "center"), 
               href="//www.sqldatadictionary.com/WideWorldImporters/")
    )
    
    about_page <- tabPanel(
        title = "About",
        titlePanel("About this Project"),
        br(),
        "Welcome, and thank you for taking the time to view my project! My name 
        is Tiana Bryant, and I prepared this dashboard as a final project 
        for the BDAT 650: Business Data Analytics Capstone at Maryville University 
        Online. Using R Shiny, I have created an interactive dashboard that will 
        share the results of my sales performance related data.",
        br()
    )
    
    main_page <- tabPanel(
        # Application title
        title = "WWI Sales Dashboard",
        titlePanel("Sample Criteria"),
        # Sidebar with a 2 inputs 
        sidebarLayout(
            sidebarPanel(
                # Inputs
                selectInput(inputId = "Sales.Territory",
                            label = "Select Region:",
                            choices = c("External" = "External", 
                            "Far West" = "Far West", 
                            "Great Lakes" = "Great Lakes",
                            "Mideast" = "Mideast",
                            "New England" = "New England",
                            "Plains" = "Plains",
                            "Rocky Mountain" = "Rocky Mountain",
                            "Southeast" = "Southeast",
                            "Southwest" = "Southwest")
                ),
                radioButtons(inputId = "Supplier",
                             label = "Supplier:",
                             choices = c(
                               "A Datum Corporation" = "A Datum Corporation",
                               "Contoso" = "Contoso",
                               "Fabrikam" = "Fabrikam",
                               "Graphic Design Institute" = "Graphic Design Institute",
                               "Litware" = "Litware",
                               "Northwind Electric Cars" = "Northwind Electric Cars",
                               "The Phone Company" = "The Phone Company",
                               "Unknown" = "Unknown"
                               )
                             )
            ),
            mainPanel(
                tabsetPanel(type = "tab",
                            #---------------------DATA EXPLORATION---------------------  
                            tabPanel("Data Exploration",
                                     h4("Summary Statistics"),
                                     p("An exploratory analysis of the dataset that provides
                                        preliminary summary statistics."),
                                        verbatimTextOutput("summary"),
                                     br(),
                                     
                                     
                                     h4("Business Questions"),
                                     p("Are sales increasing or decreasing by year?"),
                                     plotOutput("Plot1"),
                                     br(),
                                     p("What are the top 5 sales territories?"),
                                     plotOutput("Plot2"),
                                     br(),
                                     p("Who are the top 5 salespeople for in the region and
                                       given supplier?"),
                                     plotOutput("Plot3"),
                                     br(),
                                     p("Who are the top suppliers?"),
                                     plotOutput("Plot4"),
                                     br(),
                                     p("Frequency Distribution - Profit Histogram"),
                                     plotOutput("histogramPlot"),
                                     br(),
                                     p("What is the ROI on products purchased from 
                                       all suppliers by WWI?"),
                                     verbatimTextOutput("ROI"),
                                     br(),
                                     p("What are are the top products sold by unit price? - Filter 
                                       by unit price to find out, or filter/search based on your 
                                       preferences."),
                                     DT::dataTableOutput("unitprice"),
                                     br(),
                                     p("What kind of products are commonly sold in this region
                                       for the given supplier?"),
                                     DT::dataTableOutput("topproducts"),
                                     br(),
                                     p("What sales territories are the 3 least profitable, 
                                       and what is the costs associated with doing business 
                                       in these territories? - Filter by total stock profit/total 
                                       cost to find out, or filter/search based on your 
                                       preferences."),
                                     DT::dataTableOutput("leastprofits"),
                                     br(),
                                     p("Which of WWI’s suppliers are associated with the most 
                                       profits? What are the costs of doing business relative
                                       tor profit? - Filter by total stock profit/total
                                       cost/cost ratio (output as %) to find out, or filter/search based 
                                       on your preferences."),
                                     DT::dataTableOutput("mostprofits"),
                                     br(),
                                    p("Below is a summary table of the products sold
                                      related to your criteria selection."),
                                    DT::dataTableOutput("Products.sold"),
                                    br()
                                    
                            ),  
                            #---------------------LINEAR REGRESSION---------------------
                            tabPanel("Linear Modeling",
                                     h4("Purpose"),
                                     p("To understand the common occurances
                                       contained within the dataset, a linear regression
                                       model was created. The results
                                       a few variables with great statistical significance, including
                                       Month, Calculated Unit Cost and Stock Item."),
                                     br(),
                                     p("I first began with a correlation plot to show relationships 
                                        that exist within the dataset. Because the goal is to understand 
                                       how profit is impacted, profit will serve as the response
                                       predictor for a majority of the analysis."),
                                     plotOutput("corrPlot"),
                                     br(),
                                     verbatimTextOutput("lm.fit"),
                                     br(),
                                     p("The results
                                       a few variables with great statistical significance, including
                                       Month, Calculated Unit Cost and Stock Item. The visualization
                                       below focuses on Calculated Unit Cost."),
                                     br(),
                                     plotOutput("regplot"),
                                     h4("Results"),
                                     p("After the exploratory analysis, I noticed that
                                       there was a strong positive correlation between
                                       calculated unit cost and profit, so I developed
                                       a visualization to represent this. I could also
                                       spot several outliers in the dataset."),
                                     br(),
                            ),
    
                            #---------------------STEPWISE REGRESSION---------------------
                            tabPanel("Stepwise Regression",
                                     h4("Purpose"),
                                     p("To get a better idea of what variables
                                       might be the most important in the dataset,
                                       stepwise regression was used. This was done 
                                       to identify which independent variables might
                                       most influence the dataset."),
                                     h4("Step backward"),
                                     plotOutput("Plot5"),
                                     br(),
                                     h4("Step forward"),
                                     plotOutput("Plot6"),
                                     br(),
                                     h4("Both"),
                                     plotOutput("Plot7"),
                                     br(),
                                     h4("Results"),
                                     p("The approach of using both would be suitable,
                                       as the best results were obtained from this method.
                                       Statistically significant predictor variables 
                                       that warranted further examination include year, 
                                       stock item key, quantity, unit price, tax rate 
                                       and calculated unit cost."),
                                     verbatimTextOutput("comparison"),
                                     br(),
                                     verbatimTextOutput("lm.both"),
                                     p("* ME, RSME, MAE, MPE and MAPE"),
                                     br(),
                            ),
                            #---------------------K MEANS---------------------
                            tabPanel("K Means Clustering",
                                     h4("Purpose"),
                                     p("Though this data was highly structured, I wanted
                                       to explore further to see if there were any other
                                       associations within the dataset that I had not yet 
                                       caught.  Using K Means helps to find those associations."),
                                     br(),
                                     h3("Elbow Method"),
                                     img(src='elbow.png', height="70%", width="70%", 
                                         align = "center"),
                                     h3("K Means Cluster Visualization"),
                                     img(src='k means cluster.png', height="70%", 
                                         width="70%", align = "center"),
                                     h3("K Means Summary"),
                                     img(src='kmeans summary.png', height="70%", 
                                         width="70%", align = "center"),
                                     br(),
                                    p("*Please note that images are used here to
                                      reduce processing time of the application."),
                                    h4("Results"),
                                    p("The k originally selected  was 2. However, 6 was
                                       found to be the best using the elbow method. In looking 
                                       at the results, it looks like cluster 4 appears 
                                       to be the outliers/catch all cluster or represent 
                                       the sales that were low quantity, etc. Higher 
                                       profits and costs are associated with cluster 3, 
                                       higher quantities in cluster 5."),
                                    br(),
                                    br(),
                            ),
                            #---------------------NEURAL NETWORK---------------------
                             tabPanel("Neural Network",
                                    h4("Purpose"),
                                    p("In using a neural network, patterns can be
                                      detected and help identify issues and improve
                                      decision making within the business. Using some 
                                      of the predictors found from the stepwise regression
                                      exercise."),
                                    h3("Model Results - Training Set"),
                                    verbatimTextOutput("nn.sales.sum1"),
                                    plotOutput("nn.plot1"),
                                    verbatimTextOutput("rsme1"),
                                    br(),
                                    h3("Model Results - Validation Set"),
                                    verbatimTextOutput("nn.sales.sum2"),
                                    plotOutput("nn.plot2"),
                                    verbatimTextOutput("rsme2"),
                                    h4("Results"),
                                    p("Examining the neural network that was developed,
                                      there are clear strong relationships between the
                                      predictors identified in the stepwise regression
                                      and profit. The model performed well between the 
                                      training and validation sets based on RSME."),
                                    br(),
                                    
            
                            ),
                            #---------------------KNN---------------------
                            tabPanel("K Nearest Neighbors",
                                     h4("Purpose"),
                                     p("This model further examines the supplier
                                      Litware. In the discovery phases of the analysis, 
                                      certain suppliers, including Litware, were found 
                                      to have a high impact of the probability
                                      of a sale. This model seeks to understand the
                                      likelihood of the supplier Litware translating
                                      to a sale based on the historical data."),
                                     br(),
                                     verbatimTextOutput("confusionMatrix"),
                                    
                                     h4("Results"),
                                     p("After making the determination of the best
                                       k = 7, it was found with 99.99% accuracy that
                                       if a product were Litware, it was likely to 
                                       result in a sale."),
                                     br(),
                            ),
                            tabPanel("Findings",
                                    h4("Putting It All Together"),
                                    br(),
                                    p("Using various methods of exploring the dataset
                                     through the predictive models developed, it was 
                                      discovered that there were a few key predictors of
                                      profit. It can be determined unit costs have a significant
                                      relationship to profits. Consistently throughout the analysis,
                                      the as unit costs went up, so did profits. This influenced
                                      the analysis and the focus narrowed to how costs impact sales
                                      and, ultimately, profits."),
                                    br(),
                                    p("While we have limited data from 2016, it appears
                                      that sales are consistently growing year-over-year. The 
                                      most profitable regions over the four year lookback 
                                      are Southeast ($34MM), Mideast ($23MM) and Southwest 
                                      ($22MM). Top performers for all regions include Amy Trefl, 
                                      Anthony Grosse, and Archer Lamble."),
                                    br(),
                                    p("Briefly, it may appear that the External region
                                      performs superiorly with a mean profit of $383. When
                                      diving deeper, the costs associated with doing business
                                      are the highest of all the regions with a mean at $24,
                                      exceeding the other regions by nearly a dollar or more. Other
                                      low profit regions include New England and Rocky Mountain."),
                                    br(),
                                    p("In terms of suppliers, it appears that Litware, Fabrikam and
                                      Northwind Electric Cars are the largest suppliers contained 
                                      regarding sales within the dataset. The top products sold
                                      include Stock Item Keys 5 (Air Cushion Machine), 145 (Ride On 
                                      Big Wheel Monster Truck) and 146 (Ride On Vintage American Coupe) 
                                      respectively.  All of these products fall within the Novelty Shop
                                      product type."),
                                    br(),
                                    h4("Recommendations"),
                                    br(),
                                    h5("Proposal 1"),
                                    p("Reconsider doing business in the External, New 
                                      England and Rocky Mountain regions, given the cost
                                      relative to profit exceeds top performers."),
                                    br(),
                                    h5("Proposal 2"),
                                    p("Consider shifting sales strategy to utilize suppliers
                                      whose products cost less compared to profit. For example,
                                      while Northwind Electric Cars had 2 out of the top 3 in
                                      terms of products sold, the cost relative to profit is 26.1%. 
                                      This means that for every $1 profit, there is in excess of 
                                      26 cents to earn that profit. Litware, which held the number 1
                                      sales spot, is 5 cents for every dollar. The KNN model also 
                                      supports this proposal, as this supplier's involvement in
                                      a given transaction is a likely predictor of a sale."),
                                    h5("Proposal 3"),
                                    p("Consider eliminating doing business with A Datum Corporation
                                      and Contoso.  While costs are relatively low, there is not much
                                      profit gained in selling products from these two suppliers. Consider
                                      replacing or shifting efforts to suppliers whose products produce
                                      a higher yield."),
                                    br(),
                                    h5("Conclusion"),
                                    p("Currently, ROI stands at 884%, which is quite respectable. 
                                      If cost cutting measures associated with the aforementioned
                                      proposals take place, the business might find that the efficacy of the
                                      ROI improves and ROI increases."),
                                    br(),
                            ),
                    
                )
            )
        )
    )
    
ui <- navbarPage(theme = shinytheme("journal"),
                    title = "Wide World Importers Sales Performance", 
                    userguide_page,
                    main_page,
                    about_page)
  
# Define server logic ----
server <- function(input, output) {
    selections <- reactive({
        req(input$Sales.Territory)
        req(input$Supplier)
        filter(sales.df, Sales.Territory==input$Sales.Territory)%>%
            filter(Supplier %in% input$Supplier)
    })
    #---------------------DATA EXPLORATION---------------------   
    #Summary stats
    output$summary <- renderPrint({
        dataset <- selections()
        summary(dataset)
    })
    # What is the ROI on products purchased from all suppliers by WWI?
    print(paste("The ROI for WWI is: ", round(Total.ROI), "%"))
    
    ## Are sales increasing or decreasing by year for this supplier?
    output$Plot1 <- renderPlot({
        ggplot(data=sales.df, mapping=aes(x=Year))+
            geom_bar()+
            labs(
                x = "Year",
                y = "No. of Sales"
            )
    })
    ## What are the top 5 sales territories?
    output$Plot2 <- renderPlot({
        ggplot(data=sales.df, mapping=aes(x=Sales.Territory, fill=Sales.Territory))+
        geom_bar()+
        labs(
            x = "Sales Territory",
            y = "No. of Sales"
        ) +
        coord_flip()
    })
    
    ## Who are the top 5 salespeople for WWI?
    output$Plot3 <- renderPlot({
        ggplot(data=selections(), mapping=aes(x=Employee, fill=Employee))+
            geom_bar()+
            geom_label(stat="count", mapping=aes(label=..count.., fill=Employee))+
            labs(
                x = "Employee",
                y = "No. of Sales"
            ) +
            coord_flip()
    })

    ## Who are the top suppliers?
    output$Plot4<- renderPlot({
        ggplot(data=sales.df, mapping=aes(x=Supplier, fill=Supplier))+
            geom_bar()+
            geom_label(stat="count", mapping=aes(label=..count.., fill=Supplier))+
            labs(
                x = "Supplier",
                y = "No. of Products Sold"
            ) +
            coord_flip()
    })
    
    ## Products table
    output$Products.sold <- DT::renderDataTable({
        (datatable(selections()[,c(5,6,7,8,9,12,17,19)],
                   rownames = TRUE,
                   class = 'cell-border stripe',
                   caption = 'Filter the table results to your preferences.',
                   filter = 'none'))
    })
    ## What are are the top products sold by unit price? 
    output$unitprice<- DT::renderDataTable({
    unit.price.top <- sales.df %>%
        group_by(Stock.Item.Key) %>%
        summarize(
          Total.Unit.Price = sum(sort(round(Unit.Price))))%>%
        arrange(desc(Total.Unit.Price))
    print(unit.price.top)
    })
    
    ## What kind of products are commonly sold?
    output$topproducts<- DT::renderDataTable({
    top.prod <-selections() %>%
      group_by(Category) %>%
      summarize(
        Total.Unit.Price = sum(sort(round(Unit.Price))))%>%
      arrange(desc(Total.Unit.Price))
    print(top.prod)
    })
    
    ## What sales territories are the 3 least profitable, and what is the costs 
    ## associated with doing business in these territories?
    output$leastprofits<- DT::renderDataTable({
    least.profits <- sales.df %>%
      group_by(Sales.Territory) %>%
      summarize(
        Total.Profit = sum(sort(round(Profit))),
        Total.Cost = sum(round(Calc.Unit.Cost)))%>%
      arrange(Total.Cost)
      print(least.profits)
    })
    
    ## Which of WWI’s suppliers are associated with the most profits? 
    output$mostprofits<- DT::renderDataTable({
    most.profits <- sales.df %>%
      group_by(Supplier) %>%
      summarize(
        Total.Profit = sum(sort(round(Profit))),
        Total.Cost = sum((round(Calc.Unit.Cost))),
        Cost.Ratio = round((Total.Cost/Total.Profit)*100))%>%
      arrange(desc(Total.Profit))
    print(most.profits)
    })
    
     # Histogram Plot
    output$histogramPlot <- renderPlot({
        ggplot(sales.df, aes(x=Profit, fill = Profit)) + 
          geom_histogram(binwidth=5)
    })
    # ROI results
    output$ROI <- renderPrint({
      
      
        print(paste("The overall ROI for WWI is: ", round(Total.ROI), "%"))
    })
    
    # Correlation plot
    output$corrPlot <- renderPlot({
        corrplot(cor(corr.sales), 
                 method = "color", 
                 type = "full",
                 diag = TRUE,
                 tl.col = "black",
                 tl.srt = 35,
                 col = NULL
        )
    })
    
    #---------------------LINEAR REGRESSION---------------------
    #Summary stats
    output$lm.fit <- renderPrint({
        lm.fit <-summary(fit.lm)
        print(lm.fit)
    })
    
    # regression plots
    output$regplot <- renderPlot({
        ggplot(model.sales.df.norm, aes(x = Profit, y = Calc.Unit.Cost)) + 
        geom_point() +
        stat_smooth(method = "lm", col = "red")
    })

    #---------------------STEPWISE REGRESSION---------------------
    #Summary stats
    output$comparison<- renderPrint({
      print(comparison)
    })
    
    output$stepboth<- renderPrint({
      lm.both <-summary(sales.lm.step)
      print(lm.both)
    })
    # lift chart - backward
    output$Plot5 <- renderPlot({
      plot(c(0, gainBack$cume.pct.of.total*sum(actual)) ~ c(0, gainBack$cume.obs), 
           xlab = "# cases", ylab = "Cumulative Profit", type="l", 
           main="Lift Chart - Backward")
      lines(c(0,sum(actual))~c(0,dim(valid.df)[1]), col="red", lty=2)
    })
    # lift chart - forward
    output$Plot6 <- renderPlot({
      plot(c(0, gainFwd$cume.pct.of.total*sum(actual)) ~ c(0, gainFwd$cume.obs), 
           xlab = "# cases", ylab = "Cumulative Profit", type="l", 
           main="Lift Chart - Forward")
      lines(c(0,sum(actual))~c(0,dim(valid.df)[1]), col="red", lty=2)
    })
    # lift chart - both
    output$Plot7 <- renderPlot({
      plot(c(0, gainStep$cume.pct.of.total*sum(actual)) ~ c(0, gainStep$cume.obs), 
           xlab = "# cases", ylab = "Cumulative Profit", type="l", 
           main="Lift Chart - Both")
      lines(c(0,sum(actual))~c(0,dim(valid.df)[1]), col="red", lty=2)
    })
    
    #---------------------K MEANS---------------------
    #none - images used due to processing speed
    
    #---------------------NEURAL NETWORK---------------------
    #Summary stats - train set
    output$nn.sales.sum1<- renderPrint({
      nn.sales.sum1 <-summary(nn.sales1)
      print(nn.sales.sum1)
    })
    #Summary stats - validation set
    output$nn.sales.sum2<- renderPrint({
      nn.sales.sum2 <-summary(nn.sales2)
      print(nn.sales.sum2)
    })
    
    # nn plot
    output$nn.plot1 <- renderPlot({
      plotnet(nn.sales1, bias = FALSE)
    })
    
    # nn plot
    output$nn.plot2 <- renderPlot({
      plotnet(nn.sales2, bias = FALSE)
    })
    
    #RSME1
    output$rsme1<- renderPrint({
      print(paste("The RSME is: ", (rsme1)))
    })
    
    #RSME 2
    output$rsme2<- renderPrint({
      print(paste("The RSME is: ", (rsme2)))
    })
    
    #---------------------KNN---------------------
    output$confusionMatrix <- renderPrint({
      confusionMatrix(knn.pred, factor(valid.norm.df[, "SupplierLitware"])
      )
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)
