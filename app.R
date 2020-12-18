shinyApp(
    ui = tagList(
        shinythemes::themeSelector(),
        navbarPage(
           theme = shinythemes::shinytheme("cerulean"), 
            "Applied statistics and Machine Learning CA1",
            tabPanel("Discriptive Analysis",
                     # Sidebar panel for inputs ----
                     sidebarPanel(
                         
                         # Input: Select a dataset ----
                         selectInput("dataset", "Choose a dataset:",
                                     choices = c( "Attitude","Cars","Islands", "Rock","Seatbelts")),
                         
                         #Choose file
                         #fileInput("file","Or Upload a File input"),
                         
                         
                         # Input: Select a file ----
                         fileInput("file1", "or Choose CSV File of your own",
                                   multiple = FALSE,
                                   accept = c("text/csv",
                                              "text/comma-separated-values,text/plain",
                                              ".csv")),
                         
                         
                         
                         # Input: Specify the number of observations to view ----
                         numericInput("obs", "Number of observations to view:", 10),
                         
                         # Include clarifying text ----
                         helpText("Note: while the data view will show only the specified",
                                  "number of observations, the summary will still be based",
                                  "on the full dataset."),
                         
                         # Input: actionButton() to defer the rendering of output ----
                         # until the user explicitly clicks the button (rather than
                         # doing it immediately when inputs change). This is useful if
                         # the computations required to render output are inordinately
                         # time-consuming.
                         actionButton("update", "Update View")
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Summary of data set from drop down",
                                      # Output: Header + summary of distribution ----
                                      h4("Summary"),
                                      verbatimTextOutput("summary"),
                                      
                                      # Output: Header + table of distribution ----
                                      h4("Observations"),
                                      tableOutput("view"),
                                      
                                      
                                      
                             ),
                             tabPanel("Summary of file uploaded", 
                                      
                                      h4("Summary of dataset uploaded"),
                                      
                                      fluidRow(verbatimTextOutput("value")),
                                      
                                      h4("Observations"),
                                      # Output: Data file ----
                                      tableOutput("contents")
                                      
                                      )
                             #tabPanel("Tab 3", "This panel is intentionally left blank")
                         )
                     )
            ),
            tabPanel("Discrete Models",
                     
                     sidebarPanel( 
                         
                         selectInput("dismodel", "Select Model", 
                                     
                                     choices = c("Binomial" = "binomial", 
                                                 
                                                 "Poisson" = "poisson", 
                                                 
                                                 "Geometric" = "geometric"), 
                                     
                                     selected = "binomial" 
                                     
                         ), 
                         
                         conditionalPanel( 
                             
                             condition = "input.dismodel == 'binomial'", 
                             
                             numericInput("n", "No. of trails" , value = 10), 
                             
                             numericInput("p", "Proability of success" , min = 0, max = 1,value = 0.5,step = 0.1) 
                             
                         ), 
                         
                         
                         
                         conditionalPanel(     
                             
                             condition = "input.dismodel == 'poisson'", 
                             
                             
                             numericInput("lam", "parameter lambda in Poisson" , value = 1) 
                             
                         ), 
                         
                         
                         
                         conditionalPanel(     
                             
                             condition = "input.dismodel == 'geometric'", 
                             
                             numericInput("p", "parameter p in Geometric" ,min = 0, max = 1,value = 0.5,step = 0.1) 
                             
                         ), 
                         
                         
                         
                         numericInput("max", "upper limit for x" , value = 5),  
                         
                         sliderInput("s", "number of simulated data" ,min=1, max=10000, value = 10),  
                         
                         
                         
                         conditionalPanel( 
                             
                             condition = "input.dismodel == 'binomial'", 
                             
                             numericInput("j1", "j for Bin" , value = 1) 
                             
                         ), 
                         
                         
                         
                         conditionalPanel( 
                             
                             condition = "input.dismodel == 'poisson'", 
                             
                             numericInput("j2", "j for Poisson" , value = 1) 
                             
                         ), 
                         
                         
                         
                         conditionalPanel( 
                             
                             condition = "input.dismodel == 'geometric'", 
                             
                             numericInput("j3", "j for geometric" , value = 1) 
                             
                         ) 
                         
                     ),
                     
                     
                     mainPanel(  
                         
                         plotOutput("histogram"),  
                         
                         tableOutput('tab')  
                         
                     )  ),
                     
            tabPanel("Continous Models",
                     
                     
                     sidebarPanel( 
                         selectInput("conmodel", "Select Model", 
                                     choices = c("Normal" = "normal", 
                                                 "Exponential" = "exponential", 
                                                 "Uniform" = "uniform"), 
                                     selected = "normal" 
                         ), 
                         
                         sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 10), 
                         
                         conditionalPanel(     
                             condition = "input.conmodel == 'exponential'", 
                             numericInput("lamda", "parameter lambda in exponential" ,min=0, max=10, value = 1)  
                         ), 
                         
                         conditionalPanel( 
                             condition = "input.conmodel == 'normal'", 
                             numericInput("mu", "parameter mu in Normal" , value = 0),  
                             numericInput("sigma", "parameter sigma in Normal" , value = 1,min=0) 
                         ), 
                         
                         numericInput("i", "support" , value = 2), 
                         
                         
                         conditionalPanel(     
                             condition = "input.conmodel == 'normal'", 
                             numericInput("j1", "j in Normal" , value = 0) 
                             
                         ), 
                         
                         conditionalPanel(     
                             condition = "input.conmodel == 'exponential'", 
                             numericInput("j2", "j in exponential" , value = 0) 
                         ), 
                         
                         conditionalPanel( 
                             condition = "input.conmodel == 'uniform'", 
                             numericInput("a", "parameter a in Normal" , value = -2),  
                             numericInput("b", "parameter b in Normal" , value = 0.8) 
                         ) 
                         
                         
                         
                     ),  
                     mainPanel(  
                         plotOutput("nav3histogram1"),  
                         tableOutput('nav3tab'), 
                         tableOutput('nav3prob')  
                     )  
              

                     ),
            
            tabPanel("Classification Models",
                     
                     sidebarPanel(
                       selectInput("ds", "Select Model", 
                                              choices = c("Iris" = "iris", 
                                                          "SuperMarket" = "sm"), 
                                              selected = "iris" 
                       ),
                       
                       
                       
                       sliderInput("mc", "No of Multicollinearity" ,min=10, max=100, value = 10)
                       
                     ),
                     
                     
                     mainPanel(  
                       h4('Comparision of NB MLR and SVM for 40,60 and 80 train set'),
                       plotOutput("histogram2"),  
                       h5('Data Visiualisation'),
                       #par(mfrow=c(1,3)),
                       plotOutput("dvplot"),
                       
                       plotOutput("dvplot1"),
                      
                        
                     ) 
                     
                     
                     
                     )
            #tabPanel("Regression Models")
        )
    ),
    server = function(input, output) {
      
      ############Navigation Tab1########################
      library(class) 
      library(e1071) 
      library(MASS) 
      library(gdata) 
      library(dplyr)    #select columns from data frame
      library(ggplot2)  #ggplot
        # Return the requested dataset ----
        # Note that we use eventReactive() here, which depends on
        # input$update (the action button), so that the output is only
        # updated when the user clicks the button
        datasetInput <- eventReactive(input$update, {
            switch(input$dataset,
                   "Rock" = rock,
                   "Attitude" = attitude,
                   "Islands" = islands,
                   "Seatbelts" = Seatbelts,
                   "Cars" = cars)
        }, ignoreNULL = FALSE)
        
        # Generate a summary of the dataset ----
        output$summary <- renderPrint({
            dataset <- datasetInput()
            summary(dataset)
        })
        
        # Show the first "n" observations ----
        # The use of isolate() is necessary because we don't want the table
        # to update whenever input$obs changes (only when the user clicks
        # the action button)
        output$view <- renderTable({
            head(datasetInput(), n = isolate(input$obs))
        })
        
        output$value <- renderPrint({
            #if(is.null(input$file1))     return(NULL) 
            validate(
                need(input$file1 != "", "No data has been uploaded")
            )
            ufile <- read.csv(input$file1$datapath,
                              header = TRUE,
            )
            #fileuploaded<- input$file
            summary(ufile)
        })
        
        output$contents <- renderTable({
            
            # input$file1 will be NULL initially. After the user selects
            # and uploads a file, head of that data file by default,
            # or all rows if selected, will be shown.
            
            req(input$file1)
            
            # when reading semicolon separated files,
            # having a comma separator causes `read.csv` to error
            tryCatch(
                {
                    df <- read.csv(input$file1$datapath,
                                   header = TRUE,
                    )
                },
                error = function(e) {
                    # return a safeError if a parsing error occurs
                    stop(safeError(e))
                }
            )
            
            
            #return(head(df))
          return(head(df, n = input$obs))
            
            
            
        })
        
        ############Navigation Tab2 Discrete models########################
        
        output$histogram <- renderPlot({ 
            
            
            
            # binomial  
            
            if (input$dismodel == 'binomial') { 
                
                par(mfrow=c(2,1))  
                
                d <- density(rbinom(input$s,input$n,input$p))  
                
                plot(d, main="Kernel Density of generated data")  
                
                polygon(d, col="blue", border="red") 
                
                x=0:input$max
                
                plot(x,dbinom(x,input$n,input$p),type = 'o')  
                
                
                
                
            } 
            
            
            
            
            
            # poisson 
            
            
            
            if (input$dismodel == 'poisson') { 
                
                par(mfrow=c(1,2))   
                
                D=rpois(input$s, input$lam)  
                
                tab=table(D)  
                
                barplot(tab,col='blue')  
                
                x1=0:input$max  
                
                y1=dpois(x1,input$lam)  
                
                plot(x1,y1,type='b')  
                
            } 
            
            ##  
            
            
            
            
            
            
            
            # geometric  
            
            if (input$dismodel == 'geometric') { 
                
                par(mfrow=c(1,2)) 
                
                D=rgeom(input$s, input$p)  
                
                tab=table(D)  
                
                barplot(tab,col='blue')  
                
                x2=0:input$max  
                
                y2=dgeom(x2,input$p)  
                
                plot(x2,y2,type='b')  
                
            } 
            
            
            
        })    
        
        
        
        output$tab <- renderTable({  
            
            
            
            p1=dbinom(input$j1,input$n, input$p)  
            
            p2=dpois(input$j2,input$lam)  
            
            p3=dgeom(input$j3,input$p)  
            
            
            
            
            
            c(p1,p2,p3) 
            
            
        })
        
        ############Navigation Tab3 Continous model########################
        
        output$nav3histogram1 <- renderPlot({ 
            
            # normal  
            if (input$conmodel == 'normal') { 
                par(mfrow=c(1,2))  
                x=seq(-input$i,input$i,0.01)  
                plot(x,dnorm(x,input$mu,input$sigma),type='l', col='red')  
                
            } 
            
            
            # exponential 
            
            if (input$conmodel == 'exponential') { 
                # exponential  
                par(mfrow=c(1,2)) 
                x=seq(0,input$i,0.01)  
                plot(x,dexp(x,input$lamda),type='l',col='green') 
                
                
            } 
            #uniform
            
            if (input$conmodel == 'uniform') { 
                a <- input$a 
                b <- input$b 
                n1 <- input$s 
                
                rand.unif <- runif(n1, min = a, max = b) 
                
                hist(rand.unif,  
                     freq = FALSE,  
                     xlab = 'x',   
                     ylim = c(0, 0.4), 
                     xlim = c(-3,3), 
                     density = 20, 
                     main = "Uniform distribution") 
                
                
                curve(dunif(x, min = a, max = b),  
                      from = -3, to = 3,  
                      n = n1,  
                      col = "darkblue",  
                      lwd = 2,  
                      add = TRUE,  
                      yaxt = "n", 
                      ylab = 'probability') 
                
                
            } 
            
        })    
        
        output$nav3tab <- renderTable({  
            Normal=rnorm(input$s,input$mu, input$sigma)  
            Exp=rexp(input$s,input$lamda)  
            
            if (input$conmodel == 'exponential') { 
                d2=data.frame(Exp)  
            } 
            else 
            { 
                d1=data.frame(Normal)  
            } 
            
            
            
        })  
        
        output$nav3prob <- renderPrint({  
            p1=pnorm(input$j1,input$mu, input$sigma)  
            p2=pexp(input$j2,input$lamds)  
            
            if (input$conmodel == 'exponential') { 
                c(p2)  
            } 
            
            if (input$conmodel == 'normal') { 
                c(p1)  
            } 
            
            
        })
        
        
        ##################Classification model########################
        
        output$histogram2 <- renderPlot({ 
          

          pred = 0
          pred_mlr=0
          pred_svm= 0
          actual =0
          t1=0
          t2 =0
          t3=0
          if(input$ds == 'iris'){
          Tit=iris}
          
          if(input$ds == 'sm'){
            
            df=read.csv('C:/Users/Nidhi/Downloads/supermarket-sales/supermarket_sales_sample.csv')
            #Tit = airquality
            df <- na.omit(df)
            
            # Drop the columns of the dataframe
            #Tit <- select (df,-c(gross.margin.percentage,Date,Invoice.ID))
            
            Tit <- select (df,c(Branch,City,Customer.type,Gender,Product.line,Unit.price,Quantity,Total,Payment,Rating))
            
            }
          
          M=matrix(NA,3,3) 
          
          ratio=c(40,60,80) 
          
          for (j in 1:3){ 
            
            
            
            acc=c(0,0) 
            
            mc=input$mc 
            
            for(i in 1:mc){ 
              
              n=nrow(Tit) 
              
              indexes = sample(n,n*(ratio[j]/100)) 
              
              trainset = Tit[indexes,] 
              
              testset = Tit[-indexes,] 
              
              #NB 
              
              if(input$ds == 'iris'){
              
              m <- naiveBayes(Species ~ ., data = trainset) 
              
              actual=testset$Species }
              
              if(input$ds == 'sm'){
                
                m <- naiveBayes(Gender ~ ., data = trainset) 
                
                actual=testset$Gender }
              
              
              
              pred= predict(m, testset) 
              
              cm1=table(pred,actual)  # confusion matrix  
              
              accuracy=mean(pred==actual) 
              
              # Mulitnomial Logistic Regression 
              
              library(nnet) 
              
              if(input$ds == 'iris'){
              
              mlr<- multinom(Species ~ ., data = trainset) 
              actual=testset$Species
              
              }
              
              if(input$ds == 'sm'){
                
                mlr<- multinom(Gender ~ ., data = trainset) 
                actual=testset$Gender
                
              }
              
              pred_mlr= predict(mlr, testset) 
              
              cm2=table(pred_mlr,actual)  # confusion matrix  
              
              accuracy_mlr=mean(pred_mlr==actual) 
              # SVM Logistic Regression 
              
              if(input$ds == 'iris'){
              svm_model<- svm(Species ~ ., data = trainset,kernel='linear') 
              actual=testset$Species
              }
              
              if(input$ds == 'sm'){
                svm_model<- svm(Gender ~ ., data = trainset,kernel='linear') 
                actual=testset$Gender
              }
              
              pred_svm= predict(svm_model, testset) 
              
              cm3=table(pred_svm,actual)  # confusion matrix  
              
              accuracy_svm=mean(pred_svm==actual) 
              
              
              vector_accuracy = c(accuracy,accuracy_mlr,accuracy_svm) 
              acc=acc+(1/mc)*vector_accuracy
              
            } 
            
            M[j,]= acc 
            
          } 
          
          M 
          
          x=ratio 
          
          if(input$ds == 'iris'){
          plot(x, M[,1],ylim=c(0.93,.98),col='blue', type='b',xlab='%Train Set',ylab='Accuracy') 
          }
          
          if(input$ds == 'sm'){
            plot(x, M[,1],ylim=c(0.4,.6),col='blue', type='b',xlab='%Train Set',ylab='Accuracy') 
          }
          
          lines(x, M[,2],col='red', type='b') 
          
          lines(x, M[,3],col='green', type='b')
          
          legend("topleft", legend=c("NB", "MLR","SVM"),
                 col=c("blue","red","green"), lty=1:2, cex=0.8,
                 title="Line types", bg='lightblue')
          
          
        })
        
        
        output$dvplot <-renderPlot({
          library(reshape2)
          
          if(input$ds == 'iris'){
          ggplot(iris,aes(x= Sepal.Length, y= Sepal.Width, col= Species,size=Petal.Width))+geom_point()
            
          }
          else{
            df=read.csv('C:/Users/Nidhi/Downloads/supermarket-sales/supermarket_sales_sample.csv') 
            #Tit = airquality
            df <- na.omit(df)
            print(df)
            # Drop the columns of the dataframe
            #Tit <- select (df,-c(gross.margin.percentage,Date,Invoice.ID))
            
            Tit <- select (df,c(Branch,City,Customer.type,Gender,Product.line,Unit.price,Quantity,Total,Payment,Rating))
            #No of sales per City
            ggplot (Tit)+
              geom_bar(mapping=aes(x=City, fill=Product.line), position="dodge")
            
            
          }
          
          
          
        })
        
        output$dvplot1 <-renderPlot({
          library(reshape2)
          if(input$ds == 'iris'){
            
            iris2 <- melt(iris, id.vars="Species")
            iris2[1:3,]
            bar1 <- ggplot(data=iris2, aes(x=Species, y=value, fill=variable))
            bar1 + geom_bar(stat="identity", position="dodge") + 
              scale_fill_manual(values=c("orange", "blue", "darkgreen", "purple"),
                                name="Iris\nMeasurements",
                                breaks=c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
                                labels=c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width"))
            
          }
          
          else{
            
            df=read.csv('C:/Users/Nidhi/Downloads/supermarket-sales/supermarket_sales_sample.csv')
            #Tit = airquality
            df <- na.omit(df)
            print(df)
            # Drop the columns of the dataframe
            #Tit <- select (df,-c(gross.margin.percentage,Date,Invoice.ID))
            
            Tit <- select (df,c(Branch,City,Customer.type,Gender,Product.line,Unit.price,Quantity,Total,Payment,Rating))
            
            ggplot(Tit) +
              geom_density(mapping=aes(x=Rating, fill=Branch), alpha=.3)
            
          }
          
        })
        
        
        
        
        
        
    }
)