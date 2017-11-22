dynamicplot <- function(var,dbdata=NULL,overlay=NULL,eps=50,MinPts=50) {
  require(shiny)
  require(ggplot2)
  shinyApp(
    ui <- fluidPage(
      fluidRow(
        column(width = 4,
               conditionalPanel(
                 condition = "input.plotType == 'scatter'",plotOutput("plot1", height = 600,
                          # Equivalent to: click = clickOpts(id = "plot_click")
                          #click = "plot1_click",
                          brush = brushOpts(
                            id = "plot1_brush",resetOnNew = TRUE,clip=TRUE
                          )
               )
        ),conditionalPanel(condition = "input.plotType == 'hist'",plotOutput("plot3", height = 600,
                                                               # Equivalent to: click = clickOpts(id = "plot_click")
                                                               #click = "plot1_click",
                                                               brush = brushOpts(
                                                                 id = "plot3_brush",resetOnNew = TRUE,direction = "x",clip=TRUE
                                                               )
        ))),
        
        column(width = 8,
               plotOutput("plot2", height = 600,
                          #click = "plot2_click",
                          brush = brushOpts(
                            id = "plot2_brush",resetOnNew = TRUE,clip=TRUE))
               
        )
      ),
      fluidRow(column(6, selectInput("plotType", "Plot Type",
                                     c(Scatter = "scatter", Histogram = "hist")
      )),
               column(6,
                      actionButton("resetzoom", "Reset Zoom!"))),
      # fluidRow(
      #   column(6,
      #          verbatimTextOutput("info")
      #   ),
      #       column(6,
      #              verbatimTextOutput("info2")
      #       )
      # ),
      fluidRow(
        #     column(width = 6,
        #            h4("Points near click"),
        #            verbatimTextOutput("click_info")
        #     ),
        column(width = 6,
               conditionalPanel(
                 condition = "input.plotType == 'scatter'",
               uiOutput("xaxis_select"),
               uiOutput("yaxis_select")),conditionalPanel(condition = "input.plotType == 'hist'",
                                                          uiOutput("hist_select"),
                                                          sliderInput("breakCount", "Break Count", min = 1, max = 100, value = 20))
        ),
        #     column(width = 6,
        #            h4("Points near click"),
        #            verbatimTextOutput("click_info2")
        #     ),
            column(width = 6
                  # h4("Brushed points")
                   
      )
      
    )),
    server <- function(input, output) {
      
      # Create a Progress object
      progress <- shiny::Progress$new()
      # Make sure it closes when we exit this reactive, even if there's an error
      on.exit(progress$close())
      
      progress$set(message = "Processing data", value = 0)
      
      progress$inc(1/4, detail = "calculating DBSCAN")
      locdata <- var
      locdata$color <- 1
      if(is.null(dbdata)){
      dbdata <- SMOLR_DBSCAN(locdata,eps=eps,MinPts = MinPts)
      locdata$cluster <- dbdata$dbscan$Cluster
      }
     # locdata$color[locdata$cluster>0] <- 2
      if(is.null(overlay)){
        progress$inc(2/4, detail = "calculating overlay")
        overlay <- SMOLR(locdata,fast = T)
      }
      clusterdata <- SMOLR_POINT_FEATURES(dbdata)
      clusterdata$color <- 1
      
      progress$inc(3/4, detail = "Finishing up")
      statdata <- dbdata$parameters
      
      ori_locdata <- locdata
      ori_clusterdata <- clusterdata
      ori_overlay <- overlay 
      ori_statdata <- statdata
      
      progress$inc(4/4, detail ="Done")
      makeReactiveBinding('clusterdata')
      makeReactiveBinding('locdata') 
      
      output$xaxis_select <- renderUI({
        selectInput("xaxis", "X-axis:", 
                    choices=colnames(clusterdata),selected="sd")
      })
      output$yaxis_select <- renderUI({
        selectInput("yaxis", "Y-axis:", 
                    choices=colnames(clusterdata),selected="N")
      })
      output$hist_select <- renderUI({
        selectInput("hist_axis", "Variable:", 
                    choices=colnames(clusterdata),selected="N")
      })
      
      
      
      output$plot1 <- renderPlot({
        ggplot(clusterdata, aes_string(input$xaxis,input$yaxis )) + geom_point(aes(colour = color))+theme(axis.text=element_text(size=12),
                                                                                                           axis.title=element_text(size=14,face="bold"))
      })
      
      output$plot2 <- renderPlot({
        SMOLR_PLOT(locdata,color = locdata$color,xlim=c(statdata$min_x[1],statdata$max_x[1]),
                   ylim=c(statdata$min_y[1],statdata$max_y[1]),overlay=overlay,contrast=10,alpha=0.1)
      })
      
      output$plot3 <- renderPlot({
        ggplot(clusterdata, aes_string(input$hist_axis)) + geom_histogram(aes(colour = color),bins=input$breakCount)+theme(axis.text=element_text(size=12),
                                                                                                                            axis.title=element_text(size=14,face="bold"))
      })
      
      # output$click_info <- renderPrint({
      #   # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
      #   # were a base graphics plot, we'd need those.
      #   nearPoints(out, input$plot1_click, addDist = TRUE)
      # })
      
      output$brush_info <- renderPrint({
        brushedPoints(clusterdata, input$plot1_brush)
      })
      
      # output$click_info2 <- renderPrint({
      #   # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
      #   # were a base graphics plot, we'd need those.
      #   nearPoints(clusterdatadata, input$plot2_click, addDist = TRUE,xvar = "X",yvar = "Y",threshold=10)
      # })
      
      output$brush_info2 <- renderPrint({
        brushedPoints(locdata, input$plot2_brush,xvar = "X",yvar = "Y")
      })
      
      output$info <- renderText({
        xy_str <- function(e) {
          if(is.null(e)) return("NULL\n")
          paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
        }
        xy_range_str <- function(e) {
          if(is.null(e)) return("NULL\n")
          paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
                 " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
        }
        
        paste0(
          "brush: ", xy_range_str(input$plot1_brush)
        )
      })
      
        output$info2 <- renderText({
          xy_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("x=", round(e$x, 1), " y=", round(e$y, 1), "\n")
          }
          xy_range_str <- function(e) {
            if(is.null(e)) return("NULL\n")
            paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1),
                   " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
          }

          paste0(
            "brush: ", xy_range_str(input$plot2_brush)
          )
        })
      
      observeEvent(input$resetzoom, {
        ori_locdata ->> locdata
        ori_clusterdata ->> clusterdata
        ori_overlay ->> overlay 
        ori_statdata ->> statdata
      })
      
      observeEvent(input$plot1_brush, {
        # Get 1 datapoint within 15 pixels of click, see ?nearPoints
        np <-   brushedPoints(clusterdata, input$plot1_brush)
        
        clusterdata$color <<- rep(1,nrow(clusterdata))
        clusterdata$color[clusterdata$cluster %in% np$cluster] <<- 2
        locdata$color <<- rep(1,nrow(locdata))
        locdata$color[locdata$cluster %in% np$cluster] <<- 2
        
      })
      
      observeEvent(input$plot3_brush, {
        # Get 1 datapoint within 15 pixels of click, see ?nearPoints
        np <-   brushedPoints(clusterdata, input$plot3_brush,yvar=NULL)
        
        clusterdata$color <<- rep(1,nrow(clusterdata))
        clusterdata$color[clusterdata$cluster %in% np$cluster] <<- 2
        locdata$color <<- rep(1,nrow(locdata))
        locdata$color[locdata$cluster %in% np$cluster] <<- 2
        
      })
      
      observeEvent(input$plot2_brush, {
        # Get 1 datapoint within 15 pixels of click, see ?nearPoints
        np <-  brushedPoints(locdata, input$plot2_brush,xvar = "X",yvar = "Y")
        
        
        oldlim <- as.numeric(statdata[6:9])
        newlim <- c(min(np$X)-10,max(np$X)+10,min(np$Y)-10,max(np$Y)+10)
        
        clusterdata <<- subset(clusterdata,clusterdata$cluster %in% np$cluster)
        
        np$X <- np$X-newlim[1]
        np$Y <- np$Y-newlim[3]
        locdata <<- np
        dimensions <- dim(overlay$img)[1:2]
        pixsize <- c((as.numeric(statdata[7])-as.numeric(statdata[6]))/dimensions[1],
                     (as.numeric(statdata[9])-as.numeric(statdata[8]))/dimensions[2])
        
        xmin <<- floor((newlim[1]-oldlim[1])/pixsize[1])
        xmax <<- ceiling((newlim[2]-newlim[1])/pixsize[1]+xmin)
        ymin <<- floor((newlim[3]-oldlim[3])/pixsize[2])
        ymax <<- ceiling((newlim[4]-newlim[3])/pixsize[2]+ymin)
        
        tmp <- array(data=0, dim=(c(xmax-xmin+1,ymax-ymin+1,1)))
        # browser()
        tmp[,,1] <- overlay$img[xmin:xmax,ymin:ymax,1]
        
        overlay$img <<- tmp
        
        newlim[1:2] <- newlim[1:2]-newlim[1]
        newlim[3:4] <- newlim[3:4]-newlim[3]
        
        statdata[6:9] <<- newlim
        
      })
    }
    #options =  list(display.mode="showcase")
  )}


#load("D:/Dropbox/PhD R_dam/manuscripts/Methods paper/Singlemoleculer_GUI/locdata.R")

#overlay <- SMOLR(locdata)
#dynamicplot(locdata,overlay=overlay)
