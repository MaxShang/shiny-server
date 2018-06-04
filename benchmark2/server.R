source("load_packages.R")
data_folder<-"user_input_data"

get_time_human <- function() {
        format(Sys.time(), "%Y%m%d-%H%M%OS")
}

saveData <- function(data,id) {
        file_name <- paste0(id,".csv")
        data <- t(c(data,id))
        write.csv(x = data, file = file.path(data_folder, file_name), 
                  row.names = FALSE, quote = TRUE)
}

loadData <- function() {
        files <- list.files(file.path(data_folder), full.names = TRUE)
        data <- lapply(files, read.csv, stringsAsFactors = FALSE) %>% 
                do.call(rbind, .)
        data<-data %>% 
                mutate(feed = homegrown_feed+purchased_feed) %>% 
                mutate(labour = exp_l_family+exp_l_hired) %>% 
                rename(farm_id=X) %>% 
                mutate(farm_id=as.character(farm_id)) %>% 
                arrange(farm_id)
        data
}

# Define the fields we want to save from the form
fields <- c("q_milk",
            "purchased_feed",
            "homegrown_feed",
            "capital",
            "exp_l_family",
            "exp_l_hired",
            "misc",
            "year",
            "business_type",
            "edu",
            "housing",
            "milking_system",
            "feeding_system",
            "robots",
            "milking_cows_tmr",
            "heifers_tmr",
            "separated",
            "manure",
            "breed",
            "milk_recording",
            "age_calving",
            "calving_interval",
            "barn_capacity",
            #output
            "q_butterfat",
            "n_lactating_cows",
            "n_dry_cows",
            "n_heifers",
            # expense info
            "labour_family",
            "labour_hired"
)
# server -----
server <- function(input, output) {
  # welcome-----

  output$cowbg<-renderImage(
    #width  <- session$clientData$output_cowbg_width
    #height <- session$clientData$output_cowbg_height
    list(
    src = "cowbg.png",
    filetype = "image/png",
    width = 720,
    height = 720,
    alt = "This is background"),deleteFile = FALSE)
  
  # Whenever a field is filled, aggregate all form data
  formData <- reactive({
    data <- sapply(fields, function(x) input[[x]])
    data
  })
  
  
  # When the Submit button is clicked, save the form data
  observeEvent(input$submit, {
        # alert------
        showModal(modalDialog(
                  title = "Submitted Successfully",
                  "Benchmark takes a few seconds.  
                  Accuracy of benchmark results depends on your input.    
                  To see the results, please click links on the left panel.",
                  easyClose = TRUE
        )) 
    rdm_id <-floor(runif(1,10^9,10^11))
    id<-paste0(as.integer(Sys.time()),"-",get_time_human(),"-",rdm_id)
    saveData(formData(),id =id )
    
    # Read the data
    testData<-loadData()
    write.csv(testData,paste0("data",".csv"))
    # testData <- testData %>% 
    #                      drop_na()
    
    n.row<-which(testData$farm_id == id)
    testData<-testData[1:n.row,]
    # overall efficiency ---------------
    
    X<-testData %>% 
            select(feed,capital,labour,misc)
    X<-data.matrix(X)
    Y<-testData %>% 
            select(q_milk)
    Y<-data.matrix(Y)
    DEA.results <- dea(X, Y,  RTS=3);
    eff.scores.org <- DEA.results$eff;
    myscore = eff.scores.org[n.row]
    eff.scores <- sort(eff.scores.org);
    d.plot<-data.frame(n.farm=1:length(eff.scores),es=eff.scores)
    n.myfarm<-d.plot$n.farm[which.min(abs(eff.scores-myscore))]
    d.myfarm<-data.frame(x=n.myfarm,y=myscore,lab=paste0("My Farm: ",round(myscore,3)))
    
    # milk output ----------------------
    
    milk_output<-testData %>% 
      mutate(total_cost = purchased_feed+
               homegrown_feed+
               capital+
               exp_l_family+
               exp_l_hired+
               misc) %>% 
      mutate(milk_per_cow = q_milk/n_lactating_cows) %>% 
      mutate(feed_per_litre_milk = (purchased_feed+homegrown_feed)/q_milk) %>% 
      mutate(capital_per_litre_milk = capital/q_milk) %>% 
      mutate(labour_per_litre_milk = (exp_l_family+exp_l_hired)/q_milk) %>% 
      mutate(misk_per_litre_milk = misc/q_milk) %>% 
      mutate(ttcost_per_litre_milk = total_cost/q_milk) %>% 
      select(milk_per_cow,
             feed_per_litre_milk,
             capital_per_litre_milk,
             labour_per_litre_milk,
             misk_per_litre_milk,
             ttcost_per_litre_milk)
    milk_output$overall_eff_score <- eff.scores.org
    top10<-milk_output %>% 
      top_n(10,wt = overall_eff_score) %>% 
      colMeans() %>% 
      unlist()
    
    tb_milk_output_data<-data.frame(
      `My Operation`=unlist(milk_output[n.row,1:6]),
      `Average of All Operations`=unlist(colMeans(milk_output[,1:6])),
      top10=top10[1:6],
      row.names =c("Milk Output(Litres) Per Cow",
                   "Cost of Feed(Dollars) Per Milk Output(Litres)",
                   "Cost of Capital(Dollars) Per Milk Output(Litres)",
                   "Cost of Labour(Dollars) Per Milk Output(Litres)",
                   "Cost of Miscellaneous(Dollars) Per Milk Output(Litres)",
                   "Total Cost per Litre")
    )
    colnames(tb_milk_output_data)<-c("My Operation",
                                     "Average of All Operations",
                                     "Average of Top 10 Operations"
    )
    
    output$tb_milk_output<-renderTable(striped = TRUE,bordered = TRUE,rownames = TRUE,
                                       caption = "Summary of Milk Output",
                                       caption.placement = getOption("xtable.caption.placement", "top"),
                                       caption.width = getOption("xtable.caption.width", NULL),
                                       tb_milk_output_data)
    # expense breakdown--------------
    exp_breakdown <- testData %>%
      mutate(Feed = purchased_feed + homegrown_feed) %>%
      mutate(Labour = exp_l_family + exp_l_hired) %>%
      select(Feed, Labour, capital, misc)
    
    myfarm <-
      unlist(c(
        exp_breakdown[n.row, "Feed"],
        exp_breakdown[n.row, "Labour"],
        exp_breakdown[n.row, "capital"],
        exp_breakdown[n.row, "misc"]
      ))
    avefarm <- colMeans(exp_breakdown)
    df <- data.frame(
      Cost = c("Feed", "Labour", "Capital", "Misc"),
      value = myfarm / sum(myfarm) * 100
    )
    df <- df %>% arrange(desc(value))
    #df$Cost<- factor(df$Cost, levels = rev(as.character(df$Cost)))
    df$Cost <-
      factor(df$Cost, levels = c("Misc", "Capital", "Labour", "Feed"))
    

    output$my.exp.brkd<-renderPlot(
    
      ggplot(df, aes(x="",y = value, fill = Cost)) +
      geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
      coord_polar("y") +
      geom_text(aes(label = paste0(round(value), "%")), 
                position = position_stack(vjust = 0.5)) +
      labs(x = NULL, y = NULL, fill = NULL, 
           title = "Breakdown of My Expenses") +
      guides(fill = guide_legend(reverse = TRUE)) +
      scale_fill_manual(values = c("#969696","#F3DE8A", "#EB9486", "#92DDC2")) +
      theme_classic() +
      theme(axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            plot.title = element_text(hjust = 0.5, color = "#666666"))
    )
    
    df2 <- data.frame(
      Cost = c("Feed", "Labour", "Capital","Misc"),
      Value = myfarm,
      Percent = myfarm/sum(myfarm)*100
    )
    
    colnames(df2)=c("Cost","Value ($)", "Share (%)")
    
    output$my.exp.brkd.tb<-renderTable(striped = TRUE,
                                       caption = "Breakdown of My Expenses",
                                       caption.placement = getOption("xtable.caption.placement", "top"),
                                       caption.width = getOption("xtable.caption.width", NULL),
      df2
    )
    # average farm
    df.avefarm <- data.frame(
      Cost = c("Feed", "Labour", "Capital","Misc"),
      Value = avefarm,
      Percent = avefarm/sum(avefarm)*100
    )
    df.avefarm<-df.avefarm %>% arrange(desc(Percent))
    #df.avefarm$Cost<- factor(df.avefarm$Cost, levels = rev(as.character(df.avefarm$Cost)))
    df.avefarm$Cost<-factor(df$Cost, levels = c("Misc", "Capital","Labour", "Feed"))
    colnames(df.avefarm)=c("Cost","Value ($)", "Share (%)")
    
    output$my.exp.brkd.ave<-renderPlot(
      
      ggplot(df.avefarm, aes(x="",y = `Share (%)`, fill = Cost)) +
        geom_bar(width = 1, size = 1, color = "white", stat = "identity") +
        coord_polar("y") +
        geom_text(aes(label = paste0(round(`Share (%)`), "%")), 
                  position = position_stack(vjust = 0.5)) +
        labs(x = NULL, y = NULL, fill = NULL, 
             title = "Breakdown of Average Operation's Expenses") +
        guides(fill = guide_legend(reverse = TRUE)) +
        scale_fill_manual(values = c("#969696","#F3DE8A", "#EB9486", "#92DDC2")) +
        theme_classic() +
        theme(axis.line = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              plot.title = element_text(hjust = 0.5, color = "#666666"))
    )
    output$my.exp.brkd.ave.tb<-renderTable(striped = TRUE,
                                       caption = "Breakdown of Average Operation's Expenses",
                                       caption.placement = getOption("xtable.caption.placement", "top"),
                                       caption.width = getOption("xtable.caption.width", NULL),
                                       df.avefarm
    )
    # feed efficiency ---------------
    X.f<-testData %>% 
            select(feed)
    X.f<-data.matrix(X.f)
    #Y<-testData[,1]
    #Y<-data.matrix(Y)
    DEA.results.f <- dea(X.f, Y,  RTS=3);
    eff.scores.f <- DEA.results.f$eff;
    myscore.f = eff.scores.f[dim(testData)[1]]
    eff.scores.f <- sort(DEA.results.f$eff);
    d.plot.f<-data.frame(n.farm=1:length(eff.scores.f),es=eff.scores.f)
    n.myfarm.f<-d.plot.f$n.farm[which.min(abs(eff.scores.f-myscore.f))]
    d.myfarm.f<-data.frame(x=n.myfarm.f,y=myscore.f,lab=paste0("My Farm: ",round(myscore.f,3)))
    
    
    
    sum_table<-data.frame(
      Value=c(round(min(eff.scores),2),
              round(median(eff.scores),2),
              round(mean(eff.scores),2),
              round(max(eff.scores),2)),
      row.names = c("Minimum","Median","Average","Max")
    )
    output$eff_score_summary<-renderTable(striped = TRUE,
                                          caption = "Summary of all Efficiency Scores",
                                          caption.placement = getOption("xtable.caption.placement", "top"),
                                          caption.width = getOption("xtable.caption.width", NULL),
      t(sum_table)
      
    )
    
    output$eff_hist<-renderPlot(
      ggplot()+
        geom_histogram(mapping = aes(eff.scores))+
        geom_vline(xintercept =myscore,color="red" )+
        labs(title = "Histogram of Overall Efficiency Scores",
             x="Efficiency Score",
             y="Number of Farms")
    )
    # plot overall_eff ----------------------------------------------
    output$main_plot <- renderPlot({
      input$submit
      plot1<-ggplot() +
        geom_col(data = d.plot, aes(x = n.farm, y = es)) +
        geom_hline(
          data = d.plot,
          yintercept = myscore,
          lty = 3,
          col = "red"
        ) +
        labs(title = "Overall Efficiency Scores",
             x = "Farm", y = "Overall Efficiency Score") +
        geom_col(aes(x = n.myfarm, y = myscore), fill = "red")+
        geom_label(
          data = d.myfarm,
          aes(
            x = x,
            y = min(y+0.1,1),
            label = lab
            
          ),fill = "firebrick1",alpha=0.65,
          colour = "white",
          fontface = "bold",
          vjust = "inward", hjust = "inward"
        )
      plot1
      
    })

    # plot feed efficiency score-------------------------------------
  output$main_plot.f <- renderPlot({
    input$submit
    plot1<-ggplot() +
      geom_col(data = d.plot.f, aes(x = n.farm, y = es)) +
      geom_hline(
        data = d.plot.f,
        yintercept = myscore.f,
        lty = 3,
        col = "red"
      ) +
      labs(title = "Feed Efficiency Scores",
           x = "Farm", y = "Feed Efficiency Score") +
      geom_col(aes(x = n.myfarm.f, y = myscore.f), fill = "red")+
      geom_label(
        data = d.myfarm.f,
        aes(
          x = x,
          y = min(y+0.1,1),
          label = lab
          
        ),fill = "firebrick1",alpha=0.65,
        colour = "white",
        fontface = "bold",
        vjust = "inward", hjust = "inward"
      )
    plot1
    
  })
    
    output$text_eff_score<-renderText(
      "Your farm efficiency score is the red one. 
    Efficiency score ranges from 0 (the least efficient) 
    to 1 (the most efficient). 
    ")
    output$text_eff_score_dist<-renderText(
      "The red vertical line indicates your farm efficiency score.")
    
    output$infobox_overall_es <- renderValueBox({
      valueBox(
        round(myscore, digits = 3), "Overall Efficiency Score", icon = icon("signal"),
        color = "light-blue"
      )
    })
    
    output$infobox_overall_esf <- renderValueBox({
      valueBox(
        round(myscore.f,digits = 3), "Feed Efficiency Score", icon = icon("signal"),
        color = "light-blue"
      )
    })
   
    # generate report---------------
    
    output$report <- downloadHandler(
            
            
      # # For PDF output, change this to "report.pdf"
      filename = "report.pdf",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
          q_milk = input$q_milk,
          purchased_feed= input$purchased_feed,
          homegrown_feed= input$homegrown_feed,
          exp_l_family= input$exp_l_family,
          exp_l_hired= input$exp_l_hired,
          capital = input$capital,
          misc = input$misc,
          n.row = n.row
        )

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        input$report
        withProgress(message = 'Generating Report: ', value = 0, {
                n=100
                for (i in 1:100){
                        incProgress(1/n, detail = paste("  ",i,"%","done"))
                        Sys.sleep(0.01)}
                rmarkdown::render(tempReport, output_file = file,
                                  params = params,
                                  envir = new.env(parent = globalenv())
                )
        
                
               
        
        
      })}
    )
})
  
  
  # about us server -----------------------------
  output$about_us_txt<-renderText(
    "This website will analyze your performance
    and display results compared to dairy farmers in Ontario. 
    The benchmark project has been created by students and professors 
    at the University of Guelph. 
    Overall the project is a collaboration of the School of 
    Computing and the Department of Food, Agricultural, and Resource Economics."
  )
  
  
  
  
  # note------
  output$note<-renderText(
    "*To estimate your family labour expense multiply the number of family hours worked by the average wage per hour you pay your hired labour. If you do not have any hired labour then use $20/hour as a default wage rate.

**To estimate homegrown feed expense valued at market prices multiply the quantity of feed produced by its per unit price if you were to buy it from your local distributor."
  )
  
  output$rendered <-   renderUI({
    numericInput("misc",  tags$span("Miscellaneous expense",   
                                             tipify(bsButton("pB2", "(?)", style = "inverse", size = "extra-small"),
                                                    "Includes Hydro, Gasoline, Diesel, Fertilizer, Herbicides, Seeds, and other costs.")),
                       
                 value = 1,
                 min = 0, 
                 max = NA, 
                 step = NA,
                 width = NULL)
    
    
  })
  
}