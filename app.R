#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

ui <- fluidPage(
  tags$head(tags$style(HTML("
                            .shiny-text-output {
                            background-color:#FF0000;
                            } 
                            "))),
  
  h1(span("Idiographic Networks", style = "font-weight: 300"), 
     style = "font-family: 'Source Sans Pro';
     color: #fff; text-align: center;
     background-color:#6A51A3;
     padding: 20px"),
  
  
  br(),
  # Application title
  #titlePanel("Idiographic Personality Networks"),
  # Sidebar with a slider input for number of bins 
  tabsetPanel(
    tabPanel("Plot", 
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("SID",
                                "Participant ID #1:",
                                choices = ""),
                 radioButtons("type1", "Type of Data:", 
                                choices = c("Raw", "Composites", "Client Predictions")),
                 radioButtons("resid1", "Residualized?", 
                                choices = c("None", "Day", "Survey", "Day and Survey")),
                 radioButtons("wave",
                                "Wave:",
                                choices = c("1", "2")),
                 radioButtons("Cor1",
                                "Select Correlation Type",
                                choices = c("Lagged", "Contemporaneous")),
                 selectizeInput("SID2",
                                "Participant ID #2:",
                                choices = ""),
                 radioButtons("type2", "Type of Data:", 
                              choices = c("Raw", "Composites", "Client Predictions")),
                 radioButtons("resid2", "Residualized?", 
                              choices = c("None", "Day", "Survey", "Day and Survey")),
                 radioButtons("wave2",
                                "Wave:",
                                choices = c("1", "2")),
                 radioButtons("Cor2",
                                "Select Correlation Type",
                                choices = c("Lagged", "Contemporaneous"))
               ), 
               mainPanel(
                 plotOutput("gVARPlot")
               ))),
    tabPanel("Centrality",  
             sidebarLayout(
               sidebarPanel(
                 selectizeInput("SID3",
                                "Participant ID #1:",
                                choices = ""),
                 radioButtons("type3", "Type of Data:", 
                              choices = c("Raw", "Composites", "Client Predictions")),
                 radioButtons("resid3", "Residualized?", 
                              choices = c("None", "Day", "Survey", "Day and Survey")),
                 radioButtons("Cor3",
                                "Select Correlation Type",
                                choices = c("Lagged", "Contemporaneous")),
                 selectizeInput("SID4",
                                "Participant ID #2:",
                                choices = ""),
                 radioButtons("type4", "Type of Data:", 
                              choices = c("Raw", "Composites", "Client Predictions")),
                 radioButtons("resid4", "Residualized?", 
                              choices = c("None", "Day", "Survey", "Day and Survey")),
                 radioButtons("Cor4",
                                "Select Correlation Type",
                                choices = c("Lagged", "Contemporaneous"))
               ), 
               mainPanel(
                 plotOutput("centrality")
               )))#,
    # tabPanel("Edge Distributions",
    #          sidebarLayout(
    #            sidebarPanel(
    #              selectizeInput("Cor5",
    #                             "Select Correlation Type",
    #                             choices = c("Lagged", "Contemporaneous")),
    #              selectizeInput("type5", "Type of Data:", 
    #                             choices = c("Residualized", "Composites", "Client Predictions"))
    #            ), 
    #            mainPanel(
    #              plotOutput("dist")
    #            )))
  )
  
  # Show a plot of the generated distribution
  
  )



# Define server logic required to draw a histogram

load_url <- function (url, ..., sha1 = NULL) {
  # based very closely on code for devtools::source_url
  stopifnot(is.character(url), length(url) == 1)
  temp_file <- tempfile()
  on.exit(unlink(temp_file))
  request <- httr::GET(url)
  httr::stop_for_status(request)
  writeBin(httr::content(request, type = "raw"), temp_file)
  file_sha1 <- digest::digest(file = temp_file, algo = "sha1")
  if (is.null(sha1)) {
    message("SHA-1 hash of file is ", file_sha1)
  }
  else {
    if (nchar(sha1) < 6) {
      stop("Supplied SHA-1 hash is too short (must be at least 6 characters)")
    }
    file_sha1 <- substr(file_sha1, 1, nchar(sha1))
    if (!identical(file_sha1, sha1)) {
      stop("SHA-1 hash of downloaded file (", file_sha1, 
           ")\n  does not match expected value (", sha1, 
           ")", call. = FALSE)
    }
  }
  load(temp_file, envir = .GlobalEnv)
}
library(RColorBrewer)
edge_colors <- RColorBrewer::brewer.pal(8, "Purples")[c(3,4,6)]

idio_plot_fun <- function(data, subject, wave, type, Source){
  if(type == "Lagged"){data_mod <- data$PDC}
  else{data_mod <- data$PCC}
  keep <- colSums(is.na(data_mod)) < nrow(data_mod) | rowSums(is.na(data_mod)) < nrow(data_mod)
  data_mod <- data_mod[keep,keep]
  if(type != "Lagged"){
    data_mod[upper.tri(data_mod)] <- t(data_mod)[upper.tri(data_mod)]
    diag(data_mod) <- 1
  } #else {diag(data_mod) <- NA}
  colnames(data_mod) <- mapvalues(colnames(data_mod), from = vars$old, vars$new, warn_missing = F)
  rownames(data_mod) <- mapvalues(rownames(data_mod), from = vars$old, vars$new, warn_missing = F)
  if(grepl("Comp", Source) == F){
    cent_col <- centrality_pred %>% filter(ID == subject) %>% select(-ID) %>%
      filter(Variable %in% rownames(data_mod)) %>%
      full_join(tibble(Variable = rownames(data_mod))) %>%
      mutate(`Most/Least` = ifelse(is.na(`Most/Least`), "Neither", `Most/Least`),
             color = mapvalues(`Most/Least`, c("Most", "Least", "Neither"), c("black", "white", "gray")),
             weight = mapvalues(`Most/Least`, unique(`Most/Least`), c(2, 4, 6)),
             Variable = factor(Variable, levels = rownames(data_mod))) %>%
      arrange(Variable)
    groups <- list(Most = which(cent_col$`Most/Least` == "Most"),
                   Least = which(cent_col$`Most/Least` == "Least"),
                   Neither = which(cent_col$`Most/Least` == "Neither"))
  }
  if(Source == "Client Predictions"){
    node_col <- cent_col$color
    border_width <- rep(2, nrow(data_mod))
  } else if (grepl("Comp", Source)){
    groups <- list(all = seq(1, nrow(data_mod)))
    node_col <- rep("white", nrow(data_mod))
    border_width <- rep(2, nrow(data_mod))
  } else {
    node_col <- rep("white", nrow(data_mod))
    border_width <- as.numeric(cent_col$weight)
  }
  plot <- 
    qgraph(data_mod, layout = "spring", loop = .7, node.width = 1.85, edge.width = 1,
           esize = 7, title = sprintf("%s %s Wave %s for S%s", Source, type, wave, subject),
           label.font = 2, repulsion = .8, label.fill.vertical = 1, border.width = border_width,
           label.fill.horizontal = 1, edge.color = "black", 
           groups = groups, color = node_col,
           legend = F, DoNotPlot = TRUE, mar = c(4,4,4,4))
  #change lines to dashed
  plot$graphAttributes$Edges$lty[plot$Edgelist$weight < 0] <- 2
  #change line colors
  plot$graphAttributes$Edges$color <-
    ifelse(abs(plot$Edgelist$weight) <.05, edge_colors[1],
           ifelse(abs(plot$Edgelist$weight) <.1, edge_colors[2], edge_colors[3]))
  # change labels of dark nodes to white
  dark_colors <- c("#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D")
  plot$graphAttributes$Nodes$label.color[plot$graphAttributes$Nodes$color %in% dark_colors] <- "white"
  #change variable names
  plot$graphAttributes$Nodes$labels <- gsub("_", "\n", names(plot$graphAttributes$Nodes$labels))
  return(plot)
}


centrality_Plot_fun <- function(x, ct, d){
  centrality_long  %>%
    filter(ID %in% x & source == ct & dir == d) %>%
    arrange(measure, wave) %>%
    ggplot(aes(x = var, y = z, group = wave))+
    geom_line(aes(linetype = wave), color = "black", size = .3) + 
    geom_point(aes(shape = wave), size = 2) + 
    labs(x = NULL, y = "z-score", linetype = "Wave", shape = "Wave",
         title = sprintf("Participant %s", x),
         subtitle = ct) +
    scale_y_continuous(limits = c(-3,3), breaks = seq(-3,3,1)) + 
    geom_hline(aes(yintercept = 0)) + 
    coord_flip() + 
    facet_grid(~dir + measure) + 
    theme_classic()+ 
    theme(axis.text = element_text(face = "bold"),
          axis.title = element_text(face = "bold"),
          legend.title = element_text(face = "bold"),
          legend.position = "bottom",
          plot.title = element_text(face = "bold", hjust = .5),
          plot.subtitle = element_text(face = "bold", hjust = .5),
          strip.text = element_text(face = "bold", color = "white", size = rel(1)),
          strip.background = element_rect(fill = "black"))
}

# distribution_plot_fun <- function(Type, l){
#   if(Type == "Lagged"){
#     temp_long %>% 
#       filter(lambda_scale == l) %>%
#       ggplot(aes(x = weight, y = from, fill = factor(wave))) +
#       # tidybayes::geom_halfeyeh() +
#       geom_density_ridges(rel_min_height = 0.025) +
#       # geom_density() +
#       labs(x = "From", y = "Edge Weight", 
#            title = sprintf("%s %s Partial Correlations", l, str_to_title(Type))) +
#       facet_grid(to~wave) +
#       theme_classic() +
#       theme(legend.position = "none",
#             plot.title = element_text(hjust = .5))
#   } else {
#     contemp_long %>% 
#       filter(lambda_scale == l) %>%
#       ggplot(aes(x = weight, y = var, fill = factor(wave))) +
#       # tidybayes::geom_halfeyeh() +
#       geom_density_ridges(rel_min_height = 0.025) +
#       # geom_density() +
#       labs(x = "From", y = "Edge Weight", 
#            title = sprintf("%s %s Partial Correlations", l, str_to_title(Type))) +
#       facet_grid(~wave) +
#       theme_classic() +
#       theme(legend.position = "none",
#             plot.title = element_text(hjust = .5))
#   }
# }

load_url("https://github.com/emoriebeck/PSC_EMA/blob/master/app_data.RData?raw=true")

library(qgraph)
library(ggridges)
library(tidyverse)
library(gridExtra)


server <- function(input, output, session) {
  observe({
    if(as.character(input$wave) == "1"){
      subs1 <- unique((gVAR_data %>% filter(wave == "1"))$ID)
      updateSelectizeInput(session, 'SID', choices = c("", subs1))
    } else {
      subs1 <- unique((gVAR_data %>% filter(wave == "2"))$ID)
      updateSelectizeInput(session, 'SID', choices = c("", subs1))
    }
  })
  
  observe({
    if(as.character(input$wave2) == "1"){
      subs2 <- unique((gVAR_data %>% filter(wave == "1"))$ID)
      updateSelectizeInput(session, 'SID2', choices = c("", subs2))
    } else {
      subs2 <- unique((gVAR_data %>% filter(wave == "2"))$ID)
      updateSelectizeInput(session, 'SID2', choices = c("", subs2))
    }
    
  })
  
  type_fun <- function(type, resid){
    source <- ifelse(!resid == "None" & type == "Composites", paste(resid, "Residualized Composites"),
              ifelse(!resid == "None" & type == "Raw", paste(resid, "Residualized Composites"),
              ifelse(resid == "None" & type == "Composites", "Composites",
              ifelse(resid == "None" & type == "Raw", "Raw",
              "Client Predictions"))))
  }
  
  observe({
    source3 <- type_fun(input$type3, input$resid3)
    source4 <- type_fun(input$type4, input$resid4)
    subs3 <- unique((centrality_long %>% filter(dir == input$Cor3 & source == source3))$ID)
    subs4 <- unique((centrality_long %>% filter(dir == input$Cor4 & source == source4))$ID)
    updateSelectizeInput(session, 'SID3', choices = c("",subs3))
    updateSelectizeInput(session, 'SID4', choices = c("", subs4))
  })
  
  output$gVARPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    validate(
      need(input$SID, input$SID2, 'Please select 2 Subject IDs'),
      need(input$SID2, 'Please select 2 Subject IDs'))
      
      source1 <- type_fun(input$type1, input$resid1)
      dat <- (gVAR_data %>% filter(ID == input$SID & wave == input$wave & source == source1 & gamma == .1))$gVAR[[1]]
      plot1 <- idio_plot_fun(dat, input$SID, input$wave, input$Cor1, source1)
      
      source2 <- type_fun(input$type2, input$resid2)
      dat <- (gVAR_data %>% filter(ID == input$SID2 & wave == input$wave2 & source == source2 & gamma == .1))$gVAR[[1]]
      plot2 <- idio_plot_fun(dat, input$SID2, input$wave2, input$Cor2, source2)
      # plot1  <-  plot_beta_w1[[input$SID]]
      print(plot1); print(plot2)
    # draw the histogram with the specified number of bins
    if(!("" %in% input)){
      par(mfrow = c(1,2))
      plot(plot1)
      plot(plot2)
    }
  })
  
  output$centrality <- renderPlot({
    # generate bins based on input$bins from ui.R
    validate(
      need(input$SID3, input$SID4, 'Please select 2 Subject IDs'),
      need(input$SID4, 'Please select 2 Subject IDs'))
      source3 <- type_fun(input$type3, input$resid3)
      source4 <- type_fun(input$type4, input$resid4)
      plot1  <-  centrality_Plot_fun(input$SID3, source3, input$Cor3)
      plot2  <-  centrality_Plot_fun(input$SID4, source4, input$Cor4)
    
    grid.arrange(plot1, plot2, ncol = 2)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

