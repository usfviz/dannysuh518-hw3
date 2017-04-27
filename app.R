rm(list = ls())
cat("\014")

##### Libraries ####
library(shiny)
library(plyr)
library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)
library(tidyr)
library(GGally)

##### Diabetics Data #####

df_diab <- read.csv('diabetic_data.csv', header=TRUE, na.strings = "?", stringsAsFactors = FALSE)

df_diab['diag_1_diabetes'] <- floor(as.double(df_diab$diag_1)) == 250
df_diab[is.na(df_diab$diag_1_diabetes), 'diag_1_diabetes'] <- FALSE
df_diab[df_diab$diag_1_diabetes, 'diag_diabetes_when'] <- "Primary Test"

df_diab['diag_2_diabetes'] <- floor(as.double(df_diab$diag_2)) == 250
df_diab[is.na(df_diab$diag_2_diabetes), 'diag_2_diabetes'] <- FALSE
df_diab[!df_diab$diag_1_diabetes & df_diab$diag_2_diabetes, 'diag_diabetes_when'] <- "Secondary Test"

df_diab['diag_3_diabetes'] <- floor(as.double(df_diab$diag_3)) == 250
df_diab[is.na(df_diab$diag_3_diabetes), 'diag_3_diabetes'] <- FALSE
df_diab[!df_diab$diag_1_diabetes & !df_diab$diag_2_diabetes & df_diab$diag_3_diabetes, 'diag_diabetes_when'] <- "Tertiary Test"

df_diab['diag_3_diabetes'] <- floor(as.double(df_diab$diag_3)) == 250
df_diab[is.na(df_diab$diag_3_diabetes), 'diag_3_diabetes'] <- FALSE
df_diab[!df_diab$diag_1_diabetes & !df_diab$diag_2_diabetes & !df_diab$diag_3_diabetes, 'diag_diabetes_when'] <- "Not Diagnosed"

df_diab['A1Cresult'] = factor(df_diab$A1Cresult, levels=c('None','Norm','>7','>8'))
df_diab['max_glu_serum'] = factor(df_diab$max_glu_serum, levels=c('None','Norm','>200','>300'))

df_diab['bool_readmitted'] <- (df_diab$readmitted != "NO") * 1.0
df_diab['gender_race_concat'] <- paste(df_diab$gender, df_diab$race, sep="_")

df_diab <- df_diab[df_diab$gender != "Unknown/Invalid",]
df_diab <- df_diab[!is.na(df_diab$race),]

##### Facebook Data #####

df_fb <- read.table('dataset_Facebook.csv', header=TRUE, sep=';')
df_fb['post_id'] <- rownames(df_fb)
df_fb['Like'] <- df_fb$like/df_fb$Lifetime.Post.Total.Reach * 100
df_fb['Share'] <- df_fb$share/df_fb$Lifetime.Post.Total.Reach * 100
df_fb['Comment'] <- df_fb$comment/df_fb$Lifetime.Post.Total.Reach * 100

df_fb <- df_fb[!is.na(df_fb$Like) & !is.na(df_fb$Share) & !is.na(df_fb$Comment), ]


##### UI #####
ui <- navbarPage(
  title="Danny Suh Data Viz Homework",

  ##### Heatmap #####
  tabPanel(
    title = "Heatmap",
    sidebarLayout(
      sidebarPanel(
        p("Description: Purpose of this heatmap is to 
          compare readmission rate among age groups and races."),
        
        radioButtons(
          inputId = "gender1",
          label = "Gender",
          choices = list(
            "All",
            "Male",
            "Female"
          ),
          selected = "All"
        )
      ),
      mainPanel(
        plotOutput("plot1", height = "500px")
      )
    )
  ),
  
  ##### Small Multiples #####
  tabPanel(
    title = "Small Multiples",
    sidebarLayout(
      sidebarPanel(
        p("Description: Purpose of this small multiples is 
          to compare readmission rate between patients with
          different conditions. <30 and >30 indicates readmission
          within 30 days and after 30 days, respectively."),
        
        radioButtons(
          inputId = "gender2",
          label = "Gender",
          choices = list("All", "Male","Female"),
          selected = "All"
        ),
        
        radioButtons(
          inputId = "race",
          label = "Race",
          choices = list("All", "Caucasian", "AfricanAmerican",
                         "Asian", "Hispanic", "Other"),
          selected = "All"
        )
        
      ),
      mainPanel(
        plotOutput("plot2", height = "500px")
      )
    )
  ),
  
  ##### Parallel Coordinates Plot #####
  tabPanel(
    title = "Parallel Coordinates Plot",
    sidebarLayout(
      sidebarPanel(
        p("Description: Purpose of this plot is to compare
          percentage of user interactions (likes, shares, comments) 
          among different types of posts."),
        radioButtons(
          inputId = "type",
          label = "Type",
          choices = list("All", "Link", "Photo", "Status", "Video"),
          selected = "All"
        ),
        
        sliderInput(
          inputId = "month",
          label = "Month",
          value=c(1,12), min=1, max=12, step=1,
          ticks = FALSE)
        
      ),
      
      mainPanel(
        plotOutput("plot3", height = "500px")
      )
    )
    
  )
  #####
)

##### Server ####
server <- function(input, output) {
  
  ##### Heatmap #####
  df_heatmap_subset <- reactive({
    if (input$gender1 == 'All') {
      df_temp = df_diab
    } else {
      df_temp = df_diab[df_diab$gender == input$gender1,]
    }
    df_subset <- aggregate(df_temp$bool_readmitted,
                           by=list(df_temp$race, df_temp$age), FUN=mean)
    df_subset
  })
  output$plot1 <- renderPlot({
    ggplot(df_heatmap_subset(), aes(Group.2, Group.1)) + 
      geom_tile(aes(fill = x), colour = "white") +
      ggtitle("Readmission Rate Heatmap Age vs Race") +
      scale_fill_gradient(
        name="Readmission\nRate",
        low = "lightblue1", high = "steelblue") +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(
        expand = c(0, 0),
        limits = c("Other", "AfricanAmerican", "Caucasian", 
                   "Hispanic", "Asian")) +
      theme_grey(base_size = 18) +
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
  })
  
  ##### Small Multiples #####
  df_sm_subset <- reactive({
    if (input$gender2 == 'All') {
      df_temp = df_diab
    } else {
      df_temp = df_diab[df_diab$gender == input$gender2,]
    }
    
    if (input$race == 'All') {
      df_subset = df_temp
    } else {
      df_subset = df_temp[df_temp$race == input$race,]
    }
    
    df_subset
  })
  output$plot2 <- renderPlot({
    df = df_sm_subset()
    
    group_count_p1 <- ddply(
      .data=df, 
      .(diag_diabetes_when), 
      summarize, 
      n=paste("n =", length(readmitted))
    )
    
    group_count_p2 <- ddply(
      .data=df, 
      .(A1Cresult), 
      summarize, 
      n=paste("n =", length(readmitted))
    )
    
    group_count_p3 <- ddply(
      .data=df, 
      .(max_glu_serum), 
      summarize, 
      n=paste("n =", length(readmitted))
    )
    
    p1 <- ggplot(data=df, aes(x=readmitted)) +
      facet_grid(~diag_diabetes_when)+
      geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
      geom_text(data=group_count_p1, aes(x=1.1, y=0.45, label=n), 
                colour="black", inherit.aes=FALSE, parse=FALSE, size=5) +
      theme_grey(base_size = 18) +
      theme(axis.title.x=element_blank()) +
      labs(y="Diagonosed with\nDiabetes During") 
    
    p2 <- ggplot(data=df, aes(x=readmitted)) +
      facet_grid(~A1Cresult) +
      geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
      geom_text(data=group_count_p2, aes(x=1.1, y=0.45, label=n), 
                colour="black", inherit.aes=FALSE, parse=FALSE, size=5) +
      theme_grey(base_size = 18) +
      theme(axis.title.x=element_blank()) +
      labs(y="A1C Test\n")
    
    p3 <- ggplot(data=df, aes(x=readmitted)) +
      facet_grid(~max_glu_serum) +
      geom_bar(aes(y = (..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..])) +
      geom_text(data=group_count_p3, aes(x=1.1, y=0.45, label=n), 
                colour="black", inherit.aes=FALSE, parse=FALSE, size=5) +
      theme_grey(base_size = 18) +
      theme(axis.title.x=element_blank()) +
      labs(y="Glucose\nSerum Test") 
    
    
    grid.arrange(p1, p2, p3, nrow=3,
                 # padding = unit(c(4, 4), "mm"),
                 top = textGrob("Readmission Rate Density Histogram\nfor Various Conditions",
                                gp=gpar(fontsize=20,font=1)),
                 bottom = textGrob("Readmitted", gp=gpar(fontsize=15,font=1)))
  })
  
  ##### Parallel Coordinates Plot #####
  df_pcplot_subset <- reactive({
    df_subset <- df_fb[df_fb$Post.Month >= input$month[1] &
                         df_fb$Post.Month <= input$month[2],]
    df_subset_gathered <- gather(df_subset, 'column', 'value', Like:Comment)
    df_subset_gathered
  })
  
  list_type <- reactive({
    if (input$type == 'All') {
      list_type = c("Link", "Photo", "Status", "Video")
    } else {
      list_type = c(input$type)
    }
    list_type
  })
  
  color_values <- reactive({
    values = c("Link" = "#B79F00", "Photo" = "#F8766D",
               "Status" = "#00BFC4", "Video" = "#00BA38")
    if (input$type != 'All') {
      values[!(names(values) %in% c(input$type))] <- 'grey'
    }
    
    values
  })
  
  output$plot3 <- renderPlot({
    df <- df_pcplot_subset()
    
    ggplot() +
      geom_line(data=df[!(df$Type %in% list_type()),],
                aes(column, value, col=Type, group=post_id), alpha = 0.3) +
      geom_line(data=df[df$Type %in% list_type(),],
                aes(column, value, col=Type, group=post_id), alpha = 0.3, size=1) +
      
      ylim(0.0, 8.0) +
      
      ggtitle("Parellel Coordinate Plot of User Interaction on Posts") +
      labs(y="Percentage (%)") +
      
      scale_colour_manual(values = color_values()) +
      scale_x_discrete(
        expand = c(0, 0),
        limits=c("Like", "Share", "Comment")) +
      
      theme_grey(base_size = 18) +
      
      theme(panel.background = element_blank(),
            panel.grid.major = element_line(colour = "grey95"),
            axis.title.x=element_blank(),
            plot.margin = unit(c(0.5,0.5,0.5,0.5), "cm"))
  })
}



shinyApp(ui = ui, server = server)

# shiny::runGitHub("usfviz", "dannysuh518-hw3")


