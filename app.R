library(shiny)
library(dplyr)
library(ggplot2)
library(fmsb)
library(scales)
library(rsconnect)


char_df <- read.csv("mental_anxiety.csv")
year_df <- summarise(group_by(mental_anxiety_df, Year), avg_mental_disorders = mean(Prevalence...Mental.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE),avg_anxiety_disorders = mean(Prevalence...Anxiety.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE))
rate_df <- summarise(group_by(mental_anxiety_df, Year),
                     anxiety = 100 * (mean(Prevalence...Anxiety.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE) / mean(Prevalence...Mental.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE)),
                     schizophrenia = 100* (mean(Prevalence...Schizophrenia...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE) / mean(Prevalence...Mental.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE)),
                     bipolar= 100* (mean(Prevalence...Bipolar.disorder...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE) / mean(Prevalence...Mental.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE)),
                     eating = 100* (mean(Prevalence...Eating.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE) / mean(Prevalence...Mental.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE)),
                     drug = 100* (mean(Prevalence...Drug.use.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE) / mean(Prevalence...Mental.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE)),
                     depressive= 100* (mean(Prevalence...Depressive.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE) / mean(Prevalence...Mental.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE)),
                     alcohol= 100* (mean(Prevalence...Alcohol.use.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE) / mean(Prevalence...Mental.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE)))
sum_df <- summarise(rate_df,
                    Anxiety = mean(rate_df$anxiety),
                    Schizophrenia = mean(rate_df$schizophrenia),
                    Bipolar = mean(rate_df$bipolar),
                    Eating = mean(rate_df$eating),
                    Drug_use = mean(rate_df$drug),
                    Depressive = mean(rate_df$depressive),
                    Alcohol_use = mean(rate_df$alcohol),
                    Other = 100 - mean(rate_df$anxiety) - mean(rate_df$schizophrenia) - mean(rate_df$bipolar) - mean(rate_df$eating) - mean(rate_df$drug) - mean(rate_df$depressive) - mean(rate_df$alcohol))

about_view <- fluidPage(
  h1("Mental Health: Anxiety", class = "heading"),
  div(class = "smallspacer"),
  h3("Background:", class = "smallhead"),
  p("In this society, people will always face a range of different challenges and pressures 
  that can have a significant impact on their mental health. The fear of failure, combined 
  with the desire to succeed, can lead to high levels of stress and anxiety and other serious 
  mental health issues. Sometimes, people may be hesitant to seek help for fear of being perceived 
  as weak or a failure. This reluctance to address mental health issues can exacerbate the symptoms.",
    class = "paragraph"),
  
  div(class = "smallspacer"),
  
  tags$div(
    style = "display: flex; justify-content: center;",
    img(src = "https://images.axa-contento-118412.eu/www-axa-com%2F108cce09-998a-4f96-8480-9f773e40a9d3_cover-3.jpg?auto=compress,format&rect=0,0,1920,1281&w=1920&h=1281",width = "700px", height = "450px"),
  ),
  
  div(class = "spacer"),
  
  h3("Why Anxiety?", class = "smallhead"),
  p("In daily life, depression is often the first thing people think of when mental health problem
  is mentioned. However, as more and more data and research has shown, the growth of anxiety 
  disorders has made this symptoms of mental health problems more prominent and impossible to ignore.",
    class = "paragraph"),

  tags$div(
    style = "display: flex; justify-content: center;",
    img(src = "https://quotewizard.com/media/tfllindg/study-mental-health-during-pandemic-og.png",width = "800px", height = "400px"),
  ),
  
  div(class = "spacer"),
  
  h3("About:", class = "smallhead"),
  p("Description:", class = "smallhead2"),
  p("We anaylzed the data of the prevalence of mental health problems in different country from 1990-2019. 
  It also analyzes various factors and specific manifestations and symptoms of mental health problems. 
  Anxiety, as the main subject of our study, our purpose is to find the changes in anxiety disorder prevalence 
  within mental health from late 1900s to early 2000s. We are also here to provide some useful and meaningful suggestions!",
    class = "paragraph"),
  p("Source:", class = "smallhead2"),
  p("Institute of Health Metrics and Evaluation. Global Burden of Disease Study (2019)",
    class = "paragraph"),
  
  div(class = "spacer"),
  
  p("FROM BA5 YINGCHEN YANG & SAMMI HUANG", class = "name")
)

country_view <- fluidPage(
  titlePanel("Prevalence of mental disorders and anxiety disorders by country"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "char_name",
        label = "Select a Country",
        choices = unique(char_df$Entity)
      ),
      checkboxGroupInput(
        inputId = "checkboxes",
        label = "Select Disorders to Display:",
        choices = c("Anxiety Disorders", "Mental Disorders"),
        selected = c("Anxiety Disorders")
      )
    ),
    
    mainPanel(
      plotOutput("countryplot")
    )
  ),
  div(class = "smallspacer"),
  p("Please select the country and the diorders you want to see >_<", class = "tip"),
  div(class = "spacer"),
  h3("What can we see?", class = "smallhead"),
  p("When we choose different countries, we can find that the trend of their prevalence of mental disorders is mostly 
    different, there is an increase or a decrease, but when we choose the prevalence of ANXIETY disorders, we can find 
    that most of the countries have an increasing trend of the prevalence of this disorder from 
    the end of the 19th century to the beginning of the 20th century.", class = "paragraph")
)

time_view <- fluidPage(
  titlePanel("Mean Prevalence of mental disorders and anxiety disorders by Year"),
  mainPanel(
    tabsetPanel(
      tabPanel("Graph for Anxiety Disorders", h3("Mean Prevalence of anxiety disorders"),
               plotOutput("plot2")),
      tabPanel("Graph for Mental Disorders", h3("Mean Prevalence of mental disorders"),
               plotOutput("plot1")),
      tabPanel(
        "Table",
        numericInput(
          inputId = "char_time",
          label = "Please enter a Year between 1990 - 2019",
          value = min(year_df$Year),
          min = min(year_df$Year),
          max = max(year_df$Year)
        ),
        tableOutput((outputId = "table"))
      ),
    )
  ),
  div(class = "spacer"),
  div(class = "spacer"),
  div(class = "spacer"),
  div(class = "spacer"),
  
  h2("Please check the graphs & enter/select a year on the table", class = "tip2"),
  h2("<-- (ON THE LEFT)", class = "tip2"),
  div(class = "bigspacer"),
  
  p("The graph and table above allow us to visualize the average prevalence of mental disorders 
    and anxiety disorders for each year. For mental disorders, the trend has been upward and then 
    downward. Especially since the beginning of the 20th century, there has been a steady decline. 
    This is a good indication that people are beginning to take their mental health more seriously.", 
    class = "paragraph"),
  div(class = "smallspacer"),
  p("However, when we look at the average prevalence of anxiety disorders, we can see that it is
    constantly increasing. This shows that anxiety is an area that we have not been aware of. 
    It can continue to grow even when mental health in general is declining. We need to recognize 
    the importance of anxiety disorders and take timely action. ", class = "paragraph")
)


factor_view<- fluidPage(
  titlePanel("All disorders and symptoms of mental health problem"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "char_factor",
        label = "Select a Year",
        choices = unique(rate_df$Year)
      ),
      verbatimTextOutput("legend_text")
    ),
    mainPanel(
      plotOutput("factorplot", height = "620px", width = "810px")
    )
  ),
  div(class = "spacer"),
  h3("Conclusion:", class = "smallhead"),
  p("When we select different years, we can see that the prevalence of ANXIETY disorder is consistently
    higher than the symptoms of other mental health problems. And when we summarize these almost 30 years 
    of data as a pie chart and observe them together, we can also find that the disorders of anxiety make up a 
    whopping 32.39% of mental health problems. Therefore, we can determine that in the late 19th century and 
    early 20th century, the prevalence of anxiety disorders has been increasing at the same time as it has been the
    main factor and symptom in mental health problems.", class = "paragraph"),
  div(class = "smallspacer"),
  mainPanel(
    div(class = "center-piechart", 
        plotOutput("piechart")
    ),
    div(class = "smallspacer"),
    div(class = "smallspacer"),
    div(
      class = "tips-container",
      h3("Suggestions:", class = "suggestion"),
      div(
        class = "paragraph",
        tags$ul(
          tags$li("Do mindfulness practice such as deep breathing and meditation."),
          tags$li("Exercise is a great method to release stress."),
          tags$li("Maintain a balanced diet and get adequate sleep."),
          tags$li("Seek social support from friends, family, or support groups."),
          tags$li("Consider therapy or counseling to learn coping strategies.")
        )
      )
    )
  )
)
  

ui <- fluidPage(
  tags$style(HTML("
    body {
      background-color: #f9f9f9;
    }
    .navbar {
      background-color: #B0C4DE;
    }
    .navbar-brand {
      color: #222 !important;
      font-family: 'Parisienne', cursive;
      font-weight: bold;
    }
    .heading {
      font-size: 39px;
      font-weight: bold;
      color: #6a8aff;
    }
    .smallhead {
      font-size: 24px;
      font-weight: bold;
      color: #444;
    }
    .paragraph {
      font-size: 22px;
      line-height: 1.5;
      color: #444;
    }
    .smallspacer {
      height: 10px;
    }
    .spacer {
      height: 40px;
    }
    .bigspacer {
      height: 290px;
    }
    .smallhead2 {
      font-size: 22px;
      font-style: italic;
      text-decoration: underline;
      color: #222;
    }
    .name {
      font-size: 19px;
      font-style: italic;
      color: #8a65cc;
      text-align: right;
    }
    .tip {
      font-size: 15px;
      font-style: italic;
      color: #6a8aff;
      text-align: right;
    }
    .tip2 {
      font-size: 22px;
      font-style: italic;
      color: #6a8aff;
      text-align: right;
    }
    .center-piechart {
      display: flex;
      justify-content: center;
      align-items: center;
    }
    .tips-container {
      margin-top: 20px;
    }
    .suggestion {
      font-size: 24px;
      font-style: italic;
      font-weight: bold;
      text-decoration: underline;
      color: #6a8aff;
    }
  ")),
    
  navbarPage(
  "Mental Health - Anxiety",
  tabPanel("Intro", about_view),
  tabPanel("Country", country_view),
  tabPanel("Year", time_view),
  tabPanel("All disorders & Suggestions", factor_view)
))

server <- function(input, output){
  output$countryplot <- renderPlot({
    selected_country <- filter(char_df, Entity == input$char_name)
    
    if (is.null(input$checkboxes) || length(input$checkboxes) == 0) {
      plot(NULL, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", main = "Please select at least one disorders!")
    } else if (length(input$checkboxes) == 2) {
      ggplot(selected_country, aes(x = Year)) +
        geom_line(aes(y = Prevalence...Mental.disorders...Sex..Both...Age..Age.standardized..Percent., color = "Mental Disorders")) +
        geom_line(aes(y = Prevalence...Anxiety.disorders...Sex..Both...Age..Age.standardized..Percent., color = "Anxiety Disorders")) +
        scale_color_manual(values = c("Mental Disorders" = "blue", "Anxiety Disorders" = "red")) +
        theme_bw() +
        xlab("Year") +
        ylab("Prevalence of disorders") +
        ggtitle(paste("Cases over time in", input$char_name))
    } else if ("Mental Disorders" == input$checkboxes) {
      ggplot(selected_country, aes(x = Year, y = Prevalence...Mental.disorders...Sex..Both...Age..Age.standardized..Percent.)) +
        geom_line(color = "blue") +
        theme_bw() +
        xlab("Year") +
        ylab("Prevalence of Mental Disorders") +
        ggtitle(paste("Mental Disorders cases over time in", input$char_name))
    } else if ("Anxiety Disorders" == input$checkboxes) {
      ggplot(selected_country, aes(x = Year, y = Prevalence...Anxiety.disorders...Sex..Both...Age..Age.standardized..Percent.)) +
        geom_line(color = "red") +
        theme_bw() +
        xlab("Year") +
        ylab("Prevalence of Anxiety Disorders") +
        ggtitle(paste("Anxiety Disorders cases over time in", input$char_name))
    } else {
      ggplot() + theme_void()
    }
  })
  
  filtered_data <- reactive({
    filter(year_df, Year == input$char_time)
  })
  output$table <- renderTable({
    filtered_data()
  })
  
  output$plot1 <- renderPlot({
    ggplot(year_df, aes(x = Year, y = avg_mental_disorders)) +
      geom_line() +
      labs( x = "Year", y = "Prevalence")
  })
  
  output$plot2 <- renderPlot({
    ggplot(year_df, aes(x = Year, y = avg_anxiety_disorders)) +
      geom_line() +
      labs( x = "Year", y = "Prevalence")
  })
  
  output$factorplot <- renderPlot({
    filtered_data <- subset(rate_df, Year == input$char_factor)
    bar_data <- t(filtered_data[, -1])
    bar_names <- rownames(bar_data)
    bar_values <- as.numeric(bar_data)
    sorted_indices <- order(bar_values)
    sorted_names <- bar_names[sorted_indices]
    sorted_values <- bar_values[sorted_indices]
    colors <- ifelse(sorted_names == "anxiety", "#FF6666", "grey")
    barplot(sorted_values, col = colors, names.arg = sorted_names,
            xlab = "", ylab = "",
            main = paste("Percentage of different disorders for Year", input$char_factor),
            horiz = TRUE
    )
  })
  
  output$legend_text <- renderText({
    filtered_data <- subset(rate_df, Year == input$char_factor)
    bar_data <- t(filtered_data[, -1])
    bar_names <- rownames(bar_data)
    bar_values <- as.numeric(bar_data)
    bar_values_rounded <- round(bar_values, 2)
    legend_text <- paste(bar_names, ": ", bar_values_rounded, "%", collapse = "\n")
    legend_text
  })
  
  output$piechart <- renderPlot({
    mean_scores <- unlist(sum_df)
    colors <- ifelse(names(sum_df) == "Anxiety", "#FF6666", gray(seq(0.1, 0.9, length.out = length(mean_scores) - 1)))
    
    # Create the pie chart
    pie(mean_scores, labels = names(sum_df), col = colors, main = "All symptoms")
    
    # Add a legend to the pie chart
    legend("topright", legend = percent(mean_scores / sum(mean_scores)), fill = colors)
  })
}


shinyApp(ui = ui, server = server)
