ui <- fluidPage(
  theme = shinytheme("lumen"),
  titlePanel("Stroke Prediction"),
  sidebarLayout(
    mainPanel(
      tabsetPanel(id = "main_tab",
                  type = "tabs",
                  tabPanel("Data Exploration", plotOutput("exploration_plot", width = "100%")),
                  tabPanel("Regression", list(tableOutput("regression_table"),
                                              uiOutput("dynamic_inputs_regression")
                                              # fluidRow(column(6, selectInput(inputId = "test_input1",
                                              #                                label = strong("Test"),
                                              #                                choices = c("Histogram", "Feature correlation", "Clustering"),
                                              #                                multiple = FALSE)),
                                              #          column(6, selectInput(inputId = "test_input2",
                                              #                                label = strong("Test"),
                                              #                                choices = c("Histogram", "Feature correlation", "Clustering"),
                                              #                                multiple = FALSE)))
                                              )),
                  tabPanel("Decision Tree", plotOutput("decision_tree_plot", width = "100%")),
                  
      )
    ),
    sidebarPanel(
      conditionalPanel(condition = "input.main_tab == 'Data Exploration'",
                       selectInput(inputId = "exploration_graph",
                                   label = strong("Graph"),
                                   choices = c("Histogram", "Feature correlation", "Clustering"),
                                   multiple = FALSE)),
      conditionalPanel(condition = "input.main_tab == 'Data Exploration' & input.exploration_graph == 'Histogram'",
                       selectInput(inputId = "hist_feature",
                                   label = strong("Feature"),
                                   choices = list_features_formatted,
                                   multiple = FALSE)),
      # Can't have the two features be the same!
      conditionalPanel(condition = "input.main_tab == 'Data Exploration' & input.exploration_graph == 'Feature correlation'",
                       selectInput(inputId = "corr_feature_1",
                                   label = strong("Feature 1"),
                                   choices = c(list_features_formatted[1], list_features_formatted[3:12]),
                                   selected = list_features_formatted[1],
                                   multiple = FALSE)),
      conditionalPanel(condition = "input.main_tab == 'Data Exploration' & input.exploration_graph == 'Feature correlation'",
                       selectInput(inputId = "corr_feature_2",
                                   label = strong("Feature 2"),
                                   choices = list_features_formatted[2:12],
                                   selected = list_features_formatted[2],
                                   multiple = FALSE)),
      conditionalPanel(condition = "input.main_tab == 'Data Exploration' & input.exploration_graph == 'Clustering'",
                       selectInput(inputId = "cluster_feature",
                                   label = strong("Feature"),
                                   choices = list_features_formatted,
                                   multiple = FALSE)),
      conditionalPanel(condition = "input.main_tab == 'Regression'",
                       selectInput(inputId = "regression_features",
                                   label = strong("Features"),
                                   choices = list_features_formatted,
                                   selected = list_features_formatted,
                                   multiple = TRUE)),
      
    )
  )
)