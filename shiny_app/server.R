server <- function(input, output, session) {
  
  observeEvent(input$corr_feature_2, {
    updateSelectInput(session, "corr_feature_1",
                      choices = setdiff(list_features_formatted, input$corr_feature_2),
                      selected = isolate(input$corr_feature_1))
  })
  
  observeEvent(input$corr_feature_1, {
    updateSelectInput(session, "corr_feature_2",
                      choices = setdiff(list_features_formatted, input$corr_feature_1),
                      selected = isolate(input$corr_feature_2))
  })

  output$exploration_plot <- renderPlot({
    if (input$exploration_graph == "Histogram") {
      index_feature <- which(list_features_formatted == input$hist_feature)
      if (list_features[index_feature] %in% c("age", "avg_glucose_level", "bmi")) {
        ggplot(stroke_data, aes_string(x=list_features[index_feature])) + 
          geom_histogram(binwidth=1)
      } else {
        ggplot(stroke_data, aes_string(x=list_features[index_feature])) + 
          geom_bar()
      }
      
    } else if (input$exploration_graph == "Feature correlation") {
      feature_1 <- list_features[which(list_features_formatted == input$corr_feature_1)]
      feature_2 <- list_features[which(list_features_formatted == input$corr_feature_2)]
      
      if ((feature_1 %in% list_features_continuous) & (feature_2 %in% list_features_continuous)) {
        ggplot(stroke_data, aes_string(x=feature_1, y=feature_2)) +
          geom_point() + geom_smooth(method=lm)
      } else if ((feature_1 %in% list_features_continuous) & (feature_2 %in% list_features_categorical)) {
        ggplot(stroke_data, aes_string(x=feature_2, y=feature_1)) +
          geom_boxplot()
      } else if ((feature_1 %in% list_features_categorical) & (feature_2 %in% list_features_continuous)) {
        ggplot(stroke_data, aes_string(x=feature_1, y=feature_2)) +
          geom_boxplot()
      } else if ((feature_1 %in% list_features_categorical) & (feature_2 %in% list_features_categorical)) {
        # Fix this as percentage of feature 1
        counts_data <- as.data.frame(table(stroke_data[,c(feature_1, feature_2)]))
        colnames(counts_data) <- c(feature_1, feature_2, "count")
        ggplot(counts_data, aes_string(x=feature_1, y=feature_2, fill="count")) +
          geom_tile() + geom_text(aes(label = count), color = "white", size = 4) +
          coord_fixed()
      }
      
    } else if (input$exploration_graph == "Clustering") {
      # Need to add the other categorical features
      cluster_feature <- list_features[which(list_features_formatted == input$cluster_feature)]
      df_pca_results <- as.data.frame(prcomp(na.omit(stroke_data[,c("age", "avg_glucose_level", "bmi", "hypertension", "heart_disease", "stroke")]), scale=TRUE)$x)
      df_pca_results[, cluster_feature] <- na.omit(stroke_data)[, cluster_feature]
      ggplot(df_pca_results, aes_string(x="PC1", y="PC2", color=cluster_feature)) + geom_point()
      # add clusters?
    }
  })
  
  output$regression_table <- function() {
    glm.fit <- glm(stroke ~ age + hypertension + heart_disease + bmi, data = stroke_data, family = binomial)
    output_regression <- data.frame(coef(summary(glm.fit))[2:5,c(1, 4)])
    # formattable(output_regression, list(
    #   Estimate = color_tile("lightpink", "lightblue")))
    output_regression %>%
      kbl() %>%
      column_spec(c(2, 3), color = "white",
                  background = spec_color(output_regression$Estimate, option = "A"))
  }

  output$dynamic_inputs_regression <- renderUI({
    row_idx <- length(input$regression_features) %>% seq_len
    row_idx <- row_idx[row_idx %% 2 == 1]
    row_idx %>%
      map( ~ {
        if (!is.na(input$regression_features[.x + 1])) {
          fluidRow(column(
            width = 6,
            if 
            selectInput(
              inputId = paste0("var", .x),
              label = input$regression_features[.x],
              choices = c("this", "that"),
              multiple = FALSE)
          ),
          column(
            width = 6,
            selectInput(
              inputId = paste0("var", .x + 1),
              label = input$regression_features[.x + 1],
              choices = c("this", "that"),
              multiple = FALSE)
          ))
        } else {
          fluidRow(column(
            width = 6,
            selectInput(
              inputId = paste0("var", .x),
              label = input$regression_features[.x],
              choices = c("this", "that"),
              multiple = FALSE)
          ))
        }
      })
  })


  #   renderTable({
  #   glm.fit <- glm(stroke ~ age + hypertension + heart_disease + bmi, data = stroke_data, family = binomial)
  #   coef(summary(glm.fit))[,c(1, 4)]
  # }, rownames = TRUE, colnames = TRUE)
  
  
  output$decision_tree_plot <- renderPlot({
    fit <- rpart(stroke~age+heart_disease, data = na.omit(stroke_data), method = 'class')
    rpart.plot(fit, extra = 101)
  })
  
}