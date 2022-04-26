custom_input_prediction <- function(feature, feature_formatted) {
  if (feature %in% list_features_categorical) {
    selectInput(
      inputId = paste0("regression_", feature),
      label = strong(feature_formatted),
      choices = unique(stroke_data_formatted[,feature]),
      selected = unique(stroke_data_formatted[1,feature]),
      multiple = FALSE)
  } else if (feature == "age") {
    print(feature)
    print(floor(min(stroke_data[,feature])))
    print(ceiling(max(stroke_data[,feature])))
    print(median(stroke_data[,feature]))
    sliderInput(
      inputId = paste0("regression_", feature),
      label = strong(feature_formatted),
      min = floor(min(stroke_data_formatted[,feature])),
      max = 80,
      step = 1,
      value = median(stroke_data_formatted[,feature]))
  } else if (feature == "bmi") {
    print(feature)
    print(floor(min(stroke_data[,feature])))
    print(ceiling(max(stroke_data[,feature])))
    # use values for now, cause NA in data
    # CHANGE THIS LATER!!
    sliderInput(
      inputId = paste0("regression_", feature),
      label = strong(feature_formatted),
      min = 0,
      max = 100,
      step = 0.1,
      value = 50
    )
  } else if (feature == "avg_glucose_level") {
    print(feature)
    print(floor(min(stroke_data[,feature])))
    print(ceiling(max(stroke_data[,feature])))
    print(median(stroke_data[,feature]))
    sliderInput(
      inputId = paste0("regression_", feature),
      label = strong(feature_formatted),
      min = floor(min(stroke_data_formatted[,feature])),
      max = ceiling(max(stroke_data_formatted[,feature])),
      step = 0.01,
      value = median(stroke_data_formatted[,feature])
    )
  }
}


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
      if (list_features[index_feature] %in% list_features_continuous) {
        if (list_features[index_feature] == "avg_glucose_level") {
          binwidth <- 3
        } else {
          binwidth <- 1
        }
        ggplot(stroke_data_formatted, aes_string(x=list_features[index_feature])) + 
          geom_histogram(color="black", fill=brewer.pal(8, "Set2")[1], binwidth=binwidth) + theme_minimal() + 
          theme(legend.position="none") + ylab("Count") + xlab(input$hist_feature)
      } else {
        ggplot(stroke_data_formatted, aes_string(x=list_features[index_feature], fill=list_features[index_feature])) + 
          geom_bar(color="black") + scale_fill_brewer(palette = "Set2") + theme_minimal() + 
          theme(legend.position="none") + ylab("Count") + xlab(input$hist_feature)
      }
      
    } else if (input$exploration_graph == "Feature correlation") {
      feature_1 <- list_features[which(list_features_formatted == input$corr_feature_1)]
      feature_2 <- list_features[which(list_features_formatted == input$corr_feature_2)]
      
      if ((feature_1 %in% list_features_continuous) & (feature_2 %in% list_features_continuous)) {
        ggplot(stroke_data_formatted, aes_string(x=feature_1, y=feature_2)) +
          geom_point() + geom_smooth(method=lm) + theme_minimal()
      } else if ((feature_1 %in% list_features_continuous) & (feature_2 %in% list_features_categorical)) {
        ggplot(stroke_data_formatted, aes_string(x=feature_2, y=feature_1, fill=feature_2)) +
          geom_violin(width=1) + geom_boxplot(width=0.1, color="black") + scale_fill_brewer(palette = "Set2") + theme_minimal() + theme(legend.position="none")
      } else if ((feature_1 %in% list_features_categorical) & (feature_2 %in% list_features_continuous)) {
        ggplot(stroke_data_formatted, aes_string(x=feature_1, y=feature_2, fill=feature_1)) +
          geom_violin(width=1) + geom_boxplot(width=0.1, color="black") + scale_fill_brewer(palette = "Set2") + theme_minimal() + theme(legend.position="none")
      } else if ((feature_1 %in% list_features_categorical) & (feature_2 %in% list_features_categorical)) {
        # Fix this as percentage of feature 1
        counts_data <- as.data.frame(table(stroke_data_formatted[,c(feature_1, feature_2)]))
        colnames(counts_data) <- c(feature_1, feature_2, "count")
        counts_data$percent_feature1 <- apply(counts_data, 1,
                                              function(x) {as.integer(x["count"]) / 
                                                  sum(counts_data[counts_data[, feature_1] == x[feature_1],]$count)}) * 100
        counts_data$counts_text <- paste0(as.character(round(counts_data$percent_feature1, digits=2)),
                                          "% (N=", counts_data$count, ")")
        ggplot(counts_data, aes_string(x=feature_1, y=feature_2, fill="percent_feature1")) +
          geom_tile() + geom_text(aes(label = counts_text), color = "white", size = 4) +
          coord_fixed() + theme_minimal()
      }
      
    } else if (input$exploration_graph == "Clustering") {
      # Need to add the other categorical features
      cluster_feature <- list_features[which(list_features_formatted == input$cluster_feature)]
      df_pca_results <- as.data.frame(prcomp(na.omit(stroke_data_formatted[,c("age", "avg_glucose_level", "bmi", "hypertension", "heart_disease", "stroke")]), scale=TRUE)$x)
      df_pca_results[, cluster_feature] <- na.omit(stroke_data_formatted)[, cluster_feature]
      ggplot(df_pca_results, aes_string(x="PC1", y="PC2", color=cluster_feature)) + geom_point() + theme_minimal()
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
          feature_1 <- list_features[which(list_features_formatted == input$regression_features[.x])]
          feature_2 <- list_features[which(list_features_formatted == input$regression_features[.x + 1])]
          fluidRow(column(
            width = 6,
            custom_input_prediction(feature_1, input$regression_features[.x])
          ),
          column(
            width = 6,
            custom_input_prediction(feature_2, input$regression_features[.x + 1])
          ))
        } else {
          feature_1 <- list_features[which(list_features_formatted == input$regression_features[.x])]
          fluidRow(column(
            width = 6,
            custom_input_prediction(feature_1, input$regression_features[.x])
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