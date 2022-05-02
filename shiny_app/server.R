# Custom input UI for variable number of features:
custom_input_prediction <- function(feature, feature_formatted, prefix) {
  if (feature %in% list_features_categorical) {
    selectInput(
      inputId = paste0(prefix, feature),
      label = strong(feature_formatted),
      choices = unique(stroke_data[,paste0(feature, "_reformat")]),
      selected = unique(stroke_data[1,paste0(feature, "_reformat")]),
      multiple = FALSE)
  } else if (feature == "age") {
    sliderInput(
      inputId = paste0(prefix, feature),
      label = strong(feature_formatted),
      min = floor(min(stroke_data[,feature])),
      max = 80,
      step = 1,
      value = median(stroke_data[,feature]))
  } else if (feature == "bmi") {
    sliderInput(
      inputId = paste0(prefix, feature),
      label = strong(feature_formatted),
      min = floor(min(stroke_data[,feature])),
      max = ceiling(max(stroke_data[,feature])),
      step = 0.1,
      value = median(stroke_data[,feature])
    )
  } else if (feature == "avg_glucose_level") {
    sliderInput(
      inputId = paste0(prefix, feature),
      label = strong(feature_formatted),
      min = floor(min(stroke_data[,feature])),
      max = ceiling(max(stroke_data[,feature])),
      step = 0.01,
      value = median(stroke_data[,feature])
    )
  }
}


server <- function(input, output, session) {
  
  # Data exploration:
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
      feature <- list_features[which(list_features_formatted == input$hist_feature)]
      if (feature %in% list_features_continuous) {
        if (feature == "avg_glucose_level") {
          binwidth <- 3
        } else {
          binwidth <- 1
        }
        ggplot(stroke_data, aes_string(x=feature)) + 
          geom_histogram(color="black", fill=brewer.pal(8, "Set2")[1], binwidth=binwidth) + theme_minimal() + 
          theme(legend.position="none") + ylab("Count") + xlab(input$hist_feature)
      } else {
        ggplot(stroke_data, aes_string(x=paste0(feature, "_reformat"), fill=paste0(feature, "_reformat"))) + 
          geom_bar(color="black") + scale_fill_brewer(palette = "Set2") + theme_minimal() + 
          theme(legend.position="none") + ylab("Count") + xlab(input$hist_feature)
      }
      
    } else if (input$exploration_graph == "Feature correlation") {
      feature_1 <- list_features[which(list_features_formatted == input$corr_feature_1)]
      feature_2 <- list_features[which(list_features_formatted == input$corr_feature_2)]
      
      if ((feature_1 %in% list_features_continuous) & (feature_2 %in% list_features_continuous)) {
        ggplot(stroke_data, aes_string(x=feature_1, y=feature_2)) +
          geom_point() + geom_smooth(method=lm) + theme_minimal()
      } else if ((feature_1 %in% list_features_continuous) & (feature_2 %in% list_features_categorical)) {
        feature_2_reformat = paste0(feature_2, "_reformat")
        ggplot(stroke_data, aes_string(x=feature_2_reformat, y=feature_1, fill=feature_2_reformat)) +
          geom_violin(width=1) + geom_boxplot(width=0.1, color="black") + scale_fill_brewer(palette = "Set2") + theme_minimal() + theme(legend.position="none")
      } else if ((feature_1 %in% list_features_categorical) & (feature_2 %in% list_features_continuous)) {
        feature_1_reformat = paste0(feature_1, "_reformat")
        ggplot(stroke_data, aes_string(x=feature_1_reformat, y=feature_2, fill=feature_1_reformat)) +
          geom_violin(width=1) + geom_boxplot(width=0.1, color="black") + scale_fill_brewer(palette = "Set2") + theme_minimal() + theme(legend.position="none")
      } else if ((feature_1 %in% list_features_categorical) & (feature_2 %in% list_features_categorical)) {
        # Fix this as percentage of feature 1
        feature_1_reformat = paste0(feature_1, "_reformat")
        feature_2_reformat = paste0(feature_2, "_reformat")
        counts_data <- as.data.frame(table(stroke_data[,c(feature_1_reformat, feature_2_reformat)]))
        colnames(counts_data) <- c(feature_1_reformat, feature_2_reformat, "count")
        counts_data$percent_feature1 <- apply(counts_data, 1,
                                              function(x) {as.integer(x["count"]) / 
                                                  sum(counts_data[counts_data[, feature_1_reformat] == x[feature_1_reformat],]$count)}) * 100
        counts_data$counts_text <- paste0(as.character(round(counts_data$percent_feature1, digits=2)),
                                          "% (N=", counts_data$count, ")")
        ggplot(counts_data, aes_string(x=feature_1_reformat, y=feature_2_reformat, fill="percent_feature1")) +
          geom_tile() + geom_text(aes(label = counts_text), color = "white", size = 4) +
          coord_fixed() + theme_minimal() + scale_fill_gradient(limits = c(0,100))
      }
      
    } else if (input$exploration_graph == "Clustering") {
      # Need to add the other categorical features
      feature <- list_features[which(list_features_formatted == input$cluster_feature)]
      cluster_features <- colnames(stroke_data)[!endsWith(colnames(stroke_data), "_reformat")]
      df_pca_results <- as.data.frame(prcomp(stroke_data[,cluster_features], scale=TRUE)$x)
      if (feature %in% list_features_categorical) {
        feature_reformat <- paste0(feature, "_reformat")
      } else {
        feature_reformat <- feature
      }
      df_pca_results[, feature_reformat] <- stroke_data[, feature_reformat]
      ggplot(df_pca_results, aes_string(x="PC1", y="PC2", color=feature_reformat)) + geom_point() + theme_minimal()
      # add clusters?
    }
  })
  
  # Logistic regression
  regression_model <- eventReactive(input$regression_features, {
    list_regression_features <- list_features[which(list_features_formatted %in% input$regression_features)]
    if ("work_type" %in% list_regression_features) {
      list_regression_features <- append(list_regression_features, c("work_type_Govt_job", "work_type_Never_worked",
                                                                     "work_type_Private",  "work_type_Self_employed", "work_type_children"))
      list_regression_features <- list_regression_features[which(list_regression_features != "work_type")]
    }
    # list_regression_features_all <- colnames(stroke_data_oversampled)[which(starts_with(colnames(stroke_data_oversampled)))]
    # print(paste(list_regression_features, collapse = "+"))
    glm.fit <- glm(paste0("stroke ~ ", paste(list_regression_features, collapse = "+")), data = stroke_data_oversampled, family = binomial)
  })
  
  output$regression_table <- renderDataTable({
    # glm.fit <- glm(stroke ~ age + hypertension + heart_disease + bmi, data = stroke_data, family = binomial)
    glm.fit <- regression_model()
    # print(glm.fit)
    output_regression <- data.frame(coef(summary(glm.fit))[,c(1, 4)])
    print(output_regression)
    output_regression
    # formattable(output_regression, list(
    #   Estimate = color_tile("lightpink", "lightblue")))
    # output_regression %>%
    #   kbl() %>%
    #   column_spec(c(2, 3), color = "white",
    #               background = spec_color(output_regression$Estimate, option = "A"))
  }, options = list(paging = FALSE, searching = FALSE, info = FALSE))
  
  
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
            custom_input_prediction(feature_1, input$regression_features[.x], "regression_")
          ),
          column(
            width = 6,
            custom_input_prediction(feature_2, input$regression_features[.x + 1], "regression_")
          ))
        } else {
          feature_1 <- list_features[which(list_features_formatted == input$regression_features[.x])]
          fluidRow(column(
            width = 6,
            custom_input_prediction(feature_1, input$regression_features[.x], "regression_")
          ))
        }
      })
  })

  regression_model_prediction <- eventReactive(input$predict_button_regression, {
    list_regression_features <- list_features[which(list_features_formatted %in% input$regression_features)]
    glm.fit <- regression_model()
    prediction_input <- data.frame("gender" = if (input$regression_gender == "Male") 0 else 1,
                                   "age" = input$regression_age,
                                   "hypertension" = if (input$regression_hypertension == "No") 0 else 1,
                                   "heart_disease" = if (input$regression_heart_disease == "No") 0 else 1,
                                   "ever_married" = if (input$regression_ever_married == "No") 0 else 1,
                                   "Residence_type" = if (input$regression_Residence_type == "Urban") 0 else 1,
                                   "avg_glucose_level" = input$regression_avg_glucose_level,
                                   "bmi" = input$regression_bmi,
                                   "smoking_status" = (if (input$regression_smoking_status == "Never smoker") 0 
                                                       else if (input$regression_smoking_status == "Former smoker") 1
                                                       else 2)
                                   )
    # list_regression_features <- list_regression_features[which(list_regression_features != "work_type")]
    prediction_input <- prediction_input[,list_regression_features[which(list_regression_features != "work_type")]]
    if ("work_type" %in% list_regression_features) {
      prediction_input["work_type_Govt_job"] <- if (input$regression_work_type == "Government job") 1 else 0
      prediction_input["work_type_Never_worked"] <- if (input$regression_work_type == "Never worked") 1 else 0
      prediction_input["work_type_Self_employed"] <- if (input$regression_work_type == "Self-employed") 1 else 0
      prediction_input["work_type_Private"] <- if (input$regression_work_type == "Private") 1 else 0
      prediction_input["work_type_children"] <- if (input$regression_work_type == "Child") 1 else 0
    }
    prediction <- predict(glm.fit, prediction_input, type = "response")
    print(prediction)
    prediction
  })
   
  output$regression_prediction_txt <- renderText({
    regression_model_prediction()
  })

  #   renderTable({
  #   glm.fit <- glm(stroke ~ age + hypertension + heart_disease + bmi, data = stroke_data, family = binomial)
  #   coef(summary(glm.fit))[,c(1, 4)]
  # }, rownames = TRUE, colnames = TRUE)
  
  # Decision Tree:
  
  tree_model <- eventReactive(c(input$tree_features, input$max_depth), {
    list_tree_features <- list_features[which(list_features_formatted %in% input$tree_features)]
    if ("work_type" %in% list_tree_features) {
      list_regression_features <- append(list_tree_features, c("work_type_Govt_job", "work_type_Never_worked",
                                                                     "work_type_Private",  "work_type_Self_employed", "work_type_children"))
      list_tree_features <- list_tree_features[which(list_tree_features != "work_type")]
    }
    tree.fit <- rpart(paste0("stroke ~ ", paste(list_tree_features, collapse = "+")), 
          data = stroke_data_oversampled, method = "class", maxdepth = input$max_depth, cp = 0.0001)
  })
  
  output$decision_tree_plot <- renderPlot({
    tree.fit <- tree_model()
    rpart.plot(tree.fit, extra = 101)
  })
  
  output$dynamic_inputs_tree <- renderUI({
    row_idx <- length(input$tree_features) %>% seq_len
    row_idx <- row_idx[row_idx %% 2 == 1]
    row_idx %>%
      map( ~ {
        if (!is.na(input$tree_features[.x + 1])) {
          feature_1 <- list_features[which(list_features_formatted == input$tree_features[.x])]
          feature_2 <- list_features[which(list_features_formatted == input$tree_features[.x + 1])]
          fluidRow(column(
            width = 6,
            custom_input_prediction(feature_1, input$tree_features[.x], "tree_")
          ),
          column(
            width = 6,
            custom_input_prediction(feature_2, input$tree_features[.x + 1], "tree_")
          ))
        } else {
          feature_1 <- list_features[which(list_features_formatted == input$tree_features[.x])]
          fluidRow(column(
            width = 6,
            custom_input_prediction(feature_1, input$tree_features[.x], "tree_")
          ))
        }
      })
  })
  
  tree_model_prediction <- eventReactive(input$predict_button_tree, {
    list_tree_features <- list_features[which(list_features_formatted %in% input$tree_features)]
    tree.fit <- tree_model()
    prediction_input <- data.frame("gender" = if (input$tree_gender == "Male") 0 else 1,
                                   "age" = input$tree_age,
                                   "hypertension" = if (input$tree_hypertension == "No") 0 else 1,
                                   "heart_disease" = if (input$tree_heart_disease == "No") 0 else 1,
                                   "ever_married" = if (input$tree_ever_married == "No") 0 else 1,
                                   "Residence_type" = if (input$tree_Residence_type == "Urban") 0 else 1,
                                   "avg_glucose_level" = input$tree_avg_glucose_level,
                                   "bmi" = input$tree_bmi,
                                   "smoking_status" = (if (input$tree_smoking_status == "Never smoker") 0 
                                                       else if (input$tree_smoking_status == "Former smoker") 1
                                                       else 2)
    )
    # list_regression_features <- list_regression_features[which(list_regression_features != "work_type")]
    prediction_input <- prediction_input[,list_tree_features[which(list_tree_features != "work_type")]]
    if ("work_type" %in% list_tree_features) {
      prediction_input["work_type_Govt_job"] <- if (input$tree_work_type == "Government job") 1 else 0
      prediction_input["work_type_Never_worked"] <- if (input$tree_work_type == "Never worked") 1 else 0
      prediction_input["work_type_Self_employed"] <- if (input$tree_work_type == "Self-employed") 1 else 0
      prediction_input["work_type_Private"] <- if (input$tree_work_type == "Private") 1 else 0
      prediction_input["work_type_children"] <- if (input$tree_work_type == "Child") 1 else 0
    }
    prediction <- predict(tree.fit, prediction_input, type = "prob")
    print(prediction)
    prediction
  })
  
  output$tree_prediction_txt <- renderText({
    tree_model_prediction()
  })
}