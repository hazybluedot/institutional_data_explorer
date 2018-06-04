library(shiny)
library(Hmisc)

collapse_grade_success <- function(grade) {
  fct_collapse(collapse_letter_grade(grade),
               "Success" = c("A", "B", "C"),
               "Fail" = c("D", "F"),
               "Transfer" = "T",
               "Withdraw" = "W")
}

collapse_predictor_grade <- function(grade) {
  
}

fields_summary.lrm <- function(fit, stub) {
  tbl <- summary(fit)[, c("Effect", "S.E.", "Lower 0.95", "Upper 0.95")]
  #ci <- summary(reducedfit_lrm)[ , c("Lower 0.95", "Upper 0.95")]
  bi <- seq(1,nrow(tbl),2)
  ori <- seq(2,nrow(tbl),2)
  factors <- rownames(tbl[bi,])
  btbl <- tbl[bi,c(1,2)]
  if (!is.null(dim(btbl)))
    colnames(btbl) <- c("Coef.", "S.E")
  ortbl <- tbl[ori,-2]
  if (!is.null(dim(ortbl)))
    colnames(ortbl) <- c("Odds ratio", "Lower 0.95", "Upper 0.95")
  rownames(ortbl) <- factors
  #xtable(cbind(btbl, ortbl),
  #       label=paste0("tab:", stub),
  #       file=paste0(stub,".tex"))
  tbl <- cbind(btbl, ortbl)
  rownames(tbl) <- factors
  tbl
}

boxplot.lrm <- function(model) {
  tbl <- summary(model)[, c("Effect", "S.E.", "Lower 0.95", "Upper 0.95")]
  bi <- seq(1,nrow(tbl),2)
  ori <- seq(2,nrow(tbl),2)
  boxLabels <- rownames(tbl[bi,])
  ortbl <- tbl[ori,c("Effect", "Lower 0.95", "Upper 0.95")]
  if (!is.null(dim(ortbl))) {
    colnames(ortbl) <- c("boxOdds", "boxCILow", "boxCIHigh")
  }
  df <- as_tibble(ortbl) %>% mutate(yAxis = length(boxLabels):1)
  p <- ggplot(df, aes(x = boxOdds, y = yAxis))
  p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
    geom_point(size = 3.5, color = "orange") +
    theme_bw() +
    theme(panel.grid.minor = element_blank()) +
    scale_y_continuous(breaks = df$yAxis, labels = boxLabels) +
    scale_x_continuous(breaks = seq(0,7,1) ) +
    coord_trans(x = "log10") +
    ylab("") +
    xlab("Odds ratio (log scale)") #+
  #annotate(geom = "text", y =1.1, x = 3.5, 
  #         label = paste0("Pseudo R^2 = ", round(model$stats[["R2"]],2)), 
  #         size = 3.5, hjust = 0)
  p
}

successAnalysisUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    p('Performing logistic regression using forumla: '),
    verbatimTextOutput(ns("formula")),
  tabsetPanel(
    tabPanel("Summary", tagList(
      #tableOutput(ns("CrossTable")),
      verbatimTextOutput(ns("summary")),
      tableOutput(ns("summaryTable")))),
    tabPanel("Relative Importance", plotOutput(ns("anova"))),
    tabPanel("Partial Effects", plotOutput(ns("parteffect"))),
    tabPanel("Summary Plot", plotOutput(ns("summaryPlot"))),
    tabPanel("Nomogram", plotOutput(ns("nomogram")))
  )
  )
}

successAnalysis <- function(input, output, session, course_data, profile_course, grouping_var, predictor_courses) {
  vals <- reactiveValues(logit_formula = "", logit_fit = NA, data = NA)
  
  observeEvent({
    if (isTruthy(profile_course()) & isTruthy(course_data()) & length(predictor_courses()) > 0) TRUE
    else return()
  },{
    profileIDS <- distinct(profile_course(), IDS)$IDS
    predictor_course_table <- course_data() %>% filter(when == "before",
                                                     IDS %in% profileIDS,
                                                     Grade_Course %in% predictor_courses(),
                                                     !is.null(Grade_Final_Grade), 
                                                     Grade_Final_Grade %in% valid_grades) %>%
      group_by(IDS, Grade_Course) %>%
      dplyr::summarize(Grade_Final_Grade = dplyr::first(Grade_Final_Grade)) %>%
      mutate(GPA = convert_grades_numeric(Grade_Final_Grade),
             Withdraw = (Grade_Final_Grade == "W"),
             Transfer = (Grade_Final_Grade == "T")) %>%
      select(IDS, Grade_Course, GPA, Withdraw, Transfer) %>%
      gather(key, value, -IDS, -Grade_Course) %>%
      unite(variable, Grade_Course, key) %>%
      spread(variable, value) %>%
      mutate_at(vars(c(ends_with("_Transfer"), ends_with("_Withdraw"))),
                as.logical)
    
    predictor_vars <- names(predictor_course_table)[-1]
    predictor_vars <- predictor_vars[!grepl("_Transfer$", predictor_vars)]
    
    vals$data <- first_instance(profile_course()) %>% 
      select(IDS, Grade = First_Grade) %>% 
      mutate(status = collapse_grade_success(Grade),
             Success = (status == "Success")) %>%
      left_join(predictor_course_table, by = "IDS") %>% na.omit() %>% droplevels()
    
    vals$logit_formula <- paste0("Success ~ ", paste(predictor_vars, collapse = " + "))
    #vals$logit_fit <- rms::lrm(as.formula(vals$logit_formula), data = na.omit(vals$data))
    dd = rms::datadist(vals$data); options(datadist = "dd")
    vals$logit_fit <- lrm(as.formula(vals$logit_formula), data = vals$data)
  })
  
  output$formula <- renderPrint(vals$logit_formula)
  
  output$summary <- renderPrint({
      #rms::fastbw(vals$logit_fit)
      vals$logit_fit
  })

  output$anova <- renderPlot({
    #plot(anova(vals$logit_fit))
    tbl <- fields_summary.lrm(vals$logit_fit, "")
    #boxplot.lrm(vals$logit_fit)
    boxLabels <- rownames(tbl)
    d <- as.tibble(tbl) %>% 
      rename(boxOdds = `Odds ratio`, boxCILow = `Lower 0.95`, boxCIHigh = `Upper 0.95`) %>%
      mutate(yAxis = length(boxLabels):1)
    p <- ggplot(d, aes(x = boxOdds, y = yAxis))
    p + geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
      geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = .5, height = .2, color = "gray50") +
      geom_point(size = 3.5, color = "orange") +
      theme_bw() +
      theme(panel.grid.minor = element_blank()) +
      scale_y_continuous(breaks = d$yAxis, labels = boxLabels) +
      scale_x_continuous(breaks = seq(0,7,1) ) +
      coord_trans(x = "log10") +
      ylab("") +
      xlab("Odds ratio (log scale)") +
      annotate(geom = "text", y =1.1, x = 3.5, label =paste0("Pseudo R^2 = ",round(mlfit$stats[["R2"]], 2)), size = 3.5, hjust = 0) #+ 
      #ggtitle("Odds of leaving ME for another Major")
  })  
  
  output$nomogram <- renderPlot({
      nom <- rms::nomogram(vals$logit_fit,
                      fun = plogis, funlabel = "Probability",
                      fun.at = c(.01, .05, .1, .15, .20, .25,.5,.75,.9,.95,.99))
  plot(nom, xfrac = .45) 
  })
  output$parteffect <- renderPlot({
    f <- vals$logit_fit
    f <- update(f, x = TRUE, y = TRUE)
    ggplot(rms::Predict(f), sepdiscrete='vertical', vnames='names',
           rdata = vals$data,
           histSpike.opts = list(frac=function(f) .1*f/max(f) )) 
  })
  
  output$summaryPlot <- renderPlot({
    f <- vals$logit_fit
    f <- update(f, x = TRUE, y = TRUE)
    plot(summary(f), log = TRUE)
  })
  
  output$CrossTable <- renderTable({
    #shiny::validate(need(vals$data, 'need valid data'))
    #head(na.omit(vals$data))
    factor_vars <- vals$data %>%
      select(-Grade, -status, -Success) %>%
      select_if(~ is.factor(.) | is.logical(.)) %>%
      names()
    message('Found factor or logical variables: ', paste(factor_vars, collapse = ", "))
    f <- as.formula(paste0("~ Success + ", paste(factor_vars, collapse = " + ")))
    message('Cross table with formula ', f)
    xtabs(f, data = na.omit(vals$data))
  #fields_summary.lrm(vals$logit_fit, "")
    #summary(vals$logit_fit)
  })
  
  output$summaryTable <- renderTable({
    fields_summary.lrm(vals$logit_fit, "")
  })
}