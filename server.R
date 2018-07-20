# Medicine Quality Modelling App
# MORU, MAEMOD, Olivier Celhay
# server.R  server logic
# -----------------------------------------------------------------------------

shinyServer(function(input, output, session) {
  # Display parameters values
  output$parameters_values <-
    renderText(paste0(
      em(
        "Placeholder for some info on the values of the parameters common
        to both scenarios: population, proportion of non-immune cases that become
        clinical, etc."
      )
      ))
  
  # Scenario 1
  output$info_c1max_s1 <-
    renderText(paste0("Baseline value is ", parameters$c1max[1]))
  output$info_c2max_s1 <-
    renderText(paste0("Baseline value is ", parameters$c2max[1]))
  output$info_c3max_s1 <-
    renderText(paste0("Baseline value is ", parameters$c3max[1]))
  output$info_cpmax_s1 <-
    renderText(paste0("Baseline value is ", parameters$cpmax[1]))
  output$info_precmax_s1 <-
    renderText(paste0("Baseline value is ", parameters$precmax[1]))
  
  # Scenario 2
  output$info_c1max_s2 <-
    renderText(paste0("Baseline value is ", parameters$c1max[1]))
  output$info_c2max_s2 <-
    renderText(paste0("Baseline value is ", parameters$c2max[1]))
  output$info_c3max_s2 <-
    renderText(paste0("Baseline value is ", parameters$c3max[1]))
  output$info_cpmax_s2 <-
    renderText(paste0("Baseline value is ", parameters$cpmax[1]))
  output$info_precmax_s2 <-
    renderText(paste0("Baseline value is ", parameters$precmax[1]))
  
  
  # Object to store results
  simul <- reactiveValues(finished = FALSE)
  
  
  
  # Run Both Scenario ---------------------------------------------------------------------------------------------------------
  observeEvent(input$run_both, {
    source("./www/run_both_scenario.R", local = TRUE)
  })
  
  
  
  # Run Scenario 1 ------------------------------------------------------------------------------------------------------------
  observeEvent(input$run_s1, {
    source("./www/run_scenario1.R", local = TRUE)
  })
  
  
  # Run Scenario 2 ------------------------------------------------------------------------------------------------------------
  observeEvent(input$run_s2, {
    source("./www/run_scenario2.R", local = TRUE)
  })
  
  
  # Slider for navigation -----------------------------------------------------------------------------------------------------
  
  output$plot_navig_buttons <- renderUI({
    if (simul$finished == FALSE)
      return(NULL)
    else {
      tagList(
        br(),
        br(),
        sliderTextInput(
          "slider_iter",
          label = NULL,
          choices = isolate(round((0:input$total_q)/input$total_q, 2)),
          selected = 1,
          grid = TRUE,
          animate = list(interval = 1200, loop = FALSE),
          width = "100%"
        )
      )
    }
  })
  
  
  
  q = 6
  
  
  
  # Plot Output Scenario 1 ----------------------------------------------------------------------------------------------------
  
  output$plot_scenario1 <- renderPlot({
    req(simul$iter >= 1 & simul$status_s1 != 0)
    
    selected_q <-
      ifelse(
        simul$finished == TRUE & !is.null(input$slider_iter),
        round(isolate(input$total_q) * as.numeric(input$slider_iter)),
        simul$iter - 1
      )
    
    plot_incidence <-
      ggplot(simul$results_incidence_s1 %>% filter(t > 5, q == selected_q + 1),
             aes(x = t)) +
      geom_line(aes(y = totinc_month), lwd = 1.5) +
      geom_line(aes(y = inc_month),
                col = "red",
                lty = 2,
                lwd = 1.5) +
      labs(
        title = paste0("Incidence — Quality = ", round(
          selected_q / isolate(input$total_q), 2
        )),
        x = "Years",
        y = "Cases per 1,000 per month"
      ) +
      theme_minimal(base_size = 17)
    
    
    plot_global <-
      ggplot(data = simul$results_s1 %>% filter(qq <= selected_q / isolate(input$total_q)),
             aes(x = qq)) +
      geom_point(aes(y = cinc), size = 3) +
      geom_point(aes(y = cincres), size = 3, col = "red") +
      geom_vline(xintercept = selected_q / isolate(input$total_q),
                 lty = 2) +
      scale_x_continuous(limits = c(0, 1)) +
      labs(title = "Impact of Medicine Quality",
           x = "Medicine quality level (proxy for API)",
           y = "Cumulative incidence") +
      theme_minimal(base_size = 17)
    
    if(selected_q > 0) {plot_global <- plot_global + geom_line(aes(y = cinc), lwd = 1.2) +
      geom_line(aes(y = cincres), col = "red", lty = 2, lwd = 1.2)}
    
    grid.arrange(plot_incidence, plot_global, ncol = 1)
  })
  
  # Plot Output Scenario 2 ----------------------------------------------------------------------------------------------------
  
  output$plot_scenario2 <- renderPlot({
    req(simul$iter >= 1 & simul$status_s2 != 0)
    
    selected_q <-
      ifelse(
        simul$finished == TRUE & !is.null(input$slider_iter),
        round(isolate(input$total_q) * as.numeric(input$slider_iter)),
        simul$iter - 1
      )
    
    plot_incidence <-
      ggplot(simul$results_incidence_s2 %>% filter(t > 5, q == selected_q + 1),
             aes(x = t)) +
      geom_line(aes(y = totinc_month), lwd = 1.5) +
      geom_line(aes(y = inc_month),
                col = "red",
                lty = 2,
                lwd = 1.5) +
      labs(
        title = paste0("Incidence — Quality = ", round(
          selected_q / isolate(input$total_q), 2
        )),
        x = "Years",
        y = "Cases per 1,000 per month"
      ) +
      theme_minimal(base_size = 17)
    
    
    plot_global <-
      ggplot(data = simul$results_s2 %>% filter(qq <= selected_q / isolate(input$total_q)),
             aes(x = qq)) +
      geom_point(aes(y = cinc), size = 3) +
      geom_point(aes(y = cincres), size = 3, col = "red") +
      geom_vline(xintercept = selected_q / isolate(input$total_q),
                 lty = 2) +
      scale_x_continuous(limits = c(0, 1)) +
      labs(title = "Impact of Medicine Quality",
           x = "Medicine quality level (proxy for API)",
           y = "Cumulative incidence") +
      theme_minimal(base_size = 17)
    
    if(selected_q > 0) {plot_global <- plot_global + geom_line(aes(y = cinc), lwd = 1.2) +
      geom_line(aes(y = cincres), col = "red", lty = 2, lwd = 1.2)}
    
    grid.arrange(plot_incidence, plot_global, ncol = 1)
  })
})