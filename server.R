# Medicine Quality Modelling App
# MORU, MAEMOD, Olivier Celhay
# server.R  server logic
# -----------------------------------------------------------------------------

shinyServer(
  function(input, output, session) {
    
    # Parameters values
    output$parameters_values <- renderText("")
    
    # Create a reactiveValues object
    simul <- reactiveValues(iter = 0, start_time = Sys.time(), 
                            results_incidence_s1 = NULL, results_s1 = NULL, 
                            results_incidence_s2 = NULL, results_s2 = NULL, 
                            finished = FALSE)
    
    # Execute every time run is press
    observeEvent(input$run, {
      # Re-initiate values
      simul$iter <- 0
      simul$start_time <- Sys.time()
      
      simul$results_incidence_s1 <- data.frame(NULL)
      simul$results_s1 <- data.frame(
        nq = 1:(input$total_q + 1),
        qq = (0:input$total_q)/input$total_q, 
        cinc = NA, 
        cincres = NA)
      
      simul$results_incidence_s2 <- data.frame(NULL)
      simul$results_s2 <- data.frame(
        nq = 1:(input$total_q + 1),
        qq = (0:input$total_q)/input$total_q, 
        cinc = NA, 
        cincres = NA)
      
      
      simul$finished <- FALSE
      
      observe({
        # Re-execute this reactive expression immediately after it finishes (though you can also specify a longer time:
        # e.g. 1000 means re-execute after 1000 milliseconds have passed since it was last executed)
        # adapted from https://gist.github.com/bborgesr/61409e3852feb991336757f06392e52a
        invalidateLater(0, session)
        isolate({
          if (simul$finished == TRUE) {
            return()
          } else {
            simul$iter <- simul$iter + 1
            
            
            # Placeholder for the extensive computing -------------------------
            # -----------------------------------------------------------------
            
            # Correspondance rmarkdown ~ shiny app:
            # i         ~ simul$iter
            # nq        ~ input$total_q
            # results_q ~ simul$results_incidence
            # results   ~ simul$results
            
            # Scenario 1
            # -----------------------------------------------------------------

            # Update parameters and run the model
            parameters$R0 <- rep(input$r0_s1, A)
            parameters$wait_treat <- input$wait_treat_s1
            parameters["q"] <- simul$results_s1$qq[simul$iter]
            
            out <- ode(y = X, times = times, func = MedQual, parms = parameters, method = "vode")
            
            # Extract and process results
            t <- out[, 1]
            S <- out[, index_X == "Sindex"]
            T1 <- out[, index_X == "T1index"]
            T2 <- out[, index_X == "T2index"]
            T3 <- out[, index_X == "T3index"]
            Tp <- out[, index_X == "Tpindex"]
            Tr <- out[, index_X == "Trindex"]
            IC1 <- out[, index_X == "IC1index"]
            IA1 <- out[, index_X == "IA1index"]
            IU1 <- out[, index_X == "IU1index"]
            P <- out[, index_X == "Pindex"]
            R <- out[, index_X == "Rindex"]
            CumInc <- out[, index_X == "CumIncindex"]
            Fail <- out[, index_X == "Failindex"]
            positiveDay3up <- out[, index_X == "positiveDay3upindex"]
            positiveDay1up <- out[, index_X == "positiveDay1upindex"]
            pop <- S + T1 + T2 + T3 + Tp + Tr + IC1 + IA1 + IU1 + P + R
            
            # incidence
            # TODO: check if it's 2 or -more plausibly- A
            inc_month <- matrix(NA, nrow = nt, ncol = 2)
            inc_month[1, ] <- c(0, 0)
            inc_month[2:nt, ] <- CumInc[2:nt, ] - CumInc[1:(nt-1), ]
            inc_month <- 1000 * inc_month / pop
            totinc_month <- rowSums(inc_month) - inc_month[,1] * inc_month[,2] / 1000
            presinc_month <- 100 * inc_month[,2] / totinc_month
            
            # prevalence
            prev <- 100*(T1 + T2 + T3 + Tp + parameters$sensC*IC1 + parameters$sensA*IA1)/pop
            totprev <- rowSums(prev) - prev[,1] * prev[,2] / 100
            prevr <- prev[,2] - prev[,1] * prev[,2] / 100
            presprev <- 100 * prev[,2] / totprev
            
            # save key output (cumulative incidence)
            simul$results_s1[simul$iter, "qq"] <- (simul$iter - 1)/input$total_q
            simul$results_s1[simul$iter, "cinc"] <- sum(CumInc[nt, ]) - (prod(CumInc[nt,]) / (maxt - parameters$t_treat * 1000))
            simul$results_s1[simul$iter, "cincres"] <- CumInc[nt, 2]
            
            # append results
            simul$results_incidence_s1 <- bind_rows(simul$results_incidence_s1, 
                                   data.frame(
                                     q = simul$iter,
                                     t = t, 
                                     inc_month = inc_month[, 2],
                                     totinc_month = totinc_month,
                                     presinc_month = presinc_month,
                                     prev = prev[, 2],
                                     totprev = totprev,
                                     presprev = presprev,
                                     pop = 50 * pop[, 1] / parameters$N)
            )
            
            
            # Scenario 2
            # -----------------------------------------------------------------
            
            # Update parameters and run the model
            parameters$R0 <- rep(input$r0_s2, A)
            parameters$wait_treat <- input$wait_treat_s2
            parameters["q"] <- simul$results_s2$qq[simul$iter]
            
            out <- ode(y = X, times = times, func = MedQual, parms = parameters, method = "vode")
            
            # Extract and process results
            t <- out[, 1]
            S <- out[, index_X == "Sindex"]
            T1 <- out[, index_X == "T1index"]
            T2 <- out[, index_X == "T2index"]
            T3 <- out[, index_X == "T3index"]
            Tp <- out[, index_X == "Tpindex"]
            Tr <- out[, index_X == "Trindex"]
            IC1 <- out[, index_X == "IC1index"]
            IA1 <- out[, index_X == "IA1index"]
            IU1 <- out[, index_X == "IU1index"]
            P <- out[, index_X == "Pindex"]
            R <- out[, index_X == "Rindex"]
            CumInc <- out[, index_X == "CumIncindex"]
            Fail <- out[, index_X == "Failindex"]
            positiveDay3up <- out[, index_X == "positiveDay3upindex"]
            positiveDay1up <- out[, index_X == "positiveDay1upindex"]
            pop <- S + T1 + T2 + T3 + Tp + Tr + IC1 + IA1 + IU1 + P + R
            
            # incidence
            # TODO: check if it's 2 or -more plausibly- A
            inc_month <- matrix(NA, nrow = nt, ncol = 2)
            inc_month[1, ] <- c(0, 0)
            inc_month[2:nt, ] <- CumInc[2:nt, ] - CumInc[1:(nt-1), ]
            inc_month <- 1000 * inc_month / pop
            totinc_month <- rowSums(inc_month) - inc_month[,1] * inc_month[,2] / 1000
            presinc_month <- 100 * inc_month[,2] / totinc_month
            
            # prevalence
            prev <- 100*(T1 + T2 + T3 + Tp + parameters$sensC*IC1 + parameters$sensA*IA1)/pop
            totprev <- rowSums(prev) - prev[,1] * prev[,2] / 100
            prevr <- prev[,2] - prev[,1] * prev[,2] / 100
            presprev <- 100 * prev[,2] / totprev
            
            # save key output (cumulative incidence)
            simul$results_s2[simul$iter, "qq"] <- (simul$iter - 1)/input$total_q
            simul$results_s2[simul$iter, "cinc"] <- sum(CumInc[nt, ]) - (prod(CumInc[nt,]) / (maxt - parameters$t_treat * 1000))
            simul$results_s2[simul$iter, "cincres"] <- CumInc[nt, 2]
            
            # append results
            simul$results_incidence_s2 <- bind_rows(simul$results_incidence_s2, 
                                                     data.frame(
                                                       q = simul$iter,
                                                       t = t, 
                                                       inc_month = inc_month[, 2],
                                                       totinc_month = totinc_month,
                                                       presinc_month = presinc_month,
                                                       prev = prev[, 2],
                                                       totprev = totprev,
                                                       presprev = presprev,
                                                       pop = 50 * pop[, 1] / parameters$N)
            )
            
            # -----------------------------------------------------------------
            
            # Stop at the end
            if(simul$iter == isolate(input$total_q) + 1) simul$finished <- TRUE
          }
        })
      })
    })
    
    
    # Update with progress
    output$progress_update <- renderText({
      paste0("Progress: ", round(simul$iter / (isolate(input$total_q) + 1) * 100), "%", " — ",
             "total elapsed time: ", round(Sys.time() - simul$start_time, 1), " seconds")
    })
    
    # Plot the incidence
    output$plot_incidence_s1 <- renderPlot({
      if(simul$iter == 0) return(plot(0, xaxt='n', yaxt='n', bty='n', pch='', ylab='', xlab=''))
      
     ggplot(simul$results_incidence_s1 %>% 
              filter(t > 0, q == ifelse(simul$finished == TRUE & !is.null(input$slider_iter), 
                                                          input$slider_iter + 1,
                                                          simul$iter)), 
            aes(x = t)) +
        geom_line(aes(y = totinc_month), lwd = 2) +
        geom_line(aes(y = inc_month), col = "red", lwd = 2) +
       labs(title = "Incidence", x = "Years", y = "Cases per 1,000 per month") +
       theme_minimal(base_size = 17)
    })
    
    output$plot_incidence_s2 <- renderPlot({
      if(simul$iter == 0) return(plot(0, xaxt='n', yaxt='n', bty='n', pch='', ylab='', xlab=''))
      
      ggplot(simul$results_incidence_s2 %>% 
               filter(t > 0, q == ifelse(simul$finished == TRUE & !is.null(input$slider_iter), 
                                         input$slider_iter + 1,
                                         simul$iter)), 
             aes(x = t)) +
        geom_line(aes(y = totinc_month), lwd = 2) +
        geom_line(aes(y = inc_month), col = "red", lwd = 2) +
        labs(title = "Incidence", x = "Years", y = "Cases per 1,000 per month") +
        theme_minimal(base_size = 17)
    })
    
    # Plot the global incidence
    output$plot_global_s1 <- renderPlot({
      if(simul$iter == 0) return(plot(0, xaxt='n', yaxt='n', bty='n', pch='', ylab='', xlab=''))
      
      selected_q <- ifelse(simul$finished == TRUE & !is.null(input$slider_iter), 
                           input$slider_iter,
                           simul$iter - 1)
      
      ggplot(data = simul$results_s1, aes(x = qq, y = cinc)) +
        geom_line(lwd = 2) +
        geom_line(aes(y = cincres), col = "red", lty = 2, lwd = 1.5) +
        geom_vline(xintercept = selected_q / isolate(input$total_q), lty = 2) +
        # geom_rect(xmin = selected_q / isolate(input$total_q), 
        #           xmax = 1,
        #           ymin = 0,
        #           ymax = max(simul$results[1:selected_q, "cinc"]), 
        #           alpha = 0.2,
        #           color = "white") +
        labs(title = "Impact of Medicine Quality", 
             x = "Medicine quality level (proxy for API)", 
             y = "Cumulative incidence") +
        theme_minimal(base_size = 17)
    })
    
    output$plot_global_s2 <- renderPlot({
      if(simul$iter == 0) return(plot(0, xaxt='n', yaxt='n', bty='n', pch='', ylab='', xlab=''))
      
      selected_q <- ifelse(simul$finished == TRUE & !is.null(input$slider_iter), 
                           input$slider_iter,
                           simul$iter - 1)
      
      ggplot(data = simul$results_s2, aes(x = qq, y = cinc)) +
        geom_line(lwd = 2) +
        geom_line(aes(y = cincres), col = "red", lty = 2, lwd = 1.5) +
        geom_vline(xintercept = selected_q / isolate(input$total_q), lty = 2) +
        # geom_rect(xmin = selected_q / isolate(input$total_q), 
        #           xmax = 1,
        #           ymin = 0,
        #           ymax = max(simul$results[1:selected_q, "cinc"]), 
        #           alpha = 0.2,
        #           color = "white") +
        labs(title = "Impact of Medicine Quality", 
             x = "Medicine quality level (proxy for API)", 
             y = "Cumulative incidence") +
        theme_minimal(base_size = 17)
    })
    
    # Add naviguations buttons
    output$plot_navig_buttons <- renderUI({
      if(simul$finished == FALSE) return(NULL)
      else {tagList(
        br(), br(),
        sliderInput("slider_iter", label = "Medicine quality level",
                    min = 0, max = isolate(input$total_q), value = isolate(input$total_q), 
                    step = 1, animate = TRUE, width = "100%", ticks = TRUE, pre = "q = ")
      )}
    })
  })