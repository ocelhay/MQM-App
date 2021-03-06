# Notification
showNotification(
  id = "model_running",
  closeButton = FALSE,
  ui = HTML(paste0(h3("Model Running"))),
  duration = NULL,
  type = "message",
  session = session
)

# Re-initiate values
simul$iter <- 0
simul$start_time <- Sys.time()
simul$status_s1 <- 1
simul$status_s2 <- 1
simul$finished <- FALSE
simul$results_incidence_s1 <- data.frame(NULL)
simul$results_s1 <- data.frame(
  nq = 1:(input$total_q + 1),
  qq = (0:input$total_q) / input$total_q,
  cinc = NA,
  cincres = NA
)
simul$results_incidence_s2 <- data.frame(NULL)
simul$results_s2 <- data.frame(
  nq = 1:(input$total_q + 1),
  qq = (0:input$total_q) / input$total_q,
  cinc = NA,
  cincres = NA
)




observe({
  # Re-execute this reactive expression xxxms after it finishes
  # adapted from https://gist.github.com/bborgesr/61409e3852feb991336757f06392e52a
  invalidateLater(100, session)
  isolate({
    if (simul$finished == TRUE) {
      return()
    } else {
      simul$iter <- simul$iter + 1
      
      # Correspondance rmarkdown ~ shiny app:
      # i         ~ simul$iter
      # nq        ~ input$total_q
      # results_q ~ simul$results_incidence
      # results   ~ simul$results
      
      
      # Scenario 1 ----------------------------------------------------------------------------------------------------------------
      
      # Update parameters based on the values in the UI
      parameters$R0 <- rep(input$r0_s1, A)
      parameters$wait_treat <- input$wait_treat_s1
      parameters$c1max[2] <- input$c1max_s1
      parameters$c2max[2] <- input$c2max_s1
      parameters$c3max[2] <- input$c3max_s1
      parameters$cpmax[2] <- input$cpmax_s1
      parameters$nupmax[2] <- 365 / input$nupmax_s1
      parameters$precmax[2] <- input$precmax_s1

      parameters["q"] <- simul$results_s1$qq[simul$iter]
      
      set.seed(2018)
      out <-
        ode(
          y = X,
          times = times,
          func = MedQual,
          parms = parameters,
          method = "vode"
        )
      source("./www/process_results.R", local = TRUE)
      
      # save key output (cumulative incidence)
      simul$results_s1[simul$iter, "qq"] <- (simul$iter - 1) / input$total_q
      simul$results_s1[simul$iter, "cinc"] <- sum(CumInc[nt,]) - (prod(CumInc[nt, ]) / (maxt - parameters$t_treat * 1000))
      simul$results_s1[simul$iter, "cincres"] <- CumInc[nt, 2]
      
      # append results
      simul$results_incidence_s1 <-
        bind_rows(
          simul$results_incidence_s1,
          data.frame(
            q = simul$iter,
            t = t,
            inc_month = inc_month[, 2],
            totinc_month = totinc_month,
            presinc_month = presinc_month,
            prev = prev[, 2],
            totprev = totprev,
            presprev = presprev,
            pop = 50 * pop[, 1] / parameters$N
          )
        )
      
      
      
      # Scenario 2 ----------------------------------------------------------------------------------------------------------------

      # Update parameters and run the model
      parameters$R0 <- rep(input$r0_s2, A)
      parameters$wait_treat <- input$wait_treat_s2
      parameters["q"] <- simul$results_s2$qq[simul$iter]
      parameters$c1max[2] <- input$c1max_s2
      parameters$c2max[2] <- input$c2max_s2
      parameters$c3max[2] <- input$c3max_s2
      parameters$cpmax[2] <- input$cpmax_s2
      parameters$nupmax[2] <- 365 / input$nupmax_s2
      parameters$precmax[2] <- input$precmax_s2
      
      set.seed(2018)
      out <-
        ode(
          y = X,
          times = times,
          func = MedQual,
          parms = parameters,
          method = "vode"
        )
      source("./www/process_results.R", local = TRUE)
      
      # save key output (cumulative incidence)
      simul$results_s2[simul$iter, "qq"] <-
        (simul$iter - 1) / input$total_q
      simul$results_s2[simul$iter, "cinc"] <-
        sum(CumInc[nt,]) - (prod(CumInc[nt, ]) / (maxt - parameters$t_treat * 1000))
      simul$results_s2[simul$iter, "cincres"] <- CumInc[nt, 2]
      
      # append results
      simul$results_incidence_s2 <-
        bind_rows(
          simul$results_incidence_s2,
          data.frame(
            q = simul$iter,
            t = t,
            inc_month = inc_month[, 2],
            totinc_month = totinc_month,
            presinc_month = presinc_month,
            prev = prev[, 2],
            totprev = totprev,
            presprev = presprev,
            pop = 50 * pop[, 1] / parameters$N
          )
        )
      
      showNotification(
        id = "model_running",
        closeButton = FALSE,
        ui = HTML(
          paste0(
            "Progress: ",
            round(simul$iter / (isolate(
              input$total_q
            ) + 1) * 100),
            "%",
            " — ",
            "total elapsed time: ",
            round(Sys.time() - simul$start_time, 1),
            " seconds"
          )
        ),
        duration = NULL,
        type = "message",
        session = session
      )
      
      # -----------------------------------------------------------------
      
      # Stop at the end
      if (simul$iter == isolate(input$total_q) + 1) {
        simul$finished <- TRUE
        removeNotification(id = "model_running", session = session)
        # print(simul$results_incidence_s1)
        # print(simul$results_incidence_s2)
      }
    }
  })
})