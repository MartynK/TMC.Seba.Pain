
source( here::here( "inst", "functions", "load_stuff.r"))

generate_variable_matrix <- function(n) {

  # Generate random values for each parameter
  n_samp <- runif(n, min = 10, max = 200)  %>% {./10} %>% round(digits = 0) %>% {.*10}
  total_variance <- runif(n, min = 1, max = 3)  %>% round(digits = 2)
  trtA <- rep(3, n)  # Treatment A effect fixed at 5
  trtAB <- runif(n, min = 0.5, max = 3)  %>% round(digits = 2)
  trtBC <- runif(n, min = 0, max = trtAB)  %>% round(digits = 2)
  NSIM <- 10

  # Create the dataframe
  df <- data.frame(
    n_samp = n_samp,
    total_variance = total_variance,
    trtA = trtA,
    trtAB = trtAB,
    trtBC = trtBC,
    NSIM = NSIM,
    res = NA
  )

  return(df)
}

fun_process_data <- function(dat_sim_par) {
  # Process the data and return the results
  dat_sim_par$res <- NA
  pb <- utils::txtProgressBar(min = 0, max = nrow(dat_sim_par), style = 3)
  for (i in 1:nrow(dat_sim_par)) {
    counter <- 0
    for (j in 1:dat_sim_par$NSIM[i]) {
      dat_results_sim <- fun_simulate_study(
        n_samp = dat_sim_par$n_samp[i],
        var_study = dat_sim_par$total_variance[i],
        treat_eff_c = c(dat_sim_par$trtA[i],
                        dat_sim_par$trtA[i] - dat_sim_par$trtAB[i],
                        dat_sim_par$trtB[i] - dat_sim_par$trtBC[i])
      )  %>%
        eval_sim_results()

      counter <- counter + dat_results_sim
    }
    dat_sim_par$res[i] <- counter
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)

  return(dat_sim_par)

}

While_window_clicked_wrapper <- function(OUT_GEN_FUN, FUN,
                                         iter_per_core = 1,
                                         packages = c("dplyr"),
                                         max_iters = Inf) {

  # Initialize variable which breaks loop if TRUE
  buttonClicked <- FALSE
  assign("buttonClicked", buttonClicked, envir = .GlobalEnv)

  # Create main window
  win <- tktoplevel()

  # Create a canvas widget
  canvas <- tkcanvas(win, width = 200, height = 200)
  tkpack(canvas)

  # Bind a left mouse button click on the canvas to the onClick function
  tkbind(canvas, "<Button-1>", onClick)
  results_binded <- OUT_GEN_FUN(n = 1) %>% .[0,] # empty dataframe


  while (!buttonClicked) {

    # Number of iterations
    no_iter <- n.cores * 3
    # Create a dataframe to be filled with results
    out <- OUT_GEN_FUN(n = no_iter * iter_per_core)

    # Split the dataframe into parts, get the lines appropriate
    borders <- floor(seq(1, nrow(out), length.out = no_iter + 1))

    results <- foreach(
      i = 1:no_iter,
      .verbose = TRUE,
      .combine = 'rbind',
      .export = c("out", "FUN","eval_sim_results",
                  "fun_simulate_study","fun_block_randomize"),
      .errorhandling = 'remove',
      .inorder = FALSE,
      .packages = packages
    ) %dopar% {

      out_act <- out[borders[i]:borders[i + 1],]

      try({
        out_act <- FUN(out_act)
      })

      return(out_act)
    }

    results_binded <- bind_rows(results_binded, results)

    # Check the status of buttonClicked
    if (get("buttonClicked", envir = globalenv())) {
      print("The canvas was clicked.")
      break
    } else if (nrow(results_binded) > max_iters) {
      print("Max. iterations reached.")
      break
    }

  }

  # Destroy the window
  tkdestroy(win)

  return(results_binded)
}


Parallel_envir_setup(spare_korez = 5)


dat_out <-
  While_window_clicked_wrapper(
    generate_variable_matrix,
    fun_process_data,
    iter_per_core = 2,
    packages = c("dplyr","emmeans", "lmerTest")
    )


if (file.exists(here::here("data","sim_results.rda"))) {
  dat_out_new <- dat_out
  load(here::here("data","sim_results.rda"))
  dat_out <- bind_rows(dat_out, dat_out_new)
}

save(dat_out, file = here::here("data","sim_results.rda"))
