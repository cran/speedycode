#### speedy_labels ####
speedy_labels <- function(data, nrows = 5, path = "") {

  # This creates the code for labelling the variables
  qline1 <- paste("data"," <- ", "data", " %>%", sep = "")
  qline2a <- "set_variable_labels("
  qline2b <- paste(colnames(data[1]), " = ", '"QTEXT_HERE",', sep = "")
  qline3 <- paste(colnames(data[c(-1, -length(data))]), " = ", '"QTEXT_HERE"', ",", sep = "")
  qline4 <- paste(colnames(data[length(data)]), " = ", '"QTEXT_HERE"', ")", sep = "")
  qnewcode <- paste(qline3, "\n", sep = "")


  # This pastes the code for labelling the variables in the console
  cat(qline1, "\n", qline2a, "\n", qline2b, "\n", qnewcode, qline4, " %>% ", "\n", "set_value_labels(", "\n")

  # This creates a function for iterating through each of the variables
  val_code <- function(num) {
    new_data <- data %>%
      select(-last_col())

    new_data <- new_data %>%
      dplyr::select({{num}})

    new_data <- new_data %>%
      dplyr::arrange(new_data[1])

    if (nrows == 2) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[1,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[2,], "\n", ")", ",", "\n",
                         sep = "")
    } else if (nrows == 3) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[1,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[2,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[3,], "\n", ")", ",", "\n",
                         sep = "")
    } else if (nrows == 4) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[1,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[2,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[3,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[4,], "\n", ")", ",", "\n",
                         sep = "")
    } else if (nrows == 5) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[1,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[2,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[3,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[4,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[5,], "\n", ")", ",", "\n",
                         sep = "")
    } else if (nrows == 6) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[1,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[2,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[3,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[4,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[5,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[6,], "\n", ")", ",", "\n",
                         sep = "")
    } else if (nrows == 7) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[1,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[2,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[3,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[4,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[5,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[6,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[7,], "\n", ")", ",", "\n",
                         sep = "")
    } else if (nrows == 8) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[1,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[2,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[3,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[4,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[5,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[6,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[7,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[8,], "\n", ")", ",", "\n",
                         sep = "")
    } else if (nrows == 9) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[1,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[2,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[3,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[4,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[5,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[6,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[7,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[8,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[9,], "\n", ")", ",", "\n",
                         sep = "")
    } else if (nrows == 10) {
      main_body <- paste(colnames(new_data[1]), " = ", "c(", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[1,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[2,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[3,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[4,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[5,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[6,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[7,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[8,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[9,], ",", "\n",
                         '"NEW_LABEL"', " = ", unique(new_data[1])[10,], "\n", ")", ",", "\n",
                         sep = "")
    } else {
      stop("Must choose a number between 2 and 10")
    }

    # This prints the value labeling code to the console
    cat(main_body)

  }

  # This prints the final variable in the dataset to the console
  data_short <- data %>%
    select(-last_col())

  purrr::map_dfr(1:length(data_short),val_code)

  # This creates a new dataframe with the last variable in the dataset
  data_last_var <- data %>%
    select(last_col()) %>%
    distinct()

  data_last_var <- data_last_var %>%
    arrange(data_last_var[[1]])

  #  This prints the last variable in the dataset to the console
  if (nrows == 2) {
    cat(colnames(data_last_var[1]), " = ", "c(", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], "\n", ")", "\n)", sep = "")
  } else if (nrows == 3) {
    cat(colnames(data_last_var[1]), " = ", "c(", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[3,], "\n", ")", "\n)", sep = "")
  } else if (nrows == 4) {
    cat(colnames(data_last_var[1]), " = ", "c(", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[3,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[4,], "\n", ")", "\n)", sep = "")
  } else if (nrows == 5) {
    cat(colnames(data_last_var[1]), " = ", "c(", "\n",
        '"NEW_LABEL"', " = ", unlist(unique(data_last_var[1])[1,]), ",", "\n",
        '"NEW_LABEL"', " = ", unlist(unique(data_last_var[1])[2,]), ",", "\n",
        '"NEW_LABEL"', " = ", unlist(unique(data_last_var[1])[3,]), ",", "\n",
        '"NEW_LABEL"', " = ", unlist(unique(data_last_var[1])[4,]), ",", "\n",
        '"NEW_LABEL"', " = ", unlist(unique(data_last_var[1])[5,]), "\n", ")", "\n)", sep = "")
  } else if (nrows == 6) {
    cat(colnames(data_last_var[1]), " = ", "c(", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[3,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[4,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[5,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[6,], "\n", ")", "\n)", sep = "")
  } else if (nrows == 7) {
    cat(colnames(data_last_var[1]), " = ", "c(", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[3,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[4,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[5,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[6,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[7,], "\n", ")", "\n)", sep = "")
  } else if (nrows == 8) {
    cat(colnames(data_last_var[1]), " = ", "c(", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[3,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[4,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[5,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[6,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[7,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[8,], "\n", ")", "\n)", sep = "")
  } else if (nrows == 9) {
    cat(colnames(data_last_var[1]), " = ", "c(", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[3,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[4,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[5,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[6,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[7,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[8,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[9,], "\n", ")", "\n)", sep = "")
  } else if (nrows == 10) {
    cat(colnames(data_last_var[1]), " = ", "c(", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[3,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[4,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[5,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[6,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[7,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[8,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[9,], ",", "\n",
        '"NEW_LABEL"', " = ", unique(data_last_var[1])[10,], "\n", "\n)", sep = '')
  } else {
    stop("Must choose a number between 2 and 10")
  }


  # This throws an error if the code is saved out to a file that is NOT an R script
  final_path_char <- stringr::str_sub(path,-2,-1)
  if(stringr::str_detect(path, ".") && final_path_char != ".R") {
    stop('Must specify ".R" at the end of the path to save formatted code to an R script file')
  }

  oo <- options(crayon.enabled = FALSE)
  on.exit(options(oo))

  # This writes the code to a separate R script
  if (nrows == 2) {
    suppressWarnings(capture.output(cat(qline1, "\n", qline2a, "\n", qline2b, "\n",
                                        qnewcode, qline4, " %>% ", "\n",
                                        "set_value_labels(", "\n"),
                                    suppressMessages(invisible(purrr::map_dfr(1:length(data_short),val_code))),
                                    cat(colnames(data_last_var), " = ", "c(", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], "\n", ")\n)",
                                        sep = ""),
                                    file = path))
  } else if (nrows == 3) {
    suppressWarnings(capture.output(cat(qline1, "\n", qline2a, "\n", qline2b, "\n",
                                        qnewcode, qline4, " %>% ", "\n",
                                        "set_value_labels(", "\n"),
                                    suppressMessages(invisible(purrr::map_dfr(1:length(data_short),val_code))),
                                    cat(colnames(data_last_var), " = ", "c(", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[3,], "\n", ")\n)",
                                        sep = ""),
                                    file = path))
  }
  else if (nrows == 4) {
    suppressWarnings(capture.output(cat(qline1, "\n", qline2a, "\n", qline2b, "\n",
                                        qnewcode, qline4, " %>% ", "\n",
                                        "set_value_labels(", "\n"),
                                    suppressMessages(invisible(purrr::map_dfr(1:length(data_short),val_code))),
                                    cat(colnames(data_last_var), " = ", "c(", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[3,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[4,], "\n", ")\n)",
                                        sep = ""),
                                    file = path))
  } else if (nrows == 5) {
    suppressWarnings(capture.output(cat(qline1, "\n", qline2a, "\n", qline2b, "\n",
                                        qnewcode, qline4, " %>% ", "\n",
                                        "set_value_labels(", "\n"),
                                    suppressMessages(invisible(purrr::map_dfr(1:length(data_short),val_code))),
                                    cat(colnames(data_last_var), " = ", "c(", "\n",
                                        '"NEW_LABEL"', " = ", unlist(unique(data_last_var[1])[1,]), ",", "\n",
                                        '"NEW_LABEL"', " = ", unlist(unique(data_last_var[1])[2,]), ",", "\n",
                                        '"NEW_LABEL"', " = ", unlist(unique(data_last_var[1])[3,]), ",", "\n",
                                        '"NEW_LABEL"', " = ", unlist(unique(data_last_var[1])[4,]), ",", "\n",
                                        '"NEW_LABEL"', " = ", unlist(unique(data_last_var[1])[5,]), "\n", ")\n)",
                                        sep = ""),
                                    file = path))
  } else if (nrows == 6) {
    suppressWarnings(capture.output(cat(qline1, "\n", qline2a, "\n", qline2b, "\n",
                                        qnewcode, qline4, " %>% ", "\n",
                                        "set_value_labels(", "\n"),
                                    suppressMessages(invisible(purrr::map_dfr(1:length(data_short),val_code))),
                                    cat(colnames(data_last_var), " = ", "c(", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[3,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[4,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[5,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[6,], "\n", ")\n)",
                                        sep = ""),
                                    file = path))

  } else if (nrows == 7) {
    suppressWarnings(capture.output(cat(qline1, "\n", qline2a, "\n", qline2b, "\n",
                                        qnewcode, qline4, " %>% ", "\n",
                                        "set_value_labels(", "\n"),
                                    suppressMessages(invisible(purrr::map_dfr(1:length(data_short),val_code))),
                                    cat(colnames(data_last_var), " = ", "c(", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[3,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[4,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[5,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[6,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[7,], "\n", ")\n)",
                                        sep = ""),
                                    file = path))
  } else if (nrows == 8) {
    suppressWarnings(capture.output(cat(qline1, "\n", qline2a, "\n", qline2b, "\n",
                                        qnewcode, qline4, " %>% ", "\n",
                                        "set_value_labels(", "\n"),
                                    suppressMessages(invisible(purrr::map_dfr(1:length(data_short),val_code))),
                                    cat(colnames(data_last_var), " = ", "c(", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[3,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[4,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[5,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[6,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[7,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[8,], "\n", ")\n)",
                                        sep = ""),
                                    file = path))
  } else if (nrows == 9) {
    suppressWarnings(capture.output(cat(qline1, "\n", qline2a, "\n", qline2b, "\n",
                                        qnewcode, qline4, " %>% ", "\n",
                                        "set_value_labels(", "\n"),
                                    suppressMessages(invisible(purrr::map_dfr(1:length(data_short),val_code))),
                                    cat(colnames(data_last_var), " = ", "c(", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[3,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[4,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[5,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[6,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[7,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[8,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[9,], "\n", ")\n)",
                                        sep = ""),
                                    file = path))
  } else if (nrows == 10) {
    suppressWarnings(capture.output(cat(qline1, "\n", qline2a, "\n", qline2b, "\n",
                                        qnewcode, qline4, " %>% ", "\n",
                                        "set_value_labels(", "\n"),
                                    suppressMessages(invisible(purrr::map_dfr(1:length(data_short),val_code))),
                                    cat(colnames(data_last_var), " = ", "c(", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[1,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[2,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[3,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[4,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[5,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[6,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[7,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[8,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[9,], ",", "\n",
                                        '"NEW_LABEL"', " = ", unique(data_last_var[1])[10,], "\n", ")\n)",
                                        sep = ""),
                                    file = path))
  }
}

#### speedy_mutate ####
speedy_mutate <- function(data, var, var_classes = "sn", path = ""){
  x <- data %>%
    select(all_of(var)) %>%
    distinct() %>%
    arrange()

  # BOTH old and new var are strings
  if(var_classes == "ss") {
    line1 <- paste("data"," <- ", "data", " %>%", sep = "")
    line2a <- "mutate("
    line2b <- paste('NEW_VAR', " = ", "case_when(", "\n", var, " == ", '"', x[1,1], '"', " ~ ", '"', "NEW_VAL", '"', ",", sep = "")
    line2d <- character()
    for(i in 2:(nrow(x)-1)) {
      line2d <- c(line2d, paste(var, " == ", '"', x[i,1], '"', " ~ ", '"', "NEW_VAL", '"', ",", "\n", sep = ""))
      # cat(line2d)
    }
    line2e <- paste(var, " == ", '"', x[nrow(x),1], '"', " ~ ", '"', "NEW_VAL", '"', ")", "\n", sep = "")
    last_line <- paste(")", sep = "")

    # This prints the code in the console
    cat(line1, "\n", line2a, "\n", line2b, "\n", line2d, line2e, last_line)

    # This writes the code to a separate R script
    suppressWarnings(utils::capture.output(cat(line1, "\n", line2a, "\n", line2b, "\n", line2d, line2e, last_line), file = path))
  }

  else if(var_classes == "sn") {
    line1 <- paste("data"," <- ", "data", " %>%", sep = "")
    line2a <- "mutate("
    line2b <- paste('NEW_VAR', " = ", "case_when(", "\n", var, " == ", '"', x[1,1], '"', " ~ ", "NEW_VAL", ",", sep = "")
    line2d <- character()
    for(i in 2:(nrow(x)-1)) {
      line2d <- c(line2d, paste(var, " == ", '"', x[i,1], '"', " ~ ", "NEW_VAL", ",", "\n", sep = ""))
      # cat(line2d)
    }
    line2e <- paste(var, " == ", '"', x[nrow(x),1], '"', " ~ ", "NEW_VAL", ")", "\n", sep = "")
    last_line <- paste(")", sep = "")

    # This prints the code in the console
    cat(line1, "\n", line2a, "\n", line2b, "\n", line2d, line2e, last_line)

    # This writes the code to a separate R script
    suppressWarnings(utils::capture.output(cat(line1, "\n", line2a, "\n", line2b, "\n", line2d, line2e, last_line), file = path))
  }

  else if(var_classes == "nn") {
    line1 <- paste("data"," <- ", "data", " %>%", sep = "")
    line2a <- "mutate("
    line2b <- paste('NEW_VAR', " = ", "case_when(", "\n", var, " == ", x[1,1], " ~ ", "NEW_VAL", ",", sep = "")
    line2d <- character()
    for(i in 2:(nrow(x)-1)) {
      line2d <- c(line2d, paste(var, " == ", x[i,1], " ~ ", "NEW_VAL", ",", "\n", sep = ""))
      # cat(line2d)
    }
    line2e <- paste(var, " == ", x[nrow(x),1], " ~ ", "NEW_VAL", ")", "\n", sep = "")
    last_line <- paste(")", sep = "")

    # This prints the code in the console
    cat(line1, "\n", line2a, "\n", line2b, "\n", line2d, line2e, last_line)

    # This writes the code to a separate R script
    suppressWarnings(utils::capture.output(cat(line1, "\n", line2a, "\n", line2b, "\n", line2d, line2e, last_line), file = path))
  }

  else if(var_classes == "ns") {
    line1 <- paste("data"," <- ", "data", " %>%", sep = "")
    line2a <- "mutate("
    line2b <- paste('NEW_VAR', " = ", "case_when(", "\n", var, " == ", x[1,1], " ~ ", '"', "NEW_VAL", '"', ",", sep = "")
    line2d <- character()
    for(i in 2:(nrow(x)-1)) {
      line2d <- c(line2d, paste(var, " == ", x[i,1], " ~ ", '"', "NEW_VAL", '"', ",", "\n", sep = ""))
      # cat(line2d)
    }
    line2e <- paste(var, " == ", x[nrow(x),1], " ~ ", '"', "NEW_VAL", '"', ")", "\n", sep = "")
    last_line <- paste(")", sep = "")

    # This prints the code in the console
    cat(line1, "\n", line2a, "\n", line2b, "\n", line2d, line2e, last_line)

    # This writes the code to a separate R script
    suppressWarnings(utils::capture.output(cat(line1, "\n", line2a, "\n", line2b, "\n", line2d, line2e, last_line), file = path))
  }

  # This throws an error if the code is saved out to a file that is NOT an R script
  final_path_char <- stringr::str_sub(path,-2,-1)
  if(stringr::str_detect(path, ".") && final_path_char != ".R") {
    stop('Must specify ".R" at the end of the path to save formatted code to an R script file')
  }

  oo <- options(crayon.enabled = FALSE)
  on.exit(options(oo))
}

#### speedy_rename ####
speedy_rename <- function(data, path = "") {

  # This automates the code based on the provided dataset
  line1 <- paste("data"," <- ", "data", " %>%", sep = "")
  line2a <- "rename("
  line2b <- paste('NEW_NAME', " = ", colnames(data[1]), ",",  sep = "")
  line3 <- paste('NEW_NAME', " = ", colnames(data[c(-1, -length(data))]), ",", sep = "")
  line4 <- paste('NEW_NAME', " = ", colnames(data[length(data)]), "\n", ")", sep = "")
  newcode <- paste(line3, "\n", sep = "")


  # This prints the code in the console
  cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4)


  # This throws an error if the code is saved out to a file that is NOT an R script
  final_path_char <- stringr::str_sub(path,-2,-1)
  if (stringr::str_detect(path, ".") && final_path_char != ".R") {
    stop('Must specify ".R" at the end of the path to save formatted code to an R script file')
  }

  # This writes the code to a separate R script
  suppressWarnings(utils::capture.output(cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4), file = path))
}

#### speedy_classes ####
speedy_classes <- function(data, path = "") {

  # This automates the code based on the provided dataset
  class_list <- unlist(lapply(data, class))
  class_list <- as.data.frame(class_list)
  line1 <- paste("data"," <- ", "data", " %>%", sep = "")
  line2a <- "mutate("
  line2b <- paste(colnames(data[1]), " = ", "as.", class_list[1,], "(", colnames(data[1]), ")", ",",  sep = "")
  line3 <- paste(colnames(data[c(-1, -length(data))]), " = ", "as.", class_list[2:(nrow(class_list)-1),], "(", colnames(data[c(-1, -length(data))]), ")", ",", sep = "")
  line4 <- paste(colnames(data[length(data)]), " = ", "as.", class_list[nrow(class_list),], "(", colnames(data[length(data)]), ")\n)", sep = "")
  newcode <- paste(line3, "\n", sep = "")


  # This prints the code in the console
  cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4)


  # This throws an error if the code is saved out to a file that is NOT an R script
  final_path_char <- stringr::str_sub(path,-2,-1)
  if (stringr::str_detect(path, ".") && final_path_char != ".R") {
    stop('Must specify ".R" at the end of the path to save formatted code to an R script file')
  }


  # This writes the code to a separate R script
  suppressWarnings(utils::capture.output(cat(line1, "\n", line2a, "\n", line2b, "\n", newcode, line4), file = path))
}

#### speedy_varnames ####
speedy_varnames <- function(prefix = "Q",
                            first_number = 1,
                            last_number = 25){
  if(is.numeric(first_number) == F) stop("Must provide a numeric value for first_number")
  if(is.numeric(last_number) == F) stop("Must provide a numeric value for last_number")
  if(is.character(prefix) == F) stop("Must provide a character value for prefix")
  data <- paste('\"',prefix, seq(from = first_number, to = (last_number - 1), by = 1),'\"', ",", sep = "")
  final_var <- paste('"', prefix, last_number, '"', sep = "")
  # data <- noquote(data)
  cat(data, final_var)
}

#### debug_ascii ####
debug_ascii <- function(col_positions_input,
                          widths_input,
                          col_names_input) {
  col_positions_length = length(col_positions_input)
  widths_length = length(widths_input)
  col_names_length = length(col_names_input)
  data <- cbind(col_positions_length,
                widths_length,
                col_names_length)
  data <- as.data.frame(data)
  print(data)
  if(col_positions_length != widths_length |
     col_positions_length != col_names_length |
     widths_length != col_names_length) warning('The lengths of col_positions, widths, and col_names MUST be equal to run the read_rpr function')
}
