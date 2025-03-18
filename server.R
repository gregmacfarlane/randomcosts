library(shiny)
library(dplyr)
library(ggplot2)
library(tibble)
source("R/equivalence_functions.R")

# generate a problem: initial present value, one or more annuities, and betweeen
# 0 and 3 future values
generate_problem <- function(seed) {

  set.seed(seed)
  # generate a random length of project (3 to 7 years)
  project_length <- sample(3:7, 1)
  # Generate a random present value (-500 to -5000)
  present_value <- sample(seq(500, 3000, 500), 1) * -1

  # Generate a random annuity (300 to 1000)
  annuity <- sample(3:10, 1) * 100

  # Generate a random number of future values (1 to 2)
  num_future_values <- sample(1:2, 1)
  # pick random years in project length for the future values
  future_years <- sample(1:project_length, num_future_values, replace = FALSE)
  # pick random future values -500 to 500
  future_values <- sample(c(-5:-1, 1:5), num_future_values) * 100
  # Generate a vector of length project_length with num_future_values non-missing values
  futures <- rep(NA, project_length)
  futures[future_years] <- future_values
  

  # Generate a random interest rate (0.01 to 0.1)
  rates <- c(0.005, 0.01, 0.015, 0.02, 0.03, 0.04, 0.05, 0.06, 
             0.07, 0.08, 0.1, 0.12, 0.18)
  interest_rate <- sample(rates, 1) * 100

  # Return the problem as a list
  list(present_value = present_value,
       annuities = annuity,
       future_values = futures,
       interest_rate = interest_rate)
}


# turn the problem into a text problem 
problem_to_text <- function(problem) {
  # Extract the values from the problem
  present_value <- problem$present_value
  annuities <- problem$annuities
  future_values <- problem$future_values

  # Create the text problem
  text_problem <- paste("<h3>Problem</h3><br>You have a present value of", present_value, 
                        "and an annuity of", annuities, "for", length(future_values), "years.")
  
  # Add future values to the text problem
  future_text <- paste("In the following years, you have future values of: <ul> ")
  for (year in 1:length(future_values)) {
    if (!is.na(future_values[year])) {
        future_text <- paste(future_text, "<li>", future_values[year], " in year ", year, "</li>")
    }
  }

  future_text <- paste(future_text, "</ul>")
  text_problem <- paste(text_problem, future_text, " \n Find the net present value of this project using a discount rate of ", 
                        problem$interest_rate, "%.")

  
  return(text_problem)
}

# solve the net present value for a problem
solve_npv <- function(problem, interest_rate = NULL) {
  # Extract the values from the problem
  present_value <- problem$present_value
  annuities <- problem$annuities
  future_values <- problem$future_values

  # if the interest rate is not provided, use the one from the problem
  if(is.null(interest_rate)) {
    interest_rate <- problem$interest_rate
  }

  years <- length(problem$future_values)
  # Calculate the NPV
  npv <- present_value + atop(annuities, interest_rate / 100, years)

  for (year in 1:years) {
    if (!is.na(future_values[year])) {
      npv <- npv + ftop(future_values[year], interest_rate / 100, year)
    }
  }

  return(npv)
}

# find the internal rate of return for the problem
find_irr <- function(problem) {
  # the IRR is the interest rate that makes the NPV equal to zero

  # use the solve_npv function to find the IRR
  irr <- uniroot(function(x) solve_npv(problem, x), 
                 interval = c(-30, 50), 
                 tol = .Machine$double.eps^0.25)$root
  # return the IRR
  return(irr)
}


# turn the problem into a diagram using ggplot
problem_to_diagram <- function(problem) {
  # Extract the values from the problem
  present_value <- problem$present_value
  annuities <- problem$annuities
  years <- length(problem$future_values)

  # turn future_values into a vector with zeros for missing values
  fv <- vector("numeric", length = 1 + years)
  for(i in 1:years) {
    if (!is.na(problem$future_values[i])) {
      fv[i + 1] <- problem$future_values[i]
    }
  }

  # Create a data frame for the cash flows
  cash_flows <- tibble::tibble(
    Year = 0:years,
    Present = c(present_value, rep(0, years)),
    Annuity = c(0, rep(annuities, years)),
    Future = fv
  ) |>
    dplyr::mutate(CashFlow = Present + Annuity + Future)
    # pivot_longer(-Year, names_to = "Type", values_to = "CashFlow")

  # Create the plot
  ggplot2::ggplot(cash_flows, aes(x = Year, y = CashFlow)) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::geom_text(aes(label = CashFlow), vjust = 0) + 
    ggplot2::labs(x = "Year", y = "Cash Flow") +
    ggplot2::theme_minimal()
}



# Define server logic
shinyServer(function(input, output) {

  output$info <- renderUI({
    HTML("<p>This is a simple cash flow generator. It generates a random present 
         value, annuity, and future values.  It also generates a random interest 
         rate. The cash flows are then plotted on a bar chart. Practice calculating
         the NPV and the IRR using this tool. Changing the number
         below will change the problem that you see. </p>")
  })

  # Generate text output
  output$text <- renderText({
    paste("Cash Flow Diagram", input$title)
  })

  observeEvent(input$simulate, {
    problem <<- generate_problem(input$seed)
    npv <<- solve_npv(problem)
    irr <<- find_irr(problem)

    output$distPlot <- renderPlot({
      problem_to_diagram(problem)
    })
    output$problemText <- renderText({
      if(input$showProblem == 1) {
        problem_to_text(problem)
      } else {
        paste("")
      }

    })

    output$solution <- renderText({
      if (input$showButton == 1) {
        paste("<span style='color:red;'>The net present value is", round(npv, 2), "</span><br>",
              "<span style='color:blue;'>The IRR is ", round(irr, 2), "%</span><br>",
              "Note that this value is obtained with functions; if you are using",
              "factor tables you may be off a bit, but would still be marked correct.")
      } else {
        paste("")
      }

    })
  })





})
