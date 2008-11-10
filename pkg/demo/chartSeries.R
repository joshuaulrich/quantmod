# chartSeries demo
# Jeffrey A. Ryan 2008

require(quantmod)

chartSeries.demo <- function(x) {
  data(sample_matrix, package="xts")
  data <- as.xts(sample_matrix)
  cat("A simple xts object:\n")
  print(str(data))
  
  cat("chartSeries(data)\n")
  chartSeries(data)
  readline("Press <Enter> to continue")
  cat("Now we can add builtin indicators:\n\n")
  cat("Moving Average Convergence Divergence Indicator (from TTR)\n> addMACD()\n")
  plot(addMACD())
  readline("Press <Enter> to continue")
  cat("Add Bollinger Bands\n> addBBands()\n")
  plot(addBBands())
  readline("Press <Enter> to continue")
  cat("Drop Bollinger Bands\n> dropTA('BBands')\n")
  dropTA('BBands')
  readline("Press <Enter> to continue")
  cat("Zoom chart from full data to last 3 months\n> zoomChart(\"last 3 months\")\n")
  zoomChart('last 3 months')
  readline("Press <Enter> to continue")
  cat("Zoom back to full data\n> zoomChart()\n\n")
  zoomChart()
  rm(data)
}

chartSeries.demo(data)
