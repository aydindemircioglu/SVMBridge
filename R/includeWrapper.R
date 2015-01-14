loadThings <- function ()
{
  source ("software/libSVM/wrapper.R")
  source ("software/BudgetedSVM/wrapper.R")
  source ("software/LASVM/wrapper.R")
  source ("software/libCVM/wrapper.R")
  source ("software/libSVM/wrapper.R")
  source ("software/SharkSVM/wrapper.R")
#  source ("software/AESVM/wrapper.R")
  source ("software/SVMperf/wrapper.R")
}
suppressMessages(loadThings())
