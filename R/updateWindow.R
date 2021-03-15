#' Update the window
#'
#' Set a window to the minimum/maximum values of input vertices.
#' @param input [\code{data.frame(1)}]\cr a table of vertices for which a new
#'   window should be derived.
#' @param window [\code{data.frame(1)}]\cr the old window.
#' @return A new window that has the extent of \code{input}.
#' @importFrom checkmate assertNames assertDataFrame

.updateWindow <- function(input = NULL, window = NULL){

  # check arguments
  if(!testDataFrame(x = input, min.rows = 2)){
    return(window)
  }
  names(input) <- tolower(names(input))
  assertNames(x = names(input), must.include = c("x", "y"))
  names(window) <- tolower(names(window))
  assertNames(x = names(window), must.include = c("x", "y"))
  if(dim(window)[1] >= 2){
    window = as_tibble(data.frame(x = c(min(window$x), max(window$x), max(window$x), min(window$x), min(window$x)),
                                  y = c(min(window$y), min(window$y), max(window$y), max(window$y), min(window$y))))
  }
  assertDataFrame(x = window, nrows = 5)

  if(min(input$x) != min(window$x)){
    window$x[window$x %in% min(window$x)] <- min(input$x)
  }
  if(max(input$x) != max(window$x)){
    window$x[window$x %in% max(window$x)] <- max(input$x)
  }
  if(min(input$y) != min(window$y)){
    window$y[window$y %in% min(window$y)] <- min(input$y)
  }
  if(max(input$y) != max(window$y)){
    window$y[window$y %in% max(window$y)] <- max(input$y)
  }
  return(window)
}