#' Create queue
#'
#' @param name name of queue to be assigned
#' @export
#' @examples
#'
#'  .q <- queue()
#'
queue <- function(name = "base", class = NULL) {
  .q <- new.env()
  assign("name", name, envir = .q)
  assign("q", list(), envir = .q)
  structure(.q, class = c(class, "queue"))
}

#' Get size of queue
#'
#' @export
length.queue <- function(.q) {
  e <- base::get("q", envir = .q)
  length(e)
}

#' Push item into queue
#'
#' Note that it does not need to return queue obj since the environemnt is
#' stored like global variable
#'
#' @method push queue
#' @export
push.queue <- function(.q, v){
  if (is.null(v)) {
    stop("queue item to push is NULL")
  }

  e <- base::get("q", envir = .q)
  idx <- length(e)
  e[[idx + 1]] <- v
  assign("q", e, envir = .q)
  invisible()
}

#' Pop item from queue
#'
#' @method pop queue
#' @export
pop.queue <- function(.q) {
  e <- base::get("q", envir = .q)
  v <- e[[length(e)]]
  if(length(e) == 1) {
    assign("q", list(), .q)
  } else {
    assign("q", e[1:(length(e)-1)], .q)
  }

  v
}

#' @export
print.queue <- function(.q){
  rule(paste('queue:',
             base::get("name", envir = .q)))
  .list <- base::get("q", envir = .q)

  for (i in rng(1, length(.list))) {
    cat(paste0(i, ":"), as.character(.list[[i]]), "\n")
  }

  invisible()
}

#' @export
push <- function(.q, v) {
  UseMethod("push")
}

#' @export
pop <- function(.q) {
  UseMethod("pop")
}

