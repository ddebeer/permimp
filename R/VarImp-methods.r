## METHODS for "VarImp" objects.


## print for class VarImp
print.VarImp <- function(x, ranks = FALSE, ...){
   if(ranks) 
      print(ranks(x, ...))
   else 
      print(x$values, ...)
} 


## subset for class VarImp
subset.VarImp <- function(x, subset, ...) {
   selVars <- 'if'(missing(subset),
                 rep_len(TRUE, length(x$values)), 
                 subset)
      
   out <- as.VarImp(x$values[selVars],
                    perTree = x$perTree[, selVars, drop = FALSE], 
                    type = x$type, 
                    info = x$info)
  
   return(out)
}


## plot for class VarImp
plot.VarImp <- function(x, nVar = length(x$values), 
                        type = c("bar", "box", "dot", "rank"), 
                        sort = TRUE, 
                        interval = c( "no", "quantile", "sd"), 
                        intervalProbs = c(.25, .75),
                        intervalColor = NULL, 
                        horizontal = FALSE,
                        col = NULL, pch = NULL,
                        main = NULL, margin = NULL, ...)
{
  opar <- par(no.readonly =TRUE)
  on.exit(par(opar))
  
  # do matching and checking 
  stopifnot(nVar > 1)
  type <- match.arg(type)
  interval <- match.arg(interval)
  
  # select & order predictors
  X <- seq_len(nVar)
  Ranks <- ranks(x, note = FALSE)
  whichVar <- which(Ranks <= nVar)
  order <- 'if'(sort, order(Ranks[whichVar]), X)
  Y <- x$values[whichVar]
  
  # Check if "perTree" data is availble, if not give messages
  if(is.null(x$perTree) & 
    any(type == "box", interval == "quantile", interval == "sd")){
    warning(strwrap("Unable to plot distributional information 
                   (boxplot, interval,...), No 
                   'perTree' information available.", 
                    prefix = " ", initial = ""), 
            immediate. = TRUE, call. = FALSE)
    interval <- "no"
  }
   

   
   # compute interval
   intEnds <- if(interval == "no"){
     rbind(Y, Y)
   } else if(interval == "sd"){
     message(strwrap("NOTE: The plotted intervals based on the standard 
                     devaition may be misleading, the 'perTree'-values
                     are usually not symmetrically distributed", 
                     prefix = " ", initial = ""))
     sds <- apply(x$perTree[, whichVar, drop = FALSE], 2, sd)
     rbind(Y - sds, Y + sds)
     
   } else {
     apply(x$perTree[, whichVar, drop = FALSE], 2, quantile, probs = intervalProbs)
   }
   dropInts <- (intEnds[1,] == intEnds[2,])
   
   # make title
   adToTitle <- if(x$type == "Conditional Permutation"){
      paste0(" (threshold = ", x$info$threshold, ")")
   } else {""}
   title <- 'if'(is.null(main),
                 paste0(x$type, " Importance", adToTitle),
                 main)
  
  # bar plot
  if(type == "bar"){
    if(is.null(col)){col <- "skyblue"}
    if(is.null(intervalColor)){intervalColor <- "darkred"}
    if (!requireNamespace("scales", quietly = TRUE)) {
      message("To get nicer plots, please install package \"scales\".")
      } else {intervalColor <- scales::alpha(intervalColor, .5)} 
    if(horizontal){
      Y <- Y[rev(order)]
      intEnds <- intEnds[,rev(order)]
      dropInts <- dropInts[rev(order)]
      if(is.null(margin)){margin <- c(3,6,3,1)}
      op <- par(mar = margin)
      yValues <- barplot(Y, names.arg = "",
              xlim = c(min(intEnds, 0, Y), max(intEnds, Y)*1.05),
              main = title, col = col, horiz = horizontal, ...)
      text(x = min(intEnds, 0, Y), y = yValues,
           labels = names(Y), pos = 2, xpd=TRUE)
      abline(v = 0, col = "gray20", lty = 2)
      if(interval != "no"){
        arrows(x0 = intEnds[1, !dropInts], x1 = intEnds[2,!dropInts],
               y0 = yValues[!dropInts], y1 = yValues[!dropInts], 
               length = dev.size("in")[2]/length(yValues)/5, 
               angle = 90, code = 3, 
               col = intervalColor, lwd = 3)
      }
      
    } else {
      Y <- Y[order]
      intEnds <- intEnds[,order]
      dropInts <- dropInts[order]
      if(is.null(margin)){margin <- c(5,3,3,1)}
      op <- par(mar = margin)
      xValues <- barplot(Y, names.arg = "",
                         ylim = c(min(intEnds, 0, Y), max(intEnds, Y)),
                         main = title, col = col, ...)
      axis(side = 1, at = xValues, labels = FALSE) 
      text(x = xValues, y = par()$usr[3] - (par()$usr[4] - par()$usr[3])/20,
           labels = names(Y), srt=45, adj=1, xpd=TRUE)
      abline(h = 0, col = "gray20", lty = 2)
      if(interval != "no"){
        arrows(x0 = xValues[!dropInts], x1 = xValues[!dropInts], 
               y0 = intEnds[1,!dropInts], y1 = intEnds[2,!dropInts], 
               length = dev.size("in")[1]/length(xValues)/5, 
               angle = 90, code = 3, 
               col = intervalColor, lwd = 3)
      }
    }
  }
  else if(type == "box"){
    if(is.null(col)){col <- "darkred"}
    if(is.null(pch)){pch <- 19}
    if(is.null(intervalColor)){intervalColor <- "skyblue"}
    if(horizontal){
      Y <- Y[rev(order)]
      if(is.null(margin)){margin <- c(3,6,3,1)}
      op <- par(mar = margin)
      if(is.null(x$perTree)){
        perTree <- data.frame(rbind(Y, Y))
      } else{
        perTree <- x$perTree[, whichVar, drop = FALSE][,rev(order)]
      }
      box <- boxplot(perTree, yaxt = "n",
                     main = title, col = intervalColor,
                     medlwd = 1,
                     outpch = "x", outcex = .5, horizontal = horizontal, ...)
      axis(side = 2, at = X, labels = FALSE) 
      text(x = par()$usr[1] - (par()$usr[2] - par()$usr[1])/100,
           y = X,
           labels = names(Y), pos = 2, xpd=TRUE)
      abline(v = 0, col = "gray20", lty = 2)
      points(Y, X, col = col, pch = pch, ...)
    } else {
      Y <- Y[order]
      if(is.null(margin)){margin <- c(5,3,3,1)}
      op <- par(mar = margin)
      if(is.null(x$perTree)){
        perTree <- data.frame(rbind(Y, Y))
      } else{
        perTree <- x$perTree[, whichVar, drop = FALSE][,order]
      }
      box <- boxplot(perTree, xaxt = "n",
                     main = title, col = intervalColor,
                     medlwd = 1,
                     outpch = "x", outcex = .5, ...)
      axis(side = 1, at = X, labels = FALSE) 
      text(x = X, 
           y = par()$usr[3] - (par()$usr[4] - par()$usr[3])/20,
           labels = names(Y), srt=45, adj=1, xpd=TRUE)
      abline(h = 0, col = "gray20", lty = 2)
      points(X, Y, col = col, pch = pch, ...)
    }
  }
  else {
    if(is.null(col)){col <- "darkred"}
    if(is.null(intervalColor)){intervalColor <- "skyblue"}
    if (!requireNamespace("scales", quietly = TRUE)) {
      message("To get nicer plots, please install package \"scales\".")
    } else {intervalColor <- scales::alpha(intervalColor, .5)} 
    if(horizontal){
      Y <- Y[rev(order)]
      intEnds <- intEnds[,rev(order)]
      dropInts <- dropInts[rev(order)]
      if(is.null(margin)){margin <- c(3,6,3,1)}
      op <- par(mar = margin)
      plot(x = Y, y = X, type = "n", yaxt = "n",
           xlim = c(min(intEnds), max(intEnds, Y)), 
           ylim = c(1-1/nVar, nVar+1/nVar),
           xlab = "", ylab = "", main = title)
      abline(v = 0, col = "gray20", lty = 2)
      axis(side = 2, at = X, labels = FALSE) 
      text(x = par()$usr[1] - (par()$usr[2] - par()$usr[1])/100, 
           y = X, labels = names(Y), pos = 2, xpd=TRUE)
      halfWidth <- dev.size("in")[1]/length(Y)/10
      for(int in which(!dropInts)){
        polygon(x = c(intEnds[1,int], intEnds[1,int], 
                      intEnds[2,int], intEnds[2,int]),
                y = c(X[int] - .3, X[int] + .3, 
                      X[int] + .3, X[int] - .3), 
                col = intervalColor, 
                border = NA)
      }
      if (type == "dot"){
        if(is.null(pch)){pch <- 19}
        points(x = Y, y = X, type = "p", pch = pch, col = col, ...)
      } else if(type == "rank"){
        labels <- as.character(Ranks[whichVar])[rev(order)]
        text(x = Y, y = X, labels = labels, col = col, ...)
      }
    }
    else {
      Y <- Y[order]
      intEnds <- intEnds[,order]
      dropInts <- dropInts[order]
      if(is.null(margin)){margin <- c(5,3,3,1)}
      op <- par(mar = margin)
      plot(x = X, y = Y, type = "n", xaxt = "n",
           ylim = c(min(intEnds), max(intEnds, Y)), 
           xlim = c(1-1/nVar, nVar+1/nVar),
           xlab = "", ylab = "", main = title)
      abline(h = 0, col = "gray20", lty = 2)
      axis(side = 1, at = X, labels = FALSE) 
      text(x = X, y = par()$usr[3] - (par()$usr[4] - par()$usr[3])/20,
           labels = names(Y), srt=45, adj=1, xpd=TRUE)
      for(int in which(!dropInts)){
        polygon(x = c(X[int] - .3, X[int] + .3, 
                      X[int] + .3, X[int] - .3),
                y = c(intEnds[1,int], intEnds[1,int], 
                      intEnds[2,int], intEnds[2,int]), 
                col = intervalColor, 
                border = NA)
      }
      if (type == "dot"){
        if(is.null(pch)){pch <- 19}
        points(x = X, y = Y, type = "p", pch = pch, col = col, ...)
      } else if(type == "rank"){
        labels <- as.character(Ranks[whichVar])[order]
        text(x = X, y = Y, labels = labels, col = col, ...)
      }
    }
  }
} 

