
  #x <- as.dagitty(x)
  #.supportsTypes(x, c("dag", "mag", "pdag"))
  coords <- coordinates(x)
  labels <- names(coords$x)
  par(mar = rep(0, 4))
  plot.new()
  par(new = TRUE)
  wx <- sapply(paste0("mm", labels), function(s) strwidth(s, 
                                                          units = "inches"))
  wy <- sapply(paste0("\n", labels), function(s) strheight(s, 
                                                           units = "inches"))
  ppi.x <- dev.size("in")[1]/(max(coords$x) - min(coords$x))
  ppi.y <- dev.size("in")[2]/(max(coords$y) - min(coords$y))
  wx <- wx/ppi.x
  wy <- wy/ppi.y
  xlim <- c(min(coords$x - wx/2), max(coords$x + wx/2))
  ylim <- c(-max(coords$y + wy/2), -min(coords$y - wy/2))
  plot(NA, xlim = xlim, ylim = ylim, xlab = "", ylab = "", 
       bty = "n", xaxt = "n", yaxt = "n")
  wx <- sapply(labels, function(s) strwidth(paste0("xx", s)))
  wy <- sapply(labels, function(s) strheight(paste0("\n", 
                                                    s)))
  asp <- par("pin")[1]/diff(par("usr")[1:2])/(par("pin")[2]/diff(par("usr")[3:4]))
  ex <- edges(x)
  ax1 <- rep(0, nrow(ex))
  ax2 <- rep(0, nrow(ex))
  ay1 <- rep(0, nrow(ex))
  ay2 <- rep(0, nrow(ex))
  axc <- rep(0, nrow(ex))
  ayc <- rep(0, nrow(ex))
  acode <- rep(2, nrow(ex))
  has.control.point <- rep(FALSE, nrow(ex))
  for (i in seq_len(nrow(ex))) {
    if (ex[i, 3] == "<->") {
      acode[i] <- 3
      has.control.point[i] <- TRUE
    }
    if (ex[i, 3] == "--") {
      acode[i] <- 0
    }
    l1 <- as.character(ex[i, 1])
    l2 <- as.character(ex[i, 2])
    x1 <- coords$x[l1]
    y1 <- coords$y[l1]
    x2 <- coords$x[l2]
    y2 <- coords$y[l2]
    if (is.na(ex[i, 4]) || is.na(ex[i, 5])) {
      cp <- dagitty:::.autoControlPoint(x1, y1, x2, y2, asp, 0.2 * 
                                as.integer(acode[i] == 3))
    }
    else {
      cp <- list(x = ex[i, 4], y = ex[i, 5])
      has.control.point[i] <- TRUE
    }
    bi1 <- dagitty:::.lineSegBoxIntersect(x1 - wx[l1], y1 - wy[l1], 
                                x1 + wx[l1], y1 + wy[l1], x1, y1, cp$x, cp$y)
    bi2 <- dagitty:::.lineSegBoxIntersect(x2 - wx[l2], y2 - wy[l2], 
                                x2 + wx[l2], y2 + wy[l2], cp$x, cp$y, x2, y2)
    if (length(bi1) == 2) {
      x1 <- bi1$x
      y1 <- bi1$y
    }
    if (length(bi2) == 2) {
      x2 <- bi2$x
      y2 <- bi2$y
    }
    ax1[i] <- x1
    ax2[i] <- x2
    ay1[i] <- y1
    ay2[i] <- y2
    axc[i] <- cp$x
    ayc[i] <- cp$y
  }
  directed <- acode == 2 & !has.control.point
  undirected <- acode == 0 & !has.control.point
  arrows(ax1[directed], -ay1[directed], ax2[directed], -ay2[directed], 
         length = 0.1, col = "gray")
  segments(ax1[undirected], -ay1[undirected], ax2[undirected], 
           -ay2[undirected], col = "black", lwd = 2)
  for (i in which(has.control.point)) {
    dagitty:::.arc(ax1[i], -ay1[i], ax2[i], -ay2[i], axc[i], -ayc[i], 
         col = c("gray", "black")[1 + (acode[i] == 0)], code = acode[i], 
         length = 0.1, lwd = 1 + (acode[i] == 0))
  }
  text(coords$x, -coords$y[labels], labels)
