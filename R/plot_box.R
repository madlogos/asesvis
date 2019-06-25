## ------[Draw Box Chart]--------------
#' Box Chart Based on \pkg{ggplot2}
#'
#' Box chart using ggplot2.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Data frame for ploting.
#' @param xvar Variable name of x.
#' @param yvar Variable name of y.
#' @param gvar Variable name of grouping factor.
#' @param horizontal Logical. If TRUE, the boxes are horizontally presented.
#' Default FALSE.
#' @param title Chart title. Default \code{paste("Chart: boxplot of", yvar," by", xvar)}.
#' @param xlab Label wording of x axis. Default \code{xvar}.
#' @param ylab Label wording of y axis. Default \code{yvar}.
#' @param palette Palette name, default 'aetnagreen'
#' @param alt.font Define alternative font name for plotting. Default 'SimHei'.
#' @param theme The theme name. Only accepts themes in \pkg{ggplot2} and \pkg{ggthemes}.
#' Default 'wsj'.
#' @param plot.background The color name of the plot background.
#' Default 'antiquewhite1'.
#' @param ... ignore
#'
#' @return A ggplot2 object.
#' @export
#' @import ggthemes
#' @import ggplot2
#' @importFrom extrafont fonts loadfonts
#' @importFrom reshape2 dcast
#'
#' @seealso \code{\link{named_pal}}, \code{\link{ggplot2}}, \code{\link{ggthemes}}
#'
#' @examples
#'
chartBox <- function(data, xvar, yvar, gvar=NULL, horizontal=FALSE, xlab=xvar, ylab=yvar,
                     title=paste("Chart: boxplot of", yvar, "by", xvar),
                     palette=getOption("init.pal"), alt.font='SimHei', theme='wsj',
                     plot.background='antiquewhite1', ...){
    if (! tolower(theme) %in% c(
        'bw','classic','dark','gray','grey','light','linedraw','minimal','base',
        'calc','excel','economist','economist_white','few','fivethirtyeight',
        'gdocs','hc','igray','map','pander','solarized','solarized_2','solid',
        'stata','tufte','wsj'))
        stop("Only accepts themes of pkg ggplots and ggthemes")
    if (plot.background %in% colors()) {
        plot.background <-
            rgb(t(col2rgb(plot.background)), alpha=100, max=255)
    }else if (length(plot.background) %in% 3:4 & all(is.numeric(plot.background))){
        if (max(plot.background)>255 | min(plot.background)<0)
            stop("if plot.background is a vector, they must be 0-255")
        plot.background <- as.matrix(c(plot.background), ncol=1)
        plot.background <- rgb(t(plot.background),alpha=100, max=255)
    }else{
        plot.background <- rgb(1,1,1,0)
    }
    # loadPkg("ggplot2");loadPkg("ggthemes");loadPkg("scales");loadPkg("extrafont")
    # loadPkg("grid");loadPkg("reshape2")
    if (length(fonts()) < 10) loadfonts(quiet=TRUE)
    if (!is.factor(data[, xvar]))
        data[, xvar] <- factor(data[, xvar], levels=unique(data[, xvar]))
    if (is.null(gvar)){
        data <- data[,c(xvar, yvar)]
        names(data) <- c("x", "y")
        gph <- ggplot(data, aes(x=x, y=y))+
            geom_boxplot(alpha=0.7, fill=named_pal(palette)(1),
                         outlier.colour="grey40", colour="grey40")
    }else{
        data <- data[,c(xvar, yvar, gvar)]
        names(data) <- c("x", "y", "g")
        if (!is.factor(data$g))
            data$g <- factor(data$g, levels=unique(data$g))
        gph <- ggplot(data,aes(x=x, y=y, fill=g))+
            geom_boxplot(aes(x=x, fill=g, color=g), alpha=0.7, outlier.colour="grey40",
                         colour="grey40")+
            scale_fill_manual(values=named_pal(palette), breaks=levels(data$g),
                              labels=paste(levels(data$g), " "))
    }

    font.legend <- .shiftFont(levels(data$g), alt.font)
    font.axis.x <- .shiftFont(levels(data$x), alt.font)
    if (horizontal) gph <- gph + coord_flip()
    gph <- gph + eval(parse(text=paste0("theme_", theme, "(", ggthemeArgs(theme), ")")))+
        ylab(ylab)+ xlab(xlab)+ggtitle(title) +
        theme(text=element_text(family='sans'),
              plot.title=element_text(size=10), legend.title=element_blank(),
              legend.background=element_blank(),
              legend.margin=margin(0, 0, 0, 0, unit='line'),
              legend.key=element_rect(colour='black',fill='white'),
              legend.key.size=unit(0.8,'lines'),
              legend.text=element_text(family=font.legend, size=7),
              legend.position=c(1, 1.05),
              legend.direction='horizontal',
              legend.justification=c(1,0.5),
              axis.title=element_text(family='sans',size=9),
              axis.title.y=element_text(angle=90,family=.shiftFont(ylab, alt.font),
                                        vjust=-0.25),
              axis.title.x=element_text(family=.shiftFont(xlab, alt.font), vjust=-0.25),
              axis.text.x=element_text(
                  family=font.axis.x, size=ifelse(
                      max(nchar(levels(data$x)), na.rm=TRUE)>20 ||
                          sum(nchar(levels(data$x)), na.rm=TRUE)>50, 6, 7),
                  angle=ifelse(sum(nchar(levels(data$x)), na.rm=TRUE)>=80, 30, 0),
                  vjust=ifelse(sum(nchar(levels(data$x)), na.rm=TRUE)>=80 ,0.9, 0.9),
                  hjust=ifelse(sum(nchar(levels(data$x)), na.rm=TRUE)>=80, 1, 0.5)),
              axis.text.y=element_text(vjust=-0.25, size=7),
              panel.grid.major=element_line(colour='gray90'),
              panel.background=element_rect(fill='white'),
              plot.margin=unit(c(0.5, 0.5, 0.2, 0.5), units='line'),
              plot.background=element_rect(fill=plot.background))
    # if too crowded, make x-axis annotates like '-_-_-_-'
    if (is.factor(data$x)){
        if (sum(nchar(levels(data$x)), na.rm=TRUE) >= 40 &&
            sum(nchar(levels(data$x)), na.rm=TRUE) < 80){
            scale_x_lab <- paste0(c("","\n"),levels(data$x),c("\n",""))
            gph <- gph + scale_x_discrete(labels=scale_x_lab, limits=levels(data$x))
        }else{
            gph <- gph + scale_x_discrete(limits=levels(data$x))
        }
    }
    gph
}

