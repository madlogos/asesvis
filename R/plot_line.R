##------[Draw Line Chart]--------------

#' Line Chart Based on \pkg{ggplot2}
#'
#' Line chart using ggplot2.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Data frame for ploting.
#' @param xvar Variable name of x.
#' @param yvar Variable name of y.
#' @param gvar Variable name of grouping factor.
#' @param title Chart title. Default \code{paste("Chart:",yvar,"~",xvar)}.
#' @param annotate Format of the annotation. 'number(n)', 'percent(n)', 'comma'
#' or 'dollar'.
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
#' @importFrom scales pretty_breaks
#' @importFrom reshape2 dcast
#'
#' @seealso \code{\link{getHexPal}}, \code{\link{ggplot2}}
#'
#' @examples
#'
chartLine <- function(
    data, xvar, yvar, gvar=NULL, title=paste("Chart:", yvar, "~", xvar),
    annotate=NULL, xlab=NULL, ylab=NULL, palette=.pkgPara()$init.pal,
    alt.font='SimHei', theme='wsj', plot.background='antiquewhite1',
...){
    if (!all(xvar %in% names(data))) stop("x variable not found!")
    if (!all(yvar %in% names(data))) stop("y variable not found!")
    if (!is.null(gvar)) if (!all(gvar %in% names(data)))
        stop("group variable not found!")
    if (!is.null(annotate)) if (! str_detect(annotate, "percent\\d{0,1}|dollar|comma"))
        stop("annotate accepts percent,percent1, percent2,... and comma, dollar")
    if (! tolower(theme) %in% c(
        'bw', 'classic', 'dark', 'gray','grey','light','linedraw', 'minimal', 'base',
        'calc', 'excel', 'economist', 'economist_white', 'few', 'fivethirtyeight',
        'gdocs', 'hc', 'igray', 'map', 'pander', 'solarized', 'solarized_2','solid',
        'stata', 'tufte', 'wsj'))
        stop("Only accepts themes of pkg ggplots and ggthemes")
    palette <- rep(getHexPal(palette),10)
    if (plot.background %in% colors()) {
        plot.background <-
            rgb(t(col2rgb(plot.background)), alpha=100, max=255)
    }else if (length(plot.background) %in% 3:4 & all(is.numeric(plot.background))){
        if (max(plot.background)>255 | min(plot.background) < 0)
            stop("if plot.background is a vector, they must be 0-255")
        plot.background <- as.matrix(c(plot.background), ncol=1)
        plot.background <- rgb(t(plot.background), alpha=100, max=255)
    }else {
        plot.background <- rgb(1, 1, 1, 0)
    }
    ylab <- ifelse(is.null(ylab), yvar, ylab)
    xlab <- ifelse(is.null(xlab), xvar, xlab)
    # loadPkg("ggplot2");loadPkg("ggthemes");loadPkg("scales");loadPkg("extrafont")
    # loadPkg("grid")
    if (length(fonts())<10) loadfonts(quiet=TRUE)

    if (!is.null(gvar)) {
        data <- data[,c(xvar, yvar, gvar)]
        names(data) <- c("x", "y", "g")
        data$g <- as.factor(data$g)
        levels(data$g) <- enc2native(levels(data$g))
    }else {
        data <- data[, c(xvar, yvar)]
        names(data) <- c("x", "y")
        data$g <- factor(xvar)
    }
    #     if (any(data$g %in% c("Total","\u5408\u8BA1","\u603B\u8BA1","\u516C\u53F8","(all)"))){
    #         data$lineWidth <- as.factor(data$g %in% c("Total","\u5408\u8BA1","\u603B\u8BA1","\u516C\u53F8","(all)"))
    #         gph <- ggplot(data,aes(x=x,group=g,y=y,ymax=1.1*max(y,na.rm=TRUE)))+
    #             geom_line(stat='identity',alpha=0.8,aes(color=g,linetype=lineWidth))
    #     }else{
    gph <- ggplot(data, aes(x=x, group=g, y=y, ymax=1.1*max(y, na.rm=TRUE)))+
        geom_line(stat='identity', alpha=0.8, aes(color=g))
    # }

    if (is.factor(data$x)) levels(data$x) <- enc2native(levels(data$x))

    font.legend <- .shiftFont(levels(data$g), alt.font)
    font.axis.x <- .shiftFont(levels(data$x), alt.font)

    gph <- gph + eval(parse(text=paste0("theme_", theme, "(", ggthemeArgs(theme), ")"))) +
        ylab(ylab)+ xlab(xlab)+ ggtitle(title)+
        theme(text=element_text(family='sans'),
              plot.title=element_text(size=10), legend.title=element_blank(),
              legend.background=element_blank(),
              legend.margin=margin(0, 0, 0, 0, unit='line'),
              legend.key=element_rect(colour='black', fill='white'),
              legend.key.size=unit(0.8,'lines'),
              legend.text=element_text(family=font.legend, size=7),
              legend.position='top',
              legend.direction='horizontal',
              legend.justification=c(1,0.5),
              axis.title=element_text(size=9),
              axis.title.y=element_text(
                  angle=90,family=.shiftFont(ylab, alt.font), vjust=-0.25),
              axis.title.x=element_text(family=.shiftFont(xlab, alt.font), vjust=-0.25),
              axis.text.x=element_text(
                  family=font.axis.x, size=7, vjust=-0.25, angle=ifelse(
                      max(nchar(as.character(data$x)), na.rm=TRUE)>20, 15, 0)),
              axis.text.y=element_text(size=7, vjust=0.5, hjust=0.5),
              panel.grid.major=element_line(colour='gray90'),
              panel.background=element_rect(fill='white'),
              plot.margin=unit(c(0.5, 0.5, 0.2, 0.5), units='line'),
              plot.background=element_rect(fill=plot.background)
        )
    scale_breaks <- levels(data$g)
    y_breaks <- pretty_breaks(n=4)(c(0,max(data$y, na.rm=TRUE)))
    gph <- gph + scale_color_manual(breaks=scale_breaks, values=palette,
                                    labels=paste(scale_breaks," "))

    if (!is.null(annotate)){
        if (str_detect(annotate, "percent\\d{0,1}")) {
            annotate_digit <- as.numeric(sub("percent(\\d{0,1})$", "\\1", annotate))
            if (is.na(annotate_digit)) annotate_digit <- 0
            data$annotate <- convNum2Pct(data$y, annotate_digit)
            y_breaks <- round(y_breaks, annotate_digit+2)
            gph <- gph + scale_y_continuous(limits=c(0,max(y_breaks, na.rm=TRUE)),
                                            breaks=y_breaks,labels=percent)
        }
        if (annotate=='comma') {
            data$annotate <- comma(data$y)
            gph <- gph + scale_y_continuous(limits=c(0,max(y_breaks, na.rm=TRUE)),
                                            breaks=y_breaks,labels=comma)
        }
        if (annotate=='dollar') {
            data$annotate <- dollar(data$y)
            gph <- gph + scale_y_continuous(limits=c(0,max(y_breaks, na.rm=TRUE)),
                                            breaks=y_breaks,labels=dollar)
        }
        gph <- gph + geom_text(data=data,size=2,vjust=-0.5,
                               family=.shiftFont(data$annotate),
                               aes(label=annotate))
    }else{
        gph <- gph + scale_y_continuous(limits=c(0,max(y_breaks, na.rm=TRUE)),
                                        breaks=y_breaks) +
            geom_text(aes(label=sprintf("%.2f",y)),size=2,vjust=-0.5,
                      family='sans')
        if (nrow(hline)>0) hline$annotate <- sprintf("%.2f", hline$value)
        if (nrow(vline)>0) vline$annotate <- sprintf("%.2f", vline$value)
    }
    gph <- gph + guides(fill = guide_legend(override.aes = list(colour = NULL)),
                        color = guide_legend(nrow=ceiling(nlevels(data$g)/10)))
    gph
}
