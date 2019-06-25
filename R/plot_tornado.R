##------[Draw Population Pyramid]----------------
#' Tornado Chart Based on \pkg{ggplot2}
#'
#' Tornado (stacked bar) chart using ggplot2.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Data frame for ploting.
#' @param xvar Variable name of x.
#' @param freq Variable name of y.
#' @param gvar Variable name of grouping factor.
#' @param title Chart title. Default \code{paste("Chart:", freq, "~", xvar)}.
#' @param annotate Format of the annotation. Currently not working.
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
#' @importFrom reshape2 dcast
#' @importFrom extrafont fonts loadfonts
#'
#' @seealso \code{\link{named_pal}}, \code{\link{ggplot2}}
#'
#' @examples
#'
chartTornado <- function(data, xvar, freq=NULL, gvar,
                         title=paste("Chart:", freq, "~", xvar),
                         annotate=NULL, xlab=xvar, ylab=freq,
                         palette=getOption("init.pal"),
                         alt.font='SimHei', theme='wsj',
                         plot.background='antiquewhite1', ...){
    if (! tolower(theme) %in% c('bw','classic','dark','gray','grey','light','linedraw',
                                'minimal','base','calc','excel','economist',
                                'economist_white','few','fivethirtyeight',
                                'gdocs','hc','igray','map','pander','solarized',
                                'solarized_2','solid','stata','tufte','wsj'))
        stop("Only accepts themes of pkg ggplots and ggthemes")
    if (nlevels(as.factor(data[, gvar])) != 2)
        stop("gvar can only have 2 levels!")
    if (!is.null(freq)) {
        data <- data[,c(xvar,freq,gvar)]
        names(data) <- c("x", "y", "g")
    }else {
        data <- data[,c(xvar,gvar)]
        names(data) <- c("x", "g")
    }
    data$x <- as.factor(data$x)
    data$g <- as.factor(data$g)
    levels(data$x) <- enc2native(levels(data$x))
    levels(data$g) <- enc2native(levels(data$g))
    if (plot.background %in% colors() ||
        str_detect(plot.background, "#[[:xdigit:]]{6}")) {
        plot.background <-
            rgb(t(col2rgb(plot.background)), alpha=100, max=255)
    }else if (str_detect(plot.background, "#[[:xdigit:]]{8}")) {

    }else if (length(plot.background) %in% 3:4 & all(is.numeric(plot.background))){
        if (max(plot.background)>255 | min(plot.background)<0)
            stop("if plot.background is a vector, they must be 0-255")
        plot.background <- as.matrix(c(plot.background), ncol=1)
        plot.background <- rgb(t(plot.background), alpha=100, max=255)
    }else{
        plot.background <- rgb(1, 1, 1, 0)
    }

    # loadPkg("ggplot2");loadPkg("ggthemes");loadPkg("scales");loadPkg("extrafont")
    # loadPkg("grid");loadPkg("plyr");loadPkg("reshape2")

    if (length(fonts())<10) loadfonts(quiet=TRUE)

    font.legend <- .shiftFont(levels(data$g), alt.font)
    font.axis.x <- .shiftFont(levels(data$x), alt.font)
    if (is.null(freq)){
        data <- dcast(data, x~g, length, value.var='x')
        data <- melt(data,id='x')
        names(data) <- c('x', "g", 'y')
    }
    sumFreq <- sum(data$y, na.rm=TRUE)
    data$y <- data$y/sumFreq
    data$y[data$g==levels(data$g)[1]] <- -data$y[data$g==levels(data$g)[1]]
    gph <- ggplot(data,aes(x=x, y=y, fill=g, color=g)) +
        geom_bar(position='stack', stat="identity",
                 width=0.9,alpha=0.7)
    gph <- gph + coord_flip()+
        eval(parse(text=paste0("theme_", theme, "(", ggthemeArgs(theme), ")")))+
        scale_fill_manual(values=named_pal(palette)(nlevels(data$g)),
                          breaks=levels(data$g),
                          labels=paste(levels(data$g), " "))+
        scale_color_manual(breaks=levels(data$g),
                           values=named_pal(palette)(nlevels(data$g)),
                           labels=paste(levels(data$g), " "), guide='none')+
        scale_x_discrete(limits=levels(data$x))+
        scale_y_continuous(breaks=seq(-1, 1, 0.1),
                           labels=paste0(abs(seq(-100, 100, 10)), "%"))+
        ylab(ylab)+ xlab(xlab)+ ggtitle(title) +
        theme(text=element_text(family='sans'),
              plot.title=element_text(size=10), legend.title=element_blank(),
              legend.background=element_blank(),
              legend.margin=margin(0, 0, 0, 0, unit='line'),
              legend.key=element_rect(colour='black', fill='white'),
              legend.key.size=unit(0.8,'lines'),
              legend.text=element_text(family=font.legend, size=7),
              legend.position='top',
              legend.direction='horizontal',
              legend.justification=c(1, 0.5),
              axis.title=element_text(family='sans',size=9),
              axis.title.y=element_text(angle=90,family=.shiftFont(ylab, alt.font),
                                        vjust=-0.25),
              axis.title.x=element_text(family=.shiftFont(xlab, alt.font), vjust=-0.25),
              axis.text.x=element_text(
                  family=font.axis.x, size=7, vjust=-0.25, angle=ifelse(
                      max(nchar(as.character(data$x)), na.rm=TRUE)>20, 15, 0)),
              axis.text.y=element_text(vjust=-0.25),
              panel.grid.major=element_line(colour='gray90'),
              panel.background=element_rect(fill='white'),
              plot.margin=unit(c(0.5, 0.5, 0.2, 0.5),units='line'),
              plot.background=element_rect(fill=plot.background))
    #     gph <- gph +
    #           guides(fill = guide_legend(override.aes = list(colour = NULL),
    #                                  nrow=ceiling(nlevels(data$g)/10)))
    gph
}

