##------------[Draw column charts]------------------------

#' Column Chart Based on \pkg{ggplot2}
#'
#' Columns (vertical bar) chart using ggplot2, with or without benchmarking lines.
#'
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param data Data frame for ploting.
#' @param xvar Variable name of x.
#' @param yvar Variable name of y.
#' @param title String. Chart title. Default \code{paste("Chart:", yvar,"~", xvar)}
#' @param gvar Variable name of grouping factor.
#' @param position 'dodge' or 'stack'.
#' @param annotate Format of the annotation. 'number(n)', 'percent(n)', 'comma'
#' or 'dollar'.
#' @param xlab Label wording of x axis. Default \code{xvar}.
#' @param ylab Label wording of y axis. Default \code{yvar}.
#' @param palette Palette name, default 'aetnagreen'
#' @param hline A data frame with columns of 'label', 'linetype' and 'value'.
#' Each record makes a horizontal line.
#' @param vline A data frame with columns of 'label', 'linetype' and 'value'.
#' Each record makes a vertical line.
#' @param line A list. Default value is \code{list(list(label=NA, linetype='solid',
#' arrow=FALSE, value=data.frame(x=NA, y=NA)))}. You should define the label,
#' linetype, arrow and value (a data frame consisting of x and y) of the
#' free-style line.
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
#' @importFrom stringr str_detect
#' @importFrom scales pretty_breaks
#' @importFrom reshape2 dcast
#'
#' @seealso \code{\link{named_pal}}
#'
#' @examples
#' \dontrun{
#' }
chartColumn <- function(
    data, xvar, yvar, gvar=NULL, title=paste("Chart:", yvar, "~", xvar),
    position="dodge",   # dodge or stack
    annotate="number2", # number(n), percent(n), comma or dollar
    xlab=xvar, ylab=yvar, palette=getOption("init.pal"),
    hline=data.frame(label=NA, linetype='solid', value=NA),
    vline=data.frame(label=NA, linetype='solid', value=NA),
    line=list(list(label=NA, linetype='solid', arrow=FALSE,
                   value=data.frame(x=NA, y=NA))),
    alt.font='SimHei', theme='wsj',
    plot.background='antiquewhite1',
...){

    if (!all(xvar %in% names(data))) stop("x variable not found!")
    if (!all(yvar %in% names(data))) stop("y variable not found!")
    if (!is.null(gvar)) if (!all(gvar %in% names(data)))
        stop("group variable not found!")
    if (!is.null(annotate)) if (! str_detect(
        annotate, "percent\\d{0,1}|dollar|comma|number\\d{0,}"))
        stop("annotate accepts percent,percent1, percent2,..., number,number1,number2,...
             and comma, dollar")
    if (! tolower(theme) %in% c(
        'bw','classic','dark','gray','grey','light','linedraw', 'minimal','base',
        'calc','excel','economist', 'economist_white','few','fivethirtyeight',
        'gdocs','hc','igray','map','pander','solarized', 'solarized_2','solid',
        'stata','tufte','wsj'))
        stop("Only accepts themes of pkg ggplots and ggthemes")

    if (plot.background %in% colors()) {
        plot.background <- rgb(t(col2rgb(plot.background)), alpha=100, max=255)
    }else if (length(plot.background) %in% 3:4 && all(is.numeric(plot.background))){
        if (max(plot.background)>255 || min(plot.background)<0)
            stop("if plot.background is a vector, they must be 0-255")
        plot.background <- as.matrix(c(plot.background), ncol=1)
        plot.background <- rgb(t(plot.background), alpha=100, max=255)
    }else{
        plot.background <- rgb(1, 1, 1, 0)
    }
    ylab <- ifelse(is.null(ylab), yvar, ylab)
    xlab <- ifelse(is.null(xlab), xvar, xlab)

    if (length(fonts())<10) loadfonts(quiet=TRUE)

    if (!is.null(gvar)) {
        data <- data[, c(xvar, yvar, gvar)]
        names(data) <- c("x", "y", "g")
        if (is.factor(data$g)) {
            levels(data$g) <- enc2native(levels(data$g))
        }else{
            data$g <- factor(data$g, exclude=NULL, levels=unique(data$g))
        }
    }else {
        data <- data[, c(xvar, yvar)]
        names(data) <- c("x", "y")
        if (xvar!=xlab){
            data$g <- factor(xlab, exclude=NULL, levels=unique(xlab))
        }else{
            data$g <- factor(xvar, exclude=NULL, levels=unique(xvar))
        }
    }

    palette <- rep(getHexPal(palette),
                   ceiling(nlevels(data$g)/length(getHexPal(palette))))
    if (!is.numeric(data$x))
        data$x <- factor(data$x, levels=unique(data$x), exclude=NULL)
    if (nlevels(data$g) < 3){
        barWidth <- nlevels(data$g)/(0.5+nlevels(data$g))
    }else{
        barWidth <- nlevels(data$g)/(1+nlevels(data$g))
    }
    data$y <- as.numeric(data$y)

    if (position=="dodge"){
        gph <- ggplot(data, aes(x=x, group=g, y=y, ymax=1.1*max(y, na.rm=TRUE)))+
            geom_bar(stat='identity', width=barWidth * 0.9, alpha=0.7,
                     position=position_dodge(barWidth), aes(fill=g, color=g))
    }else{
        tmpDf <- dcast(data, x~g, value.var="y", sum, margins=TRUE)
        gph <- ggplot(data, aes(
            x=x, group=g, y=y,
            ymax=1.1*max(tmpDf[1:(nrow(tmpDf)-1), '(all)'], na.rm=TRUE)
            )) +
            geom_bar(stat='identity', width=barWidth * 0.9, alpha=0.7,
                     position=position_stack(), aes(fill=g, color=g))
    }

    if (is.factor(data$x)) levels(data$x) <- enc2native(levels(data$x))

    font.legend <- .shiftFont(levels(data$g), alt.font)
    font.axis.x <- .shiftFont(levels(data$x), alt.font)

    gph <- gph + eval(parse(text=paste0("theme_", theme, "(", ggthemeArgs(theme),")")))+
        ylab(ylab)+ xlab(xlab)+ ggtitle(title)+
        theme(text=element_text(family='sans'),
              plot.title=element_text(size=10), legend.title=element_blank(),
              legend.background=element_blank(),
              legend.margin=margin(0, 0, 0, 0, unit='line'),
              legend.key=element_rect(colour='black'),
              legend.key.size=unit(0.8, 'lines'),
              legend.text=element_text(family=font.legend, size=7),
              legend.position=c(1, 1.05),
              legend.direction='horizontal',
              legend.justification=c(1, 0.5),
              axis.title=element_text(size=9),
              axis.title.y=element_text(
                  angle=90, family=.shiftFont(ylab, alt.font), hjust=0.5),
              axis.title.x=element_text(family=.shiftFont(xlab,alt.font), vjust=0.5),
              axis.text.x=element_text(
                  family=font.axis.x, size=ifelse(
                      max(nchar(as.character(data$x)), na.rm=TRUE) > 20 ||
                          sum(nchar(as.character(data$x)), na.rm=TRUE) > 50, 6, 7),
                  angle=ifelse(sum(nchar(levels(data$x)), na.rm=TRUE) >= 80, 30, 0),
                  vjust=ifelse(sum(nchar(levels(data$x)), na.rm=TRUE) >= 80, 0.9, 0.9),
                  hjust=ifelse(sum(nchar(levels(data$x)), na.rm=TRUE) >= 80, 1, 0.5)),
              axis.text.y=element_text(size=7, hjust=0.5, vjust=0.5),
              panel.grid.major=element_line(colour='gray90'),
              panel.background=element_rect(fill='white'),
              plot.margin=unit(c(0.5, 0.5, 0.2, 0.5), units='line'),
              plot.background=element_rect(fill=plot.background)
        )
    scale_breaks <- levels(data$g)
    if (position=="dodge"){
        y_breaks <- y_breaks0 <- pretty_breaks(n=4)(c(0, max(data$y, na.rm=TRUE)))
    }else{
        y_breaks <- y_breaks0 <-
            pretty_breaks(n=4)(c(0, max(tmpDf[1:(nrow(tmpDf)-1),'(all)'],
                                               na.rm=TRUE)))
    }
    hline <- hline[!is.na(hline$value) & hline$linetype %in% c(
        "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), ]
    if (nrow(hline) > 0){
        hline$label[is.na(hline$label)] <- hline$value[is.na(hline$label)]
        #font.line <- .shiftFont(hline$value,alt.font)
        for (lty in unique(hline$linetype)){
            gph <- gph + geom_hline(
                aes(yintercept=value, color=label), data=hline[hline$linetype == lty,],
                linetype=lty, alpha=0.8, size=0.5)
        }
        y_breaks <- c(hline$value,y_breaks)
        #         if (is.null(gvar)){
        #             scale_breaks <- c(ylab,unique(as.character(hline$label)))
        #             gph <- gph + scale_fill_manual(breaks=scale_breaks,values=palette)+
        #                 scale_color_manual(breaks=scale_breaks, values=palette)
        #         }else{
        scale_breaks <- unique(c(levels(data$g), unique(as.character(hline$label))))
        #        }
    }
    vline <- vline[!is.na(vline$value) & vline$linetype %in% c(
        "solid", "dashed", "dotted", "dotdash", "longdash", "twodash"), ]
    if (nrow(vline)>0){
        vline$label[is.na(vline$label)] <- vline$value[is.na(vline$label)]
        #font.line <- .shiftFont(vline$value,alt.font)
        for (lty in unique(vline$linetype)){
            gph <- gph + geom_vline(
                aes(xintercept=value, color=label), data=vline[vline$linetype == lty, ],
                linetype=lty, alpha=0.8, size=0.5)
        }
        #         if (is.null(gvar)){
        #             scale_breaks <- c(ylab,unique(as.character(hline$label)))
        #             gph <- gph + scale_fill_manual(breaks=scale_breaks, values=palette)+
        #                 scale_color_manual(breaks=scale_breaks, values=palette)
        #         }else{
        if (length(scale_breaks)>0){
            scale_breaks <- unique(c(scale_breaks, unique(hline$label)))
        }else{
            scale_breaks <- unique(c(levels(data$g), unique(hline$label)))
        }
        #         }
    }
    for (i in seq_len(length(line))){
        if (is.na(line[[i]][['label']])) {
            line[[i]] <- NULL
        }else{
            dfline <- line[[i]][['value']]
            names(dfline) <- c('x', 'y')
            dfline$g <- factor(line[[i]][['label']])
            if (line[[i]][['arrow']]){
                gph <- gph + geom_line(
                    data=dfline, aes(x=x, y=y, color=g, fill=g), size=0.75,
                    linetype=line[[i]][['linetype']],
                    arrow=arrow(), alpha=0.75)
            }else{
                gph <- gph + geom_line(
                    data=dfline, aes(x=x, y=y, color=g, fill=g), size=0.75,
                    linetype=line[[i]][['linetype']], alpha=0.75)
            }
            if (length(scale_breaks)>0){
                scale_breaks <- unique(c(scale_breaks, unique(line[[i]][['label']])))
            }else{
                scale_breaks <- unique(c(levels(data$g), unique(line[[i]][['label']])))
            }
        }
    }

    gph <- gph + scale_fill_manual(breaks=scale_breaks,
                                   values=palette[order(scale_breaks)],
                                   labels=paste(scale_breaks, "  ")) +
        scale_color_manual(breaks=scale_breaks,
                           values=palette[order(scale_breaks)],
                           labels=paste(scale_breaks, "  "))

    if (!is.null(annotate)){
        if (str_detect(annotate, "percent\\d{0,1}")) {
            annotate_digit <- as.numeric(sub("percent(\\d{0,1})$","\\1", annotate))
            if (is.na(annotate_digit)) annotate_digit <- 0
            data$annotate <- convNum2Pct(data$y, annotate_digit)
            if (nrow(hline)>0) hline$annotate <- convNum2Pct(hline$value, annotate_digit)
            if (nrow(vline)>0) vline$annotate <- convNum2Pct(vline$value, annotate_digit)
            #y_breaks=round(y_breaks,annotate_digit+2)
            gph <- gph + scale_y_continuous(
                breaks=y_breaks, labels=convNum2Pct(y_breaks, annotate_digit))
        }
        if (annotate=='comma') {
            data$annotate <- comma(data$y)
            if (nrow(hline)>0) hline$annotate <- comma(hline$value)
            if (nrow(vline)>0) vline$annotate <- comma(vline$value)
            gph <- gph + scale_y_continuous(breaks=y_breaks, labels=comma)
        }
        if (annotate=='dollar') {
            data$annotate <- dollar(data$y)
            if (nrow(hline)>0) hline$annotate <- dollar(hline$value)
            if (nrow(vline)>0) vline$annotate <- dollar(vline$value)
            gph <- gph + scale_y_continuous(breaks=y_breaks, labels=dollar)
        }
        if (str_detect(annotate, "number\\d{0,}")){
            annotate_digit <- sub("number(\\d{0,})$", "\\1", annotate)
            if (annotate_digit=="") annotate_digit <- 0
            data$annotate <- sprintf(paste0("%.", annotate_digit, "f"), data$y)
            if (nrow(hline)>0)
                hline$annotate <- sprintf(paste0("%.", annotate_digit, "f"),
                                          hline$value)
            if (nrow(vline)>0)
                vline$annotate <- sprintf(paste0("%.", annotate_digit, "f"),
                                          vline$value)
        }
        gph <- gph + geom_text(
            data=data, size=2, vjust=-0.5, family=.shiftFont(gph$annotate),
            aes(label=annotate), position=position_dodge(width=barWidth))
    }else{
        gph <- gph + scale_y_continuous(breaks=y_breaks)
    }
    #     if (nrow(hline)>0)        # add annotates of the hline one by one
    #         for (i in seq_len(nrow(hline))){
    #             if (is.numeric(data$x)){
    #                 gph <- gph + geom_text(x=max(data$x)*1.01,
    #                                        y=hline[i,"value"],vjust=-0.5,
    #                                        label=hline[i,'annotate'],size=3,family='sans',
    #                                        face='plain')
    #             }else {
    #                 gph <- gph + geom_text(x=length(unique(data$x)),
    #                                        x=0,y=hline[i,"value"],
    #                                        vjust=-0.25,hjust=-0.1,face='plain',
    #                                        label=hline[i,'annotate'],size=3,family='sans')
    #             }
    #         }
    if (nrow(vline)>0)
        for (i in seq_len(nrow(vline))){
            if (is.numeric(data$y)){
                gph <- gph + geom_text(
                    y=max(data$y, na.rm=TRUE)*1.01,
                    x=hline[i, "value"], hjust=-0.5,
                    label=vline[i,'annotate'], size=2, family='sans', face='plain')
            }else {
                gph <- gph + geom_text(
                    y=length(unique(data$x)),
                    x=hline[i, "value"],
                    hjust=-0.5, vjust=-0.25, face='plain',
                    label=vline[i,'annotate'], size=2, family='sans')
            }
        }
    #     gph <- gph + guides(fill = guide_legend(override.aes = list(colour = NULL),
    #                                             nrow=ceiling(nlevels(data$g)/10)))
    # if too crowded, make x-axis annotates like '-_-_-_-'
    if (is.factor(data$x) && sum(nchar(levels(data$x)), na.rm=TRUE) >= 40 &&
        sum(nchar(levels(data$x)), na.rm=TRUE) < 80){
        scale_x_lab <- paste0(c("","\n"), unique(data$x), c("\n", ""))
        gph <- gph + scale_x_discrete(labels=scale_x_lab)
    }
    gph
}
