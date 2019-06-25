#' Hex color vector in a palette
#'
#' Get a vector of hex colors (lower case characters) or a hex color function
#' factory from a designated (named) palette.
#'
#' @details \code{getPalette} (\code{named_pal}) generally gets hex value of the
#' colors in a designated palette. The palettes include Aetna palette, palette
#' in packages \pkg{RColorBrewer}, \pkg{ggthemes} and \pkg{viridis}.
#' You can run \code{palettes.info} to check the
#' palettes information. \cr The following palettes are available:
#' \describe{
#'  \item{\strong{Aetna palttes}}{\describe{
#'    \item{'qual'}{'aetclassic_green', 'aetclassic_blue', 'aetclassic_teal',
#'     'aetclassic_cranberry', 'aetclassic_orange', 'aetclassic_violet',
#'     'aetnew_violet', 'aetnew_teal', 'aetnew_cranberry'}
#'    \item{'div'}{'aetdiv_violet', 'aetdiv_teal', 'aetdiv_cranberry'}
#'    \item{'seq'}{'aetdiv_violet', 'aetdiv_teal', 'aetdiv_cranberry'}}}
#'  \item{\strong{\pkg{\link[RColorBrewer]{RColorBrewer}} palettes}}{\describe{
#'    \item{'div'}{'BrBG', 'PiYG', 'PRGn', 'PuOr', 'RdBu', 'RdGy', 'RdYlBu',
#'     'RdYlGn', 'Spectral'}
#'    \item{'qual'}{'Accent', 'Dark2', 'Paired', 'Pastel1', 'Pastel2', 'Set1',
#'     'Set2', 'Set3'}
#'    \item{'seq'}{'Blues', 'BuGn', 'BuPu', 'GnBu', 'Greens', 'Greys', 'Oranges',
#'     'OrRd', 'PuBu', 'PuBuGn', 'PuRd', 'Purples', 'RdPu', 'Reds', 'YlGn',
#'     'YlGnBu', 'YlOrBr', 'YlOrRd'}}}
#'  \item{\strong{\pkg{\link{ggthemes}} palettes}}{\itemize{
#'    \item normal: \describe{
#'     \item{'qual'}{'calc', 'economist', 'economist_white', 'excel_line', 'excel_fill',
#'     'excel_new', 'few', 'fivethirtyeight', 'gdocs', 'pander', 'stata', 'stata1',
#'     'stata1r','statamono','hc', 'darkunica','wsj', 'wsj_rgby', 'wsj_red_green',
#'     'wsj_black_green', 'wsj_dem_rep', 'colorblind'}} \cr
#'    \item tableau (see \code{\link[ggthemes]{tableau_color_pal}}): \describe{
#'      \item{'regular' ('qual')}{"tableau10", "tableau20", "tbl_colorblind",
#'     "tbl_seattle_grays", "tbl_traffic", "tbl_miller_stone", "tbl_superfishel_stone",
#'     "tbl_nuriel_stone", "tbl_jewel_bright", "tbl_summer", "tbl_winter", "tbl_gnorte",
#'     "tbl_rdbubw", "tbl_pupkgy", "tbl_hue_circle", "tbl_classic10", "tbl_classic10_medium",
#'     "tbl_classic10_light", "tbl_classic20", "tbl_classic_gray5", "tbl_classic_colorblind",
#'     "tbl_classic_traffic_light", "tbl_classic_pugy6", "tbl_classic_pugy12",
#'     "tbl_classic_gnor6", "tbl_classic_gnor12", "tbl_classic_burd6", "tbl_classic_burd12",
#'     "tbl_classic_cyclic"}
#'     \item{'ordered-diverging' ('div')}{"tbl_orbu_div", "tbl_rdgn_div", "tbl_gnbu_div",
#'     "tbl_rdbu_div", "tbl_rdbl_div", "tbl_gdpu_div", "tbl_rdgngd_div",
#'     "tbl_sunset_sunrise_div", "tbl_orbuwi_div", "tbl_rdgnwi_div", "tbl_gnbuwi_div",
#'     "tbl_rdbuwi_div", "tbl_rdblwi_div", "tbl_orbu_light_div", "tbl_temperature_div",
#'     "tbl_classic_rdgn", "tbl_classic_rdbu", "tbl_classic_rdbl", "tbl_classic_area_rdgn",
#'     "tbl_classic_orbu", "tbl_classic_gnbu", "tbl_classic_rdwign", "tbl_classic_rdwibl",
#'     "tbl_classic_orwibu", "tbl_classic_rdwibl_light", "tbl_classic_orwibu_light",
#'     "tbl_classic_rdwign_light", "tbl_classic_rdgn_light"}
#'     \item{'ordered-sequential' ('seq')}{"tbl_bugn_seq", "tbl_blue_light", "tbl_orange_light",
#'     "tbl_blue", "tbl_orange", tbl_green", "tbl_red", "tbl_purple", "tbl_brown",
#'     "tbl_gray", "tbl_gray_warm", "tbl_bute", "tbl_orgd", "tbl_gngd", "tbl_rdgd",
#'     "tbl_classic_green", "tbl_classic_gray", "tbl_classic_blue", "tbl_classic_red",
#'     "tbl_classic_orange", "tbl_classic_area_red", "tbl_classic_area_green",
#'     "tbl_classic_area_brown"}
#'   } \cr
#'   \item solarized (see \code{\link[ggthemes]{solarized_pal}}): \describe{
#'    \item{'div'}{'solarized','solarized_red', 'solarized_yellow',
#'    'solarized_orange','solarized_magenta','solarized_violet',
#'    'solarized_blue', 'solarized_cyan', 'solarized_green'}}}}
#'  \item{\strong{\pkg{\link{viridis}} palettes}}{\describe{
#'   \item{'seq'}{'megma', 'inferno', 'plasma', 'viridis', 'cividis'}}}
#'  \item{\strong{\pkg{\link{grDevices}} palettes}}{\describe{
#'   \item{'qual'}{'rainbow'}
#'   \item{'seq'}{'terrain', 'topo', 'heat'}
#'   \item{'div'}{'cm'}}}
#' }
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param palette Character, the palette name (case insensitive).
#' @param n Numeric, the length of color vector. Default NULL, which indicates that
#' the function will return a function to generate color vectors.
#' @param show Logical, whether display the palette. Default FALSE. If \code{n} is
#' NULL, then \code{show} will be ignored.
#' @param mode Character, either "na.fill", "no.fill", or "repeat.fill". If \code{n} >
#' max number of colors of the palette, then \itemize{
#' \item na.fill: Default choice. NAs will be used to fill the output vector \cr
#' \item no.fill: the output color vector will not be suplemented \cr
#' \item repeat.fill: the color vector will be repeated to fill the length
#' }
#' @param ... Other arguments to pass to the function. E.g., \code{theme='Atlas'}
#' can be passed to \code{\link[ggthemes]{excel_new_pal}()} when \code{palette='excel_new'};
#' or \code{direction = -1} can be passed to \code{\link[ggthemes]{tableau_color_pal}}
#' when \code{palette='tableau10'}. You can always set \code{alpha} to change the
#' transparency of the output vector. \itemize{
#'  \item if \code{alpha} is between [0, 1] and is double, the [0, 1] alpha scale
#'  will be applied; \cr
#'  \item if \code{alpha} is integer (e.g., 2L, 155L, ...), the [0, 255] alpha
#'  scale will be applied.}
#'
#' @return \itemize{
#' \item \code{n} is not NULL: return a vector of Hex colors \cr
#' \item \code{n} is NULL: return the color generation function (function factory).}
#' @export
#'
#' @seealso
#' The palettes majorly come from \pkg{\link{RColorBrewer}}, \pkg{\link{ggthemes}},
#' and \pkg{\link{viridis}}. You can also refer to \pkg{\link{colorspace}} to build
#' your own palette. \cr
#' Refer to \code{\link{data:palettes.info}} to check built palettes info. And you
#' can use the generalized parser function \code{\link{hex_colors}} to get hex
#' colors more conveniently.
#'
#' @examples
#' \dontrun{
#' ## Get the entire Aetna classic green palette
#' f <- getPalette("aetclassic_gre")  # returns a function factory
#' f(8)  # returns first 8 colors
#'
#' ## Get 7 colors from palette 'terrain'
#' named_pal("terrain", 7)
#' }
getPalette <- function(
    palette, n=NULL, show=FALSE,  mode=c("na.fill", "no.fill", "repeat.fill"), ...){
    pal <- parse_palname(palette)
    getNamedPal(pal, n=n, show=show, mode=mode, ...)
}

#' @export
#' @rdname getPalette
named_pal <- getPalette

# ------method getNamedPal------
getNamedPal <- function(
    pal, n=NULL, show=FALSE, mode=c("na.fill", "no.fill", "repeat.fill"), ...)
    UseMethod(".getNamedPal", pal)


#' @importFrom stringr str_to_title
#' @importFrom crayon blue silver cyan bold
parse_palname <- function(palname){
    # parse the palette nick
    # return the formal palette name from palettes.info
    if (is.null(palname) || is.na(palname))
        return(structure("default", class="Default"))
    stopifnot(is.character(palname))

    pal <- try(match.arg(tolower(palname), choices=rownames(palettes.info)),
               silent=TRUE)

    if (inherits(pal, "try-error")){
        warn <- c(bold$blue("Below are valid palette names "),
                  blue("(case insensitive):\n"))
        warn <- c(warn, vapply(unique(palettes.info$class), function(nm){
            c(cyan(nm), ": ", silver(paste(
                rownames(palettes.info)[palettes.info$class==nm], collapse=", ")),
              "\n")
        }, FUN.VALUE=character(length=4)))
        message(warn)
        stop("Invalid palette argument provided!")
    }else{
        pal <- structure(
            palettes.info[tolower(pal), "palette"],
            class=str_to_title(palettes.info[pal, "class"]),
            category=palettes.info[pal, "category"],
            max_n=palettes.info[pal, "maxcolors"]
        )
    }
    return(pal)
}

#' @importFrom aseskit iif ifna
output_pal <- function(n, fun_pal, max_n, show=FALSE,
                       mode=c("na.fill", "no.fill", "repeat.fill"), ...){
    # if is.null(n) return a fun to generate pal
    # else return fun_pal(n)
    stopifnot(is.null(n) || is.numeric(n))
    stopifnot(is.function(fun_pal))
    stopifnot(is.na(max_n) || is.numeric(max_n))
    mode <- match.arg(mode)

    fun <- function(n, ...){
        dots <- as.list(substitute(list(...)))[-1]
        colors <- suppressWarnings(fun_pal(n, ...))
        max_n <- ifna(max_n, length(colors))
        if (n > max_n){
            if (mode == "na.fill"){
                colors <- c(colors, rep(NA, n - max_n))
                invisible(message(
                    "Note: n is larger than ", max_n, " (max number of colors of ",
                    "the palette). NAs will be used to fill the vector."))
            }else if (mode == "repeat.fill"){
                colors <- rep(colors, ceiling(n / max_n))[seq_len(n)]
                invisible(message(
                    "Note: n is larger than ", max_n, " (max number of colors of ",
                    "the palette). Colors will be repeated to fill the vector."))
            }
        }
        if ("alpha" %in% names(dots) && is.numeric(dots[["alpha"]])){
            colors <- col2rgb(colors, alpha=TRUE)
            if (! is.integer(dots[["alpha"]]) &&
                (dots[["alpha"]] <= 1 && dots[["alpha"]] >= 0)) {
                colors["alpha", ] <- dots[['alpha']] * 255
                invisible(message("alpha is between [0, 1], will be deemed as ",
                                  100 * (1-dots[["alpha"]]), "% transparent."))
            }else{
                colors["alpha", ] <- dots[['alpha']]
            }
            colors <- rgb(colors['red', ], colors['green', ], colors['blue', ],
                          alpha=colors['alpha', ], maxColorValue=255)
        }
        if (show) show_col(tolower(colors))
        return(tolower(colors))
    }
    if (is.null(n)) return(fun)

    return(tolower(fun(n, ...)))
}

#' @export
#' @importFrom scales show_col
.getNamedPal.Default <- function(
    pal, n=NULL, show=FALSE, mode=c("na.fill", "no.fill", "repeat.fill"),
...){
    o <- c('#ff7f50', '#87cefa', '#da70d6', '#32cd32', '#6495ed',
           '#ff69b4', '#ba55d3', '#cd5c5c', '#ffa500', '#40e0d0',
           '#1e90ff', '#ff6347', '#7b68ee', '#00fa9a', '#ffd700',
           '#6b8e23', '#ff00ff', '#3cb371', '#b8860b', '#30e0e0')
    fun <- function(n){
        return(o[seq_len(min(n, length(o)))])
    }
    output_pal(n, fun, length(o), show=show, mode=mode, ...)
}

#' @export
.getNamedPal.default <- .getNamedPal.Default

#' @export
#' @importFrom aseskit iif ifna
#' @importFrom scales show_col
.getNamedPal.Aetna <- function(
    pal, n=NULL, show=FALSE, mode=c("na.fill", "no.fill", "repeat.fill"),
...){

    fun <- function(n, ...) {
        o <- tolower(palette_aetna[[pal]])
        n <- min(n, ifna(attr(pal, "max_n"), n))
        if (attr(pal, "category") %in% c("seq", "div")){
            return(rgb(colorRamp(o)(seq_len(n)/n), ..., max=255))
        }else{
            return(o[seq_len(n)])
        }
    }
    output_pal(n, fun, attr(pal, "max_n"), show=show, mode=mode, ...)
}

#' @export
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @importFrom scales show_col
.getNamedPal.Brewer <- function(
    pal, n=NULL, show=FALSE, mode=c("na.fill", "no.fill", "repeat.fill"),
...){
    # pal <- aseshms_env$PALETTE$brewer[tolower(pal)]
    max_n <- attr(pal, "max_n")
    fun <- function(n, ...) {
        n <- min(n, max_n)
        return(brewer.pal(n, pal))
    }
    output_pal(n, fun, max_n, show=show, mode=mode, ...)
}

#' @export
.getNamedPal.Grdevice <- function(
    pal, n=NULL, show=FALSE, mode=c("na.fill", "no.fill", "repeat.fill"),
...){

    max_n <- attr(pal, "max_n")
    fun <- switch(pal,
                  rainbow=function(n=n, ...) substr(rainbow(n, ...), 1, 7),
                  terrain=function(n=n, ...) substr(terrain.colors(n, ...), 1, 7),
                  heat=function(n=n, ...) substr(heat.colors(n, ...), 1, 7),
                  topo=function(n=n, ...) substr(topo.colors(n, ...), 1, 7),
                  cm=function(n=n, ...) substr(cm.colors(n, ...), 1, 7)
    )
    output_pal(n, fun, max_n, show=show, mode=mode, ...)
}

#' @export
#' @import ggthemes
#' @importFrom scales show_col
.getNamedPal.Ggthemes <- function(
    pal, n=NULL, show=FALSE, mode=c("na.fill", "no.fill", "repeat.fill"),
...){
    dots <- substitute(list(...))[-1]
    fun <- switch(pal,
        pander=palette_pander,
        excel_line=excel_pal(line=TRUE, ...),
        excel_fill=excel_pal(line=FALSE, ...),
        excel_new=excel_new_pal(
            theme=if ('theme' %in% names(dots)) dots[['theme']] else
                "Office Theme", ...)(),
        economist=economist_pal(fill=TRUE, ...),
        economist_white=economist_pal(fill=FALSE, ...),
        darkunica=hc_pal(palette="darkunica", ...),
        calc=calc_pal(...),
        few=few_pal(...),
        fivethirtyeight=fivethirtyeight_pal(...),
        gdocs=gdocs_pal(...),
        stata=stata_pal(...),
        stata1=stata_pal(scheme="s1color", ...),
        stata1r=stata_pal(scheme="s1rcolor", ...),
        statamono=stata_pal(scheme="mono", ...),
        hc=hc_pal(...),
        colorblind=colorblind_pal(...),
        wsj=wsj_pal(palette="colors6", ...),
        wsj_rgby=wsj_pal(palette="rgby", ...),
        wsj_red_green=wsj_pal(palette="red_green", ...),
        wsj_black_green=wsj_pal(palette="green_black", ...),
        wsj_dem_rep=wsj_pal(palette="dem_rep", ...)
    )
    output_pal(n, fun, attr(pal, "max_n"), show=show, mode=mode, ...)
}

#' @export
#' @importFrom ggthemes tableau_color_pal
#' @importFrom scales show_col
.getNamedPal.Ggtableau <- function(
    pal, n=NULL, show=FALSE, mode=c("na.fill", "no.fill", "repeat.fill"),
...){
    max_n <- attr(pal, "max_n")
    fun <- switch(
        attr(pal, "category"),
        qual=tableau_color_pal(palette=pal, ...),
        seq=tableau_seq_gradient_pal(palette=pal, ...),
        div=tableau_div_gradient_pal(palette=pal, ...)
    )
    output_pal(n, fun, max_n, show=show, mode=mode)
}

#' @export
#' @importFrom scales show_col viridis_pal
.getNamedPal.Viridis <- function(
    pal, n=NULL, show=FALSE, mode=c("na.fill", "no.fill", "repeat.fill"),
...){

    viridis_opt <- structure(
        LETTERS[1:5], names=c("magma", "inferno", "plasma", "viridis", "cividis"))

    fun <- function(n, ...){
        o <- viridis_pal(..., option=viridis_opt[pal])(n)
        o[grepl("[Ff][Ff]$", o)] <- substr(o[grepl("[Ff][Ff]$", o)], 1, 7)
        return(o)
    }

    output_pal(n, fun, attr(pal, "max_n"), show=show, mode=mode, ...)
}

#' @export
#' @importFrom ggthemes solarized_pal
.getNamedPal.Ggsolarized <- function(
    pal, n=NULL, show=FALSE, mode=c("na.fill", "no.fill", "repeat.fill"),
...){
    accent <- unlist(strsplit(pal, "solarized_"))[2]
    if (is.na(accent)) accent <- "blue"

    fun <- solarized_pal(accent=accent, ...)

    output_pal(n, fun, attr(pal, "max_n"), show=show, mode=mode, ...)
}

rgba <- function(red, green, blue, alpha, return=c("value", "function"),
                 maxColorValue=255L, ...){
    return <- match.arg(return)

    if (missing(green) && missing(blue)) {
        if (is.matrix(red) || is.data.frame(red)) {
            red <- data.matrix(red)
            if (ncol(red) < 3L)
                stop("at least 3 columns needed")
            green <- red[, 2L]
            blue <- red[, 3L]
            red <- red[, 1L]
        }
    }

    if (return == "value"){
        return(tolower(rgb(red, green=if (missing(green)) NULL else green,
                   blue=if (missing(blue)) NULL else blue,
                   alpha=if (missing(alpha)) NULL else alpha,
                   maxColorValue=maxColorValue)))
    }else{
        alpha_missing <- missing(alpha)
        return(vapply(seq_len(length(red)), function(i) {
            paste("rgba(", red[i], ",", green[i], ",", blue[i], ",",
                  if (alpha_missing) 1 else alpha[i]/maxColorValue, ")",
                  sep="")
        }, FUN.VALUE=character(length=1L)))
    }
}

#' Generalized color values parser
#'
#' Get colors from a named palette or other formats
#' @author Yiying Wang, \email{wangy@@aetna.com}
#' @param palette A palette name with/without number of colors,
#' or a vector of color names, or a hex color value. \itemize{
#'  \item \code{\link{col2rgb}}-compatible arguments, either color names, hex color
#'  strings, or positive integer for \code{\link{palette}}()[i]. \cr
#'  \item \code{\link{named_pal}}-compatible character arguments, in a
#'  \code{"palette(n)"} format.
#' }
#' @param unlist Logical, whether unlist the result. Defalt TRUE.
#' @param ... Other arguments to pass to the function, e.g., alpha, theme, ....
#'
#' @return \itemize{
#' \item palette is col2rgb-compatible: a list of hex colors (lower case). \cr
#' \item palette is named_pal-compatible: a list of hex colors (lower case) (when
#'  in '\code{palette_name(3)}' format) or a list of functions (when in
#'  '\code{palette_name()}' format).
#'  }
#' Note: If \code{unlist}=TRUE, the function will try to unlist the output list.
#'
#' @seealso \code{\link{getPalette}}
#' @importFrom stringr str_split str_replace_all
#' @export
#'
#' @examples
#' \dontrun{
#' ## Get a hex vector based on col2rgb-compatible palette
#' getHexPal(c("red", "yellow"))  # return
#' # [1] "#ff0000" "#ffff00"
#' getHexPal("red", alpha=200)  # feed alpha to ... with rgb()
#' # [1] "#ff0000c8"
#' hex_colors(2L, alpha=200)  # returns palette()[2L] with alpha=200
#' # [1] "#ff0000c8"
#'
#' ## Get a hex vector based on a hex value
#' getHexPal("#fff")  # return
#' # [1] "#ffffff"
#'
#' ## Get a hex vector based on named_pal-compatible palette
#' getHexPal("terrain(4)")  # return
#' # [1] "#00a600" "#e6e600" "#ecb176" "#f2f2f2"
#' ## equivalent to
#' hex_colors("terrain", n=4)  # or the currying form
#' hex_colors("terrian")(4)
#'
#' getHexPal(c("terrain(2)", "magma(2)"), unlist=FALSE)  # returns
#' # [[1]]
#' # [1] "#00a600" "#f2f2f2"
#' #
#' # [[2]]
#' # [1] "#000004" "#fcfdbf"
#'
#' getHexPal("terrain")  # returns the function named_pal("terrain")
#' }
getHexPal <- function(palette, unlist=TRUE, ...){
    # build a function to extract palette info
    palette <- lapply(palette, classify_palette)
    out <- lapply(palette, general_pal, ...)
    if (unlist && (all(vapply(out, is.character, FUN.VALUE=logical(length=1))) ||
                   length(unlist(palette)) == 1)){
        out <- unlist(out)
        if (is.function(out[[1]])) out <- out[[1]]
    }
    return(out)
}

general_pal <- function(palette, ...){
    UseMethod(".generalPal", palette)
}

#' @export
.generalPal.col2rgb <- function(palette, ...){
    output <- vapply(seq_len(length(palette)), function(i){
        if (! is(try(col2rgb(palette[i]), silent=TRUE), "try-error")){
            rgba(t(col2rgb(palette[i])), ...)
        }else{
            palette[i]
        }
    }, FUN.VALUE=character(length=1))
    return(output)
}

#' @export
#' @importFrom stringr str_replace_all
.generalPal.hex3 <- function(palette){
    output <- str_replace_all(
        palette, "#(.{1})(.{1})(.{1})", "#\\1\\1\\2\\2\\3\\3")
    return(tolower(output))
}

#' @export
.generalPal.namedpal <- function(palette, ...){
    output <- lapply(seq_len(length(palette)), function(i){
        if (is.null(attr(palette, "n"))){
            getPalette(palette, ...)
        }else{
            getPalette(palette, n=attr(palette, "n"), ...)
        }
    })
    if (length(palette) == 1) output <- unlist(output)
    return(output)
}

#' @export
.generalPal.default <- .generalPal.col2rgb

#' @importFrom aseskit iif ifna
classify_palette <- function(palette){
    # classes:
    #   col2rgb --> col2rgb
    #   hex3 --> hex6
    #   namedpal --> named_pal
    stopifnot(is.character(palette) || is.numeric(palette))
    if (is.character(palette)) palette <- tolower(palette)

    if (grepl("\\w+\\(\\d*\\)", palette)) {
        palette <- unlist(strsplit(palette, "[\\(\\)]"))
        n <- ifna(as.numeric(palette[[2]]), NULL)
        palette <- palette[[1]]
    }else{
        n <- NULL
    }

    if (grepl("^#([0-9a-f]{6}|[0-9a-f]{8})$", palette) || is.numeric(palette)){
        out <- structure(palette, class="col2rgb")
    }else if (any(grepl(paste0("^", palette), colors()))){
        palette <- match.arg(palette, choices=colors())
        out <- structure(palette, class="col2rgb")
    }else if (grepl("#[0-9a-f]{3}", palette)){
        out <- structure(palette, class="hex3")
    }else if (any(grepl(paste0("^", palette), rownames(palettes.info)))){
        palette <- match.arg(palette, choices=rownames(palettes.info))
        out <- structure(palette, class="namedpal")
        if (! is.null(n)) attr(out, "n") <- n
    }else{
        stop("palette can only be a valid argument for col2rgb() or named_pal().")
    }
    return(out)
}

#' @export
#' @rdname getHexPal
hex_colors <- getHexPal
