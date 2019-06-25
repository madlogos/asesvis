#' asesvis: An R Visualization Toolkit for ASES
#'
#' An analytics toolkit comprising of a series of visualization functions. The toolkit has
#' a lot of featured funcionalities specifically designed for ASES.
#'
#' @details The package is comprised of \describe{
#' \item{Palette}{\code{\link{hex_colors}()}, \code{\link{named_pal}()}}
#' \item{Plots}{\code{\link{chartBox}()}, \code{\link{chartColumn}()},
#'   \code{\link{chartLine}()}, \code{\link{chartTornado}()}}
#' }
#'
#' @author \strong{Maintainer}: Yiying Wang, \email{wangy@@aetna.com}
#'
#' @importFrom magrittr %>%
#' @export %>%
#' @seealso \pkg{\link{aseskit}}
#' @docType package
#' @keywords internal
#' @name asesvis
NULL

#' @importFrom aseskit addRtoolsPath
.onLoad <- function(libname, pkgname="asesvis"){

    if (Sys.info()[['sysname']] == 'Windows'){
        Sys.setlocale('LC_CTYPE', 'Chs')
    }else{
        Sys.setlocale('LC_CTYPE', 'zh_CN.utf-8')
    }
    if (Sys.info()[['machine']] == "x64") if (Sys.getenv("JAVA_HOME") != "")
        Sys.setenv(JAVA_HOME="")

    addRtoolsPath()

    # pkgenv is a hidden env under pacakge:asesvis
    # -----------------------------------------------------------
    assign("pkgenv", new.env(), envir=parent.env(environment()))
    
    # ----------------------------------------------------------

    # options
    assign('op', options(), envir=pkgenv)
    options(stringsAsFactors=FALSE)

    pkgParam <- aseskit:::.getPkgPara(pkgname)
    toset <- !(names(pkgParam) %in% names(pkgenv$op))
    if (any(toset)) options(pkgParam[toset])
}

.onUnload <- function(libname, pkgname="asesvis"){
    op <- aseskit:::.resetPkgPara(pkgname)
    options(op)
}


.onAttach <- function(libname, pkgname="asesvis"){
    ver.warn <- ""
    latest.ver <- getOption(pkgname)$latest.version
    current.ver <- getOption(pkgname)$version
    if (!is.null(latest.ver) && !is.null(current.ver))
        if (latest.ver > current.ver)
            ver.warn <- paste0("\nThe most up-to-date version of ", pkgname, " is ",
			                   latest.ver, ". You are currently using ", current.ver)
    packageStartupMessage(paste("Welcome to", pkgname, current.ver,
                                 ver.warn))
}

