#' Dataset: Palettes info
#' 
#' Palettes information including name, palette full name, class, maxcolors, 
#' category and colorblind. 
#' 
#' @name data:palettes.info
#' @docType data
#' @author Yiying Wang \email{wangy@@aetna.com}
#' @details 
#' It is a 171 * 5 data.frame: \cr
#' \describe{
#'   \item{palette}{character, the formal name of the palettes}
#'   \item{class}{character, the class name of the palette: "brewer", "ggtableau",
#'    "ggthemes", "ggsolarized", "viridis", "aetna", "grdevice"}
#'   \item{maxcolors}{numeric, the max number of colors for each palette}
#'   \item{category}{chracter, category of 'div(verging)', 'qual(itative)' or 
#'    'seq(uential)'}
#'   \item{colorblind}{logical, whether the palette is colorblind-friendly}
#' }
#' @references 
#' The data set is used for \code{\link{getPalette}} and \code{\link{getHexPal}}
#' @seealso 
#' \pkg{\link{RColorBrewer}}  \pkg{\link{ggthemes}}  \pkg{\link{grDevices}}  
#' \pkg{\link{viridis}}
#' @examples 
#' \dontrun{
#' data(palette.info)
#' }
#' @keywords data palettes
#' 
NULL
