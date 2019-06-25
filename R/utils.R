##------------[Initiate ggplot theme args]---------------
ggthemeArgs <- function(theme){
    theme <- tolower(theme)
    if (theme %in% c('economist')) {
        theme_args <- "base_size=8, base_family='sans', horizontal=TRUE, dkpanel=FALSE,
        stata = FALSE"
    }else if (theme %in% c('economist_white')) {
        theme_args <- "base_size=8, base_family='sans', gray_bg=TRUE, horizontal=TRUE"
    }else if (theme %in% c('excel')) {
        theme_args <- "base_size=8, base_family='sans', horizontal=TRUE"
    }else if (theme %in% c('hc')) {
        theme_args <- "base_size=8, base_family='sans', bgcolor='default'"
    }else if (theme %in% c('pander')) {
        theme_args <- "base_size=8, base_family='sans', ff=.shiftFont(title, alt.font),
        gm=FALSE"
    }else if (theme %in% c('stata')) {
        theme_args <- "base_size=8, base_family='sans', scheme='s2color'"
    }else if (theme %in% c('wsj')) {
        theme_args <- "base_size=8, title_family=.shiftFont(title, alt.font),
        base_family='sans'"
    }else if (theme %in% c('solid')) {
        theme_args <- "base_size=8, base_family='sans', fill='white'"
    }else{
        theme_args <- "base_size=8, base_family='sans'"
    }
    return(theme_args)
}

.shiftFont <- function(x, altfont=alt.font){
    # if not pure Latin, use alt font
    if (is.null(x) || is.na(x) || aseskit:::isLatin(x)) 'sans'  else altfont
}
