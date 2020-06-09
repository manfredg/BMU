load('glassersub_R.bin')

mystackbrain <- function (geobrain) {
    stack = geobrain %>% dplyr::group_by(hemi, side) %>% dplyr::summarise_at(dplyr::vars(.long, 
        .lat), list(min = min, max = max, sd = sd)) %>% dplyr::mutate(sd = .lat_sd + 
        .long_sd)
    stack$.lat_max[1] = ifelse(stack$.lat_max[1]/4.5 < stack$.lat_sd[1], 
        stack$.lat_max[1] + stack$.lat_sd[1], stack$.lat_max[1])
    geobrain = geobrain %>% dplyr::mutate(
.lat = ifelse(side %in% "lateral", .lat + (stack$.lat_max[1]), .lat), 
.long = ifelse(hemi %in% "right" & side %in% "lateral", .long - stack$.long_min[3]+stack$.long_min[2],.long), 
.long = ifelse(hemi %in% "right" & side %in% "medial", .long - stack$.long_min[4] +stack$.long_min[2],.long),
.long = ifelse(hemi %in% "left" & side %in% "medial", .long - stack$.long_min[2],.long))
    return(geobrain)
}
environment(mystackbrain)=environment(ggseg:::stack_brain)

ggseg <- function (.data = NULL, atlas = "dkt", position = "dispersed", 
    view = NULL, hemisphere = NULL, adapt_scales = TRUE, ...) 
{
    geobrain <- if (!is.character(atlas)) {
        atlas
    }
    else {
        get(atlas)
    }
    if (!is_ggseg_atlas(geobrain)) {
        warning("This is not a ggseg_atlas-class. Attempting to convert with `as_ggseg_atlas()`")
        geobrain <- as_ggseg_atlas(geobrain)
    }
    geobrain <- unnest(geobrain, ggseg)
    stack <- case_when(grepl("stack", position) ~ "stacked", 
        grepl("disperse", position) ~ "dispersed", TRUE ~ "unknown")
    if (stack == "stacked") {
        if (any(!geobrain %>% dplyr::select(side) %>% unique %>% 
            unlist() %in% c("medial", "lateral"))) {
            warning("Cannot stack atlas. Check if atlas has medial views.")
        }
        else {
            geobrain <- mystackbrain(geobrain)
        }
    }
    else if (stack == "unknown") {
        warning(paste0("Cannot recognise position = '", position, 
            "'. Please use either 'stacked' or 'dispersed', returning dispersed."))
        stack <- "dispersed"
    }
    if (!is.null(hemisphere)) 
        geobrain <- dplyr::filter(geobrain, hemi %in% hemisphere)
    if (!is.null(view)) {
        geobrain <- dplyr::filter(geobrain, grepl(view, side))
        if (view == "lateral" & (all(c("left", "right") %in% 
            hemisphere) | is.null(hemisphere)) & stack == "dispersed") {
            geobrain <- squish_position(geobrain, hemisphere, 
                stack)
        }
    }
    if (!is.null(.data)) {
        geobrain <- data_merge(.data, geobrain)
    }
    gg <- ggplot2::ggplot(data = geobrain, ggplot2::aes(x = .long, 
        y = .lat, group = .id)) + ggplot2::geom_polygon(...) + 
        ggplot2::coord_fixed()
    if (adapt_scales) {
        gg <- gg + scale_y_brain(geobrain, stack) + scale_x_brain(geobrain, 
            stack) + scale_labs_brain(geobrain, stack)
    }
    gg + theme_brain()
}
environment(ggseg)=environment(ggseg::ggseg)

plotCortex <- function(x,atlas='glassersub',colormap='jet',position='dispersed',limits=c(NA,NA),alpha=.8,title='',cbtitle=NULL,cbpos='bottom',col='white',lwd=.1,...) {
    l0=names(x)
    if(atlas%in%'aseg') {
        l0=tolower(l0)
        l0=gsub('thalamus','thalamus proper',l0)
        l0=gsub('ventraldc','ventral DC',l0)
        l0=gsub('left-','L_',l0)
        l0=gsub('right-','R_',l0)
    }
    results = data.frame(hemi=ifelse(grepl('R_',l0),'right','left'),
                         area=gsub('[LR]_','',l0),
                         em=x,
                         stringsAsFactors=F)

    results %>%
        ggseg(mapping=aes(fill=as.numeric(em)),atlas=atlas,position=position,adapt_scales=F,col=col,lwd=lwd,...)+labs(x=NULL,y=NULL,title=title)+
        scale_x_continuous(breaks=NULL)+scale_y_continuous(breaks=NULL)+
        scale_fill_gradientn(cbtitle,colors=alpha(do.call(colormap,list(n=100)),alpha=alpha),guide = guide_colourbar(title.position=cbpos,barheight=.5),oob=squish,limits=limits,na.value='gray80')+
        theme(legend.position=cbpos,text=element_text(angle=ifelse(cbpos%in%c('right','left'),90,0),hjust=.5,size=12))
}
plotCortexLeft <- function(x,...) {
    l0=gsub('Right','R_',gsub('Left-','L_',names(x)))
    names(x)=l0
    l00=unique(gsub('[LR]_','',l0))
    plotCortex(rowMeans(cbind(x[paste0('L_',l00)],x[paste0('R_',l00)]),na.rm=T),hemi='left',...)
}
