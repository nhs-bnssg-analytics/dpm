# taken from BNSSG_theme_package by DanMaloneyBNSSG 2023-09-27
bnssgtheme <- function(base_size = 12, base_family = "sans",base_colour = "black"){theme_bw() %+replace% theme(
  axis.title.x = element_text(size = 16, color = '#003087', face = 'bold', family = "sans", margin = margin(t = 0, r = 20, b = 0, l = 0)), #x Axis Titles
  axis.title.y = element_text(size = 16, color = '#003087', angle = 90, face = 'bold', family = "sans", margin = margin(t = 0, r = 20, b = 0, l = 0)), #y Axis Titles
  axis.text = element_text(size = 12,  family = "sans", color = 'black'), #Axis text
  panel.border = element_blank(), #remove plot border
  panel.grid.major.x = element_blank(), #no major vertical lines
  panel.grid.major.y = element_line(linetype = 'dotted', size = 1), #dotted major horizontal lines
  panel.grid.minor = element_blank(), #no minor lines
  legend.position = "top", #legend on top
  legend.justification='left', #legend left
  legend.direction='horizontal', #legend to be horizontal
  legend.title = element_blank(), #No legend title
  legend.text = element_text(size = 12, family = "sans",),
  legend.key.size = unit(0.3, "cm"),
  plot.title = element_text(size = 16, color = '#003087', face="bold", margin = margin(b = 10, t=10), hjust=0),
  plot.subtitle = element_text(size = 10, margin = margin(b = 10), hjust=0)
)
}


bnssgcols <- c(`white` = "#FFFFFF",`light grey` = "#999999",`light pink` = "#D093B6",`mid pink` = "#8d488d",`light blue` = "#8AC0E5",`dark pink` = "#853358",
               `mid blue` = "#2472AA",`dark blue` = "#003087",`dark grey` = "#333333")

##Tells R Where to look for color codes, from the name
bnssg_cols <- function(...) {
  cols <- c(...)

  if (is.null(cols))
    return (bnssgcols)

  bnssgcols[cols]
}

##Set colour 'names'scheme' names:
bnssg_palettes <- list(
  "main"  = bnssg_cols("light grey", "light pink", "mid pink", "light blue", "mid blue", "dark blue", "dark grey","dark pink"),
  "pgb" = bnssg_cols("dark pink", "mid pink", "light pink", "light grey", "light blue", "mid blue", "dark blue"),
  "pgblite" = bnssg_cols("dark pink", "light grey", "dark blue"),
  "pink" = bnssg_cols("dark pink", "mid pink", "light pink"),
  "blue" = bnssg_cols("dark blue", "mid blue", "light blue")
)

##Function so that the colours can be found from the scheme name
bnssg_pal <- function(palette, reverse = FALSE, ...) {
  pal <- bnssg_palettes[[palette]]

  if (reverse) pal <- rev(pal)

  colorRampPalette(pal, ...)
}

scale_colour_bnssg <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bnssg_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("color", paste0("bnssg_", palette), palette = pal, ...)
  } else {
    scale_colour_gradientn(colors = pal(256), ...)
  }
}

scale_fill_bnssg <- function(palette = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bnssg_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("bnssg_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

core_seg_cols_greenred  <-
  c("CS1"="#77A033",
    "CS2"="#C4D22A",
    "CS3"="#FFE34D",
    "CS4"="#FFA833",
    "CS5"="#FF6C53")

