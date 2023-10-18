

f <- function(filename) {
  p <- ggplot() +  theme_void() + ggpubr::theme_transparent()
  
  #p <- file.path("logo_green_transparent.png")
  
  hexSticker::sticker(p, 
                    package="traits.build", 
                    p_color = "chartreuse4",
                    p_size=25,
                    p_y = 1.05,
                    s_x=.94, s_y=1.02,
                    s_width=.75, s_height = .75,
                    h_fill = "white", h_color = "chartreuse4",
                    filename=filename)
}

f("traits_build_hex.png")
