
# library(showtext)
# font.add.google("Poppins", "myfont")
# showtext.auto()
# showtext.opts(dpi = 600)

theme_set(theme_light(base_family = thefont) + 
            theme(legend.position = "bottom") +
            theme(plot.caption = element_text(colour = "grey50"))) 
update_geom_defaults("text", list(family = thefont))
