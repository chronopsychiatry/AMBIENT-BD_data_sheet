input_svg <- "/media/Store/Daniel/Ambient-BD/AmbientDataSheet/inst/svg/test_template_ink.svg"
output_svg <- "/media/Store/Daniel/Ambient-BD/AmbientDataSheet/inst/svg/test_output_ink.svg"
plot_file <- "/media/Store/Daniel/Ambient-BD/AmbientDataSheet/inst/svg/test_plot.svg"
rect_id <- "Sessions"
dpi <- 150

doc <- xml2::read_xml(input_svg)

rect_xpath <- paste0(".//*[@id='", rect_id, "']")
rect <- xml2::xml_find_first(doc, rect_xpath, ns = c(svg = "http://www.w3.org/2000/svg"))
if (is.na(rect)) stop(paste("No rect with id", rect_id))

# Detect unit from SVG root width attribute
svg_root <- xml2::xml_root(doc)
svg_width_attr <- xml2::xml_attr(svg_root, "width")
unit_match <- regmatches(svg_width_attr, regexpr("[a-zA-Z]+$", svg_width_attr))
svg_unit <- if (length(unit_match) == 1) unit_match else "px"

unit_to_inch <- function(val, unit) {
  switch(unit,
    "mm" = val / 25.4,
    "cm" = val / 2.54,
    "in" = val,
    "px" = val / dpi,
    val
  )
}

x <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(rect, "x")))
y <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(rect, "y")))
w <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(rect, "width")))
h <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(rect, "height")))
if (is.na(w) || is.na(h)) stop("Rect missing width/height or not numeric")

w_in <- unit_to_inch(w, svg_unit)
h_in <- unit_to_inch(h, svg_unit)

p <- ggplot2::ggplot(data.frame(x = c(0, 1, 2, 3)), ggplot2::aes(x = x)) +
  ggplot2::geom_line(ggplot2::aes(y = sin(x))) +
  ggplot2::ggtitle("Embedded ggplot2")
ggplot2::ggsave(plot_file, plot = p, device = "svg", width = w_in, height = h_in, dpi = dpi)

svg_ns <- "http://www.w3.org/2000/svg"

plot_svg_doc <- xml2::read_xml(plot_file)
plot_svg_root <- xml2::xml_root(plot_svg_doc)

plot_width <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(plot_svg_root, "width")))
plot_height <- as.numeric(gsub("[^0-9.]", "", xml2::xml_attr(plot_svg_root, "height")))

plot_group <- xml2::xml_new_root("g", ns = svg_ns)
xml2::xml_set_attr(plot_group, "transform",
  sprintf("translate(%f,%f) scale(%f,%f)",
          x,
          y,
          h / plot_height,
          w / plot_width)
)

bg_rect <- xml2::xml_find_first(plot_svg_root, ".//svg:rect", ns = c(svg = svg_ns))
if (!is.na(bg_rect)) xml2::xml_remove(bg_rect)

for (child in xml2::xml_children(plot_svg_root)) {
  xml2::xml_add_child(plot_group, child)
}

xml2::xml_remove(rect)
xml2::xml_set_attr(plot_group, "id", rect_id)
xml2::xml_add_child(svg_root, plot_group)

xml2::write_xml(doc, file = output_svg)
