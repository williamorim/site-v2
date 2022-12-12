criar_slideshow <- function(imagens, id) {
  htmltools::tagList(
    htmltools::div(
      id = id,
      htmltools::div(
        class = "slideshow-container",
        purrr::map2(
          1:length(imagens),
          imagens,
          ~ htmltools::div(
            class="mySlides fade",
            htmltools::div(
              class="numbertext",
              paste(.x, "/", length(imagens))
            ),
            htmltools::img(src = .y, style = "width: 100%;")
          )
        ),
        htmltools::a(
          class = "prev",
          onclick = paste0('plusSlides(-1, "', id, '")'),
          htmltools::HTML("&#10094;")
        ),
        htmltools::a(
          class = "next",
          onclick = paste0('plusSlides(+1, "', id, '")'),
          htmltools::HTML("&#10095;")
        )
      ),
      htmltools::div(
        style = "width:50%; text-align:center;",
        purrr::map2(
          1:length(imagens),
          rep(id, length(imagens)),
          ~ htmltools::span(
            class = ifelse(.x == 1, "dot active", "dot"),
            onclick = glue::glue('currentSlide({.x}, "{.y}")')
          )
        )
      )
    )
  )
}

criar_link <- function(link, url) {
  htmltools::a(href = url, target = "_blank", link)
}     

