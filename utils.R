create_material_object <- function(js_file, material_tag_list){
  shiny::tagList(
    shiny::singleton(
      # shiny::tags$head(
      shiny::includeScript(
        system.file(
          file.path("js", js_file),
          package = "shinymaterial"
        )
      )
      # )
    ),
    material_tag_list
  )
}

material_number_box <- function(input_id, label, min_value, max_value, initial_value, step_value, color = NULL){
  if(!is.null(color)){
    
    number_box_style <-
      shiny::tagList(
        shiny::tags$head(
          shiny::tags$style(
            paste0(
              "
              #", input_id, "_number_box.input-field input[type=number]:focus + label {
              color: ", color, ";
  }
              #", input_id, "_number_box.input-field input[type=number]:focus {
              border-bottom: 1px solid ", color, ";
              box-shadow: 0 1px 0 0 ", color, ";
}
  "
            )
            )
            )
            )
    
} else {
  number_box_style <- shiny::tags$div()
}

create_material_object(
  js_file =
    "shiny-material-number-box.js",
  material_tag_list =
    shiny::tagList(
      shiny::tags$div(
        class = "input-field",
        id = paste0(input_id, "_number_box"),
        shiny::tags$input(
          id = input_id,
          type = "number",
          value = initial_value,
          min = min_value,
          max = max_value,
          step = step_value
        ),
        shiny::tags$label(
          `for` = input_id,
          label
        )
      ),
      number_box_style
    )
)
}