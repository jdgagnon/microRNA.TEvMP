
ui <- fluidPage(

  # App title ----
  titlePanel("microRNA TE vs. MP Expression"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Select microRNAs to highlight
      selectInput(
        inputId = "miRs",
        label = "Select microRNAs to highlight:",
        choices = NULL,
        multiple = TRUE
      )
    ),

    # Main panel for displaying outputs ----
    mainPanel(

      #### Plot ####
      fluidRow(column(
        12,
        align = "center",
        imageOutput("TEplot_out", width = "100%")
      )),
      hr(),
      fluidRow(column(
        12,
        align = "center",
        imageOutput("MPplot_out", width = "100%")
      )),
      hr(),
      #### Shape, color, fill UI ####
      fluidRow(
        column(6, h4("Shapes"), uiOutput("shapes")),
        column(6, h4("Colors"), uiOutput("colors"))
      )
    )
  )
)

server <- function(input, output, session) {
  file <- microRNA.TEvMP::readData_example(path = "miRNAexpression.csv")
  df <- readr::read_csv(file, col_names = T) %>%
    tidyr::gather(-c("Type", "Day", "Replicate"),
                  key = "miRNA",
                  value = "raw_value")

  df$miRNA <- factor(df$miRNA,
                     levels = unique(df$miRNA))

  updateSelectInput(session,
                    "miRs",
                    choices = df$miRNA)

  df <- df %>%
    dplyr::group_by(miRNA, Type) %>%
    dplyr::mutate(naive_mean = mean(raw_value[Day == 0.0], na.rm = T),
           value = (2^-(naive_mean-raw_value)))

  #### Create shape picker ####
  output$shapes <- renderUI({
    req(input$miRs)

    microRNAs <- c(input$miRs)
    options <- c(19, 21, 17, 24, 15, 22)
    selection <- rep(options[seq_len(length(microRNAs))],
                     length(microRNAs))

    lapply(seq_len(length(microRNAs)), function(i) {
      shiny::tags$div(
        style = "margin-bottom:25px;",
        selectInput(
          inputId = paste0("shape", i),
          label = microRNAs[i],
          choices = options,
          selected = selection[i]
        )
      )
    })
  })

  #### Create color picker ####
  output$colors <- renderUI({
    req(input$miRs)

    microRNAs <- c(input$miRs)
    options <- viridis::viridis(n = 3, begin = 0.4, end = 0.8,option = "A")
    selection <- rep(options[seq_len(length(microRNAs))],
                     length(microRNAs))

    lapply(1:length(microRNAs), function(i) {
      colourpicker::colourInput(
        inputId = paste0("col", i),
        label = microRNAs[i],
        value = selection[i]
      )
    })
  })

  #### Create current plot ####
  TEplot <- reactive({
    req(input$miRs)
    TEdf <- df %>%
      dplyr::filter(Type == "TE")
    highlight <- c(input$miRs)
    other_miRs <- droplevels(TEdf[!(TEdf$miRNA %in% c(input$miRs)), ]$miRNA)
    TEdf$miRNA <- factor(TEdf$miRNA, levels = c(levels(other_miRs), highlight))

    lapply(1:length(unique(input$miRs)), function(i) {
      req(input[[paste0("shape", i)]],
          input[[paste0("col", i)]])
    })
    cols <- c()
    shapes <- c()
    lapply(seq_len(length(highlight)), function(i) {
      cols[i] <<- input[[paste0("col", i)]]
      shapes[i] <<- as.numeric(input[[paste0("shape", i)]])
    })
    cols <- c(rep("#22222233", length(levels(other_miRs))), cols)
    shapes <- c(rep(21, length(levels(other_miRs))), shapes)

    microRNA.TEvMP::miRplot(df = TEdf,
                            color.groups = cols,
                            shape.groups = shapes)
  })



  #### Create current plot ####
  MPplot <- reactive({
    req(input$miRs)
    MPdf <- df %>%
      dplyr::filter(Type == "MP")
    highlight <- c(input$miRs)
    other_miRs <- droplevels(MPdf[!(MPdf$miRNA %in% c(input$miRs)), ]$miRNA)
    MPdf$miRNA <- factor(MPdf$miRNA, levels = c(levels(other_miRs), highlight))

    lapply(1:length(unique(input$miRs)), function(i) {
      req(input[[paste0("shape", i)]],
          input[[paste0("col", i)]])
    })
    cols <- c()
    shapes <- c()
    lapply(seq_len(length(highlight)), function(i) {
      cols[i] <<- input[[paste0("col", i)]]
      shapes[i] <<- as.numeric(input[[paste0("shape", i)]])
    })
    cols <- c(rep("#22222233", length(levels(other_miRs))), cols)
    shapes <- c(rep(21, length(levels(other_miRs))), shapes)

    microRNA.TEvMP::miRplot(df = MPdf,
                         color.groups = cols,
                         shape.groups = shapes)
  })

  output$MPplot_out <- renderImage({

    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = ".png")

    # Generate the PNG
    png(
      outfile,
      width = 200 * 3.7795275591 * 3,
      height = 100 * 3.7795275591 * 3,
      res = 72 * 3
    )
    gridExtra::grid.arrange(MPplot())
    dev.off()

    # Return a list containing the filename
    list(
      src = outfile,
      contentType = "image/png",
      width = 200 * 3.7795275591,
      height = 100 * 3.7795275591,
      alt = "This is alternate text"
    )
  }, deleteFile = TRUE)

  output$TEplot_out <- renderImage({

    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = ".png")

    # Generate the PNG
    png(
      outfile,
      width = 200 * 3.7795275591 * 3,
      height = 100 * 3.7795275591 * 3,
      res = 72 * 3
    )
    gridExtra::grid.arrange(TEplot())
    dev.off()

    # Return a list containing the filename
    list(
      src = outfile,
      contentType = "image/png",
      width = 200 * 3.7795275591,
      height = 100 * 3.7795275591,
      alt = "This is alternate text"
    )
  }, deleteFile = TRUE)

  #### Stop app on close ####
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)
