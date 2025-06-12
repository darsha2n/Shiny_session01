library(shiny)
library(rmarkdown)

ui <- fluidPage(
  titlePanel("Resume Builder"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("name", "Full Name", "Your Name"),
      textInput("email", "Email", "your@email.com"),
      textInput("phone", "Phone", "123-456-7890"),
      textAreaInput("summary", "Professional Summary", "Brief summary about yourself...", rows = 3),
      textAreaInput("skills", "Skills (comma separated)", "R, Python, Excel"),
      textAreaInput("experience", "Work Experience", "Company A (2020-2022): Data Analyst\nCompany B (2018-2020): Intern"),
      textAreaInput("education", "Education", "BSc in XYZ\nMBA in ABC"),
      downloadButton("download", "Download Resume")
    ),
    
    mainPanel(
      h4("Instructions"),
      p("Fill out the fields and click 'Download Resume' to get your resume in HTML format.")
    )
  )
)

server <- function(input, output) {
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("resume_", Sys.Date(), ".html")
    },
    content = function(file) {
      params <- list(
        name = input$name,
        email = input$email,
        phone = input$phone,
        summary = input$summary,
        skills = input$skills,
        experience = input$experience,
        education = input$education
      )
      rmarkdown::render("resume_template.Rmd",
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}

shinyApp(ui = ui, server = server)
