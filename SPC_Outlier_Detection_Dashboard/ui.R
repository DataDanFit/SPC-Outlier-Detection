library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(
        includeCSS("www/styling.css"),
        ## Navbar --------
        tags$nav(class = "navbar", role = "navigation",
            tags$div(class = "navbar-header",
                tags$div(class = "navbar-brand", "SPC Outlier Detection Dashboard")
            ),
            tags$ul(class = "nav navbar-nav navbar-right", 
                    tags$li(class = "active", id = "PCnav",
                            tags$a(href="#PC", `data-toggle`="tab", "Stronger Performers")
                    ),
                    tags$li(id = "NCnav",
                            tags$a(href="#NC", `data-toggle`="tab", "Weaker Performers")
                    )
            )
        ),
        ## Content --------
        div(class = "container tab-content",
            ## Positive Change Alerts ----
            div(id="PC", class="tab-pane active",
                div(class = "row contentRow",
                    div(class = "col-sm-8 contentBox",
                        tags$div(class = "contentBoxHeader",tags$p("Positive Change Alerts"))
                    ),
                    div(class = "col-sm-4 contentBox")
                ),
                div(class = "row contentRow",
                    div(class = "col-sm-6 contentBox",
                        tags$p(class = "contentBoxHeader", "Select Employee:"),
                        div(class="table", dataTableOutput("PCTable", height = "100%", width = "100%"))
                    ),
                    div(class = "col-sm-2 contentBox"),
                    div(class = "col-sm-4 contentBox")
                )
            ),
            ## Negative Change Alerts ----
            div(id="NC", class="tab-pane",
                div(class = "row contentRow",
                    div(class = "col-sm-8 contentBox",
                        tags$div(class = "contentBoxHeader",tags$p("Negative Change Alerts"))
                    ),
                    div(class = "col-sm-4 contentBox")
                ),
                div(class = "row contentRow",
                    div(class = "col-sm-6 contentBox",
                        tags$p(class = "contentBoxHeader", "Select Employee:"),
                        div(class="table", dataTableOutput("NCTable", height = "100%", width = "100%"))
                    ),
                    div(class = "col-sm-2 contentBox"),
                    div(class = "col-sm-4 contentBox")
                )
            )
        )
    )
)
