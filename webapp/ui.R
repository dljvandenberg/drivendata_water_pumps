### Water Pump Maintenance Predictor web-app (user interface component)

# Variable options
var1.choices=c(TRUE, FALSE)
var2.choices=c("dam", "well", "rain")
var3.choices=c("yearly", "monthly", "weekly", "none")

# User interface
shinyUI(
    fluidPage(
        headerPanel("Water Pump Maintenance Predictor"),
        sidebarPanel(
            # Input selection
            selectInput("var1", "Variable 1:", choices=var1.choices),
            selectInput("var2", "Variable 2:", choices=var2.choices),
            selectInput("var3", "Variable 3:", choices=var3.choices),
            submitButton("Calulate!")
        ),
        mainPanel(
            div("Output of calculations will be displayed here")
        ),
        fluidRow(
            column(   12,
                      
                      h3("Usage"),
                      
                      div(
                          "Select known variables and click 'Calculate!' to calculate probabilities that water pump needs maintenance or is non-functional."
                      ),
                      
                      h3("About this application"),
                      
                      div(
                          "This web application calculates probabilities of water pumps needing maintenance or being out of order.
                          Probabilities are being calculated using a Machine Learning algorithm that has been trained on the data mentioned below.
                          The algorithm was created in the context of a",
                          a(href="http://www.drivendata.org/competitions/7/", "data science competition by DrivenData."),
                          Feel free to use, re-use or contribute to the",
                          a(href="https://github.com/dljvandenberg/drivendata_water_pumps", "code"),
                          "or to",
                          a(href="http://dljvandenberg.github.io", "contact"),
                          "me."
                      ),
                      
                      h3("About the data"),
                      
                      div(
                          "The data set used was provided by",
                          a(href="http://www.drivendata.org/competitions/7/rules/", "DrivenData"),
                          "..."
                      )
            )            
        )
    )
)