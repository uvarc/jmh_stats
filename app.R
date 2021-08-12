library(shiny) 
library(dplyr)
library(readxl)
library(ggplot2)
library(gridExtra)

ui <- fluidPage( 
    titlePanel("Research Computing Statistics"),  
    sidebarLayout(     
        sidebarPanel(
            #implementing radio buttons       
            radioButtons("p", "Select type of information to view:",
                         list("Consultations"='a',
                              "Rivanna Allocation Requests"='b', 
                              "Ivy Users"='c', 
                              "Ivy Projects"='e',
                              "Storage Usage"='d')), 

        ),
        mainPanel(       
            plotOutput("distPlot")      
        ) 
    ) # end sidebarLayout
)  # end fluidpage

    
gen_pie_chart <- function(filename){
    
    school_ndx <- list(BII = 1,
                       CLAS = 2, 
                       COMM = 3,
                       DARD = 4,
                       LAW = 5,
                       Other = 6,
                       SDS = 7, 
                       SEAS = 8,
                       SEHD = 9, 
                       SOM = 10,
                       "VPR/VPAT" = 11, 
                       NUR = 12,
                       BATT = 13,
                       ARCH = 14
    )
    
    
    all_schools <- c("BII",
                     "CLAS", 
                     "COM",
                     "DARD",
                     "LAW",
                     "Other",
                     "SDS", 
                     "SEAS",
                     "SEHD", 
                     "SOM",
                     "VP/VPAT", 
                     "NUR",
                     "BATT",
                     "ARCH"
    )
    all_colors <- c("#FFE699",
                    "#9DC3E6", 
                    "#33CCFF",
                    "#D84BFF",
                    "#71FFA0",
                    "#E4E4E4",
                    "#FF9999", 
                    "#F7C5A3",
                    "#FF1945", 
                    "#A9D18E",
                    "#FFFF00", 
                    "#C45911",
                    "#9999FF",
                    "#F73B90")

    raw_data <- read_excel(filename)
    
    raw_data <- raw_data %>% filter(Count > 0) %>%
        mutate(Percent = 100*Count/sum(Count))
    low_ndx <- which(raw_data$Percent < 1.5)
    if (length(low_ndx) > 0){
        
        Other_count <- sum(raw_data$Count[low_ndx])
        
        
        raw_data <- raw_data %>% filter(Percent > 1.5) 
        other_data <- raw_data %>% filter(grepl("Other", School))
        if (nrow(other_data) > 0){
            Other_count <- Other_count + as.numeric(other_data$Count)
        }
        Other_percent <- 100*Other_count/sum(raw_data$Count) 
        raw_data <- raw_data %>% filter(!grepl("Other", School)) %>%
            add_row(School="Other", Count=Other_count, Percent=Other_percent)
        
        
    }


        

    N <- nrow(raw_data)
    
    schools <- raw_data$School
    values <- raw_data$Count
    
    ndx <- as.numeric(unlist(school_ndx[schools]))
    total <- sum(values)
    
    percent <- round(100*values/total,0)

    percent <- paste0(percent, "%")
    cols <- all_colors[ndx]
    month <- "July 2021"
    titles <- list("Data/Consultations.xlsx"= paste0("Consultations by School\n",month, "\n"),
                   "Data/Ivy_Users.xlsx"= "Ivy Users by School", 
                   "Data/Ivy_Dist.xlsx" = paste0("Ivy Projects by School\n", month, "\n"),
                   "Data/Storage.xlsx"="Storage usage by School",
                   "Data/Rivanna_Allocations.xlsx"="Rivanna Allocation Requests by School")
    chart_title <- as.character(titles[filename])
    pie(values, labels = percent, radius = 1, clockwise=TRUE, 
        col=cols[1:N], 
        main=chart_title)
    legend("topleft", schools[1:N], cex = 0.8,
           fill = cols[1:N])
}
    

# Server logic
#writing server function 
server <- function(input, output) { 
    #referring output distPlot in ui.r as output$distPlot   
    output$distPlot <- renderPlot({ 
        #referring input p in ui.r as input$p   
        i <- 1
        if(input$p=='a'){ 
            gen_pie_chart("Data/Consultations.xlsx")
         }
        if(input$p=='b'){ 
            gen_pie_chart("Data/Rivanna_Allocations.xlsx")
        }     
        if(input$p=='c'){ 
            gen_pie_chart("Data/Ivy_Users.xlsx")
        }     
        if(input$p=='d'){ 
            gen_pie_chart("Data/Storage.xlsx")
        }   
        if(input$p=='e'){ 
            gen_pie_chart("Data/Ivy_Dist.xlsx")
        } 
 
        }) 
    }


    
# Complete app with UI and server components
shinyApp(ui, server)
    
    



