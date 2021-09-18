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
                              "Office Hour Attendees"='f',
                              "Rivanna Allocation Requests"='b', 
                              "Rivanna Usage: Core Hours"='g',
                              "New Rivanna Researchers"='h',
                              "Ivy Researchers"='c', 
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
                       ARCH = 14,
                       MC = 15
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
                     "ARCH",
                     "MC"
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
                    "#F73B90", 
                    "#0066FF")

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
    #month <- "July 2021"

    parts <- unlist(strsplit(filename, "-"))
    file_type <- gsub("Data/", "", parts[1])
    
    date_type <- gsub(".xlsx", "", parts[2])
    date_parts <- unlist(strsplit(date_type, "_"))
    myYear <- date_parts[1]
 
    monthNum <- as.numeric(date_parts[2])
    myMonth <- month.name[monthNum]
    month <- paste(myMonth, myYear)
    titles <- list("Consultations"= paste0("Consultations by School\n",month, "\n"),
                   "Office_Hours" = paste0("Office Hour Attendees by School\n",month, "\n"),
                   "Ivy_Users"= paste("Ivy Researchers\n", month, "\n"),
                   "Ivy_Dist" = paste0("Ivy Projects\n", month, "\n"),
                   "Data/Storage.xlsx"="Storage usage by School",
                   "Rivanna_Allocations"=paste0("Rivanna Allocation Requests\n", month, "\n"),
                   "Core_Hours"= paste0("Rivanna Usage: Core Hours\n",month, "\n"),
                   "New_Users"=paste0("New Rivanna Researchers\n", month, "\n"))
    if (file_type == "Rivanna_Allocations" || 
        file_type == "Core_Hours" ||
        file_type == "New_Users" ||
        file_type == "Ivy_Users" ||
        file_type == "Ivy_Dist"  ||
        file_type == "Office_Hours" ||
        file_type == "Consultations"){
        chart_title <- as.character(titles[file_type])    
       
    } else {
        chart_title <- as.character(titles[filename]) 
    }

    pie(values, labels = percent, radius = 1, clockwise=TRUE, 
        col=cols[1:N], 
        main=chart_title)
    legend("topleft", schools[1:N], cex = 0.6,
           fill = cols[1:N])
}
    

# Server logic
#writing server function 
server <- function(input, output) { 
    #referring output distPlot in ui.r as output$distPlot   
    output$distPlot <- renderPlot({ 
        #referring input p in ui.r as input$p   
        i <- 1
        files <- list.files("Data", full.names = TRUE)
        if(input$p=='a'){ 
            ndx <- grep("Consultations", files)
            if (length(ndx) > 0){
                filename <- files[ndx[1]]
                gen_pie_chart(filename)
            }
            #gen_pie_chart("Data/Consultations.xlsx")
         }
        if(input$p=='b'){ 

            ndx <- grep("Rivanna_Allocations", files)
            if (length(ndx) > 0){
                filename <- files[ndx[1]]
                gen_pie_chart(filename)
            }
            #gen_pie_chart("Data/Rivanna_Allocations.xlsx")
        }     
        if(input$p=='c'){ 
            ndx <- grep("Ivy_Users", files)
            if (length(ndx) > 0){
                filename <- files[ndx[1]]
                gen_pie_chart(filename)
            }
            #gen_pie_chart("Data/Ivy_Users.xlsx")
        }     
        if(input$p=='d'){ 
            gen_pie_chart("Data/Storage.xlsx")
        }  
        if(input$p=='e'){ 
            ndx <- grep("Ivy_Dist", files)
            if (length(ndx) > 0){
                filename <- files[ndx[1]]
                gen_pie_chart(filename)
            }
            #gen_pie_chart("Data/Ivy_Dist.xlsx")
        }
        if(input$p=='f'){ 
            ndx <- grep("Office_Hours", files)
            if (length(ndx) > 0){
                filename <- files[ndx[1]]
                gen_pie_chart(filename)
            }
            #gen_pie_chart("Data/Office_Hours.xlsx")
        } 
        if(input$p=='g'){ 
            ndx <- grep("Core_Hours", files)
            if (length(ndx) > 0){
                filename <- files[ndx[1]]
                gen_pie_chart(filename)
            }
            #gen_pie_chart("Data/Core_Hours.xlsx")
        } 
        if(input$p=='h'){ 
            ndx <- grep("New_Users", files)
            if (length(ndx) > 0){
                filename <- files[ndx[1]]
                gen_pie_chart(filename)
            }
            #gen_pie_chart("Data/New_Users.xlsx")
        } 
 
        }) 
    }


    
# Complete app with UI and server components
shinyApp(ui, server)
    
    



