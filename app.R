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
                              "Ivy Storage"= 'i',
                              "Standard Storage"= 'j',
                              "Project Storage"= 'k'))
                              #"Storage Usage"='d')), 

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
                       "VPR/PROV" = 11, 
                       NUR = 12,
                       BATT = 13,
                       ARCH = 14,
                       MC = 15,
                       ACCORD = 16
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
                     "VPR/PROV", 
                     "NUR",
                     "BATT",
                     "ARCH",
                     "MC", 
                     "ACCORD"
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
                    "#0066FF", 
                    "#0099CC")

    raw_data <- read_excel(filename)
    raw_data <- raw_data %>% filter(Count > 0) %>%
        mutate(Percent = 100*Count/sum(Count))
    
    Other_count <- max(0, (raw_data %>% filter(School=="Other"))$Count)
    Other_percent <- max(0, (raw_data %>% filter(School=="Other"))$Percent)    
    ## Keep only the schools that have a count greater than 0
    ## Compute percentages based on counts

    raw_data <- raw_data %>% filter(!School=="Other")
    ## Identify the schools that have a low percentage
    ## For ones with low percentage,combine the scores with Other
    low_ndx <- which(raw_data$Percent <= 1.5)
    if (length(low_ndx) > 0){
        
        Other_count <- Other_count + sum(raw_data$Count[low_ndx])
        Other_percent <- Other_percent + sum(raw_data$Percent[low_ndx])
        
        ## Remove the low count data from the table
        raw_data <- raw_data %>% filter(Percent > 1.5) 
        
    }

    ## Check if "Other" is too low to report

    if (Other_percent > 0.6){
        raw_data <- raw_data %>% add_row(School="Other",Count=Other_count, Percent=Other_percent)
        
    }
        

    N <- nrow(raw_data)
    
    schools <- raw_data$School
    values <- raw_data$Count
    #schools <- c(schools[1:3], schools[5], schools[4], schools[6])
    #values <- c(values[1:3], values[5], values[4], values[6])
    
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
    titles <- list("Consultations"= paste0("Advanced Consultation Projects by School\n",month, "\n"),
                   "Office_Hours" = paste0("Office Hour Attendees by School\n",month, "\n"),
                   "Ivy_Users"= paste("Ivy Researchers\n", month, "\n"),
                   "Ivy_Dist" = paste0("Ivy Projects\n", month, "\n"),
                   "Data/Storage.xlsx"="Storage usage by School",
                   "Rivanna_Allocations"=paste0("Rivanna Allocation Requests\n", month, "\n"),
                   "Core_Hours"= paste0("Rivanna Usage: Core Hours\n",month, "\n"),
                   "Ivy_Storage_Summary"=paste0("Ivy Storage usage by School\n", month, "\n"),
                   "Standard_Storage_Summary"=paste0("Research Standard Storage usage by School\n", month, "\n"),
                   "Project_Storage_Summary"=paste0("Research Project Storage usage by School\n", month, "\n"),
                   "New_Users"=paste0("New Rivanna Researchers\n", month, "\n"))
    
    if (file_type == "Rivanna_Allocations" || 
        file_type == "Core_Hours" ||
        file_type == "New_Users" ||
        file_type == "Ivy_Users" ||
        file_type == "Ivy_Dist"  ||
        file_type == "Office_Hours" ||
        file_type == "Ivy_Storage_Summary" ||
        file_type == "Standard_Storage_Summary" ||
        file_type == "Project_Storage_Summary" ||
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
        }
        if(input$p=='i'){ 
            ndx <- grep("Ivy_Storage", files)
            if (length(ndx) > 0){
                filename <- files[ndx[1]]
                gen_pie_chart(filename)
            }
        } 
        if(input$p=='j'){ 
            ndx <- grep("Standard_Storage", files)
            if (length(ndx) > 0){
                filename <- files[ndx[1]]
                gen_pie_chart(filename)
            }
        } 
        if(input$p=='k'){ 
            ndx <- grep("Project_Storage", files)
            if (length(ndx) > 0){
                filename <- files[ndx[1]]
                gen_pie_chart(filename)
            }
        } 
 
        }) 
    }


    
# Complete app with UI and server components
shinyApp(ui, server)
    
    



