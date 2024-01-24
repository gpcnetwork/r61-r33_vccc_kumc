# load packages ----------------
library(shiny)
library(shinydashboard)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)
library(ggrepel)
library(cowplot)

# load data --------------------
bp<-readRDS(file.path("data","bp_long_sample.rda"))
med<-readRDS(file.path("data","med_long_sample.rda"))

# define UI for application -------------
ui<-dashboardPage(
  dashboardHeader(title="VCCC Blood Pressure and Medication Explorer"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    fluidRow(
      column(
        3,
        selectInput(
          "id", 
          h4("Patient ID (random)"), 
          choices = c(1:100),
          selected = 1
        )
      ),
      column(
        3,
        sliderInput(
          "range",
          h4("Range of Time Window"),
          min = -365, max = 720, value = c(-180,365)
        )
      ),
      column(
        3,
        numericInput(
          "fontsize", 
          h4("Font Size"), 
          value = 16
        )
      ),
      column(
        3,
        numericInput(
          "pointsize",
          h4("Point Size"),
          value = 3
        )
      )
    ),
    
    plotOutput("mainplot",height = 800)
    
  )
)

# define server logic ------------------
server <- function(input, output, session) {
  # dashboard reactives--------------------------
  datainput1<-reactive({
    med %>% filter(id == input$id)
  })
  
  datainput2<-reactive({
    vpos<-unique(c(
      datainput1()$rx_start_since_index,
      datainput1()$rx_end_since_index
    ))
  })
  
  datainput3<-reactive({
    bp %>% 
      filter(id == input$id) %>%
      filter(days_since_index >= input$range[1] & days_since_index <= input$range[2])
  })
  
  datainput4<-reactive({
    range_vev<-c(
      input$range[1],
      input$range[2]
    )
  })
  
  # dashboard plot -----------------------------
  output$mainplot<-renderPlot({
    # individual plot of AH use
    med_sample_htn<-datainput1() %>% filter(AntiHTN_ind==1)
    if(nrow(med_sample_htn)==0){
      med_htn_plt<-ggplot() +
        theme_void() +
        geom_text(aes(0,0,label='No Antihypertive Medications')) +
        xlab(NULL)
    }else{
      med_htn_plt<-ggplot(
        med_sample_htn,
        aes(
          y = in_or_name_s, yend = in_or_name_s
        )
      ) +
        geom_segment(
          aes(
            x = rx_start_since_index,
            xend = rx_start_since_index + rx_days,
            linewidth = rx_str
          ),
          color = "grey"
        ) +
        geom_point(
          aes(
            x = rx_start_since_index
          ),
          size = input$pointsize, color = 'red'
        ) +
        geom_text_repel(
          aes(
            x = rx_start_since_index,
            label = paste0(rx_str,'mg,',rx_freq_num)
          ),
          fontface = "bold",
          size=input$fontsize/3
        ) + 
        geom_vline(xintercept = 0,linetype=2) + 
        geom_vline(xintercept = datainput2(),linetype=3) + 
        scale_x_continuous('days since enroll',breaks = seq(datainput4()[1], datainput4()[2], by=30),limits=c(datainput4()[1],datainput4()[2])) + 
        theme_classic() + theme(legend.position = "none") +
        theme(axis.title = element_blank(), 
              axis.text.x = element_blank(),
              text = element_text(face="bold",size=input$fontsize))
    }

    # individual plot of Non-AH use
    med_sample_nhtn<-datainput1() %>% filter(AntiHTN_ind==0)
    if(nrow(med_sample_nhtn)==0){
      med_htn_plt<-ggplot() +
        theme_void() +
        geom_text(aes(0,0,label='No Non-Antihypertive Medications')) +
        xlab(NULL)
    }else{
      med_nhtn_plt<-ggplot(
        med_sample_nhtn,
        aes(
          y = in_or_name_s, yend = in_or_name_s
        )
      ) +
        geom_segment(
          aes(
            x = rx_start_since_index,
            xend = rx_start_since_index + rx_days,
            linewidth = rx_str
          ),
          color = "grey"
        ) +
        geom_point(
          aes(
            x = rx_start_since_index
          ),
          size = input$pointsize, color = 'red'
        ) +
        geom_text_repel(
          aes(
            x = rx_start_since_index,
            label = paste0(rx_str,'mg,',rx_freq_num)
          ),
          fontface = "bold",
          size=input$fontsize/3
        ) + 
        geom_vline(xintercept = 0,linetype=2) + 
        geom_vline(xintercept = datainput2(),linetype=3) + 
        scale_x_continuous('days since enroll',breaks = seq(datainput4()[1], datainput4()[2], by=30),limits=c(datainput4()[1],datainput4()[2])) + 
        theme_classic() + theme(legend.position = "none") +
        theme(axis.title.y = element_blank(),
              axis.text.x = element_text(angle = 45),
              text = element_text(face="bold",size=input$fontsize))
      
    }

    # individual plots of a random sample of size k
    bp_plt<-ggplot(
      datainput3(),
      aes(x = days_since_index,
          y = bp_val,
          color = bp_type)
    )+
      geom_line(aes(group = bp_type)) +
      geom_point(aes(shape = type),size=3,alpha=0.8) +
      geom_vline(xintercept = 0,linetype=2) + 
      geom_vline(xintercept = datainput2(),linetype=3) + 
      scale_x_continuous('days since enroll',breaks = seq(datainput4()[1], datainput4()[2], by=30),limits=c(datainput4()[1],datainput4()[2])) + 
      theme_classic() + theme(legend.position = "none") +
      theme(axis.title = element_blank(), 
            axis.text.x = element_blank(),
            text = element_text(face="bold",size=input$fontsize))
    
    # put everything together
    aligned_plots<-align_plots(bp_plt,med_htn_plt,med_nhtn_plt,align="v", axis="l")
    plot_grid(aligned_plots[[1]], aligned_plots[[2]], aligned_plots[[3]], ncol = 1)
  })
}

# run app -------
shinyApp(ui, server)

