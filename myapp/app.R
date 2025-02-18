
library(plyr)
library(readxl)

library(openxlsx)

library(shiny)
library(shinyAce)
#source("chooser.R")





########################################
########UI (User Interface)#############
########################################

modul_friedman_test_ui <- function(id) {
  
  
  
  ns <- NS(id)
  
  fluidPage(
    
    
    
    tabsetPanel(
      
      tabPanel(title = tags$h5( tags$img(src = "referensi_icon.png", width = "30px"), 'Reference of Friedman Test'),
               
               
               includeHTML("reference_journal_article.html"),
               
               uiOutput(ns("pemilihan_bidang_artikel")),
               
               br(),
               
               DT::DTOutput(ns("data_artikel")),
               
               
               
               br(),
               br(),
               
               
               includeHTML("reference_book.html"),
               
               
               
               
               
               
               
               
               
               
               
               br()
               
      ), #References
      
      
      
      
      
      
      
      
      
      tabPanel(title = tags$h5( tags$img(src = "youtube.png", width = "30px"), 'Tutorial'),
               
               
               
               br()
               
               
               
               
               
      ), #Tabpanel Tutorial
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      tabPanel(title = tags$h5( tags$img(src = "dataset.png", width = "30px"), 'Upload Your Data'),
               
               
               
               
               sidebarLayout(
                 
                 
                 sidebarPanel(
                   
                   h4("Upload Your Data", style="
         font-family: 'cursive';
         color: blue;
         text-align:center
         "),
                   
                   
                   
                   fileInput(ns("ambil_file_data"), "Choose .txt/.csv/.xlsx File",
                             accept = c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv",
                               ".xlsx")
                             
                   ),
                   
                   
                   
                   radioButtons(ns("pemisah_variabel"), "Separator",
                                choices = c(Comma = ",",
                                            Semicolon = ";",
                                            Tab = "\t",
                                            xlsx = "xlsx"),
                                selected = ",", inline = TRUE)
                   
                   
                 ),
                 
                 
                 
                 
                 mainPanel(
                   
                   
                   uiOutput(ns("data_anda")),
                   
                   br(),
                   
                   DT::DTOutput(ns("dataku")),
                   
                   br(),
                   br(),
                   
                   verbatimTextOutput(ns("informasi_data")),
                   
                   
                   
                 ) #Akhir main panel
                 
                 
                 
               ),
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               
               br()
               
               
               
      ), #Akhir tabpanel Upload Data Anda
      
      
      
      tabPanel(title = tags$h5( tags$img(src = "pilih2.gif", width = "30px"), 'Select Variables'),
               
               
               
               uiOutput(ns("pilih_variabel")),
               
               
               
               # br(),
               
               #uiOutput(ns("pemilihan_variabel_checkboxGroupInput")),
               
               #br(),
               
               # DT::DTOutput(ns("data_yang_dianalisis")),
               
               
               br(),
               
               
               uiOutput(ns("pemilihan_variabel_chooser")),
               
               
               
               
               
               
               br()
               
               
               
      ), #Akhir tabpanel Pilih Variabel yang Akan Dianalisis
      
      
      
      
      
      
      
      
      
      
      
      
      tabPanel(title = tags$h5( tags$img(src = "friedman_logo.png", width = "30px"), 'Friedman Test'),
               
               
               
               h2("Result of Friedman Test", style="
    font-family: 'cursive';
    color: 	blue;
    font-size:30px;
    font-weight: bold; 
    text-align:center
    
    "),
               
               
               
               br(),
               
               sliderInput(ns("decimals_friedman"), "Decimals:",
                           min = 0, max = 30,
                           value = 4),
               
               
               DT::DTOutput(ns("hasil_uji_friedman")),
               
               
               
               br(),
               br(),
               
               
               h2("Result of Multiple Comparison Test with Wilcoxon Test", style="
    font-family: 'cursive';
    color: 	blue;
    font-size:30px;
    font-weight: bold; 
    text-align:center
    
    "),
               
               
               radioButtons(ns("Friedman_wilcoxon_test_method"), h4("Method:",style="color:orange;text-shadow: -1px 0 black,
                                                     0 1px black, 1px 0 black, 0 -1px black; text-align:left"), c('Exact'='1', 'Asymptotic' = '2'  ), inline=TRUE, selected = "2"   ),
               
               
               
               
               
               DT::DTOutput(ns("Friedman_wilcoxon_test")),
               
               
               br()
               
               
               
      ), #Akhir tabsetpanel Friedman Test
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      tabPanel(title = tags$h5( tags$img(src = "chart.png", width = "30px"), 'STATCAL in Article'),
               
               
               
               #uiOutput(ns("pilih_variabel_independen")),
               
               
               
               
               includeHTML("statcal_in_article.html"),
               
               
               br(),
               
               DT::DTOutput(ns("data_statcal_in_article")),
               
               
               
               br(),
               
               
               # uiOutput(ns("pemilihan_variabel_dependen_radiobutton")),
               
               
               br()
               
               
               
      ) #Akhir tabpanel Pilih Variabel yang Akan Dianalisis
      
      
      
      
      
      
      
      
      
    ), #Akhir tabsetPanel
    
    
    
    
    
    br()
    
  ) #Akhir Fluidpage
  
  
} #Akhir dari modul_friedman_test_ui

#Akhir dari modul_friedman_test_ui
#Akhir dari modul_friedman_test_ui
#Akhir dari modul_friedman_test_ui
#Akhir dari modul_friedman_test_ui











































































########################################
################Server##################
########################################



modul_friedman_test_server <- function(input, output, session) {
  
  
  
  kirim_nama_subjek_area <- function()
  {
    
    dat <- read.xlsx("literatur.xlsx")
    dat <- as.data.frame(dat)
    X <- dat[,c("Subject.Area")]
    X <- as.factor(X)
    bidang = levels(X)
    
    return(bidang)
    
    
  }
  
  
  
  ############
  
  output$pemilihan_bidang_artikel <- renderUI({
    
    
    
    checkboxGroupInput(session$ns("terpilih_bidang_artikel"), label="Choose Subject Area:", choices = c( kirim_nama_subjek_area()), selected=c())
    
    
    
    
  })
  
  
  
  ############Tampilkan Data Artikel
  
  
  output$data_artikel <- DT::renderDT({
    
    literatur <- read_excel("literatur.xlsx")
    
    nama = colnames(literatur)
    
    literatur = as.data.frame(literatur)
    X <- literatur[,6]
    
    Y <- input$terpilih_bidang_artikel
    
    indeks = X %in% Y
    indeks = which(indeks == TRUE)
    
    data_artikel = literatur[c(indeks),]
    
    colnames(data_artikel) = nama
    
    print(data_artikel)
    
  })
  
  
  
  
  
  
  
  
  ##########################
  
  
  
  kirim_data <- reactive({
    
    
    ambil_file_data <- input$ambil_file_data
    
    if(is.null(ambil_file_data))
      return(NULL)
    
    
    
    ##Keren juga ya, output$data_anda bisa diletak di reactive
    output$data_anda <- renderUI({
      
      
      
      h2("Your Data", style="
    font-family: 'cursive';
    color: 	blue;
    font-size:30px;
    font-weight: bold; 
    text-align:center
    
    ")
      
      
    })
    
    
    ##############
    
    output$pilih_variabel <- renderUI({
      
      
      
      h2("Select Variables", style="
    font-family: 'cursive';
    color: 	blue;
    font-size:30px;
    font-weight: bold; 
    text-align:center
    
    ")
      
      
    })
    
    
    
    ################
    
    
    
    
    output$pemilihan_variabel_checkboxGroupInput <- renderUI({
      
      
      
      checkboxGroupInput(session$ns("terpilih_checkboxGroupInput"), label="Select Variables:", choices = c( kirim_nama()), selected=c())
      
      
      
      
    })
    
    
    
    
    
    
    
    
    
    
    
    
    output$pemilihan_variabel_chooser <- renderUI({
      
      
      
      selectizeInput(session$ns('terpilih_variabel_friedman_chooser'), 'Select Variables:', choices = kirim_nama(), multiple = TRUE
      )
      
      
      
      
    })
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    ###############
    
    
    pemisah_variabel = input$pemisah_variabel
    
    
    
    #######Memproses Jika Data yang Diinput .xlsx
    
    if(pemisah_variabel == 'xlsx')
    {
      
      inFile <- input$ambil_file_data
      
      if(is.null(inFile))
        return(NULL)
      file.rename(inFile$datapath,
                  paste(inFile$datapath, ".xlsx", sep=""))
      data <- read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
      
      return(data)
      
    }
    
    
    #Memproses Jika Data yang Diupload .txt atau .csv
    if(pemisah_variabel != 'xlsx')
    {
      
      data <- read.csv(ambil_file_data$datapath, sep = input$pemisah_variabel)
      
      
      return(data)
      
    }
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
  })
  
  
  
  #########Mencetak Data yang Telah Diupload
  
  output$dataku <- DT::renderDT({
    
    
    kirim_data()
    
  })
  
  
  
  
  kirim_nama <- function()
  {
    
    dat <- kirim_data()
    
    nama <- colnames(dat)
    
    return(nama)
    
  }
  
  
  
  
  ###########
  ###########
  
  
  output$data_yang_dianalisis <- DT::renderDT({
    
    dat <- kirim_data()
    
    variabel_terpilih <- input$terpilih_checkboxGroupInput
    
    dat <- dat[c(variabel_terpilih)]
    
    print(dat)
    
    
  })
  
  
  
  
  ##################Hitung Uji Friedman
  
  fungsi_hitung_friedman <- function()
  {
    
    dat <- kirim_data()
    
    dat <- as.data.frame(dat)
    
    variabel_terpilih <- input$terpilih_variabel_friedman_chooser
    variabel_terpilih <- unlist(variabel_terpilih)
    
    dat <- dat[c(variabel_terpilih)]
    
    
    print(dat)
    
    name = colnames(dat)
    length_name = length(name)
    
    number_of_row = length(dat[,1])
    
    save_group = vector(mode="character")
    save_observation = vector(mode="character")
    save_response = vector(mode="numeric")
    
    
    
    for(i in 1 : length_name)
    {
      
      
      group = c(rep(name[i],number_of_row))
      group = as.character(group)
      row_name = c(1:number_of_row)
      row_name = as.character(row_name)
      response = dat[,i]
      response = as.numeric(response)
      
      
      if(i==1)
      {
        save_group = group
        save_observation = row_name
        save_response = response
        
      }
      
      
      if(i>1)
      {
        save_group = c(save_group,group)
        save_observation = c(save_observation,row_name)
        save_response = c(save_response, response)
        
      }
      
      
      
      
    }
    
    
    save_group = as.character(save_group)
    save_observation =as.character(save_observation)
    save_response = as.numeric(save_response)
    
    
    
    
    n_dat = data.frame(save_group,save_observation,save_response)
    
    colnames(n_dat) = c("group","observation","response")
    
    
    
    result = stats::friedman.test(response~group|observation, data = n_dat)
    
    
    
    statistic_of_Friedman = result$statistic
    statistic_of_Friedman = round(statistic_of_Friedman, digits = input$decimals_friedman)
    
    p_value = result$p.value
    p_value = round(p_value, digits = input$decimals_friedman)
    
    
    
    
    df = result$parameter
    
    critical_value = qchisq(.95, df=df)
    critical_value = round(critical_value, digits = input$decimals_friedman)
    
    hasil_statistics = c(statistic_of_Friedman, df, critical_value, p_value)
    
    
    hasil_summary = c("Statistic of Friedman","Degree of Freedom","Chi-Square Critical Value (Significance Level 5%)", "P-Value")
    
    dat_Friedman = data.frame(hasil_summary, hasil_statistics)
    
    colnames(dat_Friedman) = c("Results", "Values")
    
    rownames(dat_Friedman) = NULL
    
    return(dat_Friedman)
    
    
    
  }
  
  
  
  ############Hasil Uji Friedman
  
  output$hasil_uji_friedman <- DT::renderDT({
    
    
    
    hasil <- fungsi_hitung_friedman()
    
    
    wb <- openxlsx::createWorkbook()
    
    
    hs1 <- openxlsx::createStyle(fgFill = "#DCE6F1", halign = "CENTER", textDecoration = "italic",
                                 border = "Bottom")
    
    
    hs2 <- openxlsx::createStyle(fontColour = "#ffffff", fgFill = "#4F80BD",
                                 halign = "center", valign = "center", textDecoration = "bold",
                                 border = "TopBottomLeftRight")
    
    openxlsx::addWorksheet(wb, "Friedman Test", gridLines = TRUE)
    
    openxlsx::writeDataTable(wb, "Friedman Test", hasil, rowNames = FALSE, startRow = 2, startCol = 2, tableStyle = "TableStyleMedium21")
    
    
    
    openxlsx::openXL(wb)
    
    
    print(hasil)
    
    
  })
  
  
  
  
  
  #############
  
  
  
  output$informasi_data <- renderPrint({ 
    
    
    dat <- kirim_data()
    dat <- as.data.frame(dat)
    
    
    
    
    jumlah_baris = dim(dat)[1]
    jumlah_kolom = dim(dat)[2]
    
    
    data_kosong_hapus = dat[complete.cases(dat), ]
    jumlah_baris2 = dim(data_kosong_hapus)[1]
    jumlah_baris_kosong = jumlah_baris - jumlah_baris2
    
    
    cat(sprintf("Number of Row = %d\nNumber of Column = %d\nNumber of Row with Empty Data = %d", jumlah_baris, jumlah_kolom, jumlah_baris_kosong))
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$data_statcal_in_article <- DT::renderDT({
    
    
    library(openxlsx)
    df = read.xlsx("STATCAL IN ARTICLE.xlsx")
    
    nama = c("Title", "Journal", "Year", "Link", "STATCAL Quote in The Article", "Author")
    
    colnames(df) = nama
    
    print(df)
    
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ###########Multiple Comparison: Wilcoxon###########
  
  
  
  Friedman_wilcoxon_test <- reactive({
    
    
    dat <- kirim_data()
    
    dat <- as.data.frame(dat)
    
    variabel_terpilih <- input$terpilih_variabel_friedman_chooser
    variabel_terpilih <- unlist(variabel_terpilih)
    
    dat <- dat[c(variabel_terpilih)]
    
    
    
    
    get_name = colnames(dat)
    
    name = colnames(dat)
    length_name = length(name)
    
    
    
    
    length_dat = length(dat)
    
    
    decimal = input$decimals_friedman
    
    
    make_matrix = matrix(nrow = length_dat, ncol = length_dat)
    
    
    get_method = input$Friedman_wilcoxon_test_method
    get_method = as.numeric(get_method)
    
    
    if(get_method==1)
    {
      choose_exact = TRUE
    }
    
    if(get_method==2)
    {
      choose_exact = FALSE
    }
    
    
    for(i in 1 : length_dat)
    {
      
      for(j in i : length_dat)
      {
        result = wilcox.test(dat[,i],dat[,j], correct = FALSE, paired = TRUE, exact = choose_exact)
        
        p_value = result$p.value
        p_value = unlist(p_value)
        p_value = round(p_value, digits = decimal)
        p_value
        
        make_matrix[i,j] = p_value
        
        
      }
      
    }
    
    make_matrix = as.data.frame(make_matrix)
    
    
    
    
    colnames(make_matrix) = c(get_name)
    rownames(make_matrix) = c(get_name)
    
    #make_matrix = colnames(col_name)
    #make_matrix = rownames(row_name)
    
    
    DT::datatable(make_matrix, rownames = TRUE)
    
    
    
    
    
  })
  
  
  output$Friedman_wilcoxon_test <- DT::renderDT({
    print(Friedman_wilcoxon_test() )
  })
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
} #akhir dari modul_friedman_test_server

#akhir dari modul_friedman_test_server
#akhir dari modul_friedman_test_server
#akhir dari modul_friedman_test_server

















































































ui <- fluidPage(
  
  
  includeHTML("intro_home.html"),
  
  
  uiOutput("modul_friedman_test"),
  
  
  br()
  
) #Akhir dari UI











server <- function(input, output) {
  
  
  
  
  
  output$modul_friedman_test <- renderUI({
    
    
    
    #source("module//modul_friedman_test.R")
    callModule(module = modul_friedman_test_server, id = "modul_friedman_test")
    modul_friedman_test_ui(id = "modul_friedman_test")
    
    
    
  })
  
  
  
  
  
  
  
  
  
  
  
} #Akhir dari server










shinyApp(ui, server)














