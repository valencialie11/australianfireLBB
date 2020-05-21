function(input, output) {
  output$plot1 <- renderPlotly({
    tempplot1 <- temp111 %>% 
      filter(year == input$year1) %>% 
      filter(city_name == input$city)
    
    tempplott1 <- temp112 %>% 
      filter(city_name == input$city)
    
    if(input$year1 == "Overall"){
      plot_1 <- ggplot(data = tempplott1, mapping = aes(x = year, y = mean_temp, text = paste("Year:", year, "<br>", "Max/Min:", temp_type, "<br>", "Mean Temperature:", round(mean_temp)))) +
        geom_line(group = 1, aes(color = temp_type)) +
        labs(x = "Year", y = "Average Temperature") +
        theme_minimal() +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(size = 8))
    }
    
    else {
      plot_1 <-  ggplot(data = tempplot1, mapping = aes(x = month, y = mean_temperature, text = paste("Max/Min:", temp_type, "<br>", "Month:", month, "<br>", "Mean Temperature:", round(mean_temperature)))) +
        geom_line(group = 1, aes(color = temp_type)) +
        labs(x = "Month", y = "Average Temperature") +
        theme_minimal() +
        theme(legend.position = "none") +
        theme(axis.text.x = element_text(size = 8))}
    
    ggplotly(plot_1, tooltip = "text", height = 380)})
  
  
    
  
output$plot2 <- renderPlotly({    
    tempplot_2 <- temp222 %>% 
      filter(year == input$year2)
  
      if(input$order == "Ascending"){
       plot_2 <-  ggplot(data = tempplot_2, mapping = aes(x = reorder(city_name, -mean_temp), y = mean_temp,
                                                          text = paste("Average Temperature:", round(mean_temp)))) +
        geom_col(aes(fill = mean_temp)) +
        labs(x = NULL, y = "Average Temperature") +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_fill_viridis_c() +
        coord_flip()+
        theme(axis.text.x = element_text(size = 6))}
      
      else if(input$order == "Descending"){
        plot_2 <- ggplot(data = tempplot_2, mapping = aes(x = reorder(city_name, mean_temp), y = mean_temp, text = paste("Average Temperature:", round(mean_temp)))) +
        geom_col(aes(fill = mean_temp)) +
        labs(x = NULL, y = "Average Temperature") +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_fill_viridis_c() +
        coord_flip()+
        theme(axis.text.x = element_text(size = 6))}
        
      
      ggplotly(plot_2, tooltip = "text", height = 380)
}) 

output$plot3 <- renderLeaflet({
  rain1112 <- rain111 %>%
    filter(month == input$month) %>% 
    filter(year == input$year3)
    
  ico <- makeIcon(
    iconUrl = "unnamed.png",
    iconWidth= 20, iconHeight=20
  )
  map1 <- leaflet()
  map1 <- addTiles(map1)
  content_popup <- paste(sep = " ",
                         "Amount of Rainfall (in millimeters) :", round(rain1112$mean_rainfall), "<br>"
  , "Name of City:", rain1112$city_name)
  map1 <- addMarkers(map = map1, 
                     lng =  rain1112$long, # garis bujur
                     lat = rain1112$lat, # garis lintang
                     icon = ico,
                     popup = content_popup, #popup atau tulisan
                     
                     clusterOptions = markerClusterOptions() # membuat cluster supaya tidak overlap
                     
  )

})

output$plot4 <- renderPlot({
  
  rain5 <- rain4 %>% 
    filter(Var2 == input$cityname) 
  
  rain5
ggplot(data = rain5, mapping = aes(x = "", y = Freq, fill = Var1)) +
    geom_bar(width = 1, stat = "identity") + coord_polar("y", start=0) +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank()) +
  labs(y = "Number of poor/good quality rain")
  })

output$plot5 <- renderPlotly({
  
  rainplot1 <- rain112 %>% 
    filter(year == input$year4) %>% 
    filter(city_name == input$cityn) 
    
  rainplot2 <- rain113 %>% 
    filter(city_name == input$cityn)
  
  
  rain112
  
  if(input$year4 == "Overall"){
    plot_5 <- ggplot(data = rainplot2, mapping = aes(x = year, y = mean_rain1, text = paste("Year:", year, "<br>", "Mean Rainfall:", round(mean_rain1)))) +
      geom_line(group = 1, color = "red") +
      labs(x = "Year", y = "Average Rainfall") +
      theme_minimal() +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(size = 8))
  }
  
  else {
    plot_5 <-  ggplot(data = rainplot1, mapping = aes(x = factor(month, levels = month.name), y = mean_rain, text = paste("Month:", month, "<br>", "Mean Rainfall:", round(mean_rain)))) +
      geom_line(group = 1, color = "blue") +
      labs(x = "Month", y = "Average Rainfall") +
      theme_minimal() +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(size = 6))}
  
  ggplotly(plot_5, tooltip = "text", height = 200)}) 
  
  
output$GoodQualityBox <- renderValueBox({
  
  rainperc2 <- rainperc %>% 
    filter(Var2 == input$cityname)
  
if(input$cityname == "Adelaide") {
  valueBox("99.498%", "Good Quality Rain", icon = icon("check-circle"), color = "green")}
  
else if(input$cityname == "Brisbane") {
  valueBox("71.284%", "Good Quality Rain", icon = icon("check-circle"), color = "green")}
  
else if(input$cityname == "Canberra") {
    valueBox("43.642%", "Good Quality Rain", icon = icon("check-circle"), color = "green")}

  else if(input$cityname == "Melbourne") {
    valueBox("98.509%", "Good Quality Rain", icon = icon("check-circle"), color = "green")}
  
  else if(input$cityname == "Perth") {
    valueBox("97.784%", "Good Quality Rain", icon = icon("check-circle"), color = "green")}
  
  else {
    valueBox("93.632%", "Good Quality Rain", icon = icon("check-circle"), color = "green")}
  
})

output$PoorQualityBox <- renderValueBox({
  
  rainperc2 <- rainperc %>% 
    filter(Var2 == input$cityname)
  
  if(input$cityname == "Adelaide") {
    valueBox("0.502%", "Poor Quality Rain", icon = icon("times-circle"), color = "red")}
  
  else if(input$cityname == "Brisbane") {
    valueBox("28.716%", "Poor Quality Rain", icon = icon("times-circle"), color = "red")}
  
  else if(input$cityname == "Canberra") {
    valueBox("56.358%", "Poor Quality Rain", icon = icon("times-circle"), color = "red")}
  
  else if(input$cityname == "Melbourne") {
    valueBox("1.491%", "Poor Quality Rain", icon = icon("times-circle"), color = "red")}
  
  else if(input$cityname == "Perth") {
    valueBox("2.216%", "Poor Quality Rain", icon = icon("times-circle"), color = "red")}
  
  else {
    valueBox("6.368%", "Poor Quality Rain", icon = icon("times-circle"), color = "red")}
})

output$BestCity <- renderValueBox({
  valueBox("Adelaide", "Best City", icon = icon("trophy"))
})

output$datarain <- renderDataTable({
  datatable(rain2, options = list(scrollX = T, pageLength = 5))})


output$nasaleaf2 <- renderLeaflet({
  magcol <- function(x) {
    if (x < 350){
      x <- "yellow"
    }else if (x >= 350 & x < 400){
      x <- "orange"
    }else{
      x <- "red"}
    
  }

 nasaleaf <- nasaleaf %>% 
   filter(acq_date == input$acqdate)
 
  leaflet(nasaleaf) %>% 
    addTiles() %>%
    addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
    addCircleMarkers(lng =  ~longitude, # garis bujur
                     lat = ~latitude, # garis lintang
                     color = ~colour,
                     weight = 1,
                     radius = 1,
                     popup = paste(
                     "Average brightness: ", nasaleaf$mean_bright),
                     fillOpacity = 0.5) %>% 
    
    addLegend("bottomright", labels = c("Brightness < 350", "Brightness: 350-400", "Brightness > 400"),
                               colors = c("yellow", "orange", "red"))
                     
  
  
  

})

output$datafire <- renderDataTable({
  datatable(nasa_fire, options = list(scrollX = T, pageLength = 10))})

}



