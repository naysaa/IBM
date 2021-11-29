output$temp_line <- renderPlot({
  ggplot(city_weather_bike_df, aes((x = FORECASTDATETIME, y = TEMPERATURE)) + 
           geom_line()
           geom_point() + 
           geom_text()
         
         
         
         output$bike_line <- renderPlot(
           geom_line(city_weather_bike_df, aes(x = FORECASTDATETIME, y = BIKE_PREDICTION))+
             geom_point()+
             geom_text(),
           
           output$bike_date_output <- renderText({}))
         
