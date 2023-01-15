#' ***** required Libraries, mentioned in details section in the documentation of each function, are loaded in the script sourcing this file (dashboard-atp-tour.Rmd) to avoid multiple loads time, installing if required is taken into consideration
#' ***** Those functions are specific to this project data source, based on the return of data manipulation functions included in the data_wrangling.R and sourced in the dashboard-atp-tour.rmd files 
#' ***** In the following steps, we can consider creating a single more generic function for the barcharts returns



#' *********************************************************************************************************************
#' @title 	            final_appearance.R                                       
#' @author              Khaoula Aroui                                            
#' @description 	      provides a stacked barchart for the top 10 appearances of countries
#'                      in the finals according to ranking (winner or 2nd place)
#'                      for a specific tourney                      
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param data          table to treat: includes 3 mandatory columns (Country, Appearances, and group): the return of
#'                      the most_final_appearances function  (mandatory)
#' @param xaxis         the variable for the Country (mandatory)                           
#' @param yaxis         The variable for the Appearances (mandatory)                           
#' @param axiscolor     The color for the axis (mandatory)        
#' @param fill_color    the stacked bars fill colors, a list of 2 colors for the 2 groupping categories (mandatory)                    
#' @param y_label_font  The label font for the countries (mandatory)                           
#' @param y_label_color  the label color for the countries (mandatory)                           
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns     a plotly stacked barchart
#' @details     required packages are "ggplot2", "plotly", and "dplyr"                   
#' @examples                                                                       
#' final_appearance(Both, "Country", "Appearances", "group",  "grey10", c("#87CEFA","#00C5CD"),'Rockwell', '#009966')                                           
#' **********************************************************************************************************************

final_appearance=function(data, xaxis, yaxis, group, axiscolor, fill_color, y_label_font, y_label_color) {
  chart=ggplot(data, aes(x=.data[[xaxis]], y=.data[[yaxis]], fill = .data[[group]]))+
    geom_bar(stat="identity" )+coord_flip()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          panel.background = element_blank(), axis.line = element_line(colour = axiscolor)) +theme(legend.title=element_blank())+
    scale_fill_manual(values=fill_color)
  
  fig=plotly::ggplotly(chart)%>%
    layout(yaxis =  list(title = ' ',tickfont = list(family=y_label_font, color=y_label_color, size=15)))%>%
    layout(xaxis = list(title = 'Count of appearances', titlefont = list(size = 12)), legend = list(title=list(text=' ')))%>%
    layout(xaxis = list(tickfont = list(family='Rockwell',size=14)))
  
  return(fig)
  
}


#' ********************************************************************************************************
#' @title 	            top_10_winners_vis.R                                       
#' @author              Khaoula Aroui                                            
#' @description 	      provides a barchart for the top 10 winners according to a specific counting variable
#'                      for a specific tourney                      
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param top_10_df     table to treat: includes 2 mandatory columns: (player_name, total): the
#'                      return of the Top_10 function (mandatory)                               
#' @param variable      The variable for which we want to explore the top 10 winners (mandatory)                           
#' @param fill_color    The bars fill color (mandatory)      
#' @param order         The order for the bars, either "total ascending" or "total descending" (mandatory)            
#' @param x_label_font  The label font for the countries (mandatory)                           
#' @param x_label_color The label color for the countries (mandatory)                           
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns     a plotly barchart
#' @details     required packages are "plotly", and "dplyr"                   
#' @examples                                                                       
#' top_10_winners_vis(top_10_df,"Victories", "#1c8f7a", "total descending", 'Rockwell', '#009966')    
#' top_10_winners_vis(top_10_df,"Aces", "#1c8f7a", "total descending", 'Rockwell', '#009966')    
#' ********************************************************************************************************

top_10_winners_vis=function(top_10_df, variable, fill_color, order, x_label_font, x_label_color){

  fig <- plot_ly(top_10_df,
                 x = top_10_df$player_name,
                 y = top_10_df$total,
                 hoverinfo = 'text',
                 text=paste('</br> Player : ', top_10_df$player_name,
                            '</br> Total ', variable, ': ', top_10_df$total),
                 type = "bar",orientation='v', color=I(fill_color))%>% 
    layout(xaxis = list(categoryorder = order, tickangle=45, tickfont = list(family=x_label_font, color=x_label_color, size=15)))
  return(fig)

}



#' ******************************************************************************************************************************
#' @title 	            evolution.R                                       
#' @author              Khaoula Aroui                                            
#' @description 	      provides a line+markers chart visualizing the Final's contributers' ATP ranking evolution over the years
#'                      for a specific tourney                      
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @param table          table to treat: includes  mandatory columns (the year , the final's winner name, the second placed name, 
#'                      their ranks and their ranking points): the return of the Atp_ranks function case the parameter variable is set to default
#'                      value (mandatory)                               
#' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
#' @returns     a plotly line+ marker chart
#' @details     required packages are "plotly", and "dplyr"                   
#' @examples                                                                       
#' evolution(table)    
#' *******************************************************************************************************************************
 evolution=function(table){
   
   fig <-
     plot_ly(
       table,
       x = ~ year,
       y = ~ winner_rank,
       name = "Winner",
       type = 'scatter',
       mode = 'lines+markers',
       hovertemplate = paste(
         '</br> Player : ',
         table$winner_name,
         '</br> Ranked : ',
         table$winner_rank,
         '</br> With : ',
         table$winner_rank_points,
         ' points',
         '</br> <extra></extra>'
       )
     ) %>%
     add_trace(
       y = ~ loser_rank,
       name = "Second place",
       mode = 'lines+markers',
       hovertemplate = paste(
         '</br> Player : ',
         table$loser_name,
         '</br> Ranked : ',
         table$loser_rank,
         '</br> With : ',
         table$loser_rank_points,
         ' points <extra></extra>'
       )
     ) %>%
     layout(hovermode = 'x unified', yaxis=list(title = "ATP Rank", titlefont = list(size = 14)))%>%
     layout(xaxis = list(tickangle=45, tickfont = list(family='Rockwell',size=14), title=''))
   return(fig)
 }
 
 
 
 #' ************************************************************************************************************************
 #' @title 	             top_5_vis.R                                       
 #' @author              Khaoula Aroui                                            
 #' @description 	       provides a  barchart for the top 5 highest ranked or lowest ranked winners of the finals according 
 #'                      to their ATP Rank
 #'                      for a specific tourney
 #' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
 #' @param top_5_rank    table to treat: includes 3 mandatory columns (Country, Appearances, and group) (mandatory)                               
 #' @param fill_color    The bars fill color (mandatory)      
 #' @param order         The order for the bars, either "total ascending" or "total descending" (mandatory)            
 #' @param x_label_font  The label font for the countries (mandatory)                           
 #' @param x_label_color  the label color for the countries (mandatory)                           
 #' xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
 #' @returns     a plotly  barchart
 #' @details     required packages are "plotly", and "dplyr"                   
 #' @examples                                                                       
 #' top_5_vis(top_5_rank, "#1c8f7a", "total ascending", 'Rockwell', '#009966')    
 #' ************************************************************************************************************************
 top_5_vis=function(top_5_rank, fill_color, order, x_label_font, x_label_color){
   fig <- plot_ly(top_5_rank,
                  x = top_5_rank$winner_name,
                  y = top_5_rank$winner_rank,
                  hoverinfo = 'text',
                  text=paste('</br> Player : ', top_5_rank$winner_name,
                             '</br> Ranked : ', top_5_rank$winner_rank,
                             '</br> In : ', top_5_rank$year,
                             '</br> With : ', top_5_rank$winner_rank_points, ' points'),
                  type = "bar",orientation='v', color=I(fill_color))%>% 
                  layout(xaxis = list(categoryorder = order, tickangle=45, tickfont = list(family=x_label_font, color=x_label_color, size=15)), 
                  yaxis=list(title = "ATP Rank", titlefont = list(size = 12)))%>%add_annotations(text =top_5_rank$year,showarrow = TRUE, arrowcolor='grey')%>%
                  add_annotations(text =top_5_rank$winner_rank_points,  x = ~winner_name, y = ~winner_rank, xanchor = "center", yanchor = "top", font = list(color = "white", size = 13), showarrow = FALSE)
   
   return(fig)
   
 }