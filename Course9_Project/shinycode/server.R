# server.R
library(shiny)

# BMI Classification
BMI_category_fun <- function(weight, height_in_inches)  
{ 
  if ((weight/(height_in_inches)^2)*703 <= 15) return ('Very severely underweight') 
  else 
    if ((weight/(height_in_inches)^2)*703 <= 16) return ('Severely underweight')
  else
    if ((weight/(height_in_inches)^2)*703 <= 18.5) return ('Underweight')  
  else 
    if ((weight/(height_in_inches)^2)*703 <= 25) return ('Normal')  
  else
    if ((weight/(height_in_inches)^2)*703 <= 30) return ('Overweight')  
  else    
    if ((weight/(height_in_inches)^2)*703 <= 35) return ('Moderately obese')  
  else    
    if ((weight/(height_in_inches)^2)*703 <= 40) return ('Severely obese')  
  else
    if ((weight/(height_in_inches)^2)*703 >  40) return ('Very severely obese')  
  else return ('?')
}


# This function takes as inputs the gender, age, and calculated BMI and returns the percentile.
percentile_fun <- function(gender, age, weight, height_in_inches) 
{
  if   (gender %in% c('M') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 19.4)) return ('5th')
  else 
    if (gender %in% c('M') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 20.7)) return ('10th')
  else
    if (gender %in% c('M') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 21.4)) return ('15th')  
  else 
    if (gender %in% c('M') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 22.9)) return ('25th')  
  else
    if (gender %in% c('M') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 25.6)) return ('50th')  
  else    
    if (gender %in% c('M') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 29.9)) return ('75th')  
  else    
    if (gender %in% c('M') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 32.3)) return ('85th')  
  else
    if (gender %in% c('M') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 33.8)) return ('90th')  
  else    
    if (gender %in% c('M') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 36.5)) return ('95th') 
  
  else    
    if (gender %in% c('M') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 21.0)) return ('5th')
  else 
    if (gender %in% c('M') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 22.4)) return ('10th')
  else
    if (gender %in% c('M') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 23.3)) return ('15th')  
  else 
    if (gender %in% c('M') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 24.9)) return ('25th')  
  else
    if (gender %in% c('M') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 28.1)) return ('50th')  
  else    
    if (gender %in% c('M') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 32.0)) return ('75th')  
  else    
    if (gender %in% c('M') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 34.1)) return ('85th')  
  else
    if (gender %in% c('M') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 36.2)) return ('90th')  
  else    
    if (gender %in% c('M') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 40.5)) return ('95th')  
  
  else        
    if (gender %in% c('M') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 21.2)) return ('5th')
  else 
    if (gender %in% c('M') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 22.9)) return ('10th')
  else
    if (gender %in% c('M') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 24.0)) return ('15th')  
  else 
    if (gender %in% c('M') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 25.4)) return ('25th')  
  else
    if (gender %in% c('M') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 28.2)) return ('50th')  
  else    
    if (gender %in% c('M') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 31.7)) return ('75th')  
  else    
    if (gender %in% c('M') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 34.4)) return ('85th')  
  else
    if (gender %in% c('M') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 36.1)) return ('90th')  
  else    
    if (gender %in% c('M') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 39.6)) return ('95th')  
  
  else        
    if (gender %in% c('M') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 21.5)) return ('5th')
  else 
    if (gender %in% c('M') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 22.9)) return ('10th')
  else
    if (gender %in% c('M') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 23.9)) return ('15th')  
  else 
    if (gender %in% c('M') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 25.5)) return ('25th')  
  else
    if (gender %in% c('M') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 28.2)) return ('50th')  
  else    
    if (gender %in% c('M') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 32.0)) return ('75th')  
  else    
    if (gender %in% c('M') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 34.5)) return ('85th')  
  else
    if (gender %in% c('M') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 37.1)) return ('90th')  
  else    
    if (gender %in% c('M') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 39.9)) return ('95th')    
  
  else        
    if (gender %in% c('M') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 21.3)) return ('5th')
  else 
    if (gender %in% c('M') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 22.7)) return ('10th')
  else
    if (gender %in% c('M') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 23.8)) return ('15th')  
  else 
    if (gender %in% c('M') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 25.3)) return ('25th')  
  else
    if (gender %in% c('M') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 28.8)) return ('50th')  
  else    
    if (gender %in% c('M') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 32.5)) return ('75th')  
  else    
    if (gender %in% c('M') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 34.7)) return ('85th')  
  else
    if (gender %in% c('M') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 37.0)) return ('90th')  
  else    
    if (gender %in% c('M') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 40.0)) return ('95th')    
  
  else        
    if (gender %in% c('M') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 21.4)) return ('5th')
  else 
    if (gender %in% c('M') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 22.9)) return ('10th')
  else
    if (gender %in% c('M') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 23.8)) return ('15th')  
  else 
    if (gender %in% c('M') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 25.6)) return ('25th')  
  else
    if (gender %in% c('M') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 28.3)) return ('50th')  
  else    
    if (gender %in% c('M') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 31.3)) return ('75th')  
  else    
    if (gender %in% c('M') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 33.5)) return ('85th')  
  else
    if (gender %in% c('M') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 35.4)) return ('90th')  
  else    
    if (gender %in% c('M') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 37.8)) return ('95th')    
  
  else        
    if (gender %in% c('M') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 20.7)) return ('5th')
  else 
    if (gender %in% c('M') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 21.8)) return ('10th')
  else
    if (gender %in% c('M') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 22.8)) return ('15th')  
  else 
    if (gender %in% c('M') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 24.4)) return ('25th')  
  else
    if (gender %in% c('M') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 27.0)) return ('50th')  
  else    
    if (gender %in% c('M') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 29.6)) return ('75th')  
  else    
    if (gender %in% c('M') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 31.3)) return ('85th')  
  else
    if (gender %in% c('M') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 32.7)) return ('90th')  
  else    
    if (gender %in% c('M') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 34.5)) return ('95th')    
  
  
  if   (gender %in% c('F') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 18.8)) return ('5th')
  else 
    if (gender %in% c('F') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 19.9)) return ('10th')
  else
    if (gender %in% c('F') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 20.6)) return ('15th')  
  else 
    if (gender %in% c('F') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 21.7)) return ('25th')  
  else
    if (gender %in% c('F') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 25.3)) return ('50th')  
  else    
    if (gender %in% c('F') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 31.5)) return ('75th')  
  else    
    if (gender %in% c('F') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 36.0)) return ('85th')  
  else
    if (gender %in% c('F') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 38.0)) return ('90th')  
  else    
    if (gender %in% c('F') & age %in% c('20-29') & ((weight/(height_in_inches)^2)*703 <= 43.9)) return ('95th') 
  
  else    
    if (gender %in% c('F') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 19.4)) return ('5th')
  else 
    if (gender %in% c('F') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 20.6)) return ('10th')
  else
    if (gender %in% c('F') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 21.6)) return ('15th')  
  else 
    if (gender %in% c('F') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 23.4)) return ('25th')  
  else
    if (gender %in% c('F') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 27.2)) return ('50th')  
  else    
    if (gender %in% c('F') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 32.8)) return ('75th')  
  else    
    if (gender %in% c('F') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 36.0)) return ('85th')  
  else
    if (gender %in% c('F') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 38.1)) return ('90th')  
  else    
    if (gender %in% c('F') & age %in% c('30-39') & ((weight/(height_in_inches)^2)*703 <= 41.6)) return ('95th')  
  
  else        
    if (gender %in% c('F') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 19.3)) return ('5th')
  else 
    if (gender %in% c('F') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 20.6)) return ('10th')
  else
    if (gender %in% c('F') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 21.7)) return ('15th')  
  else 
    if (gender %in% c('F') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 23.3)) return ('25th')  
  else
    if (gender %in% c('F') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 27.3)) return ('50th')  
  else    
    if (gender %in% c('F') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 32.4)) return ('75th')  
  else    
    if (gender %in% c('F') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 36.2)) return ('85th')  
  else
    if (gender %in% c('F') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 38.1)) return ('90th')  
  else    
    if (gender %in% c('F') & age %in% c('40-49') & ((weight/(height_in_inches)^2)*703 <= 43.0)) return ('95th')  
  
  else        
    if (gender %in% c('F') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 19.7)) return ('5th')
  else 
    if (gender %in% c('F') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 21.3)) return ('10th')
  else
    if (gender %in% c('F') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 22.1)) return ('15th')  
  else 
    if (gender %in% c('F') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 24.0)) return ('25th')  
  else
    if (gender %in% c('F') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 28.3)) return ('50th')  
  else    
    if (gender %in% c('F') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 33.5)) return ('75th')  
  else    
    if (gender %in% c('F') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 36.4)) return ('85th')  
  else
    if (gender %in% c('F') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 39.3)) return ('90th')  
  else    
    if (gender %in% c('F') & age %in% c('50-59') & ((weight/(height_in_inches)^2)*703 <= 41.8)) return ('95th')    
  
  else        
    if (gender %in% c('F') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 20.7)) return ('5th')
  else 
    if (gender %in% c('F') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 21.6)) return ('10th')
  else
    if (gender %in% c('F') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 23.0)) return ('15th')  
  else 
    if (gender %in% c('F') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 24.8)) return ('25th')  
  else
    if (gender %in% c('F') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 28.8)) return ('50th')  
  else    
    if (gender %in% c('F') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 33.5)) return ('75th')  
  else    
    if (gender %in% c('F') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 36.6)) return ('85th')  
  else
    if (gender %in% c('F') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 38.5)) return ('90th')  
  else    
    if (gender %in% c('F') & age %in% c('60-69') & ((weight/(height_in_inches)^2)*703 <= 41.1)) return ('95th')    
  
  else        
    if (gender %in% c('F') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 20.1)) return ('5th')
  else 
    if (gender %in% c('F') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 21.6)) return ('10th')
  else
    if (gender %in% c('F') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 22.7)) return ('15th')  
  else 
    if (gender %in% c('F') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 24.7)) return ('25th')  
  else
    if (gender %in% c('F') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 28.6)) return ('50th')  
  else    
    if (gender %in% c('F') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 33.4)) return ('75th')  
  else    
    if (gender %in% c('F') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 36.3)) return ('85th')  
  else
    if (gender %in% c('F') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 38.7)) return ('90th')  
  else    
    if (gender %in% c('F') & age %in% c('70-79') & ((weight/(height_in_inches)^2)*703 <= 42.1)) return ('95th')    
  
  else        
    if (gender %in% c('F') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 19.3)) return ('5th')
  else 
    if (gender %in% c('F') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 20.7)) return ('10th')
  else
    if (gender %in% c('F') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 22.0)) return ('15th')  
  else 
    if (gender %in% c('F') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 23.1)) return ('25th')  
  else
    if (gender %in% c('F') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 26.3)) return ('50th')  
  else    
    if (gender %in% c('F') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 29.7)) return ('75th')  
  else    
    if (gender %in% c('F') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 31.6)) return ('85th')  
  else
    if (gender %in% c('F') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 32.5)) return ('90th')  
  else    
    if (gender %in% c('F') & age %in% c('80+') & ((weight/(height_in_inches)^2)*703 <= 35.2)) return ('95th')   
  else return ('100th')         

}


shinyServer(
  function(input, output) {
    output$ogender = renderText({input$gender})
    output$oage = renderText({input$age})
    output$oweight = renderText({input$weight}) 
    output$oheight_in_inches = renderText({input$height_in_inches}) 
    output$oBMI = renderText(({input$weight}/(({input$height_in_inches})^2))*703) 
    output$oCategory = renderText(BMI_category_fun({input$weight}, {input$height_in_inches}))
    output$oPct = renderText(percentile_fun({input$gender}, {input$age}, {input$weight}, {input$height_in_inches}))
  }
)




