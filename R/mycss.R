library(tableHTML)


mycss <- make_css(list('table', 
        c('boder-collapse', 'border', "float"), 
        c('collapse', 'none', "initial")), 
                  
list(".float-table", 
     c('display', "font-family", "-webkit-font-smoothing",
       "font-size", 'overflow', 'width', "padding","float"), 
        c("block", "san-serif", "antialiased", "115%", "auto",
       "50%", "0.5cm", "initial")),
        list("thead,firsttablerow",
                                                                    c("boder-top", "background-color", "color", "font-weigtht", "font-style", "pandding", "text-align","veritical-align", "boder-bottom", "width", "float"), 
                                                                    c("solid", "rgb(255, 255, 255)","black", "normal", "normal",
                                                                      "0.5cm", "center", "top", "double", "50%", "initial"))
,
list("tbody, lasttablerow", 
c("background-color", "color", "padding", "border-bottom", "width", "height", "text-align", "vertical-align", "float"), 
c("rgb(255, 255, 255)", "black", "0.5cm", "solid", "50%", "auto", "left", "top", "initial")), file="mycss.css")
print(mycss)


tableHTML(head(mtcars)) %>%add_css_row(css=list(c('border-top', 'border-bottom'), c('3px solid black', "3px double black")), rows = c(1))%>%add_css_row(css = list(c('border-bottom'), c('3px solid black'),list(c("0px"))), rows = c(7)) %>% add_css_row(css = list(c("border", "border-collapse", "padding"), c("0px", "collapse", "15px")), rows = c(1:7))

