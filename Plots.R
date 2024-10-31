#BALANCED ACCURACY
#Install and load the fmsb package
install.packages("fmsb")
library(fmsb)

# Create a matrix with the given data
data<- data.frame(
  row.names = c("Traffic", "Recreational", "Residential", "Other"),
  Tilia = c(0.8774, 0.7702, 0.71653,0.79656),
  Robinia = c(0.67391, 0.5, 0.498829, 0.529412),
  Aesculus = c(0.596044, 0.57907, 0.6, 0.70875),
  Acer = c(0.7968, 0.6406, 0.7082, 0.6997),
  Populus = c(0.64343, 0.6095, 0.6579, 0.6618),
  Other = c(0.74915, 0.51662, 0.91546, 0.5)
  
)

max_min <- data.frame(
  Tilia = c(1, 0.5), Robinia = c(1, 0.5), Aesculus = c(1, 0.5),
  Acer = c(1, 0.5), Populus = c(1, 0.5), Other = c(1, 0.5))

rownames(max_min) <- c("Max", "Min")


df <- rbind(max_min, data)

# Reduce plot margin using par()
op <- par(mar = c(0, 2, 1, 1))

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 2.0,
                                        caxislabels = NULL, title = NULL, calcex = 1.2,...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, calcex=calcex,...
  )
}

# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c(0.5, 0.6, 0.7, 0.8, 0.9, 1),
  color = c("#FC4E07", "#357ba3", "#648f1e", "#a50026")
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#FC4E07", "#357ba3", "#648f1e", "#a50026"),
  text.col = "black", cex = 1.2, pt.cex = 1.5, y.intersp = 0.3
)

par(op)



#-------------------------------------------------------------------------------
#SENSITIVITY

# Create a matrix with the given data
data<- data.frame(
  row.names = c("Traffic", "Recreational", "Residential", "Other"),
  Tilia = c(0.8953, 0.58416, 0.47619, 0.64179),
  Robinia = c(0.36825, 0, 0, 0.05882),
  Aesculus = c(0.193798, 0.160622, 0.2, 0.4242),
  Acer = c(0.7438, 0.5355, 0.6933, 0.68),
  Populus = c(0.50915, 0.0333, 0.8333, 0),
  Other = c(0.39656, 0.6339, 0.6069, 0.6065)
)

max_min <- data.frame(
  Tilia = c(1, 0), Robinia = c(1, 0), Aesculus = c(1, 0),
  Acer = c(1, 0), Populus = c(1, 0), Other = c(1, 0))

rownames(max_min) <- c("Max", "Min")


df <- rbind(max_min, data)

# Reduce plot margin using par()
op <- par(mar = c(0, 2, 1, 1))

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 2.0,
                                        caxislabels = NULL, title = NULL, calcex = 1.2,...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, calcex=calcex,...
  )
}

# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 0.25, 0.5, 0.75, 1),
  color = c("#FC4E07", "#357ba3", "#648f1e", "#a50026")
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#FC4E07", "#357ba3", "#648f1e", "#a50026"),
  text.col = "black", cex = 1.2, pt.cex = 1.5, y.intersp = 0.5
)

par(op)




#-------------------------------------------------------------------------------
#SPECIFICITY

# Create a matrix with the given data
data<- data.frame(
  row.names = c("Traffic", "Recreational", "Residential", "Other"),
  Tilia = c(0.8595, 0.95624, 0.95687, 0.95134  ),
  Robinia = c(0.97957, 1, 0.997658, 1),
  Aesculus = c(0.99829, 0.997518, 1, 0.99326),
  Acer = c(0.8498, 0.7456, 0.7232, 0.7195),
  Populus = c( 0.98916, 0.9999134, 0.9976, 1),
  Other = c(0.8903, 0.5852, 0.7088, 0.7181)
)

max_min <- data.frame(
  Tilia = c(1, 0.6), Robinia = c(1, 0.6), Aesculus = c(1, 0.6),
  Acer = c(1, 0.6), Populus = c(1, 0.6), Other = c(1, 0.6))

rownames(max_min) <- c("Max", "Min")

df <- rbind(max_min, data)

# Reduce plot margin using par()
op <- par(mar = c(0, 2, 1, 1))

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 2.0,
                                        caxislabels = NULL, title = NULL, calcex = 1.2,...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, calcex=calcex,...
  )
}

# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c(0.6, 0.7, 0.8, 0.9, 1),
  color = c("#FC4E07", "#357ba3", "#648f1e", "#a50026")
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#FC4E07", "#357ba3", "#648f1e", "#a50026"),
  text.col = "black", cex = 1.2, pt.cex = 1.5, y.intersp = 0.5
)

par(op)



#-------------------------------------------------------------------------------
#KAPPA

# Create a matrix with the given data
data<- data.frame(
  row.names = c("Traffic", "Recreational", "Residential", "Other"),
  Tilia = c( ),
  Robinia = c(0.97957, 1, 0.997658, 1),
  Aesculus = c(0.99829, 0.997518, 1, 0.99326),
  Acer = c(0.8498, 0.7456, 0.7232, 0.7195),
  Populus = c( 0.98916, 0.9999134, 0.9976, 1),
  Other = c(0.8903, 0.5852, 0.7088, 0.7181)
)

max_min <- data.frame(
  Tilia = c(1, 0), Robinia = c(1, 0), Aesculus = c(1, 0),
  Acer = c(1, 0), Populus = c(1, 0), Other = c(1, 0))

rownames(max_min) <- c("Max", "Min")

df <- rbind(max_min, data)

# Reduce plot margin using par()
op <- par(mar = c(0, 2, 1, 1))

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 2.0,
                                        caxislabels = NULL, title = NULL, calcex = 1.2,...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, calcex=calcex,...
  )
}

# Create the radar charts
create_beautiful_radarchart(
  data = df, caxislabels = c(0, 0.25, 0.5, 0.75, 1),
  color = c("#FC4E07", "#357ba3", "#648f1e", "#a50026")
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#FC4E07", "#357ba3", "#648f1e", "#a50026"),
  text.col = "black", cex = 1.2, pt.cex = 1.5, y.intersp = 0.5
)

par(op)

