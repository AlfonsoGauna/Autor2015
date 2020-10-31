library(data.table)

# #####################################################################################################
# @Author: Alfonso Guauna - alfonsogauna@hotmail.com
#
# Comentary:
#   - We need that the occupations are order by increasing wage and the shares
#     must be in percentajes [5% = 5, no 0.05]
#   - We hace two types of tables, all of these must have only two columns
#     with the following names;
#       1) table_share (calculating shares): occ & share
#       2) table_share (calculating wages): occ & wage
#     table like 1 are used in calc_percentiles() and weight_percentiles() instead 
#     tables like 2 are used in wage_calc_percentiles() & wage_weight_percentiles()
# #####################################################################################################

share_function <- function(occ_code, table_share){
  # function that returns the share of a occupation given a share distribution in table_share
  if (occ_code == '' | is.na(occ_code)){
    return(0)
  } else {
    table_share <- as.data.table(table_share)
    table_share[occ == occ_code]$share
  }
}


wage_function <- function(occ_code, table_share){
  # function that returns the wage of a occupation given table_share
  if (occ_code == '' | is.na(occ_code)){
    return(0)
  } else {
    table_share <- as.data.table(table_share)
    table_share[occ == occ_code]$wage
  }
}


calc_percentiles <- function(table_share){
  # function that calculates the distribution of the occupations among the percentiles
  # it's returns the percentil, the occupation and the portion (pondera) of the occupation
  # in that percentil
  table_share <- as.data.table(table_share)
  table_share <- table_share[, `:=` (share_start = cumsum(share) - share,
                                     share_end = cumsum(share))]
  percen <- c(1:99)
  cortes <- table_share$share_end
  linespace <- c(percen, cortes)
  linespace <- sort(linespace)
  percentiles_tabla <- as.data.table(linespace)
  setnames(percentiles_tabla, "linespace", "right_edge")
  percentiles_tabla <- percentiles_tabla[, left_edge := shift(right_edge)]
  percentiles_tabla[1,"left_edge"] <- 0.0
  percentiles_tabla <- percentiles_tabla[, .(left_edge, right_edge)]
  percentiles_tabla <- percentiles_tabla[, long_int := right_edge - left_edge]
  percentiles_tabla <- percentiles_tabla[, `:=` (left_edge = round(left_edge, digits = 5),
                                                 right_edge = round(right_edge, digits = 5))]
  table_share <- table_share[, `:=` (share_start = round(share_start, digits = 5),
                                     share_end = round(share_end, digits = 5))]
  
  for (occ_row in 1:nrow(table_share)) {
    occ <- as.character(table_share[occ_row, 'occ'])
    occ_left_edge <- as.numeric(table_share[occ_row, "share_start"])
    occ_right_edge  <- as.numeric(table_share[occ_row, "share_end"])
    
    for (int in 1:nrow(percentiles_tabla)) {
      int_left_edge <- as.numeric(percentiles_tabla[int, "left_edge"])
      int_right_edge <- as.numeric(percentiles_tabla[int, "right_edge"])
      
      if (occ_left_edge <= int_left_edge & occ_right_edge >= int_right_edge){
        percentiles_tabla[int, "occ"] <- occ
      } else {
        # PASS
      }
    }
  }
  
  percentiles_tabla$share <- lapply(percentiles_tabla$occ, share_function, table_share=table_share)
  percentiles_tabla$share <- as.numeric(percentiles_tabla$share) 
  percentiles_tabla <- percentiles_tabla[, pondera := long_int / share]
  percentiles_tabla <- percentiles_tabla[, percentil := floor(left_edge) + 1]
  percentiles_tabla <- percentiles_tabla[, .(percentil, pondera, occ)]
}


weight_percentiles <- function(table_percentiles, table_share){
  # function that ponderates the occupations in percentiles given a table_percentiles,
  # thas calculated with calc_percentiles()
  table_percentiles <- as.data.table(table_percentiles)
  table_share <- as.data.table(table_share)
  
  table_percentiles <- table_percentiles[, pondera := ifelse(is.na(pondera), 0, pondera)]
  table_share <- table_share[, share := ifelse(is.na(share), 0, share)]
  
  table_percentiles$share <- lapply(table_percentiles$occ, share_function, table_share=table_share)
  
  table_percentiles$share <- as.numeric(table_percentiles$share)
  table_percentiles$pondera <- as.numeric(table_percentiles$pondera)
  
  table_percentiles <- table_percentiles[, share_w := pondera * share]
  table_percentiles <- table_percentiles[, share_w := sum(share_w, na.rm = TRUE), by='percentil']
  table_percentiles <- table_percentiles[, .(percentil, share_w)]
  table_percentiles <- unique(table_percentiles)
}


wage_calc_percentiles <- function(table_share){
  # function that calculates the distribution of the occupations among the percentiles
  # it's returns the percentil, the occupation and the portion (pondera) of the percentil
  # that's correspond to that occupation.
  table_share <- as.data.table(table_share)
  table_share <- table_share[, `:=` (share_start = cumsum(share) - share,
                                     share_end = cumsum(share))]
  percen <- c(1:99)
  cortes <- table_share$share_end
  linespace <- c(percen, cortes)
  linespace <- sort(linespace)
  percentiles_tabla <- as.data.table(linespace)
  setnames(percentiles_tabla, "linespace", "right_edge")
  percentiles_tabla <- percentiles_tabla[, left_edge := shift(right_edge)]
  percentiles_tabla[1,"left_edge"] <- 0.0
  percentiles_tabla <- percentiles_tabla[, .(left_edge, right_edge)]
  percentiles_tabla <- percentiles_tabla[, long_int := right_edge - left_edge]
  percentiles_tabla <- percentiles_tabla[, `:=` (left_edge = round(left_edge, digits = 5),
                                                 right_edge = round(right_edge, digits = 5))]
  table_share <- table_share[, `:=` (share_start = round(share_start, digits = 5),
                                     share_end = round(share_end, digits = 5))]
  
  for (occ_row in 1:nrow(table_share)) {
    occ <- as.character(table_share[occ_row, 'occ'])
    occ_left_edge <- as.numeric(table_share[occ_row, "share_start"])
    occ_right_edge  <- as.numeric(table_share[occ_row, "share_end"])
    
    for (int in 1:nrow(percentiles_tabla)) {
      int_left_edge <- as.numeric(percentiles_tabla[int, "left_edge"])
      int_right_edge <- as.numeric(percentiles_tabla[int, "right_edge"])
      
      if (occ_left_edge <= int_left_edge & occ_right_edge >= int_right_edge){
        percentiles_tabla[int, "occ"] <- occ
      } else {
        # PASS
      }
    }
  }
  
  percentiles_tabla$share <- lapply(percentiles_tabla$occ, share_function, table_share=table_share)
  percentiles_tabla$share <- as.numeric(percentiles_tabla$share) 
  percentiles_tabla <- percentiles_tabla[, pondera := long_int]
  percentiles_tabla <- percentiles_tabla[, percentil := floor(left_edge) + 1]
  percentiles_tabla <- percentiles_tabla[, .(percentil, pondera, occ)]
}


wage_weight_percentiles <- function(table_percentiles, table_share){
  # function that ponderates the occupations in percentiles given a table_percentiles,
  # thas calculated with wage_calc_percentiles()  
  table_percentiles <- as.data.table(table_percentiles)
  table_share <- as.data.table(table_share)

  table_percentiles <- table_percentiles[, pondera := ifelse(is.na(pondera), 0, pondera)]
  table_share <- table_share[, wage := ifelse(is.na(wage), 0, wage)]

  table_percentiles$wage <- lapply(table_percentiles$occ, wage_function, table_share=table_share)

  table_percentiles$wage <- as.numeric(table_percentiles$wage)
  table_percentiles$pondera <- as.numeric(table_percentiles$pondera)
  
  table_percentiles <- table_percentiles[, wage_w := pondera * wage]
  table_percentiles <- table_percentiles[, wage_w := sum(wage_w, na.rm = TRUE), by='percentil']
  table_percentiles <- table_percentiles[, .(percentil, wage_w)]
  table_percentiles <- unique(table_percentiles)
}