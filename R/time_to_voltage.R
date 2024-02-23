time_to_voltage = function(CV, UV, scan = 2, scan_rate = 0.01){


  E_init <- as.numeric(colnames(CV[[scan]]$reductive))[1]
  E_stop <- tail(as.numeric(colnames(CV[[scan]]$reductive)), 1)
  E_lower <- min(as.numeric(colnames(CV[[scan]]$reductive)))
  E_upper <- max(as.numeric(colnames(CV[[scan]]$reductive)))

  #get number of spectra
  num_points = length(row.names(UV))

  if (scan_dir == "oxidative"){
    #calculate time betwenn vertecies
    t_init = 0
    t_upper = abs(E_init - E_upper) / scan_rate
    t_lower = abs(E_upper - E_lower) / scan_rate
    t_stop = abs(E_lower - E_stop) / scan_rate
    t_sum = t_upper + t_lower + t_stop

    #interpolate potential values to match num_points
    seq_1 = seq(E_init, E_upper, length.out = num_points * t_upper / t_sum)
    seq_2 = seq(E_upper, E_lower, length.out = num_points * t_lower / t_sum)
    seq_3 = seq(E_lower, E_stop, length.out = num_points * t_stop / t_sum)
    seq_sum = c(seq_1, seq_2, seq_3)

  }

  if (scan_dir == "reductive"){
    #in reductive mode change order of E_upper and E_lower
    #calculate time betwenn vertecies
    t_init = 0
    t_upper = abs(E_init - E_upper) / scan_rate
    t_lower = abs(E_upper - E_lower) / scan_rate
    t_stop = abs(E_lower - E_stop) / scan_rate
    t_sum = t_upper + t_lower + t_stop

    #interpolate potential values to match num_points
    seq_1 = seq(E_init, E_upper, length.out = num_points * t_upper / t_sum)
    seq_2 = seq(E_lower, E_upper, length.out = num_points * t_lower / t_sum)
    seq_3 = seq(E_upper, E_stop, length.out = num_points * t_stop / t_sum)
    seq_sum = c(seq_1, seq_2, seq_3)
  }

  row_names_numeric <- as.numeric(rownames(my_matrix))

  # Sort row names numerically
  sorted_row_names <- row_names_numeric[order(row_names_numeric)]

  # Sort the matrix based on sorted row names
  sorted_matrix <- my_matrix[sorted_row_names, ]

  return(seq_sum)
}
