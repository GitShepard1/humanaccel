#' @title run_accel
#'
#' @description Parses a directory of agd files for the human exercise project
#' @param rootdir directory that contains the agd files
#' @param subjects_list list of subjects
#' @import tidyverse
#' @import tibble
#' @import dplyr
#' @import convertagd
#' @import list.append
#' @import accelerometry
#' @import purrr
#' @import stringr
#' @import rlist
#' @import list.append
#' @import process_accel
#' @import get_subjects
#' @import concat_list
#' @export run_accel
#' @examples \dontrun{
#' hello()
#' }


run_accel = function(rootdir, subjects_list, outputdir){

  parameter_list <-
    list(
      nci.methods = T,
      id = NULL,
      brevity = 2,
      valid.days = 1,
      valid.week.days = 0,
      valid.weekend.days = 0,
      int.axis = "vert",
      int.cuts = c(100, 760, 2020, 5999),
      cpm.nci = FALSE,
      hourly.axis = "vert",
      days.distinct = TRUE,
      nonwear.axis = "vert",
      nonwear.window = 90, # suggestion that we use 90 min?
      nonwear.tol = 2, # spike tolerance 2 mins
      nonwear.tol.upper = 99, # less than 100 min
      nonwear.nci = FALSE,
      weartime.minimum = 600,
      weartime.maximum = 1440,
      partialday.minimum = 1440,
      active.bout.length = 10, # active
      active.bout.tol = 0,
      mvpa.bout.tol.lower = 0, # maybe 1 minute minimum bout
      vig.bout.tol.lower = 0,
      active.bout.nci = F,
      sed.bout.tol = 0,
      artifact.axis = "vert",
      artifact.thresh = 25000,
      artifact.action = 1,
      weekday.weekend = F,
      return.form = 2
    )


  logs = list()
  msg = str_c('Accelerometry Algorithm run on ', Sys.time())

  logs = rlist::list.append(
            list(msg, '----------------------PARAMETERS-----------------------'),
            # concat_list(parameter_list),
            list('-------------------------START-------------------------')
        )

  dayList = list()
  monthList = list()

  for(s in unique(subjects_list)){

    print(s)
    out = try(process_accel(filepath = rootdir, Subject = s))
    if('try-error'  %in% class(out)){

      msg = paste('ERROR:', s)
      print(msg)


    } else {

      msg = paste('COMPLETED:', s)
      print(msg)
      dayList = list.append(dayList, out@day_data)
      monthList = list.append(monthList, out@month_data)

    }

    logs = list.append(logs, msg)

  }

    day_data = dayList %>% reduce(rbind)
    month_data = monthList %>% reduce(rbind)

    # acc_path = "Q:/DATA/DATA ENTRY/XnatUploaded/sampledata/accel"

    filenames = map(list('ACC_DAY_', 'ACC_MONTH_'),

                  ~str_c(., Sys.Date(), '.csv')

                )

    writeLines(unlist(logs),file.path(outputdir, 'logs.txt'))

    write.csv(day_data, file = str_c('ACCbyDAY_', Sys.Date(), '.csv'))
    write.csv(month_data, file = str_c('ACCbyMONTH_', Sys.Date(), '.csv'))

}


# # DIRECTORY / FILES ------------------------------------------------------------
home_agd = "Q:/DATA/ActiLife/agdfiles"
# agd_files = dir(home_agd)[tools::file_ext(dir(home_agd)) == 'agd']

# subjects_list = map(agd_files, get_subjects)

# # RUN --------------------------------------------------------------------------
# accel_dir = 'Q:/DATA/ActiLife/Processed'
#

#
# run_accel = function(){
#   logs = list()
#   msg = str_c('Accelerometry Algorithm run on ', Sys.time())
#
#   logs = list.append(
#     list(msg, '----------------------PARAMETERS-----------------------'),
#     concat_list(parameter_list),
#     list('-------------------------START-------------------------')
#   )
#
#   dayList = list()
#   monthList = list()
#
#   for(s in unique(subjects_list)){
#
#     out = try(process_accel(s))
#     if('try-error'  %in% class(out)){
#
#       msg = paste('ERROR:', s)
#       print(msg)
#
#
#     } else {
#
#       msg = paste('COMPLETED:', s)
#       print(msg)
#       dayList = list.append(data_list, out@day_data)
#       monthList = list.append(data_list, out@month_data)
#
#
#     }
#
#     logs = list.append(logs, msg)
#   }
#
#   day_data = dayList %>% reduce(rbind)
#   month_data = monthList %>% reduce(rbind)
#
#
#   acc_path = "Q:/DATA/DATA ENTRY/XnatUploaded/sampledata/accel"
#
#   filenames = map(list('ACC_DAY_', 'ACC_MONTH_'),
#
#                 ~str_c(., Sys.Date(), '.csv')
#
#               )
#
#   writeLines(unlist(logs),file.path(acc_path, 'logs.txt'))
#
#   map(
#     list(day_data, month_data),
#
#     ~write.csv(file.path(acc_path, .))
#   )
#
# }
