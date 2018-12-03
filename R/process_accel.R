#' @title human exercise QBI accelerometry pacakge
#'
#' @description Parses raw agd files into xnat uploadable format
#' @param
#' @import tidyverse
#' @import tibble
#' @import dplyr
#' @import convertagd
#' @import rlist
#' @import accelerometry
#' @import purrr
#' @import stringr
#' @export process_accel
#' @examples \dontrun{
#' hello()
#' }


# "CLASS: accelerometry"
setClass("accelerometry",
         representation(
           start_date = 'vector',
           parameters = 'list',
           raw_data = 'tbl',
           day_data = 'tbl',
           month_data = 'tbl'
         )
)

process_accel = function(filepath,
                         Subject,
                         nci.methods = T,
                         start.date = as.Date("2014/1/5"), # actually I take the minimum date within agd file
                         start.time = "00:00:00",
                         id = NULL,
                         brevity = 2,
                         valid.days = 4,
                         valid.week.days = 0 ,
                         valid.weekend.days = 0,
                         int.axis = "vert",
                         int.cuts = c(100, 760, 1100, 1500),
                         cpm.nci = FALSE,
                         hourly.axis = "vert",
                         days.distinct = TRUE,
                         nonwear.axis = "vert",
                         nonwear.window = 90, # 60 minutes is also an option but 90 min window could be more optimal
                         nonwear.tol = 0,
                         nonwear.tol.upper = 99,
                         nonwear.nci = FALSE,
                         weartime.minimum = 600,
                         weartime.maximum = 1440,
                         partialday.minimum = 1440,
                         active.bout.length = 10,
                         active.bout.tol = 0,
                         mvpa.bout.tol.lower = 0,
                         vig.bout.tol.lower = 0,
                         active.bout.nci = F,
                         sed.bout.tol = 0,
                         artifact.axis = "vert",
                         artifact.thresh = 25000,
                         artifact.action = 1,
                         weekday.weekend = F,
                         return.form = 2){


  # list of parameters

  parameters <-
    list(
      "nci.methods"= nci.methods,
      "start.date"= start.date,
      "start.time"= start.time,
      "id"= id,
      "brevity"= brevity,
      "valid.days"= valid.days,
      "valid.week.days"= valid.week.days,
      "valid.weekend.days"= valid.weekend.days,
      "int.axis"= int.axis,
      "int.cuts"= int.cuts,
      "cpm.nci"= cpm.nci,
      "hourly.axis"= hourly.axis,
      "days.distinct"= days.distinct,
      "nonwear.axis"= nonwear.axis,
      "nonwear.window"= nonwear.window,
      "nonwear.tol"= nonwear.tol,
      "nonwear.tol.upper"= nonwear.tol.upper,
      "nonwear.nci"= nonwear.nci,
      "weartime.minimum"= weartime.minimum,
      "weartime.maximum"= weartime.maximum,
      "partialday.minimum"= partialday.minimum,
      "active.bout.length"= active.bout.length,
      "active.bout.tol"= active.bout.tol,
      "mvpa.bout.tol.lower"= mvpa.bout.tol.lower,
      "vig.bout.tol.lower"= vig.bout.tol.lower,
      "active.bout.nci"= active.bout.nci,
      "sed.bout.tol"= sed.bout.tol,
      "artifact.axis"= artifact.axis,
      "artifact.thresh"= artifact.thresh,
      "artifact.action"= artifact.action,
      "weekday.weekend"= weekday.weekend,
      "return.form"= return.form

    )


  sub = stringr::str_extract(Subject, '\\d{4}')

  acc_dat = tibble(

            filename = dir(filepath)[str_detect(dir(filepath), sub) &
                                       tools::file_ext(dir(filepath)) == 'agd']

            ) %>%
              dplyr::mutate(

                Subject = map(filename, get_subjects),

                interval = map(filename, get_interval)

              ) %>%
              unnest(Subject, interval) %>%
              arrange(Subject, interval) %>%
              dplyr::mutate(

                acc_data_raw = map(filename, function(f){

                  convertagd::read_agd(file.path(filepath, f), tz = "GMT")[[2]]

                }),

                acc_startdate = map(acc_data_raw, ~min(.$timedate)),

                acc_data_day = pmap(

                  list(acc_data_raw, acc_startdate),

                  .f = function(acc, date){

                accelerometry::accel.process.tri(
                                      counts.tri = acc[, 2:4],
                                      steps = acc[, 5],
                                      nci.methods = nci.methods,
                                      start.date =  as.Date(date),
                                      start.time =  start.time,
                                      id = id,
                                      brevity = brevity,
                                      valid.days = valid.days,
                                      valid.week.days = valid.week.days,
                                      valid.weekend.days = valid.weekend.days,
                                      int.axis = int.axis,
                                      int.cuts = int.cuts,
                                      cpm.nci = cpm.nci,
                                      hourly.axis = hourly.axis,
                                      days.distinct = days.distinct,
                                      nonwear.axis = nonwear.axis,
                                      nonwear.window = nonwear.window,
                                      nonwear.tol = nonwear.tol,
                                      nonwear.tol.upper = nonwear.tol.upper,
                                      nonwear.nci = nonwear.nci,
                                      weartime.minimum = weartime.minimum,
                                      weartime.maximum = weartime.maximum,
                                      partialday.minimum = partialday.minimum,
                                      active.bout.length = active.bout.length,
                                      active.bout.tol = active.bout.tol,
                                      mvpa.bout.tol.lower = mvpa.bout.tol.lower,
                                      vig.bout.tol.lower = vig.bout.tol.lower,
                                      active.bout.nci = active.bout.nci,
                                      sed.bout.tol =  sed.bout.tol,
                                      artifact.axis = artifact.axis,
                                      artifact.thresh = artifact.thresh,
                                      artifact.action = artifact.action,
                                      weekday.weekend = weekday.weekend,
                                      return.form = return.form
                    ) %>%
                      as.tibble() %>%
                      group_by(day,valid_day) %>%
                      summarise_all(mean) %>%
                      arrange(-valid_day) %>%
                      ungroup()
                  }
                ),

                acc_data_month = map(acc_data_day, function(df){

                  df %>%
                    filter(valid_day == 1) %>%
                    summarise_all(mean)

                })
              )



  return_class = new("accelerometry",
                     start_date = unique(acc_dat$acc_startdate),

                     parameters = parameters,

                     raw_data = unnest(acc_dat, acc_data_raw),

                     day_data = unnest(acc_dat, acc_data_day) %>%
                                    dplyr::select(-filename),

                     month_data = unnest(acc_dat, acc_data_month) %>%
                                    dplyr::select(-acc_data_raw,
                                                  -acc_startdate,
                                                  -acc_data_day,
                                                  -filename)
  )


  return(return_class)


}

get_subjects = function(file){

  sub = str_extract(file, "\\d{4}")
  read.csv('C:/Users/uqaho4/PycharmProjects/OPEXUploader/P1_subjectlist.csv',
           stringsAsFactors = F) %>%
    dplyr::filter(str_detect(ID, str_extract(sub, "\\d*"))) %>%
    pull(ID)

}

get_interval = function(file){

  st = str_extract(file, '(?<=_).*(?=30)')

  if(str_detect(st, '(?i)bl')) st = '0'

  str_extract(st, '\\d*') %>% as.numeric()

}

fnlist <- function(x){
  z <- deparse(substitute(x))
  cat(z, "\n")
  nams=names(x)
  for (i in seq_along(x) ) cat(nams[i],  x[[i]], "\n")
}

concat_list = function(list){


  out = list()
  for(i in seq_along(list)){

    nam = names(list)[i]
    val = parameter_list[[i]]
    out = list.append(out, str_c(nam, ':',
                                 paste(rep('', 20 - nchar(nam)), collapse = ' ')),
                      val)

  }

  return(out)

}


