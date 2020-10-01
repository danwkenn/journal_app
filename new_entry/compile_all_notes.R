compile_all_notes <- function(wd){
  message("Rendering full journal.")
  message(wd)
library(stringr)
fill_in_contacts <- function(string){
  #return(str_replace_all(string = string,pattern = "\\\\contact",replacement = "\\\\textit"))
}

fill_in_contacts <- function(string){
  return(str_replace_all(string = string,pattern = "<>",replacement = "\\\\textit"))
}

library(dplyr)
library(lubridate)
library(readxl)

date_suffix <- function(dates){
  dayy <- day(dates)
  suff <- case_when(dayy %in% c(11,12,13) ~ "th",
                    dayy %% 10 == 1 ~ 'st',
                    dayy %% 10 == 2 ~ 'nd',
                    dayy %% 10 == 3 ~'rd',
                    TRUE ~ "th")
  paste0(suff)
}

message("Reading in header...")
start <- readChar(paste0(wd,"/templates/template_start.tex"), file.info(paste0(wd,"/templates/template_start.tex"))$size)
message("Reading in journal entries...")
filenames <- dir(path = paste0(wd,"/notes/"),pattern = "^\\w*.tex$")
message("First 10:")
print(filenames[1:min(c(length(filenames),10))])
middle <- lapply(dir(path = paste0(wd,"/notes/"),pattern = "^\\w*.tex$"),
                 function(file)
                   readChar(paste0(wd,"/notes/",file), file.info(paste0(wd,"/notes/",file))$size))
message("Reading in journal footer...")
end <- readChar(paste0(wd,"/templates/template_end.tex"), file.info(paste0(wd,"/templates/template_end.tex"))$size)


# Insert dates:
dates = sapply(str_match_all(middle,"^%DATE:(\\d+-\\d+-\\d+)"), function(x) x[,2])
dates <- ymd(dates)
times <- sapply(str_match_all(middle,"^%DATE:\\d+-\\d+-\\d+(?:\n|\r)*%TIME:(\\d+:\\d+(?:am|pm))"), function(x) x[,2])
# middle <- paste0(
#   middle,
#   "Date: ",
#   dates,"\r\n"
# )

middle <- str_replace(middle,pattern = "\\\\section",replacement = "\\\\subsection")

date_file_table <- 
  data.frame(
    filenames,
    dates = dates,
    times = times,stringsAsFactors = FALSE
  )

date_file_table$dt <- ymd_hm(paste0(date_file_table$dates," ",date_file_table$times))
date_file_table$year <- year(date_file_table$dt)
unique_dates <- unique(date_file_table$dates)
unique_dates <- sort(unique_dates)
fancy_date <- function(date){
  unique_dates.dt <- ymd(date)
  
  date_fancy <- paste0(format.Date(unique_dates.dt,"%A"),
                       ", ",
                       as.numeric(format.Date(unique_dates.dt,"%d")),
                       "\\textsuperscript{"
  )
  date_fancy <- paste0(
    date_fancy,
    date_suffix(
      unique_dates.dt
    )
  )
  date_fancy <- 
    paste0(
      date_fancy,
      format.Date(unique_dates.dt,"} %B")
    )
  
  date_fancy
}

# Load Contacts:

contact_details <- read_xlsx(paste0(wd,"/contacts.xlsx"))
contact_details$short_form[is.na(contact_details$short_form)] <-
  contact_details$`First Name`[is.na(contact_details$short_form)]

#Compile full text:
full_string <- start
for(i in 1:length(unique_dates)){
  date = unique_dates[i]
  
  # Check if first Day of year:
  current_year <- year(date)
  years <- year(unique_dates)
  dates_in_current_year <- unique_dates[years == current_year]
  
  if(min(dates_in_current_year) == date){
    
    full_string <- 
      paste0(
        full_string,
        "\\pagebreak\n\n",
        "\\def\\yearcommamonth{}",
        "\\resizebox{\\linewidth}{!}{\\itshape ",current_year,"}\n\n\\pagebreak\n"
      )
  }
  
  
  full_string <- 
    paste0(
      full_string,
      "\n\\pagebreak\n",
      "\\section*{",
      fancy_date(date),
      "}\n\n\\def\\yearcommamonth{",format.Date(date,"%B, %Y"),"}"
    )
  
  # Select relevant entries:
  entries_on_date <- which(date_file_table$dates == date)
  
  # Sort based on time:
  time_order <- order(date_file_table[entries_on_date,]$dt)
  
  #
  entries <- middle[entries_on_date[time_order]]
  entries <- str_remove(entries,pattern = "^%DATE:\\d+-\\d+-\\d+\n%TIME:(\\d+:\\d+(?:am|pm))")
  
  day_string <- paste0(entries,collapse = "\n")
  
  # Fill in contacts:
  {
    other_text <- str_split(day_string,"\\\\contact\\{[[:alnum:]]+\\}")[[1]]
    contacts <- str_match_all(day_string,"\\\\contact\\{([[:alnum:]]+)\\}")[[1]][,2]
    
    if(length(contacts) > 0){
      day_string <- 
        paste0(
          paste0(
            other_text[1:(length(other_text)-1)], "<>",
            contacts,"<>",collapse = ""
          ),
          other_text[length(other_text)]
        )
    }
    
    other_text <- str_split(day_string,"<>([[:alnum:]]{1,10})<>")[[1]]
    contacts_raw <- str_match_all(day_string,"<>([[:alnum:]]{1,10})<>")[[1]][,2]
    
    if(length(contacts_raw) > 0){
      contact_ids <- match(contacts_raw,contact_details$Abbreviation)
      contacts_first <- (!duplicated(contact_ids) & !is.na(contact_ids)) 
      other_contacts <- (duplicated(contact_ids) & !is.na(contact_ids)) 
      contacts <- character(length = length(contact_ids))
      contacts[contacts_first] <- paste0(
        "\\href{mailto:",
        contact_details$email[contact_ids[contacts_first]],
        "}{\\textbf{",
        contact_details$short_form[contact_ids[contacts_first]]," ",
        contact_details$Surname[contact_ids[contacts_first]],"}}")
      
      contacts[other_contacts] <- paste0(
        "\\href{mailto:",
        contact_details$email[contact_ids[other_contacts]],
        "}{\\textbf{",
        substr(contact_details$short_form[contact_ids[other_contacts]],1,1),"",
        substr(contact_details$Surname[contact_ids[other_contacts]],1,1),"}}")
      contacts[is.na(contact_ids)] <- contacts_raw[is.na(contact_ids)]
      
      day_string <- 
        paste0(
          paste0(
            paste0(
              other_text[1:(length(other_text)-1)],
              contacts
            ),collapse = ""),
          other_text[length(other_text)]
        )
    }
  }
  
  full_string <- 
    paste0(full_string,day_string)
  
}

full_string <- paste0(full_string,end)

# full_string <- 
#   paste0(start,paste0(middle,collapse = "\n"),end)

full_string <- paste0(strsplit(full_string,split = "\r")[[1]],collapse = "")

# Put references to contacts.

cat(full_string)

sink(file = "temp.tex")
print(cat(full_string))
sink(file = NULL)

system("pdflatex temp.tex temp.pdf")
file.remove("temp.out")
file.remove("temp.aux")
file.remove("temp.log")
file.remove("temp.tex")


}
