library(rvest)

if (file.exists("flights_data.csv")) {
  existing_data <- read.csv("flights_data.csv")
} else {
  existing_data <- data.frame()  # Create an empty DataFrame if the file doesn't exist
}
# Parse the HTML
page <- read_html("zvartnots.html")

destination <- page %>%
  html_nodes("td.filterable.hidde-min:nth-child(2)") %>%
  html_text()

flight_number <- page %>%
  html_nodes("td.filterable.show-min p.flightNumber") %>%
  html_text()

code <- page %>%
  html_nodes("td.filterable.hidde-min:nth-child(3)") %>%
  html_text()

scheduled <- page %>%
  html_nodes("td.stda.show-min") %>%
  html_attr("dt")

delay <- page %>%
  html_nodes("td.col-3.col-sm-2.col-lg-2.centerAlign.show-min") %>%
  html_text()%>%
  gsub("\\s+|\\n", "", .) # Remove unwanted characters





# Create a consistent dataframe
max_rows <- max(length(destination), length(flight_number), length(code), length(scheduled), length(delay))
df <- data.frame(
  Destination = rep(destination, length.out = max_rows),
  FlightNumber = rep(flight_number, length.out = max_rows),
  Code = rep(code, length.out = max_rows),
  Scheduled = rep(scheduled, length.out = max_rows),
  Delay = rep(delay, length.out = max_rows)
)

print(df)
print(existing_data)

combined_df <- rbind(existing_data, df)

write.csv(combined_df, "flights_data.csv", row.names = FALSE)




print("Data has been updated and saved to flights_data.csv.")
