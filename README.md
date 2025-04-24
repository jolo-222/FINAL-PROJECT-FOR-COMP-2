# FINAL-PROJECT-FOR-COMP-2

# DELETE FNCTION PLANG NAGANA, HNDI EDIT

#DRAFT(2) FINAL - Fully functional Shiny App with working buttons

# Auto-install & load required packages
required_packages <- c("shiny", "DT", "ggplot2", "dplyr", "lubridate", "shinyjs")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

# CSV file for transactions
data_file <- "transactions.csv"
if (!file.exists(data_file)) {
  write.csv(data.frame(
    Date = as.Date(character()),
    Customer_Name = character(),
    Mobile_Number = character(),
    Type = character(),
    Amount = numeric(),
    Cost = numeric(),
    Profit = numeric(),
    stringsAsFactors = FALSE
  ), data_file, row.names = FALSE)
}

# UI
diag_script <- "$(document).on('click', '.editBtn, .deleteBtn, .editProduct, .deleteProduct', function() {
  Shiny.setInputValue('lastClickedId', this.id);
});"

ui <- fluidPage(
  useShinyjs(),
  tags$script(HTML(diag_script)),
  titlePanel("📋 Load & GCash in/out Profit Tracker"),
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Transactions'",
        textInput("customer_name", "Customer Name:"),
        textInput("mobile", "Mobile Number:"),
        selectInput("type", "Transaction Type:",
                    choices = c("Load", "GCash-In", "GCash-Out")),
        numericInput("cost", "Amount (₱):", value = 0),
        numericInput("amount", "Amount incl. Charge (₱):", value = 0),
        actionButton("submit", "Save Transaction", class = "btn btn-primary"),
        br(),
        actionButton("undoTrans", "↩️ Undo", class = "btn btn-warning"),
        actionButton("redoTrans", "⏩ Redo", class = "btn btn-info")
      ),
      conditionalPanel(
        condition = "input.tabs == 'Inventory'",
        textInput("product_name", "Product Name:"),
        numericInput("quantity", "Quantity:", value = 0),
        numericInput("unit_cost", "Unit Cost (₱):", value = 0),
        numericInput("unit_price", "Unit Price (₱):", value = 0),
        actionButton("saveProduct", "Add Product", class = "btn btn-success"),
        br(),
        actionButton("undoInv", "↩️ Undo", class = "btn btn-warning"),
        actionButton("redoInv", "⏩ Redo", class = "btn btn-info")
      )
    ),
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("📋 Transactions", value = "Transactions", DTOutput("data_table")),
                  tabPanel("📦 Inventory Sales", value = "Inventory", DTOutput("inventory_table"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  transHist <- reactiveValues(data = read.csv(data_file, stringsAsFactors = FALSE), undo = list(), redo = list())
  invHist <- reactiveValues(data = data.frame(Date = as.Date(character()), Product_Name = character(), Quantity = numeric(), Unit_Cost = numeric(), Unit_Price = numeric(), Profit = numeric()), undo = list(), redo = list())
  
  observe({ transHist$data$Date <- as.Date(transHist$data$Date) })
  
  observeEvent(input$submit, {
    req(input$customer_name, input$mobile, input$amount, input$cost)
    mobile_digits <- gsub("[^0-9]", "", input$mobile)
    if (nchar(mobile_digits) != 11) {
      showNotification("❌ Invalid mobile number!", type = "error")
      return()
    }
    new_entry <- data.frame(
      Date = Sys.Date(),
      Customer_Name = input$customer_name,
      Mobile_Number = input$mobile,
      Type = input$type,
      Amount = input$cost,
      Cost = input$amount,
      Profit = input$amount - input$cost,
      stringsAsFactors = FALSE
    )
    transHist$undo <- append(transHist$undo, list(transHist$data))
    transHist$redo <- list()
    transHist$data <- rbind(transHist$data, new_entry)
    write.csv(transHist$data, data_file, row.names = FALSE)
  })
  
  observeEvent(input$saveProduct, {
    req(input$product_name, input$quantity, input$unit_cost, input$unit_price)
    profit <- input$unit_price - input$unit_cost
    new_row <- data.frame(
      Date = Sys.Date(),
      Product_Name = input$product_name,
      Quantity = input$quantity,
      Unit_Cost = input$unit_cost,
      Unit_Price = input$unit_price,
      Profit = profit,
      stringsAsFactors = FALSE
    )
    invHist$undo <- append(invHist$undo, list(invHist$data))
    invHist$redo <- list()
    invHist$data <- rbind(invHist$data, new_row)
  })
  
  observeEvent(input$undoTrans, {
    if (length(transHist$undo) > 0) {
      transHist$redo <- append(list(transHist$data), transHist$redo)
      transHist$data <- tail(transHist$undo, 1)[[1]]
      transHist$undo <- head(transHist$undo, -1)
      write.csv(transHist$data, data_file, row.names = FALSE)
    }
  })
  
  observeEvent(input$redoTrans, {
    if (length(transHist$redo) > 0) {
      transHist$undo <- append(transHist$undo, list(transHist$data))
      transHist$data <- transHist$redo[[1]]
      transHist$redo <- tail(transHist$redo, -1)
      write.csv(transHist$data, data_file, row.names = FALSE)
    }
  })
  
  observeEvent(input$undoInv, {
    if (length(invHist$undo) > 0) {
      invHist$redo <- append(list(invHist$data), invHist$redo)
      invHist$data <- tail(invHist$undo, 1)[[1]]
      invHist$undo <- head(invHist$undo, -1)
    }
  })
  
  observeEvent(input$redoInv, {
    if (length(invHist$redo) > 0) {
      invHist$undo <- append(invHist$undo, list(invHist$data))
      invHist$data <- invHist$redo[[1]]
      invHist$redo <- tail(invHist$redo, -1)
    }
  })
  
  observeEvent(input$lastClickedId, {
    id <- input$lastClickedId
    if (grepl("edit_", id)) {
      index <- as.numeric(sub("edit_", "", id))
      showNotification(paste("✏️ Edit row", index, "is clicked (not yet implemented)"))
    } else if (grepl("delete_", id)) {
      index <- as.numeric(sub("delete_", "", id))
      transHist$undo <- append(transHist$undo, list(transHist$data))
      transHist$data <- transHist$data[-index, ]
      write.csv(transHist$data, data_file, row.names = FALSE)
    } else if (grepl("editp_", id)) {
      index <- as.numeric(sub("editp_", "", id))
      showNotification(paste("✏️ Edit product row", index, "is clicked (not yet implemented)"))
    } else if (grepl("deletep_", id)) {
      index <- as.numeric(sub("deletep_", "", id))
      invHist$undo <- append(invHist$undo, list(invHist$data))
      invHist$data <- invHist$data[-index, ]
    }
  })
  
  output$data_table <- renderDT({
    df <- transHist$data
    df$Edit <- sprintf('<button class="btn btn-info btn-sm editBtn" id="edit_%s">Edit</button>', seq_len(nrow(df)))
    df$Delete <- sprintf('<button class="btn btn-danger btn-sm deleteBtn" id="delete_%s">Delete</button>', seq_len(nrow(df)))
    datatable(df, escape = FALSE, selection = "none", options = list(dom = 't'))
  })
  
  output$inventory_table <- renderDT({
    df <- invHist$data
    df$Edit <- sprintf('<button class="btn btn-info btn-sm editProduct" id="editp_%s">Edit</button>', seq_len(nrow(df)))
    df$Delete <- sprintf('<button class="btn btn-danger btn-sm deleteProduct" id="deletep_%s">Delete</button>', seq_len(nrow(df)))
    datatable(df, escape = FALSE, selection = "none", options = list(dom = 't'))
  })
}

shinyApp(ui = ui, server = server)


