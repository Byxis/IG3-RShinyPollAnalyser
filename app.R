# Install and load required packages
if (!require(shiny)) install.packages("shiny")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(DT)) install.packages("DT")
if (!require(readr)) install.packages("readr")
if (!require(viridis)) install.packages("viridis")
if (!require(RColorBrewer)) install.packages("RColorBrewer")
if (!require(shadowtext)) install.packages("shadowtext")

library(shadowtext)
library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(readr)
library(viridis)
library(RColorBrewer)
library(shadowtext)

# Load data
data <- read.csv("CD.csv")
names(data) <- c(
    "Horodateur", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "Q8", "Q9",
    "Q10", "Q11", "Q12", "Q13", "Q14", "Q15", "Q16", "Q17", "Q18", "Q19"
)


# Define color palettes for plots
color_palettes <- list(
    "Default" = NULL,
    "Pastel Distincts" = c("#CAE8FF", "#AEDFF7", "#C7FFDA", "#E1FFD1", "#FFFFC7", "#FFDFB0", "#FFCBC7", "#F1CAF9", "#DFCCFF", "#C4E0E7"),
    "Pastel Chaud" = c("#FFD6C0", "#FFC3C3", "#FFB6B9", "#FFDFD3", "#FFAF87", "#FFC4E1", "#FFE6E6", "#FFCCB3", "#FFA8A8", "#FCBAD3"),
    "Pastel Froid" = c("#CCFAFF", "#BDF2FF", "#A5DFEC", "#C8F3FA", "#D5F7FF", "#C2E9FF", "#BADFFF", "#C4D4FF", "#D6E7FF", "#D8EEFF"),
    "Viridis" = viridis_pal(option = "viridis")(10),
    "Plasma" = viridis_pal(option = "plasma")(10),
    "Inferno" = viridis_pal(option = "inferno")(10),
    "Dégradé Bleu" = colorRampPalette(c("#e6f7ff", "#0070f3"))(10),
    "Dégradé Vert" = colorRampPalette(c("#e6ffee", "#00b36b"))(10),
    "Dégradé Orange" = colorRampPalette(c("#fff5e6", "#ff8000"))(10),
    "Arc-en-ciel" = rainbow(10),
    "Pastels" = c("#fbb4ae", "#b3cde3", "#ccebc5", "#decbe4", "#fed9a6", "#ffffcc", "#e5d8bd", "#fddaec", "#f2f2f2"),
    "Qualitatif" = brewer.pal(8, "Set2")
)

# Function to create a plot based on user inputs
createPlot <- function(visualType, selectedQ, compareQ = NULL, df, df_compare = NULL,
                                             question_titles, selected_palette, textStyle = "shadow") {
    question_text <- question_titles[which(paste0("Q", 1:19) == selectedQ)]

    # Bar chart
    if (visualType == "bar") {
        p <- ggplot(df, aes(x = Réponse, y = Nombre, fill = Réponse)) +
            geom_bar(stat = "identity") +
            labs(
                title = question_text,
                x = "Réponse",
                y = "Nombre de réponses"
            ) +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                legend.position = "none"
            )

        if (textStyle == "shadow") {
            p <- p + shadowtext::geom_shadowtext(
                aes(label = sprintf("%d (%.1f%%)", Nombre, Pourcentage)),
                hjust = 0.5, vjust = -0.5,
                size = 4,
                bg.colour = "white",
                colour = "black"
            )
        } else {
            p <- p + geom_text(
                aes(label = sprintf("%d (%.1f%%)", Nombre, Pourcentage)),
                hjust = 0.5, vjust = -0.5,
                size = 4,
                color = textStyle
            )
        }
    }

    # Pie chart
    else if (visualType == "pie") {
        p <- ggplot(df, aes(x = "", y = Nombre, fill = Réponse)) +
            geom_bar(stat = "identity", width = 1) +
            coord_polar("y", start = 0) +
            labs(
                title = question_text,
                x = NULL, y = NULL, fill = "Réponse"
            ) +
            theme_minimal() +
            theme(
                axis.text = element_blank(),
                axis.ticks = element_blank(),
                panel.grid = element_blank(),
                plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
            )

        if (textStyle == "shadow") {
            p <- p + shadowtext::geom_shadowtext(
                aes(label = sprintf("%s\n%d (%.1f%%)", Réponse, Nombre, Pourcentage)),
                position = position_stack(vjust = 0.5),
                bg.colour = "white",
                colour = "black"
            )
        } else {
            p <- p + geom_text(
                aes(label = sprintf("%s\n%d (%.1f%%)", Réponse, Nombre, Pourcentage)),
                position = position_stack(vjust = 0.5),
                color = textStyle
            )
        }
    }

    # Comparison chart
    else if (visualType == "compare") {
        compare_text <- question_titles[which(paste0("Q", 1:19) == compareQ)]

        p <- ggplot(df_compare, aes(x = !!sym(selectedQ), y = count, fill = !!sym(compareQ))) +
            geom_bar(stat = "identity", position = "stack") +
            labs(
                title = paste(question_text, "vs", compare_text),
                x = question_text,
                y = "Nombre de réponses",
                fill = compare_text
            ) +
            theme_minimal() +
            theme(
                axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
            )

        if (textStyle == "shadow") {
            p <- p + shadowtext::geom_shadowtext(
                aes(label = sprintf("%d\n(%.1f%%)", count, percentage)),
                position = position_stack(vjust = 0.5),
                bg.colour = "white",
                colour = "black",
                size = 3
            )
        } else {
            p <- p + geom_text(
                aes(label = sprintf("%d\n(%.1f%%)", count, percentage)),
                position = position_stack(vjust = 0.5),
                color = textStyle,
                size = 3
            )
        }
    }

    # Apply color palette if selected
    if (!is.null(selected_palette)) {
        p <- p + scale_fill_manual(values = selected_palette)
    }

    return(p)
}

question_titles <- c(
    "Savez-vous ce qu'est le CI/CD ?",
    "Avez-vous déjà rencontré des problèmes lors de l'intégration ou du déploiement \nde code ?",
    "Avez-vous déjà utilisé un outil de CI/CD (ex : GitHub Actions, GitLab CI/CD, Jenkins, etc.) ?",
    "Pensez-vous que l'adoption du CI/CD est importante dans le développement de projets en équipe?",
    "Avez-vous songé à l'autoformation ?",
    "Avez-vous réalisé de l'autoformation pendant votre formation ?",
    "Aimeriez-vous en apprendre plus sur le CI/CD à travers ?",
    "Seriez-vous intéressé par l'utilisation d'outils CI/CD dans vos futurs projets ?",
    "Pensez-vous que le CI/CD joue un rôle important dans la vie d'un développeur ?",
    "Avez-vous déjà suivi un cours ou une formation sur DevOps ou CI/CD ?",
    "Quelle est votre année d'étude ?",
    "Avez-vous quelque chose à rajouter ?",
    "Avez-vous suivi le cours sur le CI/CD, Qualité logiciel python ?",
    "Si non, pourquoi ?",
    "Le cours en IG3 que nous avons eu sur le CI/CD, Qualité logiciel python était...",
    "A propos de l'emplacement du créneau sur le cours du CI/CD d'IG3 ?",
    "Pensez-vous qu'il serait pertinent d'ajouter des cours sur le CI/CD en IG3 ?",
    "Avez-vous des recommandations pour améliorer notre formation au CI/CD ?",
    "Cochez les recommandations qui vous semblent intéressantes"
)

# UI definition
ui <- fluidPage(
    titlePanel("Analyse de sondage CI/CD"),
    sidebarLayout(
        sidebarPanel(
            selectInput("question", "Sélectionnez une question :",
                choices = setNames(paste0("Q", 1:19), question_titles),
                selected = "Q1"
            ),
            radioButtons("visualType", "Type de visualisation :",
                choices = c(
                    "Diagramme en barres" = "bar",
                    "Diagramme circulaire" = "pie",
                    "Diagramme comparatif" = "compare"
                ),
                selected = "bar"
            ),
            conditionalPanel(
                condition = "input.visualType == 'compare'",
                selectInput("compareQuestion", "Comparer avec cette question :",
                    choices = setNames(paste0("Q", 1:19), question_titles),
                    selected = "Q2"
                )
            ),
            selectInput("colorPalette", "Palette de couleurs :",
                choices = names(color_palettes),
                selected = "Default"
            ),
            radioButtons("textStyle", "Style du texte :",
                choices = c(
                    "Texte noir" = "black",
                    "Texte blanc" = "white",
                    "Texte avec contour" = "shadow"
                ),
                selected = "shadow"
            ),
            checkboxInput("useFilter", "Filtrer par la réponse à une autre question", FALSE),
            conditionalPanel(
                condition = "input.useFilter == true",
                selectInput("filterQuestion", "Question de filtrage :",
                    choices = setNames(paste0("Q", 1:19), question_titles),
                    selected = "Q1"
                ),
                uiOutput("filterChoicesUI")
            ),
            downloadButton("downloadPlot", "Télécharger le graphique")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    "Visualisation",
                    plotOutput("resultPlot", height = "400px"),
                    br(),
                    h4("Répartition des réponses"),
                    DTOutput("resultTable")
                ),
                tabPanel(
                    "Réponses individuelles",
                    DTOutput("responseTable")
                )
            )
        )
    )
)

# Server logic
server <- function(input, output, session) {
    # Filter choices based on selected question
    output$filterChoicesUI <- renderUI({
        filterQ <- input$filterQuestion
        if (is.null(filterQ)) {
            return(NULL)
        }

        choices <- unique(data[[filterQ]])
        choices <- choices[!is.na(choices) & choices != ""]

        selectInput("filterValue", "Filtrer par cette réponse :", choices = choices)
    })

    filteredData <- reactive({
        result <- data

        if (input$useFilter && !is.null(input$filterValue)) {
            result <- result[result[[input$filterQuestion]] == input$filterValue, ]
        }

        return(result)
    })

    # Response stats
    responseStats <- reactive({
        req(input$question)

        selectedQ <- input$question

        df <- filteredData()
        counts <- table(df[[selectedQ]])
        response_df <- data.frame(
            Réponse = names(counts),
            Nombre = as.numeric(counts),
            Pourcentage = round(as.numeric(counts) / sum(counts) * 100, 1)
        )

        return(response_df)
    })

    # Comparison stats
    comparisonStats <- reactive({
        req(input$question, input$compareQuestion)

        primaryQ <- input$question
        compareQ <- input$compareQuestion

        df <- filteredData()

        counts <- df %>%
            group_by(!!sym(primaryQ), !!sym(compareQ)) %>%
            summarise(count = n(), .groups = "drop") %>%
            filter(!is.na(!!sym(primaryQ)), !is.na(!!sym(compareQ)))

        totals <- counts %>%
            group_by(!!sym(primaryQ)) %>%
            summarise(total = sum(count), .groups = "drop")

        result <- counts %>%
            left_join(totals, by = primaryQ) %>%
            mutate(percentage = round(count / total * 100, 1))

        return(result)
    })

    # Display plot
    output$resultPlot <- renderPlot({
        createPlot(
            visualType = input$visualType,
            selectedQ = input$question,
            compareQ = input$compareQuestion,
            df = responseStats(),
            df_compare = comparisonStats(),
            question_titles = question_titles,
            selected_palette = color_palettes[[input$colorPalette]],
            textStyle = input$textStyle
        )
    })

    # Display response table
    output$resultTable <- renderDT({
        if (input$visualType == "compare") {
            df <- comparisonStats()
            datatable(df,
                options = list(pageLength = 10, dom = "t"),
                rownames = FALSE,
                caption = "Tableau croisé des réponses"
            )
        } else {
            datatable(responseStats(),
                options = list(pageLength = 10, dom = "t"),
                rownames = FALSE
            )
        }
    })

    # Display individual responses
    output$responseTable <- renderDT({
        selected_data <- filteredData()

        datatable(selected_data,
            options = list(scrollX = TRUE, pageLength = 10),
            caption = "Réponses individuelles au sondage"
        )
    })

    # Handle plot download
    output$downloadPlot <- downloadHandler(
        filename = function() {
            paste("graphique-", input$question, ".png", sep = "")
        },
        content = function(file) {
            p <- createPlot(
                visualType = input$visualType,
                selectedQ = input$question,
                compareQ = input$compareQuestion,
                df = responseStats(),
                df_compare = comparisonStats(),
                question_titles = question_titles,
                selected_palette = color_palettes[[input$colorPalette]],
                textStyle = input$textStyle
            )

            ggsave(file, plot = p, width = 10, height = 7, units = "in", dpi = 300)
        }
    )
}

# Run the application
shinyApp(ui, server)
