#_______________________________________________________________________________
#' BED3
#' André Wattø Sjuve, andre.sjuve@nhh.no
#' Norwegian School of Economics
#' Created: "05 juni, 2025"

#' Description: This script contains functions used to generate various
#' content across the course website 
#_______________________________________________________________________________

# FUNCTIONS -----------

# Load required packages
if (!requireNamespace("cli", quietly = TRUE)) install.packages("cli")
if (!requireNamespace("quarto", quietly = TRUE)) install.packages("quarto")

#' Convert the formula page to a PDF-friendly version
#'
#' @param input_path Path to the source Quarto file (default: "formelark.qmd")
#' @param output_path Path to the cleaned PDF version (default: "formelark-print.qmd")


convert_formula_qmd_for_pdf <- function(input_path = "formelark.qmd",
                                        output_path = "assets/formelark-print.qmd") {
    lines <- readLines(input_path, warn = FALSE)
    
    # Emoji regex pattern – remove only leading emoji from bullet labels or headers
    emoji_replace <- function(line) {
        stringr::str_replace(line, "^(-\\s*)(\\p{So}|\\p{Sk}|\\p{Emoji_Presentation}|\\p{Cntrl})+\\s*", "\\1")
    }
    
    
    # Strip ::: panel/callout lines
    lines <- gsub("^:::\\s*\\{.*\\}", "", lines)
    lines <- gsub("^:::$", "", lines)
    
    # Remove emoji from bullets and headers
    lines <- vapply(lines, emoji_replace, character(1), USE.NAMES = FALSE)
    
    # Replace YAML block
    start_yaml <- which(grepl("^---", lines))[1]
    end_yaml <- which(grepl("^---", lines))[-1][1]
    yaml_block <- c(
        "---",
        "title: \"Formelark – BED3\"",
        "format:",
        "  pdf:",
        "    documentclass: article",
        "    toc: false",
        "    include-in-header: formelark-preamble.tex",
        "    include-before-body: formelark-cover.tex",
        "    number-sections: false",
        "    keep-tex: true",
        "    papersize: a4",
        "    fontsize: 11pt",
        "mainfont: Latin Modern Roman",
        "---"
    )
    lines <- c(
        yaml_block,
        lines[(end_yaml + 1):length(lines)]
    )
    
    writeLines(lines, output_path)
    cli::cli_alert_success("PDF-ready file written to {.file {output_path}}.")
}

#' Render the PDF version using Quarto
#'
#' @param qmd_file Path to the PDF-friendly Quarto file (default: "formelark-print.qmd")


render_formelark_pdf <- function(qmd_file = "formelark-print.qmd", 
                                 move_tex_to = "assets/latex") {
    if (!fs::file_exists(qmd_file)) {
        cli::cli_abort("The file {.file {qmd_file}} does not exist.")
    }
    
    # Render the PDF from the qmd file
    quarto::quarto_render(input = qmd_file, output_format = "pdf")
    
    # Define .tex path
    tex_file <- fs::path_ext_set(qmd_file, "tex")
    
    # Create target folder if it doesn't exist
    if (!fs::dir_exists(move_tex_to)) {
        fs::dir_create(move_tex_to)
    }
    
    # Construct destination path and move the file
    new_tex_path <- fs::path(move_tex_to, fs::path_file(tex_file))
    fs::file_move(tex_file, new_tex_path)
    
    cli::cli_alert_success("Rendered PDF and moved .tex to {.file {new_tex_path}}.")
}

#_______________________________________________________________________________
# Copyright Andre W. Sjuve 2025 ----