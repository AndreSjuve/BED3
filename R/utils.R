#_______________________________________________________________________________
#' BED3
#' André Wattø Sjuve, andre.sjuve@nhh.no
#' Norwegian School of Economics
#' Created: "05 juni, 2025"

#' Description: This script contains functions used to generate various
#' content across the course website
#_______________________________________________________________________________

# FUNCTIONS -----------

# convert_figure_qmd() -----------

convert_figure_qmd <- function(qmd_file, out_dir = NULL) {
  qmd_file <- normalizePath(qmd_file, winslash = "/")
  stem <- tools::file_path_sans_ext(basename(qmd_file))
  src_dir <- normalizePath(dirname(qmd_file), winslash = "/")
  pdf_name <- paste0(stem, ".pdf")
  svg_name <- paste0(stem, ".svg")
  pdf_src <- file.path(src_dir, pdf_name)
  svg_src <- file.path(src_dir, svg_name)

  old <- getwd()
  on.exit(setwd(old), add = TRUE)
  setwd(src_dir)

  # Render til PDF (Quarto kan plassere filen i prosjektrot).
  # Render uten output-flags og flytt om nødvendig fra prosjektrot til src_dir.
  cmd_quarto <- sprintf(
    'quarto render "%s" --to pdf',
    basename(qmd_file)
  )
  system(cmd_quarto)
  if (!file.exists(pdf_src)) {
    pdf_alt <- file.path(old, pdf_name)
    if (file.exists(pdf_alt)) {
      file.rename(pdf_alt, pdf_src)
    }
  }
  if (!file.exists(pdf_src)) {
    stop("PDF ble ikke produsert")
  }

  # Konverter PDF til SVG.
  cmd_inkscape <- sprintf(
    'inkscape "%s" --export-type=svg --export-filename="%s"',
    pdf_name,
    svg_name
  )
  system(cmd_inkscape)
  if (!file.exists(svg_src)) {
    stop("SVG ble ikke produsert")
  }

  # Flytt til out_dir om ønskelig.
  if (!is.null(out_dir)) {
    dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
    pdf_tgt <- file.path(out_dir, pdf_name)
    svg_tgt <- file.path(out_dir, svg_name)
    file.rename(pdf_src, pdf_tgt)
    file.rename(svg_src, svg_tgt)
    message("Ferdig. PDF og SVG i: ", out_dir)
    return(invisible(list(pdf = pdf_tgt, svg = svg_tgt)))
  } else {
    message("Ferdig. PDF og SVG ligger ved siden av qmd.")
    return(invisible(list(pdf = pdf_src, svg = svg_src)))
  }
}


# convert_formula_qmd_for_pdf() -----------

#' Convert Quarto formula sheet to print-ready LaTeX version
#'
#' This function reads a Quarto `.qmd` file containing a formula sheet for the BED3 course,
#' and converts it into a print-optimized `.qmd` version using LaTeX tables.
#' The output is structured as a single tabularx table with labeled formulas grouped by section.
#' Emoji bullets and callout panels are removed, and the resulting file is suitable for rendering as a PDF appendix.
#'
#' @param input_path Character. Path to the source `.qmd` file (default: `"formelark.qmd"`).
#'   This file must use a specific structure: section headers as `##`, followed by bullet points (`-`)
#'   with labels in `**bold**`, and each formula enclosed in a three-line math block with `$$` delimiters.
#' @param output_path Character. Path to write the transformed output `.qmd` file
#'   (default: `"assets/formelark-print.qmd"`). The file includes an updated YAML block and a fenced
#'   LaTeX block that generates a single `tabularx` table.
#'
#' @details
#' The function performs the following transformations:
#' - Removes frontmatter YAML from the input file.
#' - Removes any download link for the PDF version.
#' - Strips callout panel lines and emoji bullets.
#' - Extracts formulas and organizes them into a single LaTeX table grouped by topic.
#' - Adds a new YAML block for PDF rendering, including LaTeX geometry, font size, and preamble inclusion.
#'
#' Section headers are rendered inside the table using `\\multicolumn{2}` with bold serif font.
#' Each formula is wrapped in `$...$` and aligned to its label.
#'
#' @return
#' No return value. The function writes a Quarto file to `output_path` and prints a success message.
#'
#' @examples
#' convert_formula_qmd_for_pdf()  # Use default paths
#' convert_formula_qmd_for_pdf("formelark.qmd", "assets/formelark-print.qmd")
#'
#' @export
convert_formula_qmd_for_pdf <- function(
  input_path = "formelark.qmd",
  output_path = "assets/formelark-print.qmd"
) {
  if (!requireNamespace("stringr", quietly = TRUE)) {
    install.packages("stringr")
  }
  if (!requireNamespace("cli", quietly = TRUE)) {
    install.packages("cli")
  }

  lines <- readLines(input_path, warn = FALSE)

  # Remove YAML frontmatter
  start_yaml <- which(lines == "---")[1]
  end_yaml <- which(lines == "---")[-1][1]
  lines <- lines[(end_yaml + 1):length(lines)]

  # Remove download block
  remove_indices <- which(
    grepl("formelark-print\\.pdf", lines, fixed = TRUE) |
      grepl("^> 📄", lines) |
      grepl("^> \\[", lines)
  )
  if (length(remove_indices) > 0) {
    lines <- lines[-unique(c(remove_indices, remove_indices - 1))]
  }

  # Emoji regex pattern – remove only leading emoji from bullet labels or headers
  emoji_replace <- function(line) {
    stringr::str_replace(
      line,
      "^(-\\s*)(\\p{So}|\\p{Sk}|\\p{Emoji_Presentation}|\\p{Cntrl})+\\s*",
      "\\1"
    )
  }

  # Strip ::: panel/callout lines
  lines <- gsub("^:::\\s*\\{.*\\}", "", lines)
  lines <- gsub("^:::$", "", lines)

  # Remove emoji from bullets and headers
  lines <- vapply(lines, emoji_replace, character(1), USE.NAMES = FALSE)

  # Parse blocks
  entries <- list()
  current_section <- NULL
  i <- 1

  while (i <= length(lines)) {
    line <- stringr::str_trim(lines[i])

    # Section header
    if (stringr::str_detect(line, "^##\\s")) {
      current_section <- stringr::str_remove(line, "^##\\s+")
      i <- i + 1
      next
    }

    # Bullet label
    if (stringr::str_detect(line, "^\\s*-.*?\\*\\*")) {
      label <- stringr::str_match(line, "\\*\\*(.*?)\\*\\*")[, 2]
      if (
        (i + 2) <= length(lines) &&
          stringr::str_trim(lines[i + 1]) == "$$" &&
          stringr::str_trim(lines[i + 3]) == "$$"
      ) {
        formula <- stringr::str_trim(lines[i + 2])
        entries <- append(entries, list(c(current_section, label, formula)))
        i <- i + 4
        next
      }
    }

    i <- i + 1
  }

  # YAML + LaTeX header
  output <- c(
    "---",
    "title: \"Vedlegg til eksamen i BED3 Investering og finans\"",
    "format:",
    "  pdf:",
    "    documentclass: article",
    "    toc: false",
    "    number-sections: false",
    "    keep-tex: true",
    "    include-in-header: latex/minimal_header.tex",
    "    geometry: top=1cm, bottom=1.5cm, left=1cm, right=1cm",
    "    papersize: a4",
    "    fontsize: 10pt",
    "    linestretch: 1",
    "    pagestyle: plain",
    "mainfont: Latin Modern Roman",
    "---",
    "",
    "```{=latex}",
    "\\renewcommand{\\arraystretch}{2.0}",
    "\\normalsize",
    "\\begin{tabularx}{\\textwidth}{@{}lX@{}}"
  )

  prev_section <- NULL
  for (entry in entries) {
    section <- entry[1]
    label <- entry[2]
    formula <- entry[3]

    if (!identical(section, prev_section)) {
      output <- c(
        output,
        glue::glue(
          "\\multicolumn{{2}}{{@{{}}l@{{}}}}{{\\textbf{{{section}}}}} \\\\ \\addlinespace"
        )
      )
      prev_section <- section
    }

    output <- c(output, glue::glue("{label} & $ {formula} $ \\\\"))
  }

  output <- c(output, "\\end{tabularx}", "```")

  # Write .qmd file
  writeLines(output, output_path)
  cli::cli_alert_success(
    "✅ Longtable PDF-ready file written to {.file {output_path}}"
  )
}

# render_formelark_pdf() -----------

#' Render the PDF version using Quarto
#'
#' @param qmd_file Path to the PDF-friendly Quarto file (default: "formelark-print.qmd")

render_formelark_pdf <- function(
  qmd_file = "assets/formelark-print.qmd",
  move_tex_to = "assets/latex"
) {
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

  cli::cli_alert_success(
    "Rendered PDF and moved .tex to {.file {new_tex_path}}."
  )
}

#_______________________________________________________________________________
# Copyright Andre W. Sjuve 2025 ----
