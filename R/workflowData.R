#' Function to download and copy data to AnVIL from specified locations
#' @name avpublish_workflow_data
#' @rdname avpublish_workflow_data
#' @description \code{avpublish_workflow_data} reads in a user provided data 
#'     frame that includes the locations of the files to be downloaded. These 
#'     locations can include ftp, http, https, Google bucket, or local paths.
#'     The function will then download the files from the provided locations and 
#'     copy them to the Google bucket associated with the AnVIL workspace. It 
#'     will also create the necessary data and data set tables neede for workflows. 
#' @param .data The data frame to be read in.
#' @param fastq_column1 The name of the column for the first fastq file. Default
#'     is "fastq1_src".
#' @param fastq_colum2 The name of the column for the second fastq file. Default 
#'     is "fastq2_src".
#' @param entity The name of the column that contains the sample/participant
#'     names. Default is set to names(.data)[[1]].
#' @param namespace The namespace of the AnVIL workspace. Default is set to 
#'     avworkspace_namespace().
#' @param name The name of the AnVIL workspace. Default is set to 
#'     avworkspace_name().
#' @importFrom AnVIL avworkspace_namespace, avworkspace_name
#' @return A .
#' @export
#' @examples
#' path <- system.file("extdata", "test_tibble.csv", package = "AnVILBulkRNASeq")
#' dat <- read.csv(path)[,-1]
#' tbl <- tibble(dat)
#' avpublish_workflow_data(tbl)

avpublish_workflow_data <- function(
    .data, fastq_column1 = "fastq1_src", fastq_column2 = "fastq2_src", 
    entity = names(.data)[[1]], namespace = avworkspace_namespace(), 
    name = avworkspace_name()) {

    ## avworkspace("bioconductor-rpci-anvil/Bioconductor-Workflow-DESeq2")

    stopifnot(AnVIL:::.is_scalar_character(fastq_column1),
        AnVIL:::.is_scalar_character(fastq_column2),
        AnVIL:::.is_scalar_character(namespace), 
        AnVIL:::.is_scalar_character(name),
        !all(c("fastq1", "fastq2") %in% names(.data)),
        any(c(entity, paste0(entity,"_id")) %in% names(.data))) 

    ## download files to workspace bucket
    urls1 <- .data %>% pull(fastq_column1)
    urls2 <- .data %>% pull(fastq_column2)
    urls <- c(urls1, urls2)

    ## could be files already exist in gs
    ## could copy from one bucket to another
    dir <- tempfile()
    dir.create(dir)
    fastqs <- file.path(dir, basename(urls))
    idx <- grepl("^(https?|ftp)://", urls)
    results <- Map(download.file, urls[idx], fastqs[idx], "curl") ## should method be set to "curl"?
    fastqs[!idx] <- urls[!idx]

    AnVIL::gsutil_cp(fastqs, file.path(AnVIL::avbucket(), "data"))

    ## push 'tbl' to DATA TABLE
    .data <- .data %>% mutate(
        fastq1 = file.path(AnVIL::avbucket(), "data", basename(urls1)), 
        fastq2 = file.path(AnVIL::avbucket(), "data", basename(urls2))
        )
    .data %>% avtable_import(entity = entity, namespace = namespace, name = name)

    ## push 'tbl_set' to DATA TABLE
    avtable(entity) %>% 
        mutate(set = paste0(entity, "_set")) %>%
        avtable_import_set(entity, "set", "name")
}
