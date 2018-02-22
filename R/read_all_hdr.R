
#' Read all DICOM headers in a directory and return a \code{data.frame}
#'
#' @param directory Directory of DICOM files
#' @param outfile Output RDS file to write.  If it exists and
#' \code{overwrite = FALSE}, then it will be read in and returned.
#' @param overwrite Should the RDS file be overwritten if it exists
#'
#' @return A \code{data.frame} of all the tags
#' @seealso \code{\link{read_dicom_header}}
#' @export
#' @importFrom utils txtProgressBar setTxtProgressBar
#' @importFrom dcmtk read_dicom_header
#' @importFrom dplyr bind_rows distinct %>% filter
read_all_hdr = function(
  directory,
  outfile =  tempfile(fileext = ".rds"),
  overwrite = FALSE
) {
  fname = NULL
  rm(list = "fname")

  if (!file.exists(outfile) || overwrite) {
    new_dirs = list.dirs(directory,
                         recursive = FALSE)


    ## how many files in that directory - under 5 - remove
    ## Keep only CT? What about MR?
    # for ( i in 1:20) {
    n_dirs = length(new_dirs)
    all_data = vector(mode = "list",
                      length = n_dirs)
    pb = txtProgressBar(min = 1,
                        max = n_dirs, style = 3)
    for (i in seq(n_dirs)) {
      setTxtProgressBar(pb, value = i)
      basedir = new_dirs[i]
      files_in_dir = list.files(path = basedir, recursive = TRUE)
      dirs_in_dir = list.dirs(
        path = basedir,
        full.names = FALSE,
        recursive = TRUE)
      files_in_dir = files_in_dir[
        !(files_in_dir %in% dirs_in_dir)]
      if (length(files_in_dir) > 0) {
        hdr = dcmtk::read_dicom_header(
          file = paste0(shQuote(basedir),
                        "/*"))
        hdr$dir = basedir
      } else {
        hdr = NULL
      }
      all_data[[i]] = hdr
    }
    close(pb)

    all_hdr = dplyr::bind_rows(all_data)

    # this should remove completely identical tags
    all_hdr = all_hdr %>% distinct()
    saveRDS(all_hdr, file = outfile)
  } else {
    message("Data exists, reading in the RDS file")
    all_hdr = readRDS(outfile)
  }
  # remove DICOMDIRs
  all_hdr$fname = basename(all_hdr$file)
  all_hdr = all_hdr %>%
    filter(!fname %in% "DICOMDIR")
  all_hdr$fname = NULL
  return(all_hdr)
}
