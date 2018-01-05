
#' Sort DICOM directories
#'
#' @param directory Directory of DICOM files/folders
#' @param remove_original Should original files be deleted? This is passed to
#' \code{\link{sortDicomDirectories}}
#' @param remove_directories Should all the old directories be removed after
#' sorting
#' @param copy_files Should the files be copied to a temporary location before
#' running.  Alternatively, you can set this to \code{FALSE} and
#' do this beforehand.  If \code{FALSE}, the data will be deleted from
#' original directory!
#' @param remove_compressed Remove folders that end in
#' \code{zip|rar|gz|tar|bz|xz}
#' @param ... additional arguments to \code{\link{sortDicomDirectories}}
#'
#' @return A List of the directory and the old/new directories
#' @export
#' @importFrom tractor.base sortDicomDirectories
#' @importFrom utils head
dcmsort = function(
  directory,
  copy_files = FALSE,
  remove_original = TRUE,
  remove_directories = TRUE,
  remove_compressed = TRUE,
  ...) {


  if (copy_files) {
    tdir = tempfile()
    dir.create(tdir, showWarnings = FALSE)
    x = list.files(
      path = directory, all.files = TRUE, full.names = TRUE,
      recursive = TRUE)
    file.copy(x, to = tdir, recursive = TRUE, overwrite = TRUE)
    directory = tdir
  }

  before_run = list.dirs(directory, recursive = FALSE)

  # remove DICOMDIRs
  dcm_dirs = list.files(
    path = directory,
    pattern = "DICOMDIR",
    recursive = TRUE)
  if (length(dcm_dirs) > 0) {
    dcm_dirs = dcm_dirs[ toupper(basename(dcm_dirs) == "DICOMDIR") ]
    if (length(dcm_dirs) > 0) {
      file.remove(dcm_dirs)
    }
  }

  if (remove_compressed) {
    # find all zip files - uncompress them, then delete zip files
    all_zip = list.files(
      path = directory,
      pattern = "[.](zip|rar|gz|tar|bz|xz)$",
      recursive = TRUE, full.names = TRUE)
    if (length(all_zip) > 0) {
      message("zip files found")
      file.remove(all_zip)
    }
  }

  files = list.files(
    directory, full.names = TRUE,
    recursive = TRUE)
  files = files[!file.info(files)$isdir]
  if (length(files) == 0) {
    stop("There are no files in this directory that aren't directories")
  }

  # sort the data
  res = tractor.base::sortDicomDirectories(
    directory,
    deleteOriginals = remove_original,
    ignoreTransferSyntax = TRUE,
    ...)

  after_run = list.dirs(directory, recursive = FALSE)

  new_dirs = setdiff(after_run, before_run)
  old_dirs = intersect(after_run, before_run)

  non_converted_files = lapply(
    old_dirs, list.files,
    recursive = TRUE,
    all.files = TRUE,
    full.names = TRUE)
  non_converted_files = lapply(
    non_converted_files,
    function(x) {
      bn = basename(x)
      x = x[ !grepl(".DS_Store", bn, fixed = TRUE)]
      x
    })
  lengths = sapply(non_converted_files, length)
  keep = lengths > 0
  non_converted_files = non_converted_files[keep]
  if (any(keep)) {
    show_files = lapply(non_converted_files, head)
  }
  potential_dcm = sapply(non_converted_files, function(x){
    if (length(x) > 0) {
      x = x[basename(toupper(x)) != "DICOMDIR"]
      keep = grepl("[.]dcm$", tolower(x))
      keep = keep | !grepl("[.]", x)
      x = x[keep]
    }
    return(x)
  })
  any_dcm = sapply(potential_dcm, length) > 0

  L = list(
    directory = directory,
    new_directories = new_dirs,
    old_directories = old_dirs)
  if (any(any_dcm)) {
    msg = "Some DICOMs may have not been converted"
    # print(show_files[any_dcm])
    L$non_converted = show_files[any_dcm]
    if (remove_directories) {
      msg = paste0(msg,
                   ", they will be deleted as remove_directories = TRUE")
    }
    warning(msg)
  }
  if (remove_directories) {
    unlink(old_dirs, recursive = TRUE)
  }
  return(L)
}










