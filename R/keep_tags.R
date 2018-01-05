
#' Tags to Keep for Filtering
#'
#' @return Character vector
#' @export
#'
#' @examples
#' relevant_tags()
relevant_tags = function() {
  c("(0008,0008)", "(0008,0060)", "(0018,1210)",
    "(0018,1160)", "(0018,1151)", "(0018,0081)",
    "(0018,1150)", "(0018,0080)", "(0008,9007)",
    "(0018,9316)", "(0018,0050)", "(0018,0010)",
    "(0028,0030)", "(0008,2112)", "(0008,103e)",
    "(0008,1030)", "(0028,1050)", "(0028,1051)",
    "(0028,1055)",
    "(0008,0050)", #AccessionNumber
    "(0020,0012)", #AcquisitionNumber
    "(0020,0011)" # SeriesNumber

  )
}

#' @export
#' @rdname relevant_tags
relevant_fields = function() {
  no_double = c("ConvolutionKernel", "ImageType", "Modality", "PixelSpacing",
                "SeriesDescription",
                "SliceThickness", "StudyDescription",
                # "SeriesNumber",
                "AccessionNumber", "AcquisitionNumber")
}
