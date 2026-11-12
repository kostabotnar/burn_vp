# new_utmb_secret() reads /dev/urandom, which Windows does not have.
skip_without_urandom <- function() {
  testthat::skip_if_not(file.exists("/dev/urandom"), "needs /dev/urandom (Linux) for a new secret")
}
