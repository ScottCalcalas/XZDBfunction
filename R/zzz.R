.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "XZDBfunction loaded! 🚀\n",
    "--Use XZDB.Run() to start the Shiny Genomic DB Browser.\n",
    "---Need help? Run: ?XZDB.Run \n",
    "----Update me: XZ.update()"
  )
}
