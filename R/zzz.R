.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "MODBrowser loaded!\n",
    "--Use MODB.Run() to start MOD Browser.\n",
    "---Need help? Run: ?MODB.Run\n",
    "----Build you own: ?modb.help\n",
    "-----Update me: modb.update()"
  )
}
