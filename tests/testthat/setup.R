# Python subprocesses import installed helper scripts during tests. Do not write
# bytecode caches into the package library used by R CMD check.
Sys.setenv(PYTHONDONTWRITEBYTECODE = "1")
