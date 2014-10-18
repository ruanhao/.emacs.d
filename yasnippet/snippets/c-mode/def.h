#ifndef DEF_H
#define DEF_H

#ifndef FILE_MODE
/* default file access permissions for new files */
#define FILE_MODE (S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH)
#endif /* FILE_MODE */

#ifndef DIR_MODE
/* default permissions for new directories */
#define DIR_MODE  (FILE_MODE | S_IXUSR | S_IXGRP | S_IXOTH)
#endif /* DIR_MODE */

#ifndef AI_FAMILY
#define AI_FAMILY AF_UNSPEC
#endif /* AI_FAMILY */

#ifndef AI_SOCKTYPE
#define AI_SOCKTYPE SOCK_STREAM
#endif /* AI_SOCKTYPE */

#ifndef DBG_INTERFACE
#define DBG_INTERFACE "dbg_interface"
#endif /* DBG_INTERFACE */

/* COLOR */
#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[36m"
#define ANSI_COLOR_RESET   "\x1b[0m"

#endif /* DEF_H */
