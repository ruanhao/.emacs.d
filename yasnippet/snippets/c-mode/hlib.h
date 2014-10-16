#ifndef INC_LIB_H
#define INC_LIB_H

#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <pthread.h>
#include <signal.h>
#include <sys/select.h>
#include <poll.h>
#include <execinfo.h>

/* COLOR */
#define ANSI_COLOR_RED     "\x1b[31m"
#define ANSI_COLOR_GREEN   "\x1b[32m"
#define ANSI_COLOR_YELLOW  "\x1b[33m"
#define ANSI_COLOR_BLUE    "\x1b[34m"
#define ANSI_COLOR_MAGENTA "\x1b[35m"
#define ANSI_COLOR_CYAN    "\x1b[36m"
#define ANSI_COLOR_RESET   "\x1b[0m"

/* DEBUG */
void print_trace(void);
void err_sys(const char *fmt, ...);
void h_do_msg(int errflag, const char *file, const char *func, int line, const char *fmt, va_list ap);
void h_debug_msg(const char *file, const char *func, int line, const char *fmt, ...);
void h_error_msg(const char *file, const char *func, int line, const char *fmt, ...);
#define H_DEBUG_MSG(fmt, ...) h_debug_msg(__FILE__, __func__, __LINE__, fmt, ##__VA_ARGS__)
#define H_ERROR_MSG(fmt, ...) h_error_msg(__FILE__, __func__, __LINE__, fmt, ##__VA_ARGS__)

/* IO */
int Poll(struct pollfd *fdarray, unsigned long nfds, int timeout);
int Select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, struct timeval *timeout);
ssize_t Read(int fd, void *ptr, size_t nbytes);
ssize_t Readn(int fd, void *vptr, size_t n);
void Write(int fd, void *ptr, size_t nbytes);
ssize_t Writen(int fd, const void *vptr, size_t n);

/* SOCKET */
int Listen(const char *host, const char *service);
int Accept(int listenfd);
int Connect(const char *host, const char *service);
char *Sock_ntop(const struct sockaddr *sa);

/* SIGNAL */
void (*Signal(int signo, void (*func)(int)))(int);

/* THREAD */
pthread_t MkThrd(void *(*fn)(void *), void *arg);

#endif /* INC_LIB_H */
