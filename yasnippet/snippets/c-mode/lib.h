#ifndef LIB_H
#define LIB_H

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
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/uio.h>
#include "def.h"


/* UTILS */
char *HHMMSS();

/* DEBUG */
void err_sys(const char *fmt, ...);
void h_do_msg(int errflag, const char *file, const char *func, int line, const char *fmt, va_list ap);
void h_debug_msg(const char *file, const char *func, int line, const char *fmt, ...);
void h_error_msg(const char *file, const char *func, int line, const char *fmt, ...);
#define H_DEBUG_MSG(fmt, ...) h_debug_msg(__FILE__, __func__, __LINE__, fmt, ##__VA_ARGS__)
#define H_ERROR_MSG(fmt, ...) h_error_msg(__FILE__, __func__, __LINE__, fmt, ##__VA_ARGS__)
void PrintTrace(void);
void PrintSockInfo(int connfd);
void PrintSocketOpts(int fd);

/* IO */
int Poll(struct pollfd *fdarray, unsigned long nfds, int timeout);
int Select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, struct timeval *timeout);
ssize_t Read(int fd, void *ptr, size_t nbytes);
#ifdef MSG_WAITALL
#define Readn(fd, ptr, nbytes) recv(fd, ptr, nbytes, MSG_WAITALL)
#else
ssize_t Readn(int fd, void *ptr, size_t nbytes);
#endif /* MSG_WAITALL */
void Write(int fd, void *ptr, size_t nbytes);
void Writen(int fd, const void *vptr, size_t n);

/* SOCKET */
int Listen(const char *host, const char *service);
int Accept(int listenfd);
int Connect(const char *host, const char *service);

/* SIGNAL */
void (*Signal(int signo, void (*func)(int)))(int);
void (*SignalIntr(int signo, void (*func)(int)))(int);

/* THREAD */
pthread_t MkThrd(void *(*fn)(void *), void *arg);

/* PROCESS */
void *Malloc(size_t size);


#endif /* LIB_H */
