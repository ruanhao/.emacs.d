#include "hlib.h"

static void h_do_msg(int errflag, const char *fmt, va_list ap)
{
    char *str;
    vasprintf(&str, fmt, ap);
    if (errflag) {
        char *str_with_err;
        asprintf(&str_with_err, "%s: %s", str, strerror(errno));
        free(str);
        fprintf(stderr, "%s\n", str_with_err);
        free(str_with_err);
        return;
    }
    fprintf(stderr, "%s\n", str);
    free(str);
    return;
}

void h_debug_msg(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    h_do_msg(0, fmt, ap);
    va_end(ap);
    return;
}

void h_error_msg(const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    h_do_msg(1, fmt, ap);
    va_end(ap);
    return;
}

void err_sys(const char *fmt, ...)
{
    char *str;
    va_list ap;
    va_start(ap, fmt);
    vasprintf(&str, fmt, ap);
    va_end(ap);
    perror(str);
    free(str);
    exit(1);
}

int Poll(struct pollfd *fdarray, unsigned long nfds, int timeout)
{
    int n;
    if ((n = poll(fdarray, nfds, timeout)) < 0)
        err_sys("poll() error");
    return(n);
}

int Select(int nfds, fd_set *readfds, fd_set *writefds, fd_set *exceptfds, struct timeval *timeout)
{
    int n;
    if ((n = select(nfds, readfds, writefds, exceptfds, timeout)) < 0)
        err_sys("select() error");
    return(n); /* can return 0 on timeout */
}

ssize_t Read(int fd, void *ptr, size_t nbytes)
{
    ssize_t n;
    if ((n = read(fd, ptr, nbytes)) == -1)
        err_sys("read() error");
    return(n);
}

/* Read "n" bytes from a descriptor. */
ssize_t Readn(int fd, void *vptr, size_t n)
{
    size_t  nleft;
    ssize_t nread;
    char    *ptr;

    ptr = vptr;
    nleft = n;
    while (nleft > 0) {
        if ((nread = read(fd, ptr, nleft)) < 0) {
            if (errno == EINTR)
                nread = 0;      /* and call read() again */
            else
                return(-1);
        } else if (nread == 0)
            break;              /* EOF */

        nleft -= nread;
        ptr   += nread;
    }
    return(n - nleft);          /* return >= 0 */
}

void Write(int fd, void *ptr, size_t nbytes)
{
    if (write(fd, ptr, nbytes) != nbytes)
        err_sys("write() error");
}

/* Write "n" bytes to a descriptor. */
ssize_t Writen(int fd, const void *vptr, size_t n)
{
    size_t      nleft;
    ssize_t     nwritten;
    const char  *ptr;

    ptr = vptr;
    nleft = n;
    while (nleft > 0) {
        if ((nwritten = write(fd, ptr, nleft)) <= 0) {
            if (nwritten < 0 && errno == EINTR)
                nwritten = 0;       /* and call write() again */
            else
                return(-1);         /* error */
        }

        nleft -= nwritten;
        ptr   += nwritten;
    }
    return(n);
}

int Listen(const char *host, const char *service)
{
    int listenfd, n;
    const int on = 1;
    struct addrinfo hints, *res, *head;

    bzero(&hints, sizeof(struct addrinfo));
    hints.ai_flags    = AI_PASSIVE;
    hints.ai_family   = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    if ((n = getaddrinfo(host, service, &hints, &res)) != 0)
        err_sys("getaddrinfo() error for %s, %s: %s",
                host, service, gai_strerror(n));
    head = res;
    do {
        listenfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
        if (listenfd < 0)
            continue;                  /* error, try next one */
        if (setsockopt(listenfd, SOL_SOCKET, SO_REUSEADDR, &on, sizeof(on)) < 0)
            err_sys("setsockopt() for SO_REUSEADDR error");
        if (bind(listenfd, res->ai_addr, res->ai_addrlen) == 0)
            break;                     /* success */
        if (close(listenfd) == -1)     /* bind error, close and try next one */
            err_sys("close() error");
    } while ((res = res->ai_next) != NULL);

    if (res == NULL)
        err_sys("socket() or bind() error for %s, %s", host, service);

    if (listen(listenfd, 5) < 0)
        err_sys("listen() error");

    freeaddrinfo(head);
    return(listenfd);
}

int Accept(int listenfd)
{
    int connfd;
    while (1) {
        if ((connfd = accept(listenfd, NULL, NULL)) < 0)
#ifdef EPROTO
            if (errno == EPROTO || errno == ECONNABORTED || errno == EINTR)
#else
                if (errno == ECONNABORTED || errno == EINTR)
#endif
                    continue;
                else
                    err_sys("accept() error");
        break;
    }
    return(connfd);
}

int Connect(const char *host, const char *service)
{
    int sockfd, n;
    struct addrinfo hints, *res, *head;

    bzero(&hints, sizeof(struct addrinfo));
    hints.ai_family = AF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;

    if ((n = getaddrinfo(host, service, &hints, &res)) != 0)
        err_sys("getaddrinfo() error for %s, %s: %s",
                host, service, gai_strerror(n));
    head = res;

    do {
        sockfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);
        if (sockfd < 0)
            continue;	             /* ignore this one */
        if (connect(sockfd, res->ai_addr, res->ai_addrlen) == 0)
            break;                   /* success */
        if (close(sockfd) == -1)     /* ignore this one */
	    err_sys("close() error");
    } while ((res = res->ai_next) != NULL);

    if (res == NULL)	        /* errno set from final connect() */
        err_sys("connect() error for %s, %s", host, service);

    freeaddrinfo(head);
    return(sockfd);
}

char *Sock_ntop(const struct sockaddr *sa)
{
    char        portstr[8];
    static char str[128];  /* Unix domain is largest */

    switch (sa->sa_family) {
    case AF_INET:
    {
        struct sockaddr_in *sin = (struct sockaddr_in *) sa;
        if (inet_ntop(AF_INET, &sin->sin_addr, str, sizeof(str)) == NULL)
            return(NULL);
        if (ntohs(sin->sin_port) != 0) {
            snprintf(portstr, sizeof(portstr), ":%d", ntohs(sin->sin_port));
            strcat(str, portstr);
        }
        break;
    }
    case AF_INET6:
    {
        struct sockaddr_in6 *sin6 = (struct sockaddr_in6 *) sa;

        str[0] = '[';
        if (inet_ntop(AF_INET6, &sin6->sin6_addr, str + 1, sizeof(str) - 1) == NULL)
            return(NULL);
        strcat(str, "]");
        if (ntohs(sin6->sin6_port) != 0) {
            snprintf(portstr, sizeof(portstr), ":%d", ntohs(sin6->sin6_port));
            strcat(str, portstr);
            return(str);
        }
        break;
    }
    default:
        snprintf(str, sizeof(str), "Unknown AF_xxx: %d", sa->sa_family);
        break;
    } // end switch
    return(str);
}

/* Reliable version of signal(), using POSIX sigaction(). */
void (*Signal(int signo, void (*func)(int)))(int)
{
    struct sigaction act, oact;
    act.sa_handler = func;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
    if (signo == SIGALRM) {
#ifdef SA_INTERRUPT
        act.sa_flags |= SA_INTERRUPT;
#endif
    } else {
#ifdef SA_RESTART
        act.sa_flags |= SA_RESTART;
#endif
    }
    if (sigaction(signo, &act, &oact) < 0)
        return(SIG_ERR);
    return(oact.sa_handler);
}

pthread_t MkThrd(void *(*fn)(void *), void *arg)
{
    int err;
    pthread_t tid;
    pthread_attr_t attr;

    if (pthread_attr_init(&attr) != 0)
        err_sys("pthread_attr_init() error");
    if (pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED) != 0)
        err_sys("pthread_attr_setdetachstate() error");
    if (pthread_create(&tid, &attr, fn, arg) != 0)
        err_sys("pthread_create() error");
    pthread_attr_destroy(&attr);
    return tid;
}
