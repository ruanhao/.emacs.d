#include "lib.h"

void PrintTrace(void)
{
    void   *array[256];
    size_t  size;
    char * *strings;
    size_t  i;
    size    = backtrace(array, 256);
    strings = backtrace_symbols(array, size);
    fprintf(stderr, "==================== BACKTRACE (%zd stack frames) ====================\n", size);
    for (i = 0; i < size; i++)
        fprintf(stderr, "%s\n", strings[i]);
    fprintf(stderr, "====================================================================\n");
    free(strings);
}

char *HHMMSS()
{
    time_t      current_time;
    struct tm  *time_info;
    char       *time_str;
    size_t      space;

    time(&current_time);
    time_info = localtime(&current_time);
    space     = sizeof(char) * 9; // space for "HH:MM:SS\0"
    time_str  = (char *) Malloc(space);
    strftime(time_str, space, "%H:%M:%S", time_info);
    return time_str;
}

void h_do_msg(int errflag, const char *file, const char *func, int line, const char *fmt, va_list ap)
{
    char *str;
    char *str_with_err;
    FILE *fp = errflag ? stderr : stdout;
    char *current_time = HHMMSS();
    vasprintf(&str, fmt, ap);
    asprintf(&str_with_err, "%s (%s)", str, strerror(errno));
    fprintf(fp, "[%s] %s%s" ANSI_COLOR_RESET "  %-70s  <%s#%s@%d>\n",
            current_time,
            errflag ? ANSI_COLOR_RED : ANSI_COLOR_GREEN,
            errflag ? "ERROR" : "DEBUG",
            errflag ? str_with_err : str,
            file, func, line);
    fflush(fp);
    free(current_time);
    free(str);
    free(str_with_err);
    return;
}

void h_debug_msg(const char *file, const char *func, int line, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    h_do_msg(0, file, func, line, fmt, ap);
    va_end(ap);
    return;
}

void h_error_msg(const char *file, const char *func, int line, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    h_do_msg(1, file, func, line, fmt, ap);
    va_end(ap);
    return;
}

void err_sys(const char *fmt, ...)
{
    char *str;
    va_list ap;
    char *current_time = HHMMSS();
    va_start(ap, fmt);
    vasprintf(&str, fmt, ap);
    va_end(ap);
    fprintf(stderr, "[%s] %s%s" ANSI_COLOR_RESET "  %s (%s)\n", current_time, ANSI_COLOR_MAGENTA, "FATAL", str, strerror(errno));
    PrintTrace();
    free(current_time);
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
    if ((n = read(fd, ptr, nbytes)) == -1) {
        if (errno == EINTR || errno == ECONNRESET)
            return 0;
        err_sys("read() error");
    }
    return n;
}

#ifndef MSG_WAITALL
static ssize_t readn(int fd, void *vptr, size_t n)
{
    size_t   nleft;
    ssize_t  nread;
    char    *ptr;
    ptr   = vptr;
    nleft = n;
    while (nleft > 0) {
        if ((nread = read(fd, ptr, nleft)) < 0) {
            if (errno == EINTR || errno == ECONNRESET)
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

ssize_t Readn(int fd, void *ptr, size_t nbytes)
{
    ssize_t n;
    if ((n = readn(fd, ptr, nbytes)) < 0)
        err_sys("readn() error");
    return(n);
}
#endif /* MSG_WAITALL */

void Write(int fd, void *ptr, size_t nbytes)
{
    if (write(fd, ptr, nbytes) != nbytes)
        err_sys("write() error");
}

static ssize_t writen(int fd, const void *vptr, size_t n)
{
    size_t      nleft;
    ssize_t     nwritten;
    const char *ptr;
    ptr   = vptr;
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

void Writen(int fd, const void *ptr, size_t nbytes)
{
    if (writen(fd, ptr, nbytes) != nbytes)
        err_sys("writen() error");
}

int Listen(const char *host, const char *service)
{
    int listenfd, n;
    const int on = 1;
    struct addrinfo hints, *res, *head;

    bzero(&hints, sizeof(struct addrinfo));
    hints.ai_flags    = AI_PASSIVE;
    hints.ai_family   = AI_FAMILY;
    hints.ai_socktype = AI_SOCKTYPE;

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
    hints.ai_family   = AI_FAMILY;
    hints.ai_socktype = AI_SOCKTYPE;

    if ((n = getaddrinfo(host, service, &hints, &res)) != 0)
        err_sys("getaddrinfo() error for %s, %s: %s",
                host, service, gai_strerror(n));
    head = res;

    do {
        sockfd = socket(res->ai_family, res->ai_socktype, res->ai_protocol);

        // struct sockaddr_in cliaddr;
        // struct in_addr     inaddr;
        // char              *clihost;
        // in_port_t          cliport;
        // bzero(&cliaddr, sizeof(cliaddr));
        // inet_aton(clihost, &inaddr);
        // cliaddr.sin_family = AF_INET;
        // cliaddr.sin_addr   = inaddr;
        // cliaddr.sin_port   = htons(cliport);
        // bind(sockfd, (struct sockaddr *) &cliaddr, sizeof(cliaddr));

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

static char *SockNtop(const struct sockaddr *sa)
{
    size_t SIZE = 128 * sizeof(char); /* Unix domain is largest */
    char   portstr[8];
    char  *str = (char *) Malloc(SIZE);

    switch (sa->sa_family) {
    case AF_INET:
    {
        struct sockaddr_in *sin = (struct sockaddr_in *) sa;
        if (inet_ntop(AF_INET, &sin->sin_addr, str, SIZE) == NULL)
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
        if (inet_ntop(AF_INET6, &sin6->sin6_addr, str + 1, SIZE - 1) == NULL)
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
        snprintf(str, SIZE, "Unknown AF_xxx: %d", sa->sa_family);
        break;
    } // end switch
    return(str);
}

void PrintSockInfo(int connfd)
{
    struct sockaddr localsock;
    struct sockaddr peersock;
    char  *localsockstr;
    char  *peersockstr;
    socklen_t len = sizeof(struct sockaddr);

    if (getsockname(connfd, &localsock, &len) < 0)
        err_sys("getsockname() error");
    if (getpeername(connfd, &peersock, &len) < 0)
        err_sys("getpeername() error");
    localsockstr = SockNtop(&localsock);
    peersockstr  = SockNtop(&peersock);
    H_DEBUG_MSG("Socket Tuple: %s <===> %s", localsockstr, peersockstr);
    free(localsockstr);
    free(peersockstr);
}

static void (*signal2(int signo, void (*func)(int)))(int)
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

/* Reliable version of signal(), using POSIX sigaction(). */
void (*Signal(int signo, void (*func)(int)))(int)
{
    void (*sigfunc)(int);
    if ((sigfunc = signal2(signo, func)) == SIG_ERR)
        err_sys("signal2() error");
    return(sigfunc);
}

static void (*signal_intr(int signo, void (*func)(int)))(int)
{
    struct sigaction act, oact;
    act.sa_handler = func;
    sigemptyset(&act.sa_mask);
    act.sa_flags = 0;
#ifdef SA_INTERRUPT
    act.sa_flags |= SA_INTERRUPT;
#endif
    if (sigaction(signo, &act, &oact) < 0)
        return(SIG_ERR);
    return(oact.sa_handler);
}

void (*SignalIntr(int signo, void (*func)(int)))(int)
{
    void (*sigfunc)(int);
    if ((sigfunc = signal_intr(signo, func)) == SIG_ERR)
        err_sys("signal_intr() error");
    return(sigfunc);
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

void *Malloc(size_t size)
{
    void *ptr;

    if ((ptr = malloc(size)) == NULL)
        err_sys("malloc() error");
    return(ptr);
}


/* PrintSocketOpts */
static union val {
    int                   i_val;
    long                  l_val;
    struct linger         linger_val;
    struct timeval        timeval_val;
} val;

static char strres[128];

static char *sock_str_flag(union val *ptr, int len)
{
    if (len != sizeof(int))
        snprintf(strres, sizeof(strres), "size (%d) not sizeof(int)", len);
    else
        snprintf(strres, sizeof(strres), "%s", (ptr->i_val == 0) ? "off" : "on");
    return(strres);
}

static char *sock_str_int(union val *ptr, int len)
{
    if (len != sizeof(int))
        snprintf(strres, sizeof(strres), "size (%d) not sizeof(int)", len);
    else
        snprintf(strres, sizeof(strres), "%d", ptr->i_val);
    return(strres);
}

static char *sock_str_linger(union val *ptr, int len)
{
    struct linger *lptr = &ptr->linger_val;
    if (len != sizeof(struct linger))
        snprintf(strres, sizeof(strres), "size (%d) not sizeof(struct linger)", len);
    else
        snprintf(strres, sizeof(strres), "l_onoff = %d, l_linger = %d", lptr->l_onoff, lptr->l_linger);
    return(strres);
}

static char *sock_str_timeval(union val *ptr, int len)
{
    struct timeval *tvptr = &ptr->timeval_val;
    if (len != sizeof(struct timeval))
        snprintf(strres, sizeof(strres), "size (%d) not sizeof(struct timeval)", len);
    else
        snprintf(strres, sizeof(strres), "%d sec, %d usec", tvptr->tv_sec, tvptr->tv_usec);
    return(strres);
}

static struct sock_opts {
    const char   *opt_str;
    int           opt_level;
    int           opt_name;
    char       *(*opt_val_str)(union val *, int);
} sock_opts[] = {
    { "SO_BROADCAST",               SOL_SOCKET,     SO_BROADCAST,      sock_str_flag    },
    { "SO_DEBUG",                   SOL_SOCKET,     SO_DEBUG,          sock_str_flag    },
    { "SO_DONTROUTE",               SOL_SOCKET,     SO_DONTROUTE,      sock_str_flag    },
    { "SO_ERROR",                   SOL_SOCKET,     SO_ERROR,          sock_str_int     },
    { "SO_KEEPALIVE",               SOL_SOCKET,     SO_KEEPALIVE,      sock_str_flag    },
    { "SO_LINGER",                  SOL_SOCKET,     SO_LINGER,         sock_str_linger  },
    { "SO_OOBINLINE",               SOL_SOCKET,     SO_OOBINLINE,      sock_str_flag    },
    { "SO_RCVBUF",                  SOL_SOCKET,     SO_RCVBUF,         sock_str_int     },
    { "SO_SNDBUF",                  SOL_SOCKET,     SO_SNDBUF,         sock_str_int     },
    { "SO_RCVLOWAT",                SOL_SOCKET,     SO_RCVLOWAT,       sock_str_int     },
    { "SO_SNDLOWAT",                SOL_SOCKET,     SO_SNDLOWAT,       sock_str_int     },
    { "SO_RCVTIMEO",                SOL_SOCKET,     SO_RCVTIMEO,       sock_str_timeval },
    { "SO_SNDTIMEO",                SOL_SOCKET,     SO_SNDTIMEO,       sock_str_timeval },
    { "SO_REUSEADDR",               SOL_SOCKET,     SO_REUSEADDR,      sock_str_flag    },
#ifdef  SO_REUSEPORT
    { "SO_REUSEPORT",               SOL_SOCKET,     SO_REUSEPORT,      sock_str_flag    },
#else
    { "SO_REUSEPORT",               0,              0,                 NULL             },
#endif
    { "SO_TYPE",                    SOL_SOCKET,     SO_TYPE,           sock_str_int     },
  /*{ "SO_USELOOPBACK",             SOL_SOCKET,     SO_USELOOPBACK,    sock_str_flag    }, */
    { "IP_TOS",                     IPPROTO_IP,     IP_TOS,            sock_str_int     },
    { "IP_TTL",                     IPPROTO_IP,     IP_TTL,            sock_str_int     },
#ifdef  IPV6_DONTFRAG
    { "IPV6_DONTFRAG",              IPPROTO_IPV6,   IPV6_DONTFRAG,     sock_str_flag    },
#else
    { "IPV6_DONTFRAG",              0,              0,                 NULL             },
#endif
#ifdef  IPV6_UNICAST_HOPS
    { "IPV6_UNICAST_HOPS",          IPPROTO_IPV6,   IPV6_UNICAST_HOPS, sock_str_int     },
#else
    { "IPV6_UNICAST_HOPS",          0,              0,                 NULL             },
#endif
#ifdef  IPV6_V6ONLY
    { "IPV6_V6ONLY",                IPPROTO_IPV6,   IPV6_V6ONLY,       sock_str_flag    },
#else
    { "IPV6_V6ONLY",                0,              0,                 NULL             },
#endif
    { "TCP_MAXSEG",                 IPPROTO_TCP,    TCP_MAXSEG,        sock_str_int     },
    { "TCP_NODELAY",                IPPROTO_TCP,    TCP_NODELAY,       sock_str_flag    },
#ifdef  SCTP_AUTOCLOSE
    { "SCTP_AUTOCLOSE",             IPPROTO_SCTP,   SCTP_AUTOCLOSE,    sock_str_int     },
#else
    { "SCTP_AUTOCLOSE",             0,              0,                 NULL             },
#endif
#ifdef  SCTP_MAXBURST
    { "SCTP_MAXBURST",              IPPROTO_SCTP,   SCTP_MAXBURST,     sock_str_int     },
#else
    { "SCTP_MAXBURST",              0,              0,                 NULL             },
#endif
#ifdef  SCTP_MAXSEG
    { "SCTP_MAXSEG",                IPPROTO_SCTP,   SCTP_MAXSEG,       sock_str_int     },
#else
    { "SCTP_MAXSEG",                0,              0,                 NULL             },
#endif
#ifdef  SCTP_NODELAY
    { "SCTP_NODELAY",               IPPROTO_SCTP,   SCTP_NODELAY,      sock_str_flag    },
#else
    { "SCTP_NODELAY",               0,              0,                 NULL             },
#endif
    { NULL,                         0,              0,                 NULL             }
};

void PrintSocketOpts(int fd)
{
    socklen_t		 len;
    struct sock_opts	*ptr;
    for (ptr = sock_opts; ptr->opt_str != NULL; ptr++) {
        if (ptr->opt_val_str == NULL)
            H_DEBUG_MSG("%-20s: UNDEFINED", ptr->opt_str);
        else {
            len = sizeof(val);
            if (getsockopt(fd, ptr->opt_level, ptr->opt_name, &val, &len) == -1)
                H_DEBUG_MSG("%-20s: ######", ptr->opt_str);
            else
                H_DEBUG_MSG("%-20s: %s", ptr->opt_str, (*ptr->opt_val_str)(&val, len));
        }
    }
}
/* PrintSocketOpts end */
