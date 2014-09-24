#include <sys/socket.h>

int socket_family(int fd) {
    struct sockaddr sockaddr = {};
    socklen_t len = sizeof(sockaddr);
 
    if (getsockname(fd, &sockaddr, &len) < 0)
        return -1;
 
    if (len < sizeof(sa_family_t))
        return -1;
 
    return sockaddr.sa_family;
}

int socket_type(int fd) {
    int type = 0;
    socklen_t len = sizeof(type);
 
    if (getsockopt(fd, SOL_SOCKET, SO_TYPE, &type, &len) < 0)
        return -1;
 
    return type;
}

int socket_listening(int fd) {
    int accepting = 0;
    socklen_t len = sizeof(accepting);
 
    if (getsockopt(fd, SOL_SOCKET, SO_ACCEPTCONN, &accepting, &len) < 0)
        return -1;

    return accepting;
}
