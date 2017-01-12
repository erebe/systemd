#include <sys/socket.h>
#include <string.h>

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


int sd_notify_with_fd(int sock
                , char *str, size_t len // message to send
                , struct sockaddr *dest, socklen_t lenaddr // for who
                , int outfd // The file descriptor to send along
                )
{
  struct msghdr msg = {0};

  // Attach the message
  struct iovec iov[1];
  iov[0].iov_base = str;
  iov[0].iov_len  = len;
  msg.msg_iov = iov;
  msg.msg_iovlen = sizeof(iov) / sizeof(iov[0]);

  // Write to who to send
  msg.msg_name = dest;
  msg.msg_namelen = lenaddr;

  // Attach extra header with the file descriptor in it
  char ancBuffer[CMSG_SPACE(sizeof(outfd))];
  msg.msg_control = ancBuffer;
  msg.msg_controllen = sizeof(ancBuffer);

  struct cmsghdr *cmsg = CMSG_FIRSTHDR(&msg);
  cmsg->cmsg_level = SOL_SOCKET;
  cmsg->cmsg_type = SCM_RIGHTS;
  cmsg->cmsg_len = CMSG_LEN(sizeof(outfd));
  char *dPtr = (char*)CMSG_DATA(cmsg);

  *(int*)dPtr = outfd;
  msg.msg_controllen = cmsg->cmsg_len;

  return sendmsg(sock, &msg, 0);
}

