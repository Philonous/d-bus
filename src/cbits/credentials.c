#include <stdlib.h>
#include <sys/types.h>
#include <sys/un.h>
#include <sys/socket.h>
#include <string.h>
#include <stdio.h>

int send_credentials_and_zero (int s) {
    char buf[1] = { '\0' };
    union {
        struct cmsghdr hdr;
        char cred[CMSG_SPACE (sizeof (struct cmsgcred))];
    } cmsg;
    struct iovec iov;
    struct msghdr msg;
    iov.iov_base = buf;
    iov.iov_len = 1;

    memset(&msg, 0, sizeof(msg));
    msg.msg_iov = &iov;
    msg.msg_iovlen = 1;

    msg.msg_control = (caddr_t) &cmsg;
    msg.msg_controllen = CMSG_SPACE (sizeof (struct cmsgcred));

    cmsg.hdr.cmsg_len = CMSG_LEN (sizeof (struct cmsgcred));
    cmsg.hdr.cmsg_level = SOL_SOCKET;
    cmsg.hdr.cmsg_type = SCM_CREDS;
    int sent = sendmsg (s, &msg, 0 );
    return sent;
}
