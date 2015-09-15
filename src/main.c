#include <stdio.h>
#include <scsi/sg.h>

int main(int argc, char** argv) {
    sg_io_hdr_t x;
    printf("%i\n", sizeof(x));
    printf("%p\n", &x.interface_id);
    printf("%p\n", &x.dxfer_direction);
    printf("%p\n", &x.cmd_len);
    printf("%p\n", &x.mx_sb_len);
    printf("%p\n", &x.iovec_count);
    printf("%p\n", &x.dxfer_len);
    printf("%p\n", &x.dxferp);
    printf("%p\n", &x.cmdp);
    printf("%p\n", &x.sbp);
    printf("%p\n", &x.timeout);
    printf("%p\n", &x.flags);
    printf("%p\n", &x.pack_id);
    printf("%p\n", &x.usr_ptr);
    printf("%p\n", &x.status);
    printf("%p\n", &x.masked_status);
    printf("%p\n", &x.msg_status);
    printf("%p\n", &x.sb_len_wr);
    printf("%p\n", &x.host_status);
    printf("%p\n", &x.driver_status);
    printf("%p\n", &x.resid);
    printf("%p\n", &x.duration);
    printf("%p\n", &x.info);
    return 0;
}
