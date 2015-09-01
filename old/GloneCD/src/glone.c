#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "sg_lib.h"
#include "sg_cmds.h"
#include "sg_io_linux.h"

int fd, forwards_fd, backwards_fd, forwards_sub_fd, backwards_sub_fd, backwards_msf_fd;
#define DEF_TIMEOUT 10000       /* 10,000 millisecs == 10 seconds */

int main( int argc, char* argv[] ) {

	int i, j;
	char *device;
	unsigned char	tur_cmd[] = {0x00, 0, 0, 0, 0, 0},
			read6_cmd[] = {0x8, 0, 0, 0, 1, 0},
			readcapacity_cdb[] = { 0x25, 0, 0, 0, 0, 0, 0, 0, 0, 0 },
			readcd12_cdb[]		= {0xBE, 0, 0, 0, 0, 0, 0, 0, 1, 0x00, 0x02, 0},
			readcdmsf12_cdb[]	= {0xB9, 0, 0, 0, 0, 0, 0, 0, 0, 0x00, 0x00, 0};
	unsigned char buf[4096];
	char *fname = malloc(strlen(argv[2])+100);

	printf( "Optical Disc Archiver 0.1\n" );
	printf( "Copyright (c) 2007  Ruud Koot\n\n" );

	device = argv[1];

	if ( opendevice( device ) ) {
		fprintf( stderr, "Could not open %s\n", device );
		exit( 1 );
	}

	scsi_command( tur_cmd, 6, 0, 0, 0 );

	memset( buf, 0, sizeof(buf) );

//	scsi_command( identify_cmd, 6, buf, 2048, 0 );

	scsi_command( readcapacity_cdb, 10, buf, 8, 0 );
	printf( "Media has %i blocks of %i bytes.\n", getint(buf+0), getint(buf+4) );

	strcpy(fname, argv[2]);
	strcat(fname, "/forwards.iso");	
	forwards_fd = open( fname, O_RDWR | O_CREAT | S_IRWXU | O_TRUNC );
	strcpy(fname, argv[2]);
	strcat(fname, "/forwards.sub");	
	forwards_sub_fd = open( fname, O_RDWR | O_CREAT | S_IRWXU | O_TRUNC );
	for ( i = 0; i < 8192; ++i ) {
		printf( "Reading (forwards) LBA %i (%.1f MiB)\r", i, (i * 2)/1024.0);
		fflush(stdout);
		readcd12_cdb[5] = i & 0xFF;
		readcd12_cdb[4] = (i >> 8) & 0xFF;
		scsi_command( readcd12_cdb, 12, buf, 4096, 0 );
		write(forwards_fd, buf, 4096);
		write(forwards_sub_fd, buf, 24);
	}
	close( forwards_fd );
	close( forwards_sub_fd );
	printf( "\n" );

	strcpy(fname, argv[2]);
	strcat(fname, "/backwards.iso");	
	backwards_fd = open( fname, O_RDWR | O_CREAT | S_IRWXU | O_TRUNC );
	strcpy(fname, argv[2]);
	strcat(fname, "/backwards.sub");	
	backwards_sub_fd = open( fname, O_RDWR | O_CREAT | S_IRWXU | O_TRUNC );

	for ( i = 8192-64; i >= 0; i-=64 ) {
		for ( j = 0; j < 64; ++j ) {
			printf( "Reading (backwards) LBA %i (%.1f MiB)\r", i+j, ((i+j) * 2)/1024.0);
			fflush(stdout);
			readcd12_cdb[5] = (i+j) & 0xFF;
			readcd12_cdb[4] = ((i+j) >> 8) & 0xFF;
			scsi_command( readcd12_cdb, 12, buf, 2354, 0 );
			lseek(backwards_fd, (i+j)*2048, SEEK_SET);
			write(backwards_fd, buf+24, 2048);
		}
	}
	close( backwards_fd );
	close( backwards_sub_fd );
	printf( "\n" );

	int m, s, f;
	strcpy(fname, argv[2]);
	strcat(fname, "/backwards.msf");	
	backwards_msf_fd = open( fname, O_RDWR | O_CREAT | S_IRWXU | O_TRUNC );
	for ( m = 2; m >= 2; --m ) {
		for ( s = 59; s >= 0; --s ) {
			for ( f = 0; f < 75; f++ ) {
				printf( "Reading (backwards) MSF %i:%i:%i      \r", m, s, f);
//				fflush(stdout);
				readcdmsf12_cdb[3] = readcdmsf12_cdb[6] = bcd(m);
				readcdmsf12_cdb[4] = readcdmsf12_cdb[7] = bcd(s);
				readcdmsf12_cdb[5] = readcdmsf12_cdb[8] = bcd(f);
				memset(buf,0,4096);
				scsi_command( readcdmsf12_cdb, 12, buf, 2354, 0 );
				lseek( backwards_msf_fd, 2048*((m-2)*75*60+s*75+f), SEEK_SET );
				write( backwards_msf_fd, buf+24, 2048);
			}
		}
	}
	close( backwards_msf_fd );
	printf( "\n" );

	free( fname );

	return 0;
}

int getint( unsigned char *buf ) {
	return ( buf[3] + buf[2]*256 + buf[1]*65536 + buf[0]*16777216 );
}

int bcd( int n ) {
	return ((n%10) + (n/10)*16);
}

int opendevice( char* device ) {

	fd = open( device, O_RDWR | O_NONBLOCK );

	return 0;
}

int scsi_command( unsigned char *cmd, unsigned char *buf, size_t read, size_t write ) {

	unsigned char sense_b[32];
	struct sg_io_hdr io_hdr;

	memset(&io_hdr, 0, sizeof(struct sg_io_hdr));
	io_hdr.interface_id = 'S';	// SG (Generic SCSI)
	io_hdr.cmd_len = sg_get_command_size(cmd[0]);
	io_hdr.dxfer_direction = read > 0 ? SG_DXFER_FROM_DEV : (write > 0 ? SG_DXFER_TO_DEV : SG_DXFER_NONE );
	io_hdr.dxfer_len = read > 0 ? read : write;
	io_hdr.dxferp = buf;
	io_hdr.cmdp = cmd;
	io_hdr.sbp = sense_b;
	io_hdr.mx_sb_len = sense_len;
	io_hdr.timeout = DEF_TIMEOUT;

	if (ioctl(fd, SG_IO, &io_hdr) < 0) {
		printf("ioctl<0\n");
	}
}
