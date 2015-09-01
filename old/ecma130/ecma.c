#define SECTOR_SIZE     2352
#define SYNC_DATA       {0x00,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,0x00}

typedef union {
        unsigned char   data[SECTOR_SIZE];
        
        struct {
                unsigned char sync[12];
                unsigned char address[3];
                unsigned char mode;
                unsigned char zero[2336]
        } mode0;
        
        struct {
                unsigned char sync[12];
                unsigned char address[3];
                unsigned char mode;
                unsigned char data[2048];
                unsigned char edc[4];
                unsigned char intermediate[8];
                unsigned char p_parity[172];
                unsigned char q_parity[104];
        } mode1;
        
        struct {
                unsigned char sync[12];
                unsigned char address[3];
                unsigned char mode;
                unsigned char data[2336]
        } mode2;
        
        struct {
                unsigned char sync[12];
                unsigned char data[2340];
        } scrambled;
        
} sector_t;

unsigned short  sec_ix[SECTOR_SIZE];

int main( int argc, char* argv[] ) {

        return 0;
        
}

void check_sync( sector_t* sector ) {
        if ( !memcmp( sector->mode0.sync, SYNC_DATA, 12 ) ) {
                printf( "%s", "invalid sync pattern" );
        }
}

void check_address( sector_t* sector ) {
        printf( "%i:%i:%i", sector->mode0.address[0],
                sector->mode0.address[1], sector->mode0.address[2] );
        }
}

void check_mode( sector_t* sector ) {
        switch ( sector->mode0.mode ) {
        case 0:
        case 1:
        case 2:
                break;
        default:
                printf( "%s", "invalid mode" );
        }
}

void check_edc( sector_t* sector ) {
}

/* Annex A */
//check_reed_solomon

/* Sector scrambling (ECMA-130: Annex B) */
void sector_scramble( sector_t* sector ) {
}

void sector_unscramble( sector_t* sector ) {
}

/* F_1-frames */
void sector_f1frame( sector_t* sector ) {
}

void sector_f1unframe( sector_t* sector ) {
}

/* CIRC encodining / F_2-Frames (ECMA-130: Annex C) */
void sector_circ_encode( sector_t* in, circ_sector_t* out ) {
}

void sector_circ_decode( circ_sector_t* in, sector_t* out ) {
}

/* Control Bytes / F_3-Frames / Sections (ECMA-130: Clause 22) */

/* Recording F_3_Frames / Channel Frame
        8-to-14 Encoding (Annex D)
        Sync Header
        Merging Channel bits (Annex E)
 */
 
/* NIZR */
