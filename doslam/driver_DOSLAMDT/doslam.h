//data
#define START_W             0x0         
#define sizeInRow_W         0x4
#define sizeInCol_W         0x8
#define threshold_W         0xc
#define MASKF_W             0x10
#define MASKR_W             0x14
#define MASKG_W             0x18
#define COLNUMFLILERIN_W    0x1c
#define ROWNUMSRCIN_W       0x20
#define COLNUMSRCIN_W       0x24
#define INVALID_W           0x28
#define TOPNUM_W            0x2c
#define THRESHOLDINIT_W     0x30
#define INPUTLENGTH_R       0x34
#define OUTPUTLENGTH_R      0x38
//DMA image read
#define DMAIMAGEBASEADDR  0x3c

#define DMAIMAGEREADVALID_W (DMAIMAGEBASEADDR + 0x0)
#define DMAIMAGEREADREADY_R (DMAIMAGEBASEADDR + 0x4)
#define DMAIMAGEREADADDR_W  (DMAIMAGEBASEADDR + 0x8)
#define DMAIMAGEREADLEN_W   (DMAIMAGEBASEADDR + 0xc)
//DMA image write
#define DMAIMAGEWRITEVALID_W (DMAIMAGEBASEADDR + 0x10)
#define DMAIMAGEWRITEREADY_R (DMAIMAGEBASEADDR + 0x14)
#define DMAIMAGEWRITEADDR_W  (DMAIMAGEBASEADDR + 0x18)
#define DMAIMAGEWRITELEN_W   (DMAIMAGEBASEADDR + 0x1c)
//DMA image read
#define DMAORBBASEADDR  0x5c
#define DMAORBREADVALID_W (DMAORBBASEADDR + 0x0)
#define DMAORBREADREADY_R (DMAORBBASEADDR + 0x4)
#define DMAORBREADADDR_W  (DMAORBBASEADDR + 0x8)
#define DMAORBREADLEN_W   (DMAORBBASEADDR + 0xc)

//ctr
#define DEVICE_NAME "DOSLAM2"
#define DEVICE_CTR_NAME "DOSLAM_ctr"
#define DEVICE_DATA_NAME "DOSLAM_data"

#define DEVICE_CTR_FILE  "/dev/DOSLAM_ctr"
#define DEVICE_DATA_FILE "/dev/DOSLAM_data"

#define DEVICE_CTR_NUM 0
#define DEVICE_DATA_NUM 1

#define DOSLAM_BASE_ADDR	(0x400400000l)
// 4MB
#define BUF_SIZE (1024 * 1024)
#define REG_LENGTH 0x1000
#define IMAGE_LENGTH (896 * 1024)
#define RSBRIEF_LENGTH (128 * 1024)

#define DOSLAM_IOCTL_CMD_STAR_WORK 0x0000
#define DOSLAM_IOCTL_CMD_READ_STATE 0xffff
#define START 0x1

// #define DEBUG_PRINTF
// #define DEBUGAA

typedef struct doslam_ioctl_data {
    //配置参数
    uint sizeInRow;
    uint sizeInCol;
    uint threshold;
    uint topNum;
    uint thresholdInit;
    //DMA启动地址1
    uint dmaImageReadAddr;
    uint dmaImageReadLen;
    //DMA启动地址2
    uint dmaImageWriteAddr;
    //DMA启动地址3
    uint dmaOrbWriteAddr;
    //DMA启动状态
    uint dmaImageReadReady;
    uint dmaImageWriteReady;
    uint dmaOrbWriteReady;
    //返回的orb数量和被筛选的数量
    uint inputLength;
    uint outputLength;
    //返回的长宽
    uint sizeOutRow;
    uint sizeOutCol;
} doslam_ioctl_data;
