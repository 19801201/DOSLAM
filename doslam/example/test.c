#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#include "doslam.h"
#include <stdint.h>



uint8_t *doslam_image;
uint8_t *doslam_result;

int fd_ctr;
int fd_data;

doslam_ioctl_data user_data;

int main()
{
    // 打开设备文件
    fd_ctr = open(DEVICE_CTR_FILE, O_RDWR);
    if (fd_ctr < 0) {
        perror("open");
        return -1;
    }

    // 打开设备文件
    fd_data = open(DEVICE_DATA_FILE, O_RDWR);
    if (fd_data < 0) {
        perror("open");
        return -1;
    }

    doslam_image = (uint8_t *)mmap(NULL, IMAGE_LENGTH, PROT_READ | PROT_WRITE, MAP_SHARED, fd_data, 0);
    if (doslam_image == MAP_FAILED) {
        perror("mmap");
        close(fd_data);
        close(fd_ctr);
        return -1;
    }

    doslam_result = (uint8_t *)mmap(NULL, RSBRIEF_LENGTH, PROT_READ | PROT_WRITE, MAP_SHARED, fd_data, IMAGE_LENGTH);
    if (doslam_result == MAP_FAILED) {
        perror("mmap");
        close(fd_data);
        close(fd_ctr);
        return -1;
    }
    user_data.sizeInRow = 512;
    user_data.sizeInCol = 383;
    user_data.threshold = 20;
    user_data.topNum = 200;
    user_data.thresholdInit = -1;
    user_data.dmaImageReadAddr = 0;
    user_data.dmaImageWriteAddr = 0x4B400;
    user_data.dmaOrbWriteAddr = IMAGE_LENGTH;

    int res = ioctl(fd_ctr, DOSLAM_IOCTL_CMD_READ_STATE, &user_data);

    printf("res %d\n",res);

    printf("dmaImageReadReady %d\n", user_data.dmaImageReadReady);
    printf("dmaImageWriteReady %d\n",user_data.dmaImageWriteReady);
    printf("dmaOrbWriteReady %d\n",  user_data.dmaOrbWriteReady);
    printf("inputLength %d\n",       user_data.inputLength);
    printf("outputLength %d\n",      user_data.outputLength);
    printf("sizeOutRow %d\n",        user_data.sizeOutRow);
    printf("sizeOutCol %d\n",        user_data.sizeOutCol);
    
    res = ioctl(fd_ctr, DOSLAM_IOCTL_CMD_STAR_WORK, &user_data);

    printf("res %d\n",res);

    printf("dmaImageReadReady %d\n", user_data.dmaImageReadReady);
    printf("dmaImageWriteReady %d\n",user_data.dmaImageWriteReady);
    printf("dmaOrbWriteReady %d\n",  user_data.dmaOrbWriteReady);
    printf("inputLength %d\n",       user_data.inputLength);
    printf("outputLength %d\n",      user_data.outputLength);
    printf("sizeOutRow %d\n",        user_data.sizeOutRow);
    printf("sizeOutCol %d\n",        user_data.sizeOutCol);

    return 0;
}
