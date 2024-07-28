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

unsigned long get_time_us() {
    struct timeval tv;
    gettimeofday(&tv, NULL);
    return tv.tv_sec * 1000000 + tv.tv_usec;
}

void readRawImage(const char* filename, unsigned char* buffer, int size) {
    int fd = open(filename, O_RDONLY);
    if (fd < 0) {
        printf("Error opening file\n");
        return;
    }

    if (read(fd, buffer, size) != size) {
        printf("Error reading file\n");
        close(fd);
        return;
    }

    close(fd);
}

void saveToFile(const char* filename, const void* buf, size_t size) {
    int fd = open(filename, O_RDWR | O_CREAT | O_TRUNC, 0644);  // 以二进制写入模式打开文件
    if (fd == NULL) {
        perror("Error opening file");
        return;
    }

    ssize_t bytes_written = write(fd, buf, size);
    if (bytes_written != size) {
        perror("Error writing to file");
        close(fd);
        return;
    }

    close(fd);
    printf("Data saved to file successfully\n");
}




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

    int rows = 480;
    int cols = 640;

    readRawImage("/home/ubuntu/doslam/img/1.raw", doslam_image, rows*cols);

    unsigned long start_time = get_time_us();

    user_data.sizeInRow = 480;
    user_data.sizeInCol = 640;
    user_data.threshold = 20;
    user_data.topNum = 200;
    user_data.thresholdInit = -1;
    user_data.dmaImageReadAddr = 0;
    user_data.dmaImageWriteAddr = 0x4B400;
    user_data.dmaOrbWriteAddr = IMAGE_LENGTH;
    int res;

    // int res = ioctl(fd_ctr, DOSLAM_IOCTL_CMD_READ_STATE, &user_data);

    // printf("res %d\n",res);

    // printf("dmaImageReadReady %d\n", user_data.dmaImageReadReady);
    // printf("dmaImageWriteReady %d\n",user_data.dmaImageWriteReady);
    // printf("dmaOrbWriteReady %d\n",  user_data.dmaOrbWriteReady);
    // printf("inputLength %d\n",       user_data.inputLength);
    // printf("outputLength %d\n",      user_data.outputLength);
    // printf("sizeOutRow %d\n",        user_data.sizeOutRow);
    // printf("sizeOutCol %d\n",        user_data.sizeOutCol);
    
    res = ioctl(fd_ctr, DOSLAM_IOCTL_CMD_STAR_WORK, &user_data);

    // printf("res %d\n",res);

    // printf("dmaImageReadReady %d\n", user_data.dmaImageReadReady);
    // printf("dmaImageWriteReady %d\n",user_data.dmaImageWriteReady);
    // printf("dmaOrbWriteReady %d\n",  user_data.dmaOrbWriteReady);
    // printf("inputLength %d\n",       user_data.inputLength);
    // printf("outputLength %d\n",      user_data.outputLength);
    // printf("sizeOutRow %d\n",        user_data.sizeOutRow);
    // printf("sizeOutCol %d\n",        user_data.sizeOutCol);


    int k = 0;
    while(ioctl(fd_ctr, DOSLAM_IOCTL_CMD_READ_STATE, &user_data) != 1){
        k++;
        // usleep(1);
        if(k > 1000){
            printf("ka le\n");
            break;
        }
    }

            // 记录结束时间
    unsigned long end_time = get_time_us();

    // 计算时间差
    unsigned long elapsed_time = end_time - start_time;

    printf("k:%d\n",k);

    // 打印结果
    printf("Elapsed time: %lu us\n", elapsed_time);

    // sleep(1);
    res = ioctl(fd_ctr, 2222, &user_data);

    printf("res %d\n",res);

    printf("dmaImageReadReady %d\n", user_data.dmaImageReadReady);
    printf("dmaImageWriteReady %d\n",user_data.dmaImageWriteReady);
    printf("dmaOrbWriteReady %d\n",  user_data.dmaOrbWriteReady);
    printf("inputLength %d\n",       user_data.inputLength);
    printf("outputLength %d\n",      user_data.outputLength);
    printf("sizeOutRow %d\n",        user_data.sizeOutRow);
    printf("sizeOutCol %d\n",        user_data.sizeOutCol);

    //saveToFile("/home/ubuntu/doslam/img/2.raw", (void *)(doslam_image + user_data.dmaImageWriteAddr), user_data.sizeOutRow * user_data.sizeOutCol);
    //saveToFile("/home/ubuntu/doslam/img/2.raw", (void *)(doslam_image + user_data.dmaImageWriteAddr), 480 * 640);
    //saveToFile("/home/ubuntu/doslam/img/2.raw", (void *)(doslam_image + user_data.dmaImageWriteAddr), user_data.sizeOutRow * user_data.sizeOutCol);
    
    //saveToFile("/home/ubuntu/doslam/img/1.raw.data", (void *)(doslam_result), 64 * user_data.outputLength);

    // for(int i = 0;i < 100;i++){
    //     printf("doslam result:%i %x\n", i,*(int *)(doslam_result + i * 4));
    // }

    return 0;
}
