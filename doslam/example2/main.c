#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <sys/mman.h>
#include <unistd.h>
#include "doslam.h"
#include <stdint.h>

// #define OURPUT_FIRE
// #define REPORT

uint8_t *doslam_image;
uint8_t *doslam_result;

char str[100];

int fd_ctr;
int fd_data;

doslam_ioctl_data user_data;

#define printfl(x,y) printf("%x,%x\n",x,y);
int matchMask(int x) {
    int result;

    switch (x) {
        case 0:
            result = 0x00e0; // 0000 0000 1110 0000
            break;
        case 7:
            result = 0x0070; // 0000 0000 0111 0000
            break;
        case 6:
            result = 0x0038; // 0000 0000 0011 1000
            break;
        case 5:
            result = 0x001c; // 0000 0000 0001 1100
            break;
        case 4:
            result = 0x000e; // 0000 0000 0000 1110
            break;
        case 3:
            result = 0x0007; // 0000 0000 0000 0111
            break;
        case 2:
            result = 0x8003; // 1000 0000 0000 0011
            break;
        case 1:
            result = 0xc001; // 1100 0000 0000 0001
            break;
        default:
            // Handle default case if needed
            result = 0; // Default value
            break;
    }

    return result;
}

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

void init_doslam(){
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
}

void init_user_data(doslam_ioctl_data *user_data){
    user_data->sizeInRow = 480;
    user_data->sizeInCol = 640;
    user_data->threshold = 30;
    user_data->topNum = 200;
    user_data->thresholdInit = -1;
    user_data->dmaImageReadAddr = 0;
    user_data->dmaImageWriteAddr = 0x4B400;
    user_data->dmaOrbWriteAddr = IMAGE_LENGTH;
}

void next_user_data(doslam_ioctl_data *user_data){
    user_data->sizeInRow = user_data->sizeOutRow;
    user_data->sizeInCol = user_data->sizeOutCol;

    uint32_t temp = user_data->dmaImageReadAddr;
    user_data->dmaImageReadAddr = user_data->dmaImageWriteAddr;
    user_data->dmaImageWriteAddr = temp;

    user_data->dmaOrbWriteAddr = user_data->dmaOrbWriteAddr + user_data->outputLength * 64;
}

void work(doslam_ioctl_data *user_data){
    int res = ioctl(fd_ctr, DOSLAM_IOCTL_CMD_STAR_WORK, user_data);

    int k = 0;
    while(ioctl(fd_ctr, DOSLAM_IOCTL_CMD_READ_STATE, user_data) != 1){
        k++;
        // usleep(1);
        if(k > 1000){
            printf("ka le\n");
            break;
        }
    }
    #ifdef OURPUT_FIRE
        sprintf(str, "/home/ubuntu/doslam/img/1_r%d_c%d.raw", user_data->sizeOutRow, (user_data->sizeOutCol+7)/8*8);
        saveToFile(str, (void *)(doslam_image + user_data->dmaImageWriteAddr), user_data->sizeOutRow * (((user_data->sizeOutCol+7)/8)*8));
        sprintf(str, "/home/ubuntu/doslam/img/1_r%d_c%d.raw.data", user_data->sizeInRow, user_data->sizeInCol);
        saveToFile(str, (void *)(doslam_result + user_data->dmaOrbWriteAddr - IMAGE_LENGTH), user_data->outputLength * 64);
    #endif

    #ifdef REPORT
        report(user_data);
    #endif
}

void report(doslam_ioctl_data *user_data){
    int res = ioctl(fd_ctr, 2222, &user_data);

    printf("res %d\n",res);

    printf("dmaImageReadReady %d\n", user_data->dmaImageReadReady);
    printf("dmaImageWriteReady %d\n",user_data->dmaImageWriteReady);
    printf("dmaOrbWriteReady %d\n",  user_data->dmaOrbWriteReady);
    printf("inputLength %d\n",       user_data->inputLength);
    printf("outputLength %d\n",      user_data->outputLength);
    printf("sizeOutRow %d\n",        user_data->sizeOutRow);
    printf("sizeOutCol %d\n",        user_data->sizeOutCol);
}

int main()
{

    init_doslam();

    int rows = 480;
    int cols = 640;

    readRawImage("/home/ubuntu/doslam/img/1.raw", doslam_image, rows*cols);

    int res;

    unsigned long start_time = get_time_us();
    //第一曾
    init_user_data(&user_data);
    work(&user_data);
    // report(&user_data);

    unsigned long end_time_0 = get_time_us();

    
    next_user_data(&user_data);
    work(&user_data);
    // report(&user_data);

    unsigned long end_time_1 = get_time_us();

    next_user_data(&user_data);
    work(&user_data);
    // report(&user_data);

    unsigned long end_time_2 = get_time_us();

    next_user_data(&user_data);
    work(&user_data);
    // report(&user_data);

    unsigned long end_time_3 = get_time_us();

    // 记录结束时间
    unsigned long end_time = get_time_us();

    // 计算时间差
    unsigned long elapsed_time = end_time - start_time;


    // 打印结果
    printf("Elapsed time: 1:%lu 2:%lu 3:%lu 4:%lu  total:%lu us\n", end_time_0 -  start_time, end_time_1 -  end_time_0, end_time_2 -  end_time_1, end_time_3 - end_time_2, elapsed_time);


    //saveToFile("/home/ubuntu/doslam/img/2.raw", (void *)(doslam_image + user_data.dmaImageWriteAddr), user_data.sizeOutRow * user_data.sizeOutCol);
    //saveToFile("/home/ubuntu/doslam/img/1.raw.data", (void *)(doslam_result), 64 * user_data.outputLength);


    return 0;
}
