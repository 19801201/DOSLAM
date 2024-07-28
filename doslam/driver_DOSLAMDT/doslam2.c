#include <linux/module.h>
#include <linux/kernel.h>
#include <linux/fs.h>
#include <linux/device.h>
#include <asm/io.h>
#include <linux/init.h>
#include <linux/platform_device.h>
#include <linux/miscdevice.h>
#include <linux/ioport.h>
#include <linux/of.h>
#include <linux/uaccess.h>
#include <linux/delay.h>
#include <linux/cdev.h>
#include <linux/mm.h>
#include <linux/dma-mapping.h>
#include "doslam.h"

static void __iomem *doslam_reg;

//定义设备号
static dev_t dev_num;
 
//定义字符设备对象
static struct cdev doslam_cdev;
 
//定义设备类指针
static struct class *cls;

struct resource *res;

//地址定义
char *dma_vaddr = NULL;
dma_addr_t dma_paddr;

#ifdef DEBUGAA

#define readlc(x) readl(doslam_reg + x);printk("read %lx\n", doslam_reg + x)
#define writelc(x, y) writel(y, doslam_reg + x);printk("write %x,%p\n",(int)y, (void *)(doslam_reg + x))

#else

#define readlc(x) readl(doslam_reg + x)
#define writelc(x, y) writel(y, doslam_reg + x)

#endif

static int doslam_drv_open(struct inode *Inode, struct file *File)
{
	return 0;
}
 
static ssize_t doslam_drv_read(struct file *file, char __user *buf, size_t count, loff_t *ppos)
{
	printk("doslam_drv_read Not supported, please use mmap!\n");
	return 0;
}
 
static ssize_t doslam_drv_write(struct file *file, const char __user *buf, size_t count, loff_t *ppos)
{
	printk("doslam_drv_write Not supported, please use mmap!\n");
    return 0;
}

static int doslam_drv_mmap(struct file *file, struct vm_area_struct *vma)
{

    //获取inode对象
    struct inode *inode = file->f_path.dentry->d_inode;
    //获取次设备号
    int minor = MINOR(inode->i_rdev);

    printk("cishebeihaois:%d",minor);
    printk("size:%lx\n",vma->vm_end - vma->vm_start);
    
    unsigned long offset = vma->vm_pgoff << PAGE_SHIFT;
    unsigned long phys = dma_paddr + offset;
    unsigned long vsize = vma->vm_end - vma->vm_start;
    unsigned long psize = BUF_SIZE - offset;

    vma->vm_page_prot = pgprot_noncached(vma->vm_page_prot);
    // if (vsize > psize) {
    //     return -EINVAL;
    // }

    return remap_pfn_range(vma, vma->vm_start, phys >> PAGE_SHIFT, vsize, vma->vm_page_prot);
    
    // ret = remap_pfn_range(vma, vma->vm_start, buf->phyaddr >> PAGE_SHIFT, vma->vm_end - vma->vm_start, vma->vm_page_prot);
    // if (ret) {
    //     printk("remap_pfn_range error %d\n",ret);
    // }
}

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

static long work_start(doslam_ioctl_data *user_data){

    uint32_t colNumFlilterIn = ((((user_data->sizeInCol + 7)/8)*8 + 9) / 10) - 1;
    uint32_t rowNumSrcIn = user_data->sizeInRow - 1;
    uint32_t colNumSrcIn = ((user_data->sizeInCol + 8) / 10) - 1;
    uint32_t maskR,maskG,maskF,inValid;

    user_data->sizeOutCol = (user_data->sizeInCol / 5) * 4;
    user_data->sizeOutRow = (user_data->sizeInRow / 5) * 4;

    if ((user_data->sizeInCol % 5) != 0)
        user_data->sizeOutCol += (user_data->sizeInCol % 5) - 1;
        
    if ((user_data->sizeInRow % 5) != 0)
        user_data->sizeOutRow += (user_data->sizeInRow % 5) - 1;

    if (user_data->sizeOutCol % 8 == 0) {
        maskR = 0x00;
    } else {
        maskR = (~((1 << (user_data->sizeOutCol % 8)) - 1)) & 0xff;
    }

    if (user_data->sizeInCol % 8 == 0) {
        inValid = 7;
        maskG = 0x00;
    } else {
        inValid = (user_data->sizeInCol % 8) - 1;
        maskG = (~((1 << (user_data->sizeInCol % 8)) - 1)) & 0xff;
    }

    maskF = matchMask(user_data->sizeInCol % 8);

    writelc(sizeInRow_W,        user_data->sizeInRow - 1);
    writelc(sizeInCol_W,        user_data->sizeInCol - 1);
    writelc(threshold_W,        user_data->threshold);
    writelc(MASKF_W,            maskF);
    writelc(MASKR_W,            maskR);
    writelc(MASKG_W,            maskG);
    writelc(COLNUMFLILERIN_W,   colNumFlilterIn);
    writelc(ROWNUMSRCIN_W,      rowNumSrcIn);
    writelc(COLNUMSRCIN_W,      colNumSrcIn);
    writelc(INVALID_W,          inValid);
    writelc(TOPNUM_W,           user_data->topNum);
    writelc(THRESHOLDINIT_W,    user_data->thresholdInit);

    writelc(DMAIMAGEREADADDR_W  ,user_data->dmaImageReadAddr + dma_paddr);
    writelc(DMAIMAGEREADLEN_W   ,user_data->sizeInRow * ((user_data->sizeInCol+7)/8) * 8);
    writelc(DMAIMAGEREADVALID_W ,START);
    //启动读数据
    writelc(START_W             ,START);
    //开始计算
    writelc(DMAIMAGEWRITEADDR_W ,user_data->dmaImageWriteAddr + dma_paddr);
    writelc(DMAIMAGEWRITELEN_W  ,user_data->sizeOutRow * ((user_data->sizeOutCol+7)/8) * 8);
    writelc(DMAIMAGEWRITEVALID_W,START);
    //搬运数据
    writelc(DMAORBREADADDR_W    ,user_data->dmaOrbWriteAddr + dma_paddr);
    writelc(DMAORBREADLEN_W     ,2048*64);
    writelc(DMAORBREADVALID_W   ,START);
    //搬运数据

    writelc(DMAIMAGEREADVALID_W ,0);
    writelc(DMAIMAGEWRITEVALID_W,0);
    writelc(DMAORBREADVALID_W   ,0);
    writelc(START_W             ,0);

    return 0;
}

static long read_state(doslam_ioctl_data *user_data){
    user_data->dmaImageReadReady  =  readlc(DMAIMAGEREADREADY_R ); 
    user_data->dmaImageWriteReady =  readlc(DMAIMAGEWRITEREADY_R);
    user_data->dmaOrbWriteReady   =  readlc(DMAORBREADREADY_R   );

    user_data->inputLength        =  readlc(INPUTLENGTH_R       );
    user_data->outputLength       =  readlc(OUTPUTLENGTH_R      );

    #ifdef DEBUG_PRINTF

        int a1 = readlc(0x006C);
        int a2 = readlc(0x0070);
        int a3 = readlc(0x0074);
        int a4 = readlc(0x0078);
        int a5 = readlc(0x007c);
        int a6 = readlc(0x0080);
        int a7 = readlc(0x0084);
        int a8 = readlc(0x0088);
        int a9 = readlc(0x008c);

        // printk("addr %x %d\n", 0x006C, a1);
        // printk("addr %x %d\n", 0x0070, a2);
        // printk("addr %x %d\n", 0x0074, a3);
        // printk("addr %x %d\n", 0x0078, a4);
        // printk("addr %x %d\n", 0x007c, a5);
        // printk("addr %x %d\n", 0x0080, a6);
        // printk("addr %x %d\n", 0x0084, a7);
        // printk("addr %x %d\n", 0x0088, a8);
        // printk("addr %x %d\n", 0x008c, a9);
        
        // a1 = readlc(0x0090);
        // a2 = readlc(0x0094);
        // a3 = readlc(0x0098);
        // a4 = readlc(0x009c);
        // a5 = readlc(0x00a0);
        // a6 = readlc(0x00a4);
        // a7 = readlc(0x00a8);
        // a8 = readlc(0x00ac);
        // a9 = readlc(0x00b0);

        // printk("addr %x %d\n", 0x0090, a1);
        // printk("addr %x %d\n", 0x0094, a2);
        // printk("addr %x %d\n", 0x0098, a3);
        // printk("addr %x %d\n", 0x009c, a4);
        // printk("addr %x %d\n", 0x00a0, a5);
        // printk("addr %x %d\n", 0x00a4, a6);
        // printk("addr %x %d\n", 0x00a8, a7);
        // printk("addr %x %d\n", 0x00ac, a8);
        // printk("addr %x %d\n", 0x00b0, a9);

        // a1 = readlc(0x00b4);
        // a2 = readlc(0x00b8);
        // a3 = readlc(0x00bc);
        // a4 = readlc(0x00c0);
        // a5 = readlc(0x00c4);
        // a6 = readlc(0x00c8);

        // printk("addr %x %d\n", 0x00b4, a1);
        // printk("addr %x %d\n", 0x00b8, a2);
        // printk("addr %x %d\n", 0x00bc, a3);
        // printk("addr %x %d\n", 0x00c0, a4);
        // printk("addr %x %d\n", 0x00c4, a5);
        // printk("addr %x %d\n", 0x00c8, a6);

        a1 = readlc(0x00cc);
        a2 = readlc(0x00d0);
        a3 = readlc(0x00d4);
        a4 = readlc(0x00d8);
        a5 = readlc(0x00dc);
        a6 = readlc(0x00e0);
        a7 = readlc(0x00e4);
        a8 = readlc(0x00e8);
        a9 = readlc(0x00ec);

        printk("addr %x %d\n", 0x00cc, a1);
        printk("addr %x %d\n", 0x00d0, a2);
        printk("addr %x %d\n", 0x00d4, a3);
        printk("addr %x %d\n", 0x00d8, a4);
        printk("addr %x %d\n", 0x00dc, a5);
        printk("addr %x %d\n", 0x00e0, a6);
        printk("addr %x %d\n", 0x00e4, a7);
        printk("addr %x %d\n", 0x00e8, a8);
        printk("addr %x %d\n", 0x00ec, a9);

    #endif

    return (long)(user_data->dmaImageReadReady && user_data->dmaImageWriteReady && user_data->dmaOrbWriteReady);
}

static long doslam_ioctl(struct file *file, unsigned int cmd, unsigned long arg)
{
    doslam_ioctl_data user_data;

    switch (cmd) {
        case DOSLAM_IOCTL_CMD_STAR_WORK:
            if (copy_from_user(&user_data, (void __user *)arg, sizeof(struct doslam_ioctl_data)))
                return -EFAULT;
            if(!read_state(&user_data)) {
                if (copy_to_user((void __user *)arg, &user_data, sizeof(struct doslam_ioctl_data)))
                    return -EFAULT;
                return 0;
            }
            work_start(&user_data);
            if (copy_to_user((void __user *)arg, &user_data, sizeof(struct doslam_ioctl_data)))
                return -EFAULT;
            return 1;

        case DOSLAM_IOCTL_CMD_READ_STATE:
            if (copy_from_user(&user_data, (void __user *)arg, sizeof(struct doslam_ioctl_data)))
                return -EFAULT;
            
            if(!read_state(&user_data)) {
                if (copy_to_user((void __user *)arg, &user_data, sizeof(struct doslam_ioctl_data)))
                    return -EFAULT;
                return 0;
            }
            if (copy_to_user((void __user *)arg, &user_data, sizeof(struct doslam_ioctl_data)))
                return -EFAULT;
            return 1;
        default:
            int a1 = readlc(0x00cc);
            int a2 = readlc(0x00d0);
            int a3 = readlc(0x00d4);
            int a4 = readlc(0x00d8);
            int a5 = readlc(0x00dc);
            int a6 = readlc(0x00e0);
            int a7 = readlc(0x00e4);
            int a8 = readlc(0x00e8);
            int a9 = readlc(0x00ec);

            printk("addr %x %s %d\n", 0x00cc, "inputIdleCnt.count       ",a1);
            printk("addr %x %s %d\n", 0x00d0, "inputWaitCnt.count       ",a2);
            printk("addr %x %s %d\n", 0x00d4, "inputNoFireCnt.count     ",a3);
            printk("addr %x %s %d\n", 0x00d8, "outputIdleCnt.count      ",a4);
            printk("addr %x %s %d\n", 0x00dc, "outputWaitCnt.count      ",a5);
            printk("addr %x %s %d\n", 0x00e0, "outputNoFireCnt.count    ",a6);
            printk("addr %x %s %d\n", 0x00e4, "fpIdleCnt.count          ",a7);
            printk("addr %x %s %d\n", 0x00e8, "fpWaitCnt.count          ",a8);
            printk("addr %x %s %d\n", 0x00ec, "fpNoFireCnt.count        ",a9);

            a1 = readlc(0x006C);
            a2 = readlc(0x0070);
            a3 = readlc(0x0074);
            a4 = readlc(0x0078);
            a5 = readlc(0x007c);
            a6 = readlc(0x0080);
            a7 = readlc(0x0084);
            a8 = readlc(0x0088);
            a9 = readlc(0x008c);

            printk("addr %x %s %d\n", 0x006C, "io.AXI_mm2s_image.r.count", a1);
            printk("addr %x %s %d\n", 0x0070, "io.AXI_s2mm_image.w,count", a2);
            printk("addr %x %s %d\n", 0x0074, "io.AXI_s2mm_orb.w.count  ", a3);
            printk("addr %x %s %d\n", 0x0078, "io.AXI_mm2s_image.r.valid", a4);
            printk("addr %x %s %d\n", 0x007c, "io.AXI_s2mm_image.w,valid", a5);
            printk("addr %x %s %d\n", 0x0080, "io.AXI_s2mm_orb.w.valid  ", a6);
            printk("addr %x %s %d\n", 0x0084, "io.AXI_mm2s_image.r.ready", a7);
            printk("addr %x %s %d\n", 0x0088, "io.AXI_s2mm_image.w.ready", a8);
            printk("addr %x %s %d\n", 0x008c, "io.AXI_s2mm_orb.w.ready  ", a9);
        
            a1 = readlc(0x0090);
            a2 = readlc(0x0094);
            a3 = readlc(0x0098);
            a4 = readlc(0x009c);
            a5 = readlc(0x00a0);
            a6 = readlc(0x00a4);
            a7 = readlc(0x00a8);
            a8 = readlc(0x00ac);
            a9 = readlc(0x00b0);

            printk("addr %x %s %d\n", 0x0090, "fast.io.mData.count           ",a1);
            printk("addr %x %s %d\n", 0x0094, "rsBrief.io.mDataRsBrief.count ",a2);
            printk("addr %x %s %d\n", 0x0098, "fpDrop.io.mData.count         ",a3);
            printk("addr %x %s %d\n", 0x009c, "fast.io.mData.valid           ",a4);
            printk("addr %x %s %d\n", 0x00a0, "rsBrief.io.mDataRsBrief.valid ",a5);
            printk("addr %x %s %d\n", 0x00a4, "fpDrop.io.mData.valid         ",a6);
            printk("addr %x %s %d\n", 0x00a8, "fast.io.mData.ready           ",a7);
            printk("addr %x %s %d\n", 0x00ac, "rsBrief.io.mDataRsBrief.ready ",a8);
            printk("addr %x %s %d\n", 0x00b0, "fpDrop.io.mData.ready         ",a9);

            a1 = readlc(0x00b4);
            a2 = readlc(0x00b8);
            a3 = readlc(0x00bc);
            a4 = readlc(0x00c0);
            a5 = readlc(0x00c4);
            a6 = readlc(0x00c8);

            printk("addr %x %s %d\n", 0x00b4, "fast.io.sData.count ", a1);
            printk("addr %x %s %d\n", 0x00b8, "score.io.sData.count", a2);
            printk("addr %x %s %d\n", 0x00bc, "nms.io.sData.count  ", a3);
            printk("addr %x %s %d\n", 0x00c0, "fast.io.sData.valid ", a4);
            printk("addr %x %s %d\n", 0x00c4, "score.io.sData.valid", a5);
            printk("addr %x %s %d\n", 0x00c8, "nms.io.sData.valid  ", a6);

            writelc(0x00f0, 0);
            writelc(0x00f0, 1);
            writelc(0x00f0, 0);
            return -ENOTTY;
    }

    return 0;
}

static struct file_operations doslam_fops =
{
	.owner = THIS_MODULE, 
	.open  = doslam_drv_open,
	.read  = doslam_drv_read, 
	.write = doslam_drv_write,
	.mmap  = doslam_drv_mmap,
    .unlocked_ioctl = doslam_ioctl,
};

static int doslam_drv_probe(struct platform_device *pdev)
{

    int ret;

    if (of_device_is_compatible(pdev->dev.of_node, "xlnx,Top-1.0")) {
        // 申请控制设备号
        ret = alloc_chrdev_region(&dev_num, 0, 2, DEVICE_NAME);
        if (ret < 0) {
            printk("Failed to allocate DOSLAM device region\n");
            return ret;
        }

        // 初始化控制字符设备
        cdev_init(&doslam_cdev, &doslam_fops);
        ret = cdev_add(&doslam_cdev, dev_num, 2);
        if (ret < 0) {
            unregister_chrdev_region(dev_num, 2);
            printk("Failed to add control char device\n");
            return ret;
        }

        // 获得内存映射
		res = platform_get_resource(pdev, IORESOURCE_MEM, 0);
		if (!res) {
			printk("Failed to get resource\n");
			cdev_del(&doslam_cdev);
			unregister_chrdev_region(dev_num, 2);
			return -ENODEV;
		}

        printk("Resource start: 0x%llx\n", (unsigned long long)res->start);
        printk("Resource end:   0x%llx\n", (unsigned long long)res->end);
        printk("Resource flags: 0x%lx\n", res->flags);
 
		// doslam_reg = devm_ioremap_resource(&pdev->dev, res);
        doslam_reg = ioremap(DOSLAM_BASE_ADDR, 0x1000);
		if (IS_ERR(doslam_reg)) {
			printk("Failed to remap IO resource\n");
			cdev_del(&doslam_cdev);
			unregister_chrdev_region(dev_num, 2);
			return PTR_ERR(doslam_reg);
		}
		//获得地址映射
		dma_vaddr = dma_alloc_coherent(&pdev->dev, BUF_SIZE, &dma_paddr, GFP_KERNEL);

        int i = 0;

        for(i = 0;i < 10;i++){
            dma_vaddr[i] = i;
        }
        
        for(i = 0;i < 10;i++){
            printk("dma_vaddr[%d] = %d\n", i, dma_vaddr[i]);
        }

		//Test
		printk("dmaOrbWriteReady:   %u\n", readl(doslam_reg + 0x60));
		printk("INPUTLENGTH:        %u\n", readl(doslam_reg + 0x34));
		printk("OUTPUTLENGTH:       %u\n", readl(doslam_reg + 0x38));
		printk("dmaImageReadReady:  %u\n", readl(doslam_reg + 0x40));
		printk("dmaImageWriteReady: %u\n", readl(doslam_reg + 0x50));

		printk("vaddr = 0x%lx, paddr = 0x%lx\n", dma_vaddr, (void *)dma_paddr);

		cls = class_create(THIS_MODULE, DEVICE_NAME);
		//创建设备文件
		device_create(cls, NULL, MKDEV(MAJOR(dev_num), DEVICE_CTR_NUM), NULL, DEVICE_CTR_NAME);
		device_create(cls, NULL, MKDEV(MAJOR(dev_num), DEVICE_DATA_NUM), NULL, DEVICE_DATA_NAME);

		printk("Device probed successfully\n");
    }

    printk("Device probed Done\n");
    return 0;
}

static int doslam_drv_remove(struct platform_device *pdev)
{
    // // 销毁设备文件
    device_destroy(cls, MKDEV(MAJOR(dev_num), DEVICE_CTR_NUM));
    device_destroy(cls, MKDEV(MAJOR(dev_num), DEVICE_DATA_NUM));

    // 清理类
    class_destroy(cls);

    // 解除内存映射
    iounmap(doslam_reg);

    // 释放DMA内存
    dma_free_coherent(&pdev->dev, BUF_SIZE, dma_vaddr, dma_paddr);

    // 删除字符设备
    cdev_del(&doslam_cdev);

    // 注销设备号
    unregister_chrdev_region(dev_num, 2);

    printk("Device removed successfully\n");
    return 0;
}


static const struct of_device_id my_device_ids[] = {
    { .compatible = "xlnx,Top-1.0" },
    {},
};

MODULE_DEVICE_TABLE(of, my_device_ids);

static struct platform_driver my_device_driver = {
    .driver = {
        .name = DEVICE_NAME,
        .of_match_table = my_device_ids,
    },
    .probe  = doslam_drv_probe,
    .remove = doslam_drv_remove,
};

module_platform_driver(my_device_driver);
 
MODULE_LICENSE("Dual BSD/GPL");
 