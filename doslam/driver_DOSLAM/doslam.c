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
 
 
//  定义设备文件名
#define DEVICE_NAME "DOSLAM"
 
#define DOSLAM_BASE_ADDR		(0x400400000l)
// 4MB
#define BUF_SIZE (4 * 1024 * 1024)

static void __iomem *doslam_reg;
 
 
 
static int doslam_drv_open(struct inode *Inode, struct file *File)
{
	return 0;
}
 
 
static ssize_t doslam_drv_read(struct file *file, char __user *buf, size_t count, loff_t *ppos)
{
	return 0;
}
 
static ssize_t doslam_drv_write(struct file *file, const char __user *buf, size_t count, loff_t *ppos)
{
	unsigned int ret = 0;
	unsigned int tmp_val;
	
	ret = copy_from_user(&tmp_val, buf, count);
	
	return ret;
}
 
//  描述与设备文件触发的事件对应的回调函数指针
static struct file_operations dev_fops =
{ 
	.owner = THIS_MODULE, 
	.open = doslam_drv_open,
	.read = doslam_drv_read, 
	.write = doslam_drv_write,
};
 
//  描述设备文件的信息   
static struct miscdevice misc =
{ 
	.minor = MISC_DYNAMIC_MINOR, 
	.name = DEVICE_NAME, 
	.fops = &dev_fops 
};
 
 
//  初始化Linux驱动
static int __init doslam_drv_init(void)
{
	int ret; 
 
	doslam_reg = ioremap(DOSLAM_BASE_ADDR, 0x1000);

	printk("Test: %p\n", doslam_reg);
	// //  建立设备文件
	ret = misc_register(&misc);
	//  输出日志信息

	if(ret)
	{
		printk("doslam_drv_init faiitrt!\n");
	}
	else
	{
		printk("doslam_drv_init success!\n");
	}
 
    printk("dmaOrbWriteReady:   %u\n", readl(doslam_reg + 0x60));
	printk("INPUTLENGTH:        %u\n", readl(doslam_reg + 0x34));
	printk("OUTPUTLENGTH:       %u\n", readl(doslam_reg + 0x38));
	printk("dmaImageReadReady:  %u\n", readl(doslam_reg + 0x40));
	printk("dmaImageWriteReady: %u\n", readl(doslam_reg + 0x50));

	

	return ret;
}
 
// 卸载Linux驱动
static void __exit doslam_drv_exit(void)
{
	iounmap(doslam_reg);
    dma_free_coherent(NULL, BUF_SIZE, src, &dma_src);
    dma_free_coherent(NULL, BUF_SIZE, dst, &dma_dst);
    misc_deregister(&misc);

	//  输出日志信息
	printk("doslam_drv_exit success!\n");
}
 
//  注册初始化Linux驱动的函数
module_init( doslam_drv_init);
//  注册卸载Linux驱动的函数
module_exit( doslam_drv_exit);
 
 
 
MODULE_LICENSE("Dual BSD/GPL");
 