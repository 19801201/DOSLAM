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
#define DEVICE_NAME "leds"
 
#define LEDS_BASE_ADDR		(0x41200000)
 
 
typedef struct{
	volatile unsigned int ODR;
}LEDS_T;
 
LEDS_T* leds;
 
 
 
static int leds_drv_open(struct inode *Inode, struct file *File)
{
	leds->ODR = 0xf;
	return 0;
}
 
 
static ssize_t leds_drv_read(struct file *file, char __user *buf, size_t count, loff_t *ppos)
{
	return 0;
}
 
static ssize_t leds_drv_write(struct file *file, const char __user *buf, size_t count, loff_t *ppos)
{
	unsigned int ret = 0;
	unsigned int tmp_val;
	
	ret = copy_from_user(&tmp_val, buf, count);
	
	leds->ODR = (~tmp_val) & 0xf;
	
	return ret;
}
 
//  描述与设备文件触发的事件对应的回调函数指针
static struct file_operations dev_fops =
{ 
	.owner = THIS_MODULE, 
	.open = leds_drv_open,
	.read = leds_drv_read, 
	.write = leds_drv_write,
};
 
//  描述设备文件的信息   
static struct miscdevice misc =
{ 
	.minor = MISC_DYNAMIC_MINOR, 
	.name = DEVICE_NAME, 
	.fops = &dev_fops 
};
 
 
//  初始化Linux驱动
static int __init leds_drv_init(void)
{
	int ret; 
 
	//leds = ioremap(LEDS_BASE_ADDR, sizeof(LEDS_T));
 
	//  建立设备文件
	ret = misc_register(&misc);
	//  输出日志信息
	if(ret)
	{
		printk("leds_drv_init faiitrt!\n");
	}
	else
	{
		printk("leds_drv_init success!\n");
	}
 
 
	return ret;
}
 
// 卸载Linux驱动
static void __exit leds_drv_exit(void)
{
	//iounmap(leds);
 
	//  删除设备文件  
	misc_deregister(&misc);
 
	//  输出日志信息
	printk("leds_drv_exit success!\n");
} 
 
//  注册初始化Linux驱动的函数
module_init( leds_drv_init);
//  注册卸载Linux驱动的函数
module_exit( leds_drv_exit);
 
 
 
MODULE_LICENSE("Dual BSD/GPL");
 