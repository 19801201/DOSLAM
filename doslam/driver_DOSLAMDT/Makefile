KVERS = $(shell uname -r)
MODENAME = doslam2
obj-m += $(MODENAME).o
#build:kernel_modules
 
#kernel_modules:
all:
	make -C /lib/modules/$(KVERS)/build M=$(CURDIR) modules
	rm *.order && rm *.symvers && rm *.mod.c && rm *.mod.o && rm *.o
clean:
	make -C /lib/modules/$(KVERS)/build M=$(CURDIR) clean
	rm *.order && rm *.symvers && rm *.ko && rm *.mod.c && rm *.mod.o && rm *.o