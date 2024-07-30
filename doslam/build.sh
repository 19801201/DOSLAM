sudo cp -r ./DOSLAM /lib/firmware/xilinx/
sudo xmutil listapps
sudo xmutil unloadapp
sudo dfx-mgr-client -load DOSLAM

cd ./driver_DOSLAMDT

make 

#显示信息
# modinfo doslam2.ko
#挂载文件
# sudo rmmod doslam2.ko
sudo insmod doslam2.ko

sudo chmod +777 /dev/DOSLAM_data
sudo chmod +777 /dev/DOSLAM_ctr

# sudo dmesg