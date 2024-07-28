下载ubuntu镜像

查看当前的
xmutil listapps
xmutil unloadapp

打开xsct
hsi open_hw_design C:/myData/data/vivado/orbslamAcc23/debugWorld2/DOSLAM.xsa
hsi set_repo_path C:/myData/ev/xilinx/device-tree-xlnx
hsi create_sw_design device-tree -os device_tree -proc psu_cortexa53_0
hsi set_property CONFIG.dt_overlay true [hsi::get_os]
hsi generate_target -dir C:/myData/data/vivado/orbslamAcc23/debugWorld2/
hsi close_hw_design design_1_wrapper


cp -r ./DOSLAM /lib/firmware/xilinx/
sudo xmutil listapps
sudo xmutil unloadapp
sudo dfx-mgr-client -load DOSLAM

cd ./driver_DOSLAMDT

make 

#显示信息
modinfo doslam2.ko
#挂载文件
sudo insmod doslam2.ko
# sudo rmmod doslam2.ko

sudo chmod +777 /dev/DOSLAM_data
sudo chmod +777 /dev/DOSLAM_ctr

sudo dmesg

# 生成 bit.bin
bit放置于文件夹下使用
echo 'all:{system.bit}'>bootgen.bif
bootgen -w -arch zynqmp -process_bitstream bin -image bootgen.bif