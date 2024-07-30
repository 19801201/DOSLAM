# 设置项目名称和路径
set project_name "DOSLAM"

set project_dir "C:/myData/data/vivado/doslam"
set doslam_dir "C:/myData/data/spinalHDL/orb-slam-acc"


set src_file "${doslam_dir}/Top.v"
set ip_script "${doslam_dir}/verilog/tcl/generateIP.tcl"
set design_block_script "${doslam_dir}/verilog/tcl/design_1.tcl"

# 创建项目并设置顶层模块
create_project $project_name $project_dir

set_property board_part xilinx.com:kr260_som:part0:1.1 [current_project]
set_property board_connections {som240_1_connector xilinx.com:kv260_carrier:som240_1_connector:1.3 som240_2_connector xilinx.com:kv260_carrier:som240_1_connector:1.3} [get_projects DOSLAM]
# 添加 Verilog 源文件
add_files -fileset sources_1 $src_file

# 生成 IP 核
source $ip_script
source $design_block_script

set_property -dict [list \
  CONFIG.output_properties {User_Defined} \
  CONFIG.p_binarywidth {44} \
  CONFIG.p_width {44} \
] [get_ips tan2Mul]

update_compile_order -fileset sources_1

# 生成bd
generate_target all [get_files "${project_dir}/DOSLAM.srcs/sources_1/bd/design_1/design_1.bd"]

catch { config_ip_cache -export [get_ips -all design_1_auto_ds_0] }

catch { config_ip_cache -export [get_ips -all design_1_auto_pc_0] }

catch { config_ip_cache -export [get_ips -all design_1_zynq_ultra_ps_e_0_0] }

catch { config_ip_cache -export [get_ips -all design_1_proc_sys_reset_0_0] }

catch { config_ip_cache -export [get_ips -all design_1_smartconnect_0_0] }

catch { config_ip_cache -export [get_ips -all design_1_smartconnect_1_0] }

export_ip_user_files -of_objects [get_files "${project_dir}/DOSLAM.srcs/sources_1/bd/design_1/design_1.bd"] -no_script -sync -force -quiet

//生成顶层

make_wrapper -files [get_files "${project_dir}/DOSLAM.srcs/sources_1/bd/design_1/design_1.bd"] -top
add_files -norecurse "${project_dir}/DOSLAM.gen/sources_1/bd/design_1/hdl/design_1_wrapper.v"

update_compile_order -fileset sources_1

set_property top design_1_wrapper [current_fileset]

update_compile_order -fileset sources_1

# 综合

set_property STEPS.WRITE_BITSTREAM.ARGS.BIN_FILE true [get_runs impl_1]