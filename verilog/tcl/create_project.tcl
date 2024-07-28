# 设置项目名称和路径
set project_name "DOSLAM"
set project_dir "C:/myData/data/vivado/doslam"
set src_file "C:/myData/data/spinalHDL/orb-slam-acc/Top.v"
set ip_script "C:/myData/data/spinalHDL/orb-slam-acc/verilog/tcl/generateIP.tcl"
set design_block_script "C:/myData/data/spinalHDL/orb-slam-acc/verilog/tcl/design_1.tcl"
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
