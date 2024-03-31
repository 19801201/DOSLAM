set addSubExit [lsearch -exact [get_ips fyMul] fyMul]
if { $addSubExit <0} {
create_ip -name c_addsub -vendor xilinx.com -library ip -version 12.0 -module_name fyMul
}
set_property -dict [list CONFIG.A_Width {14} CONFIG.A_Type {Unsigned} CONFIG.B_Width {11} CONFIG.B_Type {Unsigned} CONFIG.CE {false} CONFIG.Add_Mode {Add_Subtract} CONFIG.Implementation {Fabric} CONFIG.Out_Width {14} CONFIG.Latency {1} ] [get_ips fyMul] 

set tan2MulExit [lsearch -exact [get_ips tan2Mul] tan2Mul]
if { $tan2MulExit <0} {
create_ip -name dsp_macro -vendor xilinx.com -library ip -version 1.0 -module_name tan2Mul
}
set_property -dict [list CONFIG.Component_Name {tan2Mul} CONFIG.instruction1 {A*B-C} CONFIG.instruction2 {A*B+C} CONFIG.instruction3 {(D-A)*B-C} CONFIG.instruction4 {C-A*B} CONFIG.d_width {25} CONFIG.a_width {20} CONFIG.c_width {38} CONFIG.dreg_3 {true} CONFIG.areg_3 {true} CONFIG.areg_4 {true} CONFIG.breg_3 {true} CONFIG.breg_4 {true} CONFIG.creg_3 {true} CONFIG.creg_4 {true} CONFIG.creg_5 {true} CONFIG.opreg_3 {true} CONFIG.opreg_4 {true} CONFIG.opreg_5 {true} CONFIG.mreg_5 {true} CONFIG.preg_6 {true} CONFIG.d_binarywidth {0} CONFIG.a_binarywidth {0} CONFIG.b_width {18} CONFIG.b_binarywidth {0} CONFIG.concat_width {48} CONFIG.concat_binarywidth {0} CONFIG.c_binarywidth {0} CONFIG.pcin_binarywidth {0} CONFIG.p_full_width {44} CONFIG.p_width {44} CONFIG.p_binarywidth {0}] [get_ips tan2Mul]

set icAngleMulExit [lsearch -exact [get_ips icAngleMul] icAngleMul]
if{$addSubExit <0} {
    create_ip -name dsp_macro -vendor xilinx.com -library ip -version 1.0 -module_name icAngleMul
}
set_property -dict [list CONFIG.instruction1 {(D-A)*B} CONFIG.instruction2 {(D-A)*B+P} CONFIG.d_width {14} CONFIG.a_width {14} CONFIG.b_width {5} CONFIG.use_dsp48 {false} CONFIG.dreg_3 {true} CONFIG.areg_3 {true} CONFIG.areg_4 {true} CONFIG.breg_3 {true} CONFIG.breg_4 {true} CONFIG.creg_3 {false} CONFIG.creg_4 {false} CONFIG.creg_5 {false} CONFIG.opreg_3 {true} CONFIG.opreg_4 {true} CONFIG.opreg_5 {true} CONFIG.mreg_5 {true} CONFIG.preg_6 {true} CONFIG.d_binarywidth {0} CONFIG.a_binarywidth {0} CONFIG.b_binarywidth {0} CONFIG.concat_width {48} CONFIG.concat_binarywidth {0} CONFIG.c_width {48} CONFIG.c_binarywidth {0} CONFIG.pcin_binarywidth {0} CONFIG.p_full_width {48} CONFIG.p_width {48} CONFIG.p_binarywidth {0}] [get_ips icAngleMul]
