declare i8* @malloc(i32)
define i32 @main() {
main__entry:
  %pppppppppppppppppa_1 = bitcast i32 1 to i32
  %pppppppppppppppppb_2 = bitcast i32 2 to i32
  %pppppppppppppppppd_5 = add i32 %pppppppppppppppppa_1, %pppppppppppppppppb_2
  %pppppppppppppppppc_8 = add i32 %pppppppppppppppppa_1, %pppppppppppppppppb_2
  %ppppppppppppppppptemp5_9 = bitcast i32 %pppppppppppppppppc_8 to i32
  ret i32 %ppppppppppppppppptemp5_9
}
