EESchema Schematic File Version 4
LIBS:main-cache
EELAYER 26 0
EELAYER END
$Descr A4 11693 8268
encoding utf-8
Sheet 1 1
Title ""
Date ""
Rev ""
Comp ""
Comment1 ""
Comment2 ""
Comment3 ""
Comment4 ""
$EndDescr
$Comp
L Regulator_Switching:R-78E3.3-1.0 U1
U 1 1 5C8BA93F
P 2050 850
F 0 "U1" H 2050 1092 50  0000 C CNN
F 1 "3.3v regul" H 2050 1001 50  0000 C CNN
F 2 "Package_TO_SOT_SMD:TO-252-3_TabPin2" H 2100 600 50  0001 L CIN
F 3 "https://www.recom-power.com/pdf/Innoline/R-78Exx-1.0.pdf" H 2050 850 50  0001 C CNN
	1    2050 850 
	1    0    0    -1  
$EndComp
$Comp
L power:+5V #PWR0103
U 1 1 5C8BAA33
P 1650 750
F 0 "#PWR0103" H 1650 600 50  0001 C CNN
F 1 "+5V" H 1665 923 50  0000 C CNN
F 2 "" H 1650 750 50  0001 C CNN
F 3 "" H 1650 750 50  0001 C CNN
	1    1650 750 
	1    0    0    -1  
$EndComp
$Comp
L Device:C C1
U 1 1 5C8BAA73
P 1650 1100
F 0 "C1" H 1450 1200 50  0000 L CNN
F 1 "10uF" H 1350 1050 50  0000 L CNN
F 2 "Capacitor_SMD:C_1206_3216Metric_Pad1.42x1.75mm_HandSolder" H 1688 950 50  0001 C CNN
F 3 "~" H 1650 1100 50  0001 C CNN
	1    1650 1100
	1    0    0    -1  
$EndComp
$Comp
L Device:C C2
U 1 1 5C8BABDB
P 2450 1100
F 0 "C2" H 2565 1146 50  0000 L CNN
F 1 "10uF" H 2565 1055 50  0000 L CNN
F 2 "Capacitor_SMD:C_1206_3216Metric_Pad1.42x1.75mm_HandSolder" H 2488 950 50  0001 C CNN
F 3 "~" H 2450 1100 50  0001 C CNN
	1    2450 1100
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0104
U 1 1 5C8BAC61
P 2050 1450
F 0 "#PWR0104" H 2050 1200 50  0001 C CNN
F 1 "GND" H 2055 1277 50  0000 C CNN
F 2 "" H 2050 1450 50  0001 C CNN
F 3 "" H 2050 1450 50  0001 C CNN
	1    2050 1450
	1    0    0    -1  
$EndComp
Wire Wire Line
	2050 1450 2050 1350
Wire Wire Line
	2050 1350 2450 1350
Wire Wire Line
	2450 1350 2450 1250
Wire Wire Line
	2050 1150 2050 1350
Connection ~ 2050 1350
Wire Wire Line
	2050 1350 1650 1350
Wire Wire Line
	1650 1350 1650 1250
Wire Wire Line
	1650 750  1650 850 
Wire Wire Line
	1650 850  1750 850 
Connection ~ 1650 850 
Wire Wire Line
	1650 850  1650 950 
Wire Wire Line
	2350 850  2450 850 
Wire Wire Line
	2450 850  2450 950 
$Comp
L power:+3.3V #PWR0105
U 1 1 5C8BB197
P 2450 750
F 0 "#PWR0105" H 2450 600 50  0001 C CNN
F 1 "+3.3V" H 2465 923 50  0000 C CNN
F 2 "" H 2450 750 50  0001 C CNN
F 3 "" H 2450 750 50  0001 C CNN
	1    2450 750 
	1    0    0    -1  
$EndComp
Wire Wire Line
	2450 750  2450 850 
Connection ~ 2450 850 
Text Label 5650 5000 0    50   ~ 0
D+
Text Label 5650 4900 0    50   ~ 0
D-
$Comp
L Connector:USB_B J2
U 1 1 5C8C3135
P 10550 5300
F 0 "J2" H 10605 5767 50  0000 C CNN
F 1 "USB_B" H 10605 5676 50  0000 C CNN
F 2 "Connector_USB:USB_B_OST_USB-B1HSxx_Horizontal" H 10700 5250 50  0001 C CNN
F 3 " ~" H 10700 5250 50  0001 C CNN
	1    10550 5300
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR01
U 1 1 5C8C3141
P 10450 5900
F 0 "#PWR01" H 10450 5650 50  0001 C CNN
F 1 "GND" H 10455 5727 50  0000 C CNN
F 2 "" H 10450 5900 50  0001 C CNN
F 3 "" H 10450 5900 50  0001 C CNN
	1    10450 5900
	1    0    0    -1  
$EndComp
$Comp
L Connector:RJ45 J5
U 1 1 5C8C9829
P 9150 1300
F 0 "J5" H 9205 1967 50  0000 C CNN
F 1 "RJ45" H 9205 1876 50  0000 C CNN
F 2 "folivora:RJ45_rev" V 9150 1325 50  0001 C CNN
F 3 "~" V 9150 1325 50  0001 C CNN
	1    9150 1300
	1    0    0    -1  
$EndComp
$Comp
L Connector_Generic:Conn_01x06 J3
U 1 1 5C8C9A71
P 7750 1150
F 0 "J3" H 7830 1142 50  0000 L CNN
F 1 "Trackball_conn" H 7830 1051 50  0000 L CNN
F 2 "Connector_JST:JST_XH_S06B-XH-A_1x06_P2.50mm_Horizontal" H 7750 1150 50  0001 C CNN
F 3 "~" H 7750 1150 50  0001 C CNN
	1    7750 1150
	1    0    0    1   
$EndComp
$Comp
L power:+3.3V #PWR0109
U 1 1 5C8C9B55
P 7450 750
F 0 "#PWR0109" H 7450 600 50  0001 C CNN
F 1 "+3.3V" H 7465 923 50  0000 C CNN
F 2 "" H 7450 750 50  0001 C CNN
F 3 "" H 7450 750 50  0001 C CNN
	1    7450 750 
	1    0    0    -1  
$EndComp
$Comp
L power:GND #PWR0110
U 1 1 5C8C9BD1
P 7450 1450
F 0 "#PWR0110" H 7450 1200 50  0001 C CNN
F 1 "GND" H 7455 1277 50  0000 C CNN
F 2 "" H 7450 1450 50  0001 C CNN
F 3 "" H 7450 1450 50  0001 C CNN
	1    7450 1450
	1    0    0    -1  
$EndComp
Text Label 7550 950  2    50   ~ 0
SS_T
Text Label 7550 1050 2    50   ~ 0
SCK
Text Label 7550 1150 2    50   ~ 0
MISO
Text Label 7550 1250 2    50   ~ 0
MOSI
Wire Wire Line
	7550 1350 7450 1350
Wire Wire Line
	7450 1350 7450 1450
Wire Wire Line
	7450 750  7450 850 
Wire Wire Line
	7450 850  7550 850 
$Comp
L power:+3.3V #PWR0111
U 1 1 5C8CC19B
P 9650 800
F 0 "#PWR0111" H 9650 650 50  0001 C CNN
F 1 "+3.3V" H 9665 973 50  0000 C CNN
F 2 "" H 9650 800 50  0001 C CNN
F 3 "" H 9650 800 50  0001 C CNN
	1    9650 800 
	1    0    0    -1  
$EndComp
Wire Wire Line
	9650 800  9650 900 
Wire Wire Line
	9650 900  9550 900 
$Comp
L power:GND #PWR0112
U 1 1 5C8CC881
P 9650 1700
F 0 "#PWR0112" H 9650 1450 50  0001 C CNN
F 1 "GND" H 9655 1527 50  0000 C CNN
F 2 "" H 9650 1700 50  0001 C CNN
F 3 "" H 9650 1700 50  0001 C CNN
	1    9650 1700
	1    0    0    -1  
$EndComp
Wire Wire Line
	9650 1700 9650 1600
Wire Wire Line
	9650 1600 9550 1600
Text Label 9550 1200 0    50   ~ 0
MISO
Text Label 9550 1300 0    50   ~ 0
MOSI
Text Label 9550 1400 0    50   ~ 0
SCK
Text Label 9550 1000 0    50   ~ 0
SS_T
Text Label 9550 1100 0    50   ~ 0
SS_K
NoConn ~ 9550 1500
$Comp
L Switch:SW_Push_45deg SW1
U 1 1 5C8CE76A
P 5150 7300
F 0 "SW1" V 5196 7159 50  0000 R CNN
F 1 "SW40" V 5105 7159 50  0000 R CNN
F 2 "folivora:SW_Cherry_MX1A_1.00u_PCB" H 5150 7300 50  0001 C CNN
F 3 "" H 5150 7300 50  0001 C CNN
	1    5150 7300
	0    -1   -1   0   
$EndComp
$Comp
L Device:D D1
U 1 1 5C8CF17C
P 5200 7500
F 0 "D1" H 5200 7300 50  0000 C CNN
F 1 "D" H 5200 7400 50  0000 C CNN
F 2 "Diode_SMD:D_SOD-123" H 5200 7500 50  0001 C CNN
F 3 "~" H 5200 7500 50  0001 C CNN
	1    5200 7500
	1    0    0    -1  
$EndComp
Wire Wire Line
	5050 7500 5050 7400
$Comp
L Switch:SW_Push_45deg SW4
U 1 1 5C8D13D8
P 6650 7300
F 0 "SW4" V 6696 7159 50  0000 R CNN
F 1 "SW43" V 6605 7159 50  0000 R CNN
F 2 "folivora:SW_Cherry_MX1A_1.00u_PCB" H 6650 7300 50  0001 C CNN
F 3 "" H 6650 7300 50  0001 C CNN
	1    6650 7300
	0    -1   -1   0   
$EndComp
$Comp
L Device:D D4
U 1 1 5C8D13DE
P 6700 7500
F 0 "D4" H 6700 7300 50  0000 C CNN
F 1 "D" H 6700 7400 50  0000 C CNN
F 2 "Diode_SMD:D_SOD-123" H 6700 7500 50  0001 C CNN
F 3 "~" H 6700 7500 50  0001 C CNN
	1    6700 7500
	1    0    0    -1  
$EndComp
Wire Wire Line
	6550 7500 6550 7400
Wire Wire Line
	5250 7200 5250 7100
Wire Wire Line
	6750 7100 6750 7200
Text Label 5350 7500 3    50   ~ 0
READ0
Text Label 6850 7500 3    50   ~ 0
READ3
Text Label 5250 7100 2    50   ~ 0
SELECT4
$Comp
L Connector_Generic:Conn_01x07 J4
U 1 1 5C8D54E7
P 4300 6500
F 0 "J4" H 4380 6542 50  0000 L CNN
F 1 "ROW0" H 4380 6451 50  0000 L CNN
F 2 "Connector_JST:JST_XH_S07B-XH-A_1x07_P2.50mm_Horizontal" H 4300 6500 50  0001 C CNN
F 3 "~" H 4300 6500 50  0001 C CNN
	1    4300 6500
	1    0    0    -1  
$EndComp
Text Label 4100 6200 2    50   ~ 0
SELECT0
Text Label 4100 6300 2    50   ~ 0
READ0
Text Label 4100 6400 2    50   ~ 0
READ1
Text Label 4100 6500 2    50   ~ 0
READ2
Text Label 4100 6600 2    50   ~ 0
READ3
Text Label 4100 6700 2    50   ~ 0
READ4
Text Label 4100 6800 2    50   ~ 0
READ5
$Comp
L Connector_Generic:Conn_01x07 J6
U 1 1 5C8D5A0A
P 5100 6500
F 0 "J6" H 5180 6542 50  0000 L CNN
F 1 "ROW1" H 5180 6451 50  0000 L CNN
F 2 "Connector_JST:JST_XH_S07B-XH-A_1x07_P2.50mm_Horizontal" H 5100 6500 50  0001 C CNN
F 3 "~" H 5100 6500 50  0001 C CNN
	1    5100 6500
	1    0    0    -1  
$EndComp
Text Label 4900 6200 2    50   ~ 0
SELECT1
Text Label 4900 6300 2    50   ~ 0
READ0
Text Label 4900 6400 2    50   ~ 0
READ1
Text Label 4900 6500 2    50   ~ 0
READ2
Text Label 4900 6600 2    50   ~ 0
READ3
Text Label 4900 6700 2    50   ~ 0
READ4
Text Label 4900 6800 2    50   ~ 0
READ5
$Comp
L Connector_Generic:Conn_01x07 J7
U 1 1 5C8D6446
P 5950 6500
F 0 "J7" H 6030 6542 50  0000 L CNN
F 1 "ROW2" H 6030 6451 50  0000 L CNN
F 2 "Connector_JST:JST_XH_S07B-XH-A_1x07_P2.50mm_Horizontal" H 5950 6500 50  0001 C CNN
F 3 "~" H 5950 6500 50  0001 C CNN
	1    5950 6500
	1    0    0    -1  
$EndComp
Text Label 5750 6200 2    50   ~ 0
SELECT2
Text Label 5750 6300 2    50   ~ 0
READ0
Text Label 5750 6400 2    50   ~ 0
READ1
Text Label 5750 6500 2    50   ~ 0
READ2
Text Label 5750 6600 2    50   ~ 0
READ3
Text Label 5750 6700 2    50   ~ 0
READ4
Text Label 5750 6800 2    50   ~ 0
READ5
$Comp
L Connector_Generic:Conn_01x07 J8
U 1 1 5C8D6E56
P 6750 6500
F 0 "J8" H 6830 6542 50  0000 L CNN
F 1 "ROW3" H 6830 6451 50  0000 L CNN
F 2 "Connector_JST:JST_XH_S07B-XH-A_1x07_P2.50mm_Horizontal" H 6750 6500 50  0001 C CNN
F 3 "~" H 6750 6500 50  0001 C CNN
	1    6750 6500
	1    0    0    -1  
$EndComp
Text Label 6550 6200 2    50   ~ 0
SELECT3
Text Label 6550 6300 2    50   ~ 0
READ0
Text Label 6550 6400 2    50   ~ 0
READ1
Text Label 6550 6500 2    50   ~ 0
READ2
Text Label 6550 6600 2    50   ~ 0
READ3
Text Label 6550 6700 2    50   ~ 0
READ4
Text Label 6550 6800 2    50   ~ 0
READ5
Text Label 5650 4500 0    50   ~ 0
MOSI
Text Label 5650 4400 0    50   ~ 0
MISO
Text Label 5650 4300 0    50   ~ 0
SCK
Text Label 5650 4100 0    50   ~ 0
SS_T
Text Label 5650 4200 0    50   ~ 0
SS_K
Text Label 5650 5300 0    50   ~ 0
READ0
Text Label 4350 5300 2    50   ~ 0
READ1
Text Label 4350 5200 2    50   ~ 0
READ2
Text Label 4350 5100 2    50   ~ 0
READ3
Text Label 4350 5000 2    50   ~ 0
READ4
Text Label 4350 4900 2    50   ~ 0
READ5
Text Label 5650 3800 0    50   ~ 0
SELECT0
Text Label 5650 3900 0    50   ~ 0
SELECT1
Text Label 5650 4000 0    50   ~ 0
SELECT2
Text Label 5650 4700 0    50   ~ 0
SELECT3
Text Label 5650 4800 0    50   ~ 0
SELECT4
$Comp
L Device:LED D5
U 1 1 5C8E115D
P 10250 1450
F 0 "D5" V 10288 1333 50  0000 R CNN
F 1 "LED" V 10197 1333 50  0000 R CNN
F 2 "LED_SMD:LED_1206_3216Metric" H 10250 1450 50  0001 C CNN
F 3 "~" H 10250 1450 50  0001 C CNN
	1    10250 1450
	0    -1   -1   0   
$EndComp
$Comp
L Device:LED D6
U 1 1 5C8E12F3
P 10600 1450
F 0 "D6" V 10638 1333 50  0000 R CNN
F 1 "LED" V 10547 1333 50  0000 R CNN
F 2 "LED_SMD:LED_1206_3216Metric" H 10600 1450 50  0001 C CNN
F 3 "~" H 10600 1450 50  0001 C CNN
	1    10600 1450
	0    -1   -1   0   
$EndComp
$Comp
L Device:LED D7
U 1 1 5C8E1415
P 10950 1450
F 0 "D7" V 10988 1333 50  0000 R CNN
F 1 "LED" V 10897 1333 50  0000 R CNN
F 2 "LED_SMD:LED_1206_3216Metric" H 10950 1450 50  0001 C CNN
F 3 "~" H 10950 1450 50  0001 C CNN
	1    10950 1450
	0    -1   -1   0   
$EndComp
$Comp
L Device:R R1
U 1 1 5C8E14F7
P 10250 1050
F 0 "R1" H 10320 1096 50  0000 L CNN
F 1 "R" H 10320 1005 50  0000 L CNN
F 2 "Resistor_SMD:R_0603_1608Metric" V 10180 1050 50  0001 C CNN
F 3 "~" H 10250 1050 50  0001 C CNN
	1    10250 1050
	1    0    0    -1  
$EndComp
$Comp
L Device:R R2
U 1 1 5C8E15A7
P 10600 1050
F 0 "R2" H 10670 1096 50  0000 L CNN
F 1 "R" H 10670 1005 50  0000 L CNN
F 2 "Resistor_SMD:R_0603_1608Metric" V 10530 1050 50  0001 C CNN
F 3 "~" H 10600 1050 50  0001 C CNN
	1    10600 1050
	1    0    0    -1  
$EndComp
$Comp
L Device:R R3
U 1 1 5C8E1628
P 10950 1050
F 0 "R3" H 11020 1096 50  0000 L CNN
F 1 "R" H 11020 1005 50  0000 L CNN
F 2 "Resistor_SMD:R_0603_1608Metric" V 10880 1050 50  0001 C CNN
F 3 "~" H 10950 1050 50  0001 C CNN
	1    10950 1050
	1    0    0    -1  
$EndComp
$Comp
L power:+3.3V #PWR0113
U 1 1 5C8E1EDC
P 10250 800
F 0 "#PWR0113" H 10250 650 50  0001 C CNN
F 1 "+3.3V" H 10265 973 50  0000 C CNN
F 2 "" H 10250 800 50  0001 C CNN
F 3 "" H 10250 800 50  0001 C CNN
	1    10250 800 
	1    0    0    -1  
$EndComp
$Comp
L power:+3.3V #PWR0114
U 1 1 5C8E1F52
P 10600 800
F 0 "#PWR0114" H 10600 650 50  0001 C CNN
F 1 "+3.3V" H 10615 973 50  0000 C CNN
F 2 "" H 10600 800 50  0001 C CNN
F 3 "" H 10600 800 50  0001 C CNN
	1    10600 800 
	1    0    0    -1  
$EndComp
$Comp
L power:+3.3V #PWR0115
U 1 1 5C8E1FC8
P 10950 800
F 0 "#PWR0115" H 10950 650 50  0001 C CNN
F 1 "+3.3V" H 10965 973 50  0000 C CNN
F 2 "" H 10950 800 50  0001 C CNN
F 3 "" H 10950 800 50  0001 C CNN
	1    10950 800 
	1    0    0    -1  
$EndComp
Wire Wire Line
	10950 800  10950 900 
Wire Wire Line
	10600 900  10600 800 
Wire Wire Line
	10250 900  10250 800 
Wire Wire Line
	10250 1300 10250 1200
Wire Wire Line
	10600 1300 10600 1200
Wire Wire Line
	10950 1300 10950 1200
Wire Wire Line
	10250 1700 10250 1600
Wire Wire Line
	10600 1700 10600 1600
Wire Wire Line
	10950 1600 10950 1700
$Comp
L Jumper:SolderJumper_2_Open JP2
U 1 1 5C8F84BB
P 8650 5000
F 0 "JP2" H 8650 5205 50  0000 C CNN
F 1 "L" H 8650 5114 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 8650 5000 50  0001 C CNN
F 3 "~" H 8650 5000 50  0001 C CNN
	1    8650 5000
	1    0    0    -1  
$EndComp
$Comp
L Jumper:SolderJumper_2_Bridged JP3
U 1 1 5C8F98DC
P 8650 5300
F 0 "JP3" H 8650 5505 50  0000 C CNN
F 1 "R" H 8650 5414 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 8650 5300 50  0001 C CNN
F 3 "~" H 8650 5300 50  0001 C CNN
	1    8650 5300
	1    0    0    -1  
$EndComp
Wire Wire Line
	10450 5700 10450 5900
Text Label 10850 5100 0    50   ~ 0
USB_1
Text Label 10850 5400 0    50   ~ 0
USB_2
Text Label 10850 5300 0    50   ~ 0
USB_3
Text Label 10550 5700 3    50   ~ 0
USB_4
Wire Wire Line
	8800 5000 8900 5000
Wire Wire Line
	8900 5000 8900 5300
Wire Wire Line
	8900 5300 8800 5300
Text Label 8900 5000 0    50   ~ 0
USB_1
$Comp
L power:+5V #PWR0101
U 1 1 5C8FC683
P 8300 4950
F 0 "#PWR0101" H 8300 4800 50  0001 C CNN
F 1 "+5V" H 8315 5123 50  0000 C CNN
F 2 "" H 8300 4950 50  0001 C CNN
F 3 "" H 8300 4950 50  0001 C CNN
	1    8300 4950
	1    0    0    -1  
$EndComp
Wire Wire Line
	8300 4950 8300 5300
Wire Wire Line
	8300 5300 8500 5300
Text Label 8500 5000 2    50   ~ 0
D-
$Comp
L Jumper:SolderJumper_2_Open JP4
U 1 1 5C9017B0
P 8650 5650
F 0 "JP4" H 8650 5855 50  0000 C CNN
F 1 "L" H 8650 5764 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 8650 5650 50  0001 C CNN
F 3 "~" H 8650 5650 50  0001 C CNN
	1    8650 5650
	1    0    0    -1  
$EndComp
$Comp
L Jumper:SolderJumper_2_Bridged JP5
U 1 1 5C9017B6
P 8650 5950
F 0 "JP5" H 8650 6155 50  0000 C CNN
F 1 "R" H 8650 6064 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 8650 5950 50  0001 C CNN
F 3 "~" H 8650 5950 50  0001 C CNN
	1    8650 5950
	1    0    0    -1  
$EndComp
Wire Wire Line
	8800 5650 8900 5650
Wire Wire Line
	8900 5650 8900 5950
Wire Wire Line
	8900 5950 8800 5950
Text Label 8900 5650 0    50   ~ 0
USB_2
Text Label 8500 5950 2    50   ~ 0
D-
$Comp
L power:+5V #PWR0102
U 1 1 5C90439E
P 8300 5550
F 0 "#PWR0102" H 8300 5400 50  0001 C CNN
F 1 "+5V" H 8315 5723 50  0000 C CNN
F 2 "" H 8300 5550 50  0001 C CNN
F 3 "" H 8300 5550 50  0001 C CNN
	1    8300 5550
	1    0    0    -1  
$EndComp
Wire Wire Line
	8300 5550 8300 5650
Wire Wire Line
	8300 5650 8500 5650
$Comp
L Jumper:SolderJumper_2_Open JP6
U 1 1 5C9096F4
P 9700 5000
F 0 "JP6" H 9700 5205 50  0000 C CNN
F 1 "L" H 9700 5114 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 9700 5000 50  0001 C CNN
F 3 "~" H 9700 5000 50  0001 C CNN
	1    9700 5000
	1    0    0    -1  
$EndComp
$Comp
L Jumper:SolderJumper_2_Bridged JP7
U 1 1 5C9096FA
P 9700 5300
F 0 "JP7" H 9700 5505 50  0000 C CNN
F 1 "R" H 9700 5414 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 9700 5300 50  0001 C CNN
F 3 "~" H 9700 5300 50  0001 C CNN
	1    9700 5300
	1    0    0    -1  
$EndComp
Wire Wire Line
	9850 5000 9950 5000
Wire Wire Line
	9950 5000 9950 5300
Wire Wire Line
	9950 5300 9850 5300
Text Label 9950 5000 0    50   ~ 0
USB_3
Text Label 9550 5300 2    50   ~ 0
D+
$Comp
L Jumper:SolderJumper_2_Open JP8
U 1 1 5C90970D
P 9700 5650
F 0 "JP8" H 9700 5855 50  0000 C CNN
F 1 "L" H 9700 5764 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 9700 5650 50  0001 C CNN
F 3 "~" H 9700 5650 50  0001 C CNN
	1    9700 5650
	1    0    0    -1  
$EndComp
$Comp
L Jumper:SolderJumper_2_Bridged JP9
U 1 1 5C909713
P 9700 5950
F 0 "JP9" H 9700 6155 50  0000 C CNN
F 1 "R" H 9700 6064 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 9700 5950 50  0001 C CNN
F 3 "~" H 9700 5950 50  0001 C CNN
	1    9700 5950
	1    0    0    -1  
$EndComp
Wire Wire Line
	9850 5650 9950 5650
Wire Wire Line
	9950 5650 9950 5950
Wire Wire Line
	9950 5950 9850 5950
Text Label 9950 5650 0    50   ~ 0
USB_4
Text Label 9550 5650 2    50   ~ 0
D+
$Comp
L power:GND #PWR0119
U 1 1 5C90DF50
P 9350 6050
F 0 "#PWR0119" H 9350 5800 50  0001 C CNN
F 1 "GND" H 9355 5877 50  0000 C CNN
F 2 "" H 9350 6050 50  0001 C CNN
F 3 "" H 9350 6050 50  0001 C CNN
	1    9350 6050
	1    0    0    -1  
$EndComp
Wire Wire Line
	9350 6050 9350 5950
Wire Wire Line
	9350 5950 9550 5950
$Comp
L power:GND #PWR0120
U 1 1 5C90F23C
P 9350 5300
F 0 "#PWR0120" H 9350 5050 50  0001 C CNN
F 1 "GND" H 9355 5127 50  0000 C CNN
F 2 "" H 9350 5300 50  0001 C CNN
F 3 "" H 9350 5300 50  0001 C CNN
	1    9350 5300
	1    0    0    -1  
$EndComp
Wire Wire Line
	9350 5000 9550 5000
Wire Wire Line
	9350 5300 9350 5000
$Comp
L power:PWR_FLAG #FLG0101
U 1 1 5C98B18C
P 1400 750
F 0 "#FLG0101" H 1400 825 50  0001 C CNN
F 1 "PWR_FLAG" H 1400 924 50  0000 C CNN
F 2 "" H 1400 750 50  0001 C CNN
F 3 "~" H 1400 750 50  0001 C CNN
	1    1400 750 
	1    0    0    -1  
$EndComp
Wire Wire Line
	1400 750  1400 850 
Wire Wire Line
	1400 850  1650 850 
$Comp
L Connector_Generic:Conn_01x04 J1
U 1 1 5C9E413A
P 11050 3850
F 0 "J1" H 11130 3842 50  0000 L CNN
F 1 "Conn_01x04" H 11130 3751 50  0000 L CNN
F 2 "Connector_PinSocket_2.54mm:PinSocket_1x04_P2.54mm_Vertical" H 11050 3850 50  0001 C CNN
F 3 "~" H 11050 3850 50  0001 C CNN
	1    11050 3850
	1    0    0    -1  
$EndComp
$Comp
L Jumper:SolderJumper_2_Open JP11
U 1 1 5C9E66B8
P 9100 3650
F 0 "JP11" H 9200 3550 50  0000 C CNN
F 1 "L" H 9050 3550 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 9100 3650 50  0001 C CNN
F 3 "~" H 9100 3650 50  0001 C CNN
	1    9100 3650
	1    0    0    -1  
$EndComp
Text Label 9350 3550 0    50   ~ 0
OLED1
$Comp
L Jumper:SolderJumper_2_Open JP10
U 1 1 5C9E469A
P 9100 3550
F 0 "JP10" H 9200 3650 50  0000 C CNN
F 1 "R" H 9050 3650 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 9100 3550 50  0001 C CNN
F 3 "~" H 9100 3550 50  0001 C CNN
	1    9100 3550
	1    0    0    -1  
$EndComp
Wire Wire Line
	9350 3550 9250 3550
Wire Wire Line
	9350 3550 9350 3650
Wire Wire Line
	9350 3650 9250 3650
$Comp
L power:GND #PWR05
U 1 1 5C9F30C1
P 8750 3650
F 0 "#PWR05" H 8750 3400 50  0001 C CNN
F 1 "GND" H 8755 3477 50  0000 C CNN
F 2 "" H 8750 3650 50  0001 C CNN
F 3 "" H 8750 3650 50  0001 C CNN
	1    8750 3650
	1    0    0    -1  
$EndComp
Wire Wire Line
	8750 3650 8750 3550
Wire Wire Line
	8750 3550 8950 3550
Text Label 8950 3650 2    50   ~ 0
SDA
$Comp
L Jumper:SolderJumper_2_Open JP13
U 1 1 5C9F9BAC
P 9100 4300
F 0 "JP13" H 9200 4200 50  0000 C CNN
F 1 "L" H 9050 4200 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 9100 4300 50  0001 C CNN
F 3 "~" H 9100 4300 50  0001 C CNN
	1    9100 4300
	1    0    0    -1  
$EndComp
Text Label 9350 4200 0    50   ~ 0
OLED2
$Comp
L Jumper:SolderJumper_2_Open JP12
U 1 1 5C9F9BB3
P 9100 4200
F 0 "JP12" H 9200 4300 50  0000 C CNN
F 1 "R" H 9050 4300 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 9100 4200 50  0001 C CNN
F 3 "~" H 9100 4200 50  0001 C CNN
	1    9100 4200
	1    0    0    -1  
$EndComp
Wire Wire Line
	9350 4200 9250 4200
Wire Wire Line
	9350 4200 9350 4300
Wire Wire Line
	9350 4300 9250 4300
Wire Wire Line
	8850 4100 8850 4200
Wire Wire Line
	8850 4200 8950 4200
$Comp
L Jumper:SolderJumper_2_Open JP15
U 1 1 5CA11896
P 10100 3650
F 0 "JP15" H 10200 3550 50  0000 C CNN
F 1 "L" H 10050 3550 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 10100 3650 50  0001 C CNN
F 3 "~" H 10100 3650 50  0001 C CNN
	1    10100 3650
	1    0    0    -1  
$EndComp
Text Label 10350 3550 0    50   ~ 0
OLED3
$Comp
L Jumper:SolderJumper_2_Open JP14
U 1 1 5CA1189D
P 10100 3550
F 0 "JP14" H 10200 3650 50  0000 C CNN
F 1 "R" H 10050 3650 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 10100 3550 50  0001 C CNN
F 3 "~" H 10100 3550 50  0001 C CNN
	1    10100 3550
	1    0    0    -1  
$EndComp
Wire Wire Line
	10350 3550 10250 3550
Wire Wire Line
	10350 3550 10350 3650
Wire Wire Line
	10350 3650 10250 3650
$Comp
L Jumper:SolderJumper_2_Open JP17
U 1 1 5CA118AF
P 10100 4300
F 0 "JP17" H 10200 4200 50  0000 C CNN
F 1 "L" H 10050 4200 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 10100 4300 50  0001 C CNN
F 3 "~" H 10100 4300 50  0001 C CNN
	1    10100 4300
	1    0    0    -1  
$EndComp
Text Label 10350 4200 0    50   ~ 0
OLED4
$Comp
L Jumper:SolderJumper_2_Open JP16
U 1 1 5CA118B6
P 10100 4200
F 0 "JP16" H 10200 4300 50  0000 C CNN
F 1 "R" H 10050 4300 50  0000 C CNN
F 2 "Jumper:SolderJumper-2_P1.3mm_Open_TrianglePad1.0x1.5mm" H 10100 4200 50  0001 C CNN
F 3 "~" H 10100 4200 50  0001 C CNN
	1    10100 4200
	1    0    0    -1  
$EndComp
Wire Wire Line
	10350 4200 10250 4200
Wire Wire Line
	10350 4200 10350 4300
Wire Wire Line
	10350 4300 10250 4300
Text Label 8950 4300 2    50   ~ 0
SCL
Wire Wire Line
	9750 3550 9750 3650
Wire Wire Line
	9750 3650 9950 3650
Text Label 9950 3550 2    50   ~ 0
SCL
$Comp
L power:GND #PWR08
U 1 1 5CA202D0
P 9750 4400
F 0 "#PWR08" H 9750 4150 50  0001 C CNN
F 1 "GND" H 9755 4227 50  0000 C CNN
F 2 "" H 9750 4400 50  0001 C CNN
F 3 "" H 9750 4400 50  0001 C CNN
	1    9750 4400
	1    0    0    -1  
$EndComp
Wire Wire Line
	9750 4400 9750 4300
Wire Wire Line
	9750 4300 9950 4300
Text Label 9950 4200 2    50   ~ 0
SDA
Text Label 10850 4050 2    50   ~ 0
OLED4
Text Label 10850 3950 2    50   ~ 0
OLED3
Text Label 10850 3850 2    50   ~ 0
OLED2
Text Label 10850 3750 2    50   ~ 0
OLED1
Text Label 10250 1700 3    50   ~ 0
LED1
Text Label 10600 1700 3    50   ~ 0
LED2
Text Label 10950 1700 3    50   ~ 0
LED3
Text Label 4350 4600 2    50   ~ 0
LED1
Text Label 4350 4700 2    50   ~ 0
LED2
Text Label 4350 4300 2    50   ~ 0
LED3
Text Label 4350 4400 2    50   ~ 0
SCL
Text Label 4350 4500 2    50   ~ 0
SDA
$Comp
L Device:Crystal Y2
U 1 1 5C90D5DE
P 2650 4500
F 0 "Y2" V 2650 4450 50  0000 L CNN
F 1 "8MHz" H 2550 4650 50  0000 L CNN
F 2 "Crystal:Crystal_HC49-U_Vertical" H 2650 4500 50  0001 C CNN
F 3 "~" H 2650 4500 50  0001 C CNN
	1    2650 4500
	-1   0    0    1   
$EndComp
$Comp
L Device:R R4
U 1 1 5C90DB92
P 2650 4300
F 0 "R4" V 2650 4250 50  0000 L CNN
F 1 "10Mohm" V 2550 4150 50  0000 L CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" V 2580 4300 50  0001 C CNN
F 3 "~" H 2650 4300 50  0001 C CNN
	1    2650 4300
	0    1    1    0   
$EndComp
$Comp
L Device:C C5
U 1 1 5C911140
P 2400 4750
F 0 "C5" V 2250 4750 50  0000 C CNN
F 1 "20pF" V 2550 4750 50  0000 C CNN
F 2 "Capacitor_SMD:C_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 2438 4600 50  0001 C CNN
F 3 "~" H 2400 4750 50  0001 C CNN
	1    2400 4750
	-1   0    0    1   
$EndComp
$Comp
L Device:C C6
U 1 1 5C911C6D
P 2900 4750
F 0 "C6" V 3050 4750 50  0000 C CNN
F 1 "20pF" V 2750 4750 50  0000 C CNN
F 2 "Capacitor_SMD:C_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 2938 4600 50  0001 C CNN
F 3 "~" H 2900 4750 50  0001 C CNN
	1    2900 4750
	-1   0    0    1   
$EndComp
Wire Wire Line
	2400 4600 2400 4500
Wire Wire Line
	2400 4500 2500 4500
Wire Wire Line
	2500 4300 2400 4300
Wire Wire Line
	2400 4300 2400 4500
Connection ~ 2400 4500
Wire Wire Line
	2800 4300 2900 4300
Wire Wire Line
	2900 4300 2900 4500
Wire Wire Line
	2800 4500 2900 4500
Connection ~ 2900 4500
Wire Wire Line
	2900 4500 2900 4600
Wire Wire Line
	2400 4900 2400 5000
Wire Wire Line
	2400 5000 2650 5000
Wire Wire Line
	2900 5000 2900 4900
$Comp
L power:GND #PWR0106
U 1 1 5C933111
P 2650 5100
F 0 "#PWR0106" H 2650 4850 50  0001 C CNN
F 1 "GND" H 2655 4927 50  0000 C CNN
F 2 "" H 2650 5100 50  0001 C CNN
F 3 "" H 2650 5100 50  0001 C CNN
	1    2650 5100
	1    0    0    -1  
$EndComp
Wire Wire Line
	2650 5100 2650 5000
Connection ~ 2650 5000
Wire Wire Line
	2650 5000 2900 5000
Text Label 2400 4300 2    50   ~ 0
XTAL_A
Text Label 2900 4300 0    50   ~ 0
XTAL_B
$Comp
L Device:Crystal Y1
U 1 1 5C937E67
P 1600 4500
F 0 "Y1" V 1600 4450 50  0000 L CNN
F 1 "32.768kHz" H 1400 4650 50  0000 L CNN
F 2 "Crystal:Crystal_DS15_D1.5mm_L5.0mm_Vertical" H 1600 4500 50  0001 C CNN
F 3 "~" H 1600 4500 50  0001 C CNN
	1    1600 4500
	-1   0    0    1   
$EndComp
$Comp
L Device:C C3
U 1 1 5C937E73
P 1350 4750
F 0 "C3" V 1200 4750 50  0000 C CNN
F 1 "12.5pF" V 1500 4750 50  0000 C CNN
F 2 "Capacitor_SMD:C_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 1388 4600 50  0001 C CNN
F 3 "~" H 1350 4750 50  0001 C CNN
	1    1350 4750
	-1   0    0    1   
$EndComp
$Comp
L Device:C C4
U 1 1 5C937E79
P 1850 4750
F 0 "C4" V 2000 4750 50  0000 C CNN
F 1 "12.5pF[" V 1700 4750 50  0000 C CNN
F 2 "Capacitor_SMD:C_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 1888 4600 50  0001 C CNN
F 3 "~" H 1850 4750 50  0001 C CNN
	1    1850 4750
	-1   0    0    1   
$EndComp
Wire Wire Line
	1350 4600 1350 4500
Wire Wire Line
	1350 4500 1450 4500
Wire Wire Line
	1350 4300 1350 4500
Connection ~ 1350 4500
Wire Wire Line
	1850 4300 1850 4500
Wire Wire Line
	1750 4500 1850 4500
Connection ~ 1850 4500
Wire Wire Line
	1850 4500 1850 4600
Wire Wire Line
	1350 4900 1350 5000
Wire Wire Line
	1350 5000 1600 5000
Wire Wire Line
	1850 5000 1850 4900
$Comp
L power:GND #PWR0107
U 1 1 5C937E8C
P 1600 5100
F 0 "#PWR0107" H 1600 4850 50  0001 C CNN
F 1 "GND" H 1605 4927 50  0000 C CNN
F 2 "" H 1600 5100 50  0001 C CNN
F 3 "" H 1600 5100 50  0001 C CNN
	1    1600 5100
	1    0    0    -1  
$EndComp
Wire Wire Line
	1600 5100 1600 5000
Connection ~ 1600 5000
Wire Wire Line
	1600 5000 1850 5000
Text Label 1350 4300 2    50   ~ 0
XTAL_C
Text Label 1850 4300 0    50   ~ 0
XTAL_D
Text Label 4350 3100 2    50   ~ 0
XTAL_A
Text Label 4350 3200 2    50   ~ 0
XTAL_B
Text Label 4350 3500 2    50   ~ 0
XTAL_C
Text Label 4350 3600 2    50   ~ 0
XTAL_D
$Comp
L Device:C C11
U 1 1 5C940327
P 4600 2300
F 0 "C11" V 4550 2400 50  0000 C CNN
F 1 "0.1uF" V 4550 2150 50  0000 C CNN
F 2 "Capacitor_SMD:C_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 4638 2150 50  0001 C CNN
F 3 "~" H 4600 2300 50  0001 C CNN
	1    4600 2300
	0    1    1    0   
$EndComp
$Comp
L Device:C C10
U 1 1 5C94A64A
P 4600 2100
F 0 "C10" V 4550 2200 50  0000 C CNN
F 1 "0.1uF" V 4550 1950 50  0000 C CNN
F 2 "Capacitor_SMD:C_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 4638 1950 50  0001 C CNN
F 3 "~" H 4600 2100 50  0001 C CNN
	1    4600 2100
	0    1    1    0   
$EndComp
$Comp
L Device:C C9
U 1 1 5C94EF6A
P 4600 1900
F 0 "C9" V 4550 2000 50  0000 C CNN
F 1 "0.1uF" V 4550 1750 50  0000 C CNN
F 2 "Capacitor_SMD:C_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 4638 1750 50  0001 C CNN
F 3 "~" H 4600 1900 50  0001 C CNN
	1    4600 1900
	0    1    1    0   
$EndComp
$Comp
L Device:C C8
U 1 1 5C94EF70
P 4600 1700
F 0 "C8" V 4550 1800 50  0000 C CNN
F 1 "0.1uF" V 4550 1550 50  0000 C CNN
F 2 "Capacitor_SMD:C_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 4638 1550 50  0001 C CNN
F 3 "~" H 4600 1700 50  0001 C CNN
	1    4600 1700
	0    1    1    0   
$EndComp
Wire Wire Line
	4750 2300 4850 2300
Wire Wire Line
	4850 2300 4850 2500
Wire Wire Line
	4950 2500 4950 2300
Wire Wire Line
	4950 2100 4750 2100
Wire Wire Line
	4750 1900 5050 1900
Wire Wire Line
	5050 1900 5050 2100
Wire Wire Line
	4750 1700 5150 1700
Wire Wire Line
	5150 1700 5150 1900
Wire Wire Line
	4850 2300 4950 2300
Connection ~ 4850 2300
Connection ~ 4950 2300
Wire Wire Line
	4950 2300 4950 2100
Wire Wire Line
	4950 2100 5050 2100
Connection ~ 4950 2100
Connection ~ 5050 2100
Wire Wire Line
	5050 2100 5050 2500
Wire Wire Line
	5050 1900 5150 1900
Connection ~ 5050 1900
Connection ~ 5150 1900
Wire Wire Line
	5150 1900 5150 2500
Wire Wire Line
	5250 2500 5250 1700
Wire Wire Line
	5250 1700 5150 1700
Connection ~ 5150 1700
$Comp
L power:+3V3 #PWR0108
U 1 1 5C9829E8
P 5250 1600
F 0 "#PWR0108" H 5250 1450 50  0001 C CNN
F 1 "+3V3" H 5265 1773 50  0000 C CNN
F 2 "" H 5250 1600 50  0001 C CNN
F 3 "" H 5250 1600 50  0001 C CNN
	1    5250 1600
	1    0    0    -1  
$EndComp
Wire Wire Line
	5250 1600 5250 1700
Connection ~ 5250 1700
$Comp
L power:GND #PWR0116
U 1 1 5C98942C
P 4350 2400
F 0 "#PWR0116" H 4350 2150 50  0001 C CNN
F 1 "GND" H 4355 2227 50  0000 C CNN
F 2 "" H 4350 2400 50  0001 C CNN
F 3 "" H 4350 2400 50  0001 C CNN
	1    4350 2400
	1    0    0    -1  
$EndComp
Wire Wire Line
	4350 2400 4350 2300
Wire Wire Line
	4350 2300 4450 2300
Wire Wire Line
	4350 2300 4350 2100
Wire Wire Line
	4350 2100 4450 2100
Connection ~ 4350 2300
Wire Wire Line
	4350 2100 4350 1900
Wire Wire Line
	4350 1900 4450 1900
Connection ~ 4350 2100
Wire Wire Line
	4350 1900 4350 1700
Wire Wire Line
	4350 1700 4450 1700
Connection ~ 4350 1900
Text Label 4350 2700 2    50   ~ 0
RST
$Comp
L Switch:SW_Push SW5
U 1 1 5C9B0FA2
P 3750 2700
F 0 "SW5" H 3750 2985 50  0000 C CNN
F 1 "RST_SW" H 3750 2894 50  0000 C CNN
F 2 "Button_Switch_THT:SW_Tactile_SPST_Angled_PTS645Vx31-2LFS" H 3750 2900 50  0001 C CNN
F 3 "" H 3750 2900 50  0001 C CNN
	1    3750 2700
	1    0    0    -1  
$EndComp
$Comp
L Device:C C7
U 1 1 5C9B1140
P 3750 2850
F 0 "C7" V 3900 2950 50  0000 C CNN
F 1 "10uF" V 3900 2800 50  0000 C CNN
F 2 "Capacitor_SMD:C_0603_1608Metric_Pad1.05x0.95mm_HandSolder" H 3788 2700 50  0001 C CNN
F 3 "~" H 3750 2850 50  0001 C CNN
	1    3750 2850
	0    1    1    0   
$EndComp
$Comp
L Device:R R5
U 1 1 5C9BF5A2
P 4050 2450
F 0 "R5" V 4050 2500 50  0000 R CNN
F 1 "10kohm" V 4150 2600 50  0000 R CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" V 3980 2450 50  0001 C CNN
F 3 "~" H 4050 2450 50  0001 C CNN
	1    4050 2450
	-1   0    0    1   
$EndComp
Wire Wire Line
	4050 2600 4050 2700
Wire Wire Line
	4050 2700 4350 2700
Wire Wire Line
	3950 2700 4050 2700
Connection ~ 4050 2700
Wire Wire Line
	3900 2850 4050 2850
Wire Wire Line
	4050 2850 4050 2700
Wire Wire Line
	3600 2850 3450 2850
Wire Wire Line
	3450 2850 3450 2700
Wire Wire Line
	3450 2700 3550 2700
$Comp
L power:GND #PWR0117
U 1 1 5C9F8932
P 3450 2950
F 0 "#PWR0117" H 3450 2700 50  0001 C CNN
F 1 "GND" H 3455 2777 50  0000 C CNN
F 2 "" H 3450 2950 50  0001 C CNN
F 3 "" H 3450 2950 50  0001 C CNN
	1    3450 2950
	1    0    0    -1  
$EndComp
Wire Wire Line
	3450 2950 3450 2850
Connection ~ 3450 2850
$Comp
L power:+3V3 #PWR0118
U 1 1 5CA00D58
P 4050 2200
F 0 "#PWR0118" H 4050 2050 50  0001 C CNN
F 1 "+3V3" H 4065 2373 50  0000 C CNN
F 2 "" H 4050 2200 50  0001 C CNN
F 3 "" H 4050 2200 50  0001 C CNN
	1    4050 2200
	1    0    0    -1  
$EndComp
Wire Wire Line
	4050 2200 4050 2300
Text Label 5650 5100 0    50   ~ 0
SWDIO
Text Label 5650 5200 0    50   ~ 0
SWCLK
Text Label 4350 4000 2    50   ~ 0
BOOT1
Text Label 4350 2900 2    50   ~ 0
BOOT0
Text Label 5650 4600 0    50   ~ 0
JTDI
$Comp
L MCU_ST_STM32F1:STM32F103C8Tx U2
U 1 1 5C900C42
P 5050 4000
F 0 "U2" H 5600 2450 50  0000 C CNN
F 1 "STM32F103C8Tx" H 5550 2550 50  0000 C CNN
F 2 "Package_QFP:LQFP-48_7x7mm_P0.5mm" H 4450 2600 50  0001 R CNN
F 3 "http://www.st.com/st-web-ui/static/active/en/resource/technical/document/datasheet/CD00161566.pdf" H 5050 4000 50  0001 C CNN
	1    5050 4000
	1    0    0    -1  
$EndComp
Text Label 4350 4100 2    50   ~ 0
JTDO
Text Label 4350 4200 2    50   ~ 0
JNTRST
NoConn ~ 4350 3900
NoConn ~ 4350 3800
NoConn ~ 4350 3400
$Comp
L power:GND #PWR0121
U 1 1 5CBB001C
P 4850 5700
F 0 "#PWR0121" H 4850 5450 50  0001 C CNN
F 1 "GND" H 4855 5527 50  0000 C CNN
F 2 "" H 4850 5700 50  0001 C CNN
F 3 "" H 4850 5700 50  0001 C CNN
	1    4850 5700
	1    0    0    -1  
$EndComp
Wire Wire Line
	4850 5700 4850 5600
Wire Wire Line
	4950 5500 4950 5600
Wire Wire Line
	4950 5600 4850 5600
Connection ~ 4850 5600
Wire Wire Line
	4850 5600 4850 5500
Wire Wire Line
	5050 5500 5050 5600
Wire Wire Line
	5050 5600 4950 5600
Connection ~ 4950 5600
Wire Wire Line
	5150 5500 5150 5600
Wire Wire Line
	5150 5600 5050 5600
Connection ~ 5050 5600
Text Label 5100 850  2    50   ~ 0
RST
NoConn ~ 5100 950 
$Comp
L Connector_Generic:Conn_02x05_Counter_Clockwise J9
U 1 1 5CBD1DBC
P 5300 1050
F 0 "J9" H 5350 1467 50  0000 C CNN
F 1 "STLink" H 5350 1376 50  0000 C CNN
F 2 "Connector_PinSocket_2.54mm:PinSocket_2x05_P2.54mm_Horizontal" H 5300 1050 50  0001 C CNN
F 3 "~" H 5300 1050 50  0001 C CNN
	1    5300 1050
	1    0    0    -1  
$EndComp
Text Label 5600 850  0    50   ~ 0
SWCLK
Text Label 5600 950  0    50   ~ 0
SWDIO
$Comp
L power:GND #PWR02
U 1 1 5CBE6A34
P 5000 1050
F 0 "#PWR02" H 5000 800 50  0001 C CNN
F 1 "GND" V 5005 922 50  0000 R CNN
F 2 "" H 5000 1050 50  0001 C CNN
F 3 "" H 5000 1050 50  0001 C CNN
	1    5000 1050
	0    1    1    0   
$EndComp
Wire Wire Line
	5000 1050 5100 1050
$Comp
L power:GND #PWR09
U 1 1 5CBED9FD
P 5700 1050
F 0 "#PWR09" H 5700 800 50  0001 C CNN
F 1 "GND" V 5705 922 50  0000 R CNN
F 2 "" H 5700 1050 50  0001 C CNN
F 3 "" H 5700 1050 50  0001 C CNN
	1    5700 1050
	0    -1   -1   0   
$EndComp
Wire Wire Line
	5700 1050 5600 1050
$Comp
L power:+3.3V #PWR03
U 1 1 5CBF4A95
P 5000 1150
F 0 "#PWR03" H 5000 1000 50  0001 C CNN
F 1 "+3.3V" V 5015 1278 50  0000 L CNN
F 2 "" H 5000 1150 50  0001 C CNN
F 3 "" H 5000 1150 50  0001 C CNN
	1    5000 1150
	0    -1   -1   0   
$EndComp
Wire Wire Line
	5000 1150 5100 1150
$Comp
L power:+3.3V #PWR010
U 1 1 5CBFBC13
P 5700 1150
F 0 "#PWR010" H 5700 1000 50  0001 C CNN
F 1 "+3.3V" V 5715 1278 50  0000 L CNN
F 2 "" H 5700 1150 50  0001 C CNN
F 3 "" H 5700 1150 50  0001 C CNN
	1    5700 1150
	0    1    1    0   
$EndComp
Wire Wire Line
	5700 1150 5600 1150
$Comp
L power:+5V #PWR04
U 1 1 5CC02F35
P 5000 1250
F 0 "#PWR04" H 5000 1100 50  0001 C CNN
F 1 "+5V" V 5015 1378 50  0000 L CNN
F 2 "" H 5000 1250 50  0001 C CNN
F 3 "" H 5000 1250 50  0001 C CNN
	1    5000 1250
	0    -1   -1   0   
$EndComp
Wire Wire Line
	5000 1250 5100 1250
$Comp
L power:+5V #PWR011
U 1 1 5CC18D18
P 5700 1250
F 0 "#PWR011" H 5700 1100 50  0001 C CNN
F 1 "+5V" V 5715 1378 50  0000 L CNN
F 2 "" H 5700 1250 50  0001 C CNN
F 3 "" H 5700 1250 50  0001 C CNN
	1    5700 1250
	0    1    1    0   
$EndComp
Wire Wire Line
	5700 1250 5600 1250
$Comp
L Switch:SW_SPDT SW6
U 1 1 5CC2299D
P 10550 2350
F 0 "SW6" H 10550 2635 50  0000 C CNN
F 1 "BOOT0" H 10550 2544 50  0000 C CNN
F 2 "Button_Switch_THT:SW_E-Switch_EG1224_SPDT_Angled" H 10550 2350 50  0001 C CNN
F 3 "" H 10550 2350 50  0001 C CNN
	1    10550 2350
	1    0    0    -1  
$EndComp
Text Label 9950 2350 2    50   ~ 0
BOOT0
$Comp
L Device:R R6
U 1 1 5CC23566
P 10100 2350
F 0 "R6" V 10100 2350 50  0000 C CNN
F 1 "10kohm" V 9984 2350 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" V 10030 2350 50  0001 C CNN
F 3 "~" H 10100 2350 50  0001 C CNN
	1    10100 2350
	0    1    1    0   
$EndComp
Wire Wire Line
	10350 2350 10250 2350
$Comp
L Switch:SW_SPDT SW7
U 1 1 5CC32180
P 10550 2800
F 0 "SW7" H 10550 3085 50  0000 C CNN
F 1 "BOOT1" H 10550 2994 50  0000 C CNN
F 2 "Button_Switch_THT:SW_E-Switch_EG1224_SPDT_Angled" H 10550 2800 50  0001 C CNN
F 3 "" H 10550 2800 50  0001 C CNN
	1    10550 2800
	1    0    0    -1  
$EndComp
Text Label 9950 2800 2    50   ~ 0
BOOT1
$Comp
L Device:R R7
U 1 1 5CC32187
P 10100 2800
F 0 "R7" V 10100 2800 50  0000 C CNN
F 1 "10kohm" V 9984 2800 50  0000 C CNN
F 2 "Resistor_SMD:R_0603_1608Metric_Pad1.05x0.95mm_HandSolder" V 10030 2800 50  0001 C CNN
F 3 "~" H 10100 2800 50  0001 C CNN
	1    10100 2800
	0    1    1    0   
$EndComp
Wire Wire Line
	10350 2800 10250 2800
$Comp
L power:+3.3V #PWR012
U 1 1 5CC3A3BD
P 10850 2250
F 0 "#PWR012" H 10850 2100 50  0001 C CNN
F 1 "+3.3V" V 10865 2378 50  0000 L CNN
F 2 "" H 10850 2250 50  0001 C CNN
F 3 "" H 10850 2250 50  0001 C CNN
	1    10850 2250
	0    1    1    0   
$EndComp
$Comp
L power:GND #PWR013
U 1 1 5CC41C86
P 10850 2450
F 0 "#PWR013" H 10850 2200 50  0001 C CNN
F 1 "GND" V 10855 2322 50  0000 R CNN
F 2 "" H 10850 2450 50  0001 C CNN
F 3 "" H 10850 2450 50  0001 C CNN
	1    10850 2450
	0    -1   -1   0   
$EndComp
Wire Wire Line
	10850 2450 10750 2450
Wire Wire Line
	10750 2250 10850 2250
$Comp
L power:+3.3V #PWR014
U 1 1 5CC5139B
P 10850 2700
F 0 "#PWR014" H 10850 2550 50  0001 C CNN
F 1 "+3.3V" V 10865 2828 50  0000 L CNN
F 2 "" H 10850 2700 50  0001 C CNN
F 3 "" H 10850 2700 50  0001 C CNN
	1    10850 2700
	0    1    1    0   
$EndComp
$Comp
L power:GND #PWR015
U 1 1 5CC513A1
P 10850 2900
F 0 "#PWR015" H 10850 2650 50  0001 C CNN
F 1 "GND" V 10855 2772 50  0000 R CNN
F 2 "" H 10850 2900 50  0001 C CNN
F 3 "" H 10850 2900 50  0001 C CNN
	1    10850 2900
	0    -1   -1   0   
$EndComp
Wire Wire Line
	10850 2900 10750 2900
Wire Wire Line
	10750 2700 10850 2700
$Comp
L Connector_Generic:Conn_02x05_Counter_Clockwise J10
U 1 1 5CC59EA4
P 6700 1050
F 0 "J10" H 6750 1467 50  0000 C CNN
F 1 "JTAG" H 6750 1376 50  0000 C CNN
F 2 "Connector_PinSocket_2.54mm:PinSocket_2x05_P2.54mm_Horizontal" H 6700 1050 50  0001 C CNN
F 3 "~" H 6700 1050 50  0001 C CNN
	1    6700 1050
	1    0    0    -1  
$EndComp
$Comp
L power:+3.3V #PWR0122
U 1 1 5CC5A8E5
P 6400 850
F 0 "#PWR0122" H 6400 700 50  0001 C CNN
F 1 "+3.3V" V 6415 978 50  0000 L CNN
F 2 "" H 6400 850 50  0001 C CNN
F 3 "" H 6400 850 50  0001 C CNN
	1    6400 850 
	0    -1   -1   0   
$EndComp
Wire Wire Line
	6400 850  6500 850 
$Comp
L power:GND #PWR0123
U 1 1 5CC62963
P 6400 950
F 0 "#PWR0123" H 6400 700 50  0001 C CNN
F 1 "GND" V 6405 822 50  0000 R CNN
F 2 "" H 6400 950 50  0001 C CNN
F 3 "" H 6400 950 50  0001 C CNN
	1    6400 950 
	0    1    1    0   
$EndComp
Wire Wire Line
	6400 950  6500 950 
$Comp
L power:GND #PWR0124
U 1 1 5CC6A6E5
P 6400 1050
F 0 "#PWR0124" H 6400 800 50  0001 C CNN
F 1 "GND" V 6405 922 50  0000 R CNN
F 2 "" H 6400 1050 50  0001 C CNN
F 3 "" H 6400 1050 50  0001 C CNN
	1    6400 1050
	0    1    1    0   
$EndComp
Wire Wire Line
	6400 1050 6500 1050
Text Label 6500 1150 2    50   ~ 0
JNTRST
$Comp
L power:GND #PWR0125
U 1 1 5CC82C61
P 6400 1250
F 0 "#PWR0125" H 6400 1000 50  0001 C CNN
F 1 "GND" V 6405 1122 50  0000 R CNN
F 2 "" H 6400 1250 50  0001 C CNN
F 3 "" H 6400 1250 50  0001 C CNN
	1    6400 1250
	0    1    1    0   
$EndComp
Wire Wire Line
	6400 1250 6500 1250
Text Label 7000 1150 0    50   ~ 0
JTDI
Text Label 7000 1050 0    50   ~ 0
JTDO
Text Label 7000 950  0    50   ~ 0
SWCLK
Text Label 7000 850  0    50   ~ 0
SWDIO
NoConn ~ 7000 1250
$Comp
L power:PWR_FLAG #FLG0102
U 1 1 5CC9E8B0
P 1650 1500
F 0 "#FLG0102" H 1650 1575 50  0001 C CNN
F 1 "PWR_FLAG" H 1650 1673 50  0000 C CNN
F 2 "" H 1650 1500 50  0001 C CNN
F 3 "~" H 1650 1500 50  0001 C CNN
	1    1650 1500
	-1   0    0    1   
$EndComp
Wire Wire Line
	1650 1350 1650 1500
Connection ~ 1650 1350
$Comp
L power:+3.3V #PWR0126
U 1 1 5C91FBE1
P 9750 3550
F 0 "#PWR0126" H 9750 3400 50  0001 C CNN
F 1 "+3.3V" H 9765 3723 50  0000 C CNN
F 2 "" H 9750 3550 50  0001 C CNN
F 3 "" H 9750 3550 50  0001 C CNN
	1    9750 3550
	1    0    0    -1  
$EndComp
$Comp
L power:+3.3V #PWR0127
U 1 1 5C91FCBD
P 8850 4100
F 0 "#PWR0127" H 8850 3950 50  0001 C CNN
F 1 "+3.3V" H 8865 4273 50  0000 C CNN
F 2 "" H 8850 4100 50  0001 C CNN
F 3 "" H 8850 4100 50  0001 C CNN
	1    8850 4100
	1    0    0    -1  
$EndComp
NoConn ~ 4350 4800
Wire Wire Line
	5250 7100 6750 7100
$EndSCHEMATC