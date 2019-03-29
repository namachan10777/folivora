EESchema Schematic File Version 4
EELAYER 29 0
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
L Switch:SW_Push SW1
U 1 1 5C9E341B
P 6200 3250
F 0 "SW1" H 6200 3535 50  0000 C CNN
F 1 "Key" H 6200 3444 50  0000 C CNN
F 2 "folivora:MX_KailhLP" H 6200 3450 50  0001 C CNN
F 3 "~" H 6200 3450 50  0001 C CNN
	1    6200 3250
	1    0    0    -1  
$EndComp
$Comp
L Device:D D1
U 1 1 5C9E3741
P 6200 3550
F 0 "D1" H 6200 3334 50  0000 C CNN
F 1 "D" H 6200 3425 50  0000 C CNN
F 2 "Diode_SMD:D_SOD-123" H 6200 3550 50  0001 C CNN
F 3 "~" H 6200 3550 50  0001 C CNN
	1    6200 3550
	-1   0    0    1   
$EndComp
Wire Wire Line
	5900 3550 5900 3250
Wire Wire Line
	5900 3250 6000 3250
$Comp
L Connector:TestPoint TP5
U 1 1 5C9E55BB
P 6600 3250
F 0 "TP5" V 6554 3438 50  0000 L CNN
F 1 "READ" V 6645 3438 50  0000 L CNN
F 2 "TestPoint:TestPoint_Pad_2.0x2.0mm" H 6800 3250 50  0001 C CNN
F 3 "~" H 6800 3250 50  0001 C CNN
	1    6600 3250
	0    1    1    0   
$EndComp
$Comp
L Connector:TestPoint TP4
U 1 1 5C9E59D7
P 6600 3050
F 0 "TP4" V 6554 3238 50  0000 L CNN
F 1 "READ" V 6645 3238 50  0000 L CNN
F 2 "TestPoint:TestPoint_Pad_2.0x2.0mm" H 6800 3050 50  0001 C CNN
F 3 "~" H 6800 3050 50  0001 C CNN
	1    6600 3050
	0    1    1    0   
$EndComp
$Comp
L Connector:TestPoint TP6
U 1 1 5C9E6A62
P 6600 3550
F 0 "TP6" V 6554 3738 50  0000 L CNN
F 1 "SELECT" V 6645 3738 50  0000 L CNN
F 2 "TestPoint:TestPoint_Pad_2.0x2.0mm" H 6800 3550 50  0001 C CNN
F 3 "~" H 6800 3550 50  0001 C CNN
	1    6600 3550
	0    1    1    0   
$EndComp
$Comp
L Connector:TestPoint TP7
U 1 1 5C9E6E69
P 6600 3750
F 0 "TP7" V 6554 3938 50  0000 L CNN
F 1 "SELECT" V 6645 3938 50  0000 L CNN
F 2 "TestPoint:TestPoint_Pad_2.0x2.0mm" H 6800 3750 50  0001 C CNN
F 3 "~" H 6800 3750 50  0001 C CNN
	1    6600 3750
	0    1    1    0   
$EndComp
Wire Wire Line
	6600 3050 6500 3050
Wire Wire Line
	6500 3050 6500 3250
Wire Wire Line
	6500 3250 6400 3250
Wire Wire Line
	6500 3250 6600 3250
Connection ~ 6500 3250
Wire Wire Line
	6600 3550 6500 3550
Wire Wire Line
	6500 3550 6500 3750
Wire Wire Line
	6500 3750 6600 3750
Connection ~ 6500 3550
Wire Wire Line
	6500 3550 6350 3550
Wire Wire Line
	5900 3550 6050 3550
$Comp
L folivora:SK6812 U1
U 1 1 5C9EA580
P 6350 4500
F 0 "U1" H 6700 4300 50  0000 L CNN
F 1 "SK6812" H 6600 4200 50  0000 L CNN
F 2 "folivora:SK6812MINI_rev" H 5725 4400 50  0001 C CNN
F 3 "" H 5725 4400 50  0001 C CNN
	1    6350 4500
	1    0    0    -1  
$EndComp
$Comp
L Connector:TestPoint TP2
U 1 1 5C9EB8C2
P 6350 4100
F 0 "TP2" H 6408 4218 50  0000 L CNN
F 1 "Vdd" H 6408 4127 50  0000 L CNN
F 2 "TestPoint:TestPoint_Pad_2.0x2.0mm" H 6550 4100 50  0001 C CNN
F 3 "~" H 6550 4100 50  0001 C CNN
	1    6350 4100
	1    0    0    -1  
$EndComp
$Comp
L Connector:TestPoint TP3
U 1 1 5C9EBCAB
P 6350 4900
F 0 "TP3" H 6292 4926 50  0000 R CNN
F 1 "GND" H 6292 5017 50  0000 R CNN
F 2 "TestPoint:TestPoint_Pad_2.0x2.0mm" H 6550 4900 50  0001 C CNN
F 3 "~" H 6550 4900 50  0001 C CNN
	1    6350 4900
	-1   0    0    1   
$EndComp
$Comp
L Connector:TestPoint TP1
U 1 1 5C9EC24F
P 5900 4500
F 0 "TP1" V 6095 4572 50  0000 C CNN
F 1 "DIN" V 6004 4572 50  0000 C CNN
F 2 "TestPoint:TestPoint_Pad_2.0x2.0mm" H 6100 4500 50  0001 C CNN
F 3 "~" H 6100 4500 50  0001 C CNN
	1    5900 4500
	0    -1   -1   0   
$EndComp
$Comp
L Connector:TestPoint TP8
U 1 1 5C9EC86A
P 6800 4500
F 0 "TP8" V 6754 4688 50  0000 L CNN
F 1 "DOUT" V 6845 4688 50  0000 L CNN
F 2 "TestPoint:TestPoint_Pad_2.0x2.0mm" H 7000 4500 50  0001 C CNN
F 3 "~" H 7000 4500 50  0001 C CNN
	1    6800 4500
	0    1    1    0   
$EndComp
Wire Wire Line
	6800 4500 6700 4500
Wire Wire Line
	6350 4100 6350 4200
Wire Wire Line
	6350 4800 6350 4900
Wire Wire Line
	6000 4500 5900 4500
$EndSCHEMATC