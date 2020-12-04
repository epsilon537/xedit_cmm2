@ECHO OFF

arm-none-eabi-gcc -c -mcpu=cortex-m7 -mfpu=fpv5-d16 -mfloat-abi=hard -mthumb -Wall -Wno-main -ffunction-sections -O2 -fPIC -I. %1.c -o %1.o
IF ERRORLEVEL 1 Goto Done

arm-none-eabi-ld -nostartfiles -T arm-gcc-link.ld -o %1.elf %1.o
IF ERRORLEVEL 1 Goto Done

C:\cmm2\DOS_MMBasic\MMBasic ArmCfGenV142.bas %1.elf %2 
Notepad %1.bas

:Done
