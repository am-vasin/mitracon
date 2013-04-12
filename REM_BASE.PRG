*╔════════════════════════════════════════════════════════════════════════╗
*║ Файл Rem_Base     Разработчик Андрей Васин           14.01.98 16:45:56 ║
*╟────────────────────────────────────────────────────────────────────────╢
*║                                                                        ║
*║                 Очистка базы и восстановление баланса.                 ║
*║                                                                        ║
*╚════════════════════════════════════════════════════════════════════════╝
PROCEDURE Rem_Base

SET DATE GERMAN
SET DELETED ON
SET TALK OFF

base_path = "D:\ROZN\base\"
sys_char = "B"

USE (base_path+"PARMS")

CLEAR
@ 10, 10 SAY "Устанавливаем стартовую дату" GET DAT_START
READ
CLEAR

USE

IF READKEY() % 256 = 12
  RETURN
ENDIF

WAIT WINDOW " Продажи, давани, Андрюха, ANY KEY! " NOWAIT
DO Rem_Sal
WAIT WINDOW " Приход, давани, Андрюха, ANY KEY! " NOWAIT
DO Rem_Inc
WAIT WINDOW " Депозит, давани, Андрюха, ANY KEY! " NOWAIT
DO Rem_Acc
WAIT WINDOW " Рабочие места, давани, Андрюха, ANY KEY! " NOWAIT
DO Rem_Arm
WAIT WINDOW " Касса, давани, Андрюха, ANY KEY! " NOWAIT
DO Rem_Kas
WAIT WINDOW " Наличие, давани, Андрюха, ANY KEY! " NOWAIT
DO Rem_Nal
WAIT WINDOW " Удаляем! Давани, Андрюха, ANY KEY! " NOWAIT
DO Rem_Del

CLEAR

@ 10, 10 SAY "Все!"
